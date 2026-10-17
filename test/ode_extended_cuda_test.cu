// Adapted from GEPT's tests/test_cuda_ode.cu.
#include "numerics/ode.h"
#include "ode/explicit_adaptive_checks.h"
#include "ode/finite_error.h"
#include "ode/typed_event_checks.h"

#include "ode/cuda_support.h"

#include <iostream>
#include <memory>

namespace {

struct State {
    double x = 1;
    double v = 0;
};

struct PartitionedSystem {
    using first_derivative_type = double;
    using second_derivative_type = double;

    __host__ __device__ bool evaluate_first(const State& state,
                                            double& derivative) const {
        derivative = state.v;
        return true;
    }
    __host__ __device__ bool evaluate_second(const State& state,
                                             double& derivative) const {
        derivative = -state.x;
        return true;
    }
    template <std::size_t N>
    __host__ __device__ State
    advance(const State& initial,
            const std::array<double, N>& first,
            const std::array<double, N>& second,
            const std::array<double, N>& first_weights,
            const std::array<double, N>& second_weights,
            std::size_t count,
            double h,
            double,
            double) const {
        auto state = initial;
        for (std::size_t i = 0; i < count; ++i) {
            state.x += h * first_weights[i] * first[i];
            state.v += h * second_weights[i] * second[i];
        }
        return state;
    }
};

struct System : PartitionedSystem {
    __host__ __device__ bool solve_stage(State& state,
                                         double h_first,
                                         double h_second,
                                         std::size_t) const {
        state.v = (state.v - h_second * state.x) / (1 + h_first * h_second);
        state.x += h_first * state.v;
        return true;
    }
};

struct SeparableSystem : PartitionedSystem {
    static constexpr bool SEPARABLE = true;
};

struct StateNorm {
    __host__ __device__ double operator()(const State& a,
                                          const State& b) const {
        return ode_test::finite_max(std::abs(a.x - b.x), std::abs(a.v - b.v));
    }
};

template <typename Tableau,
          typename OrbitSystem = SeparableSystem,
          bool ITERATE = false>
__host__ __device__ bool partitioned_orbit(State& forward, State& backward) {
    const ode::PartitionedIntegrator solver(Tableau{});
    const OrbitSystem system;
    State state;
    const auto step = [&](double h) {
        if constexpr (ITERATE)
            return solver.step(state, h, system, state, StateNorm{},
                               {1e-14, 100});
        else
            return solver.step(state, h, system, state);
    };
    for (int i = 0; i < 200; ++i)
        if (!step(0.005)) return false;
    forward = state;
    for (int i = 0; i < 200; ++i)
        if (!step(-0.005)) return false;
    backward = state;
    return true;
}

struct DecaySystem {
    using derivative_type = double;

    __host__ __device__ bool evaluate(double state, double& derivative) const {
        derivative = -state;
        return true;
    }
    template <std::size_t N>
    __host__ __device__ double advance(double state,
                                       const std::array<double, N>& derivatives,
                                       const std::array<double, N>& weights,
                                       std::size_t count,
                                       double h,
                                       double) const {
        for (std::size_t i = 0; i < count; ++i)
            state += h * weights[i] * derivatives[i];
        return state;
    }
};

struct ScalarNorm {
    __host__ __device__ double operator()(double a, double b) const {
        return std::abs(a - b);
    }
};

struct ClockState {
    double time;
};

template <typename Tableau>
__host__ __device__ bool gauss_orbit(double& forward, double& backward) {
    const ode::Integrator solver(Tableau{});
    const DecaySystem system;
    typename decltype(solver)::template StepCache<double, DecaySystem> cache;
    // Two cycles sum to one and exercise changing-step predictors.
    constexpr std::array steps{0.0625, 0.03125, 0.125, 0.03125, 0.125, 0.125};
    double state = 1;
    for (int cycle = 0; cycle < 2; ++cycle)
        for (double h : steps)
            if (!solver.step(state, h, system, state, cache, ScalarNorm{},
                             {1e-14, 100}))
                return false;
    forward = state;
    cache.reset();
    for (int cycle = 0; cycle < 2; ++cycle)
        for (std::size_t i = steps.size(); i > 0; --i)
            if (!solver.step(state, -steps[i - 1], system, state, cache,
                             ScalarNorm{}, {1e-14, 100}))
                return false;
    backward = state;
    return true;
}

struct Result {
    State forward;
    State backward;
    std::array<State, 6> partitioned_forward{};
    std::array<State, 6> partitioned_backward{};
    std::array<double, 4> gauss_forward{};
    std::array<double, 4> gauss_backward{};
    bool valid = false;
};

__host__ __device__ Result integrate_orbit() {
    const ode::PartitionedIntegrator<double> solver;
    const System system;
    State state;
    Result result;
    bool valid = true;
    for (int i = 0; i < 200; ++i)
        valid &= solver.step(state, 0.005, system, state);
    result.forward = state;
    for (int i = 0; i < 200; ++i)
        valid &= solver.step(state, -0.005, system, state);
    result.backward = state;
    result.valid = valid;
    result.valid &= partitioned_orbit<ode::PositionVerletTableau<double>>(
        result.partitioned_forward[0], result.partitioned_backward[0]);
    result.valid &= partitioned_orbit<ode::VelocityVerletTableau<double>>(
        result.partitioned_forward[1], result.partitioned_backward[1]);
    result.valid &= partitioned_orbit<ode::BorisTableau<double>>(
        result.partitioned_forward[2], result.partitioned_backward[2]);
    result.valid &= partitioned_orbit<ode::LobattoIIIAIIIB4Tableau<double>,
                                      PartitionedSystem, true>(
        result.partitioned_forward[3], result.partitioned_backward[3]);
    result.valid &= partitioned_orbit<ode::ImplicitMidpointPairTableau<double>,
                                      PartitionedSystem, true>(
        result.partitioned_forward[4], result.partitioned_backward[4]);
    result.valid &=
        partitioned_orbit<ode::ImplicitMidpointPairTableau<double>, System>(
            result.partitioned_forward[5], result.partitioned_backward[5]);
    result.valid &= gauss_orbit<ode::Gauss6Tableau<double>>(
        result.gauss_forward[0], result.gauss_backward[0]);
    result.valid &= gauss_orbit<ode::Gauss8Tableau<double>>(
        result.gauss_forward[1], result.gauss_backward[1]);
    result.valid &= gauss_orbit<ode::Gauss10Tableau<double>>(
        result.gauss_forward[2], result.gauss_backward[2]);
    result.valid &= gauss_orbit<ode::Gauss12Tableau<double>>(
        result.gauss_forward[3], result.gauss_backward[3]);
    return result;
}

__global__ void integrate(Result* result) {
    *result = integrate_orbit();
}

__global__ void dispatch_events(ode_test::TypedEventChecks* results,
                                std::size_t count) {
    const std::size_t i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < count) results[i] = ode_test::check_typed_events(i * .125);
}

bool checked(cudaError_t status) {
    if (status == cudaSuccess) return true;
    std::cerr << cudaGetErrorString(status) << '\n';
    return false;
}

template <typename Tableau>
__global__ void explicit_adaptive_kernel(
    ode_test::ExplicitAdaptiveChecks* results,
    std::size_t count) {
    const auto i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i >= count) return;
    using T = typename Tableau::val_type;
    results[i] = ode_test::check_explicit_adaptive<Tableau>(T(i) / (8 * count));
}

template <typename Tableau>
bool test_explicit_adaptive_cuda() {
    using T = typename Tableau::val_type;
    using Checks = ode_test::ExplicitAdaptiveChecks;
    constexpr std::size_t COUNT = 256;
    std::array<Checks, COUNT> results{};
    Checks* d_results = nullptr;
    if (!checked(cudaMalloc(&d_results, sizeof(results)))) return false;
    bool success = false, freed = false;
    {
        const auto release = [&](Checks* pointer) {
            freed = checked(cudaFree(pointer));
        };
        const std::unique_ptr<Checks, decltype(release)> allocation(d_results,
                                                                    release);
        explicit_adaptive_kernel<Tableau><<<2, 128>>>(allocation.get(), COUNT);
        success = checked(cudaGetLastError()) &&
                  checked(cudaMemcpy(results.data(), allocation.get(),
                                     sizeof(results), cudaMemcpyDeviceToHost));
    }
    if (!success || !freed) return false;
    constexpr T TOLERANCE =
        64 * (std::numeric_limits<T>::digits <= 24 ? T{1e-5} : T{1e-9}) +
        4096 * std::numeric_limits<T>::epsilon();
    for (std::size_t i = 0; i < COUNT; ++i) {
        const auto reference =
            ode_test::check_explicit_adaptive<Tableau>(T(i) / (8 * COUNT));
        const auto& actual = results[i];
        if (actual.failures != 0 || reference.failures != 0) {
            std::cerr << "CUDA explicit adaptive RK" << Tableau::ORDER
                      << ", scalar bytes=" << sizeof(T) << ", case=" << i
                      << ": GPU mask=" << actual.failures
                      << ", CPU mask=" << reference.failures << '\n';
            return false;
        }
        for (std::size_t j = 0; j < actual.values.size(); ++j)
            if (!(std::abs(actual.values[j] - reference.values[j]) <=
                  TOLERANCE)) {
                std::cerr << "CPU/CUDA explicit adaptive result differs\n";
                return false;
            }
    }
    return true;
}

}  // namespace

int main() {
    int calls = 0;
    const std::array events{ode::Event<ClockState>{
        [](const ClockState& state) { return state.time - 0.5; },
        [&](const ClockState&) { ++calls; }}};
    const auto stop = ode::EventDispatcher<ClockState>(events).advance(
        {0}, {1}, [](double fraction) { return ClockState{fraction}; });
    if (stop || calls != 1) return 1;
    if (!ode_test::require_cuda_device()) return EXIT_FAILURE;
    if (!test_explicit_adaptive_cuda<ode::RungeKutta2Tableau<float>>() ||
        !test_explicit_adaptive_cuda<ode::RungeKutta3Tableau<float>>() ||
        !test_explicit_adaptive_cuda<ode::RungeKutta4Tableau<float>>() ||
        !test_explicit_adaptive_cuda<ode::RungeKutta2Tableau<double>>() ||
        !test_explicit_adaptive_cuda<ode::RungeKutta3Tableau<double>>() ||
        !test_explicit_adaptive_cuda<ode::RungeKutta4Tableau<double>>())
        return 1;
    Result* d_result = nullptr;
    if (!checked(cudaMalloc(&d_result, sizeof(Result)))) return 1;
    Result result;
    bool success;
    bool freed = false;
    {
        const auto release = [&](Result* pointer) {
            freed = checked(cudaFree(pointer));
        };
        const std::unique_ptr<Result, decltype(release)> allocation(d_result,
                                                                    release);
        integrate<<<1, 1>>>(allocation.get());
        success = checked(cudaGetLastError()) &&
                  checked(cudaMemcpy(&result, allocation.get(), sizeof(Result),
                                     cudaMemcpyDeviceToHost));
    }
    const Result reference = integrate_orbit();
    if (!success || !freed || !result.valid || !reference.valid ||
        !(std::abs(result.forward.x - std::cos(1.0)) < 1e-5) ||
        !(std::abs(result.forward.v + std::sin(1.0)) < 1e-5) ||
        !(std::abs(result.backward.x - 1) < 1e-12) ||
        !(std::abs(result.backward.v) < 1e-12) ||
        !(std::abs(result.forward.x - reference.forward.x) < 2e-13) ||
        !(std::abs(result.forward.v - reference.forward.v) < 2e-13) ||
        !(std::abs(result.backward.x - reference.backward.x) < 2e-13) ||
        !(std::abs(result.backward.v - reference.backward.v) < 2e-13)) {
        std::cerr << "CUDA ODE solve failed: valid=" << result.valid
                  << ", forward=(" << result.forward.x << ", "
                  << result.forward.v << "), backward=(" << result.backward.x
                  << ", " << result.backward.v << ")\n";
        return 1;
    }
    for (std::size_t i = 0; i < result.partitioned_forward.size(); ++i) {
        const auto forward = result.partitioned_forward[i];
        const auto backward = result.partitioned_backward[i];
        const auto h_forward = reference.partitioned_forward[i];
        const auto h_backward = reference.partitioned_backward[i];
        if (!(std::abs(forward.x - std::cos(1.0)) < 1e-5) ||
            !(std::abs(forward.v + std::sin(1.0)) < 1e-5) ||
            !(std::abs(backward.x - 1) < 1e-12) ||
            !(std::abs(backward.v) < 1e-12) ||
            !(std::abs(forward.x - h_forward.x) < 2e-13) ||
            !(std::abs(forward.v - h_forward.v) < 2e-13) ||
            !(std::abs(backward.x - h_backward.x) < 2e-13) ||
            !(std::abs(backward.v - h_backward.v) < 2e-13)) {
            std::cerr << "CUDA partitioned RK pair " << i << " failed\n";
            return 1;
        }
    }
    for (std::size_t i = 0; i < result.gauss_forward.size(); ++i)
        if (!(std::abs(result.gauss_forward[i] - std::exp(-1.0)) < 2e-10) ||
            !(std::abs(result.gauss_backward[i] - 1) < 2e-12) ||
            !(std::abs(result.gauss_forward[i] - reference.gauss_forward[i]) <
              2e-13) ||
            !(std::abs(result.gauss_backward[i] - reference.gauss_backward[i]) <
              2e-13)) {
            std::cerr << "CUDA Gauss " << i + 3 << " stages: forward error="
                      << result.gauss_forward[i] - std::exp(-1.0)
                      << ", reversal error=" << result.gauss_backward[i] - 1
                      << '\n';
            return 1;
        }
    // Cross block boundaries and vary each thread's trajectory time origin.
    constexpr std::size_t EVENT_COUNT = 513;
    using EventChecks = ode_test::TypedEventChecks;
    EventChecks* d_events = nullptr;
    if (!checked(cudaMalloc(&d_events, EVENT_COUNT * sizeof(EventChecks))))
        return 1;
    std::array<EventChecks, EVENT_COUNT> event_results{};
    freed = false;
    {
        const auto release = [&](EventChecks* pointer) {
            freed = checked(cudaFree(pointer));
        };
        const std::unique_ptr<EventChecks, decltype(release)> allocation(
            d_events, release);
        dispatch_events<<<3, 256>>>(allocation.get(), EVENT_COUNT);
        success =
            checked(cudaGetLastError()) &&
            checked(cudaMemcpy(event_results.data(), allocation.get(),
                               sizeof(event_results), cudaMemcpyDeviceToHost));
    }
    if (!success || !freed) return 1;
    for (std::size_t i = 0; i < EVENT_COUNT; ++i) {
        const auto expected = ode_test::check_typed_events(i * .125);
        const auto actual = event_results[i];
        if (!expected.passed() || !actual.passed() ||
            !(std::abs(actual.stop_time - expected.stop_time) < 2e-10)) {
            std::cerr << "CUDA typed event dispatch failed for thread " << i
                      << ": order=" << actual.ordering
                      << ", stop=" << actual.stopping
                      << ", reuse=" << actual.reuse
                      << ", nonlinear=" << actual.nonlinear
                      << ", statuses=" << actual.statuses
                      << ", atomicity=" << actual.atomicity
                      << ", empty=" << actual.empty << '\n';
            return 1;
        }
    }
    return 0;
}
