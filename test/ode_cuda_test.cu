#include "numerics/ode.h"

#include "ode/cuda_support.h"

#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <stdexcept>

namespace {

template <typename T>
struct State {
    using val_type = T;

    val_type value = 0;
    val_type time = 0;
};

// y' = (1 + t)y, y(0) = 1 has solution exp(t + t^2/2).
template <typename T>
struct System {
    using val_type = T;
    using derivative_type = val_type;

    __host__ __device__ bool evaluate(const State<T>& state,
                                      derivative_type& derivative) const {
        derivative = (1 + state.time) * state.value;
        return true;
    }

    template <std::size_t N>
    __host__ __device__ State<T> advance(
        State<T> state,
        const std::array<derivative_type, N>& derivatives,
        const std::array<val_type, N>& weights,
        std::size_t active_stages,
        val_type h,
        val_type time_fraction) const {
        for (std::size_t i = 0; i < active_stages; ++i)
            state.value += h * weights[i] * derivatives[i];
        state.time += h * time_fraction;
        return state;
    }
};

template <typename T>
struct Norm {
    using val_type = T;

    __host__ __device__ val_type operator()(const State<T>& a,
                                            const State<T>& b) const {
        return std::abs(a.value - b.value);
    }
};

template <typename T>
struct Result {
    using val_type = T;

    State<T> fixed;
    State<T> adaptive;
    val_type h = 0;
    bool success = false;
};

template <typename Tableau>
__host__ __device__ Result<typename Tableau::val_type> solve() {
    using T = typename Tableau::val_type;
    using Solver = ode::Integrator<T, Tableau>;
    const Solver solver;
    const System<T> system;
    const Norm<T> norm;
    typename Solver::template StepCache<State<T>, System<T>> cache;
    typename Solver::template Stages<State<T>, System<T>> stages{};
    const ode::FixedPointOptions<T> iteration{
        8 * std::numeric_limits<T>::epsilon(), 100};
    Result<T> result;
    result.fixed = {1, 0};
    for (int i = 0; i < 256; ++i) {
        if (!solver.step(result.fixed, T{1} / 256, system, result.fixed, cache,
                         stages, norm, iteration))
            return result;
    }
    if constexpr (Solver::IMPLICIT) {
        cache.reset();
        result.adaptive = {1, 0};
        result.h = T{0.25};
        if (!solver.step_adaptive(result.adaptive, result.h, T{0.5}, system,
                                  result.adaptive, cache, norm, norm,
                                  {T{1e-4}, T{1e-3}, 30}, iteration))
            return result;
    }
    result.success = true;
    return result;
}

template <typename Tableau>
__global__ void integrate(Result<typename Tableau::val_type>* result) {
    *result = solve<Tableau>();
}

void check_cuda(cudaError_t status) {
    if (status != cudaSuccess)
        throw std::runtime_error(cudaGetErrorString(status));
}

template <typename T>
struct DeviceResult {
    using val_type = T;

    Result<T>* data = nullptr;

    DeviceResult() { check_cuda(cudaMalloc(&data, sizeof(Result<T>))); }
    ~DeviceResult() { cudaFree(data); }
    DeviceResult(const DeviceResult&) = delete;
    DeviceResult& operator=(const DeviceResult&) = delete;
};

template <typename Tableau>
void test_method(const char* name) {
    using T = typename Tableau::val_type;
    const auto host = solve<Tableau>();
    DeviceResult<T> device;
    integrate<Tableau><<<1, 1>>>(device.data);
    check_cuda(cudaGetLastError());
    check_cuda(cudaDeviceSynchronize());
    Result<T> result;
    check_cuda(cudaMemcpy(&result, device.data, sizeof(result),
                          cudaMemcpyDeviceToHost));
    if (!host.success || !result.success)
        throw std::runtime_error("CPU or CUDA integration failed");

    std::cout << name << (std::is_same_v<T, float> ? " float" : " double")
              << ": fixed error="
              << std::abs(result.fixed.value - std::exp(1.5));
    if constexpr (Tableau::IMPLICIT) {
        const double t = result.adaptive.time;
        std::cout << ", adaptive error="
                  << std::abs(result.adaptive.value - std::exp(t + t * t / 2))
                  << ", h=" << result.h << ", CPU h=" << host.h
                  << ", CPU value difference="
                  << std::abs(result.adaptive.value - host.adaptive.value);
    }
    std::cout << '\n';

    const double tolerance = std::is_same_v<T, float> ? 2e-4
                             : Tableau::ORDER == 2    ? 1e-4
                             : Tableau::ORDER == 3    ? 5e-7
                                                      : 5e-10;
    const double agreement = std::is_same_v<T, float> ? 1e-4 : 1e-11;
    if (result.fixed.time != 1 ||
        !(std::abs(result.fixed.value - std::exp(1.5)) <= tolerance) ||
        !(std::abs(result.fixed.value - host.fixed.value) <= agreement))
        throw std::runtime_error(
            "CUDA fixed step disagrees with exact/CPU solution");

    if constexpr (Tableau::IMPLICIT) {
        const double t = result.adaptive.time;
        // The 1e-4 defect target permits much larger steps than fixed 1/256.
        const double adaptive_tolerance =
            std::is_same_v<T, float> ? 2e-5 : 1e-7;
        if (!(result.h > 0 && result.h < T{0.5}) || t != result.h ||
            !(std::abs(result.adaptive.value - std::exp(t + t * t / 2)) <=
              adaptive_tolerance) ||
            !(std::abs(result.h - host.h) <= agreement) ||
            !(std::abs(result.adaptive.value - host.adaptive.value) <=
              agreement))
            throw std::runtime_error(
                "CUDA adaptive step disagrees with exact/CPU solution");
    }
}

template <typename T>
void test_precision() {
    test_method<ode::RungeKutta2Tableau<T>>("RK2");
    test_method<ode::RungeKutta3Tableau<T>>("RK3");
    test_method<ode::RungeKutta4Tableau<T>>("RK4");
    test_method<ode::LobattoIIIA4Tableau<T>>("Lobatto IIIA");
    test_method<ode::LobattoIIIB4Tableau<T>>("Lobatto IIIB");
    test_method<ode::Gauss4Tableau<T>>("Gauss");
}

}  // namespace

int main() {
    if (!ode_test::require_cuda_device()) return EXIT_FAILURE;
    std::cout << std::setprecision(16);
    try {
        test_precision<double>();
        test_precision<float>();
    } catch (const std::exception& error) {
        std::cerr << "ode_cuda_test: " << error.what() << '\n';
        return EXIT_FAILURE;
    }
    std::cout << "All six methods passed on CUDA in float and double.\n";
    return EXIT_SUCCESS;
}
