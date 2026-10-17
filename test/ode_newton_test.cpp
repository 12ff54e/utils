// Adapted from GEPT's tests/test_newton.cpp.
#include "ode/newton_checks.h"

#include <cstdlib>
#include <iostream>
#include <vector>

namespace {

using State = ode_test::NewtonState<double>;
using ScalarSystem = ode_test::NewtonSystem<double>;
using Equation = ode_test::NewtonEquation;

void check(bool condition, const char* message) {
    if (!condition) throw std::runtime_error(message);
}

template <typename T>
void test_shared_checks() {
    const auto results = ode_test::check_newton<T>();
    for (std::size_t i = 0; i < results.size(); ++i) {
        if (results[i].failures == 0) continue;
        std::cerr << "Newton case " << i << ", scalar bytes=" << sizeof(T)
                  << ": failures=" << results[i].failures
                  << ", value=" << results[i].value
                  << ", residual=" << results[i].residual << '\n';
        check(false, "shared Newton checks failed");
    }
}

struct RecordingSystem : ScalarSystem {
    mutable std::vector<State> samples;
    bool evaluate(const State& state, double& derivative) const {
        samples.push_back(state);
        return ScalarSystem::evaluate(state, derivative);
    }
};

template <typename Solver>
auto starting_guesses(
    const Solver& solver,
    const State& initial,
    double h,
    typename Solver::template StepCache<State, ScalarSystem> cache,
    bool warm = true) {
    constexpr auto EXTRA = Solver::predictor_type::EXTRA_STAGES == 3
                               ? 2
                               : Solver::predictor_type::EXTRA_STAGES;
    RecordingSystem system;
    system.equation = Equation::CUBIC;
    const auto extra =
        warm ? EXTRA : (Solver::predictor_type::EXTRA_STAGES == 3 ? 1 : 0);
    // Stop at the first residual evaluation, after observing the predictor.
    system.call_limit = extra + Solver::STAGES;
    State output;
    check(!solver.step(initial, h, system, output, cache,
                       ode_test::NewtonNorm<double>{},
                       ode::NewtonOptions<double>{}),
          "starting-guess probe unexpectedly succeeded");
    check(system.samples.size() == extra + Solver::STAGES + 1,
          "Newton did not initialize from the selected stage predictor");
    std::array<State, Solver::STAGES> result{};
    for (std::size_t i = 0; i < Solver::STAGES; ++i)
        result[i] = system.samples[extra + i];
    return result;
}

template <typename Predictor>
void test_transaction(Predictor predictor) {
    const ode::Integrator solver(ode::Gauss4Tableau<double>{}, predictor);
    using Cache =
        typename decltype(solver)::template StepCache<State, ScalarSystem>;
    using Stages =
        typename decltype(solver)::template Stages<State, ScalarSystem>;
    const ode_test::NewtonNorm<double> norm;
    const ode::NewtonOptions<double> options{1e-14, 20};
    ScalarSystem system;
    system.equation = Equation::CUBIC;
    Cache cache;
    State state{1, .25};
    constexpr double H = .03125;
    for (int i = 0; i < 12; ++i)
        check(solver.step(state, H, system, state, cache, norm, options),
              "Newton history warmup failed");
    const auto expected = starting_guesses(solver, state, H, cache);
    const State sentinel{91, 92};
    constexpr auto EXTRA =
        Predictor::EXTRA_STAGES == 3 ? 2 : Predictor::EXTRA_STAGES;
    for (std::size_t limit :
         {std::size_t{0}, EXTRA + solver.STAGES, EXTRA + 2 * solver.STAGES}) {
        auto failing = system;
        failing.calls = 0;
        failing.call_limit = limit;
        State output = sentinel;
        Stages stages;
        for (auto& stage : stages.states) stage = sentinel;
        stages.derivatives.fill(93);
        check(!solver.step(state, 2 * H, failing, output, cache, stages, norm,
                           options) &&
                  output == sentinel,
              "failed Newton RHS changed output");
        for (const auto& stage : stages.states)
            check(stage == sentinel, "failed Newton RHS changed stage records");
        for (double derivative : stages.derivatives)
            check(derivative == 93,
                  "failed Newton RHS changed derivative records");
        check(starting_guesses(solver, state, H, cache) == expected,
              "failed Newton solve changed predictor history");
    }
    State output = sentinel;
    const auto invalid_result_norm = [&](const State& a, const State& b) {
        if (a.time == state.time + H && b.time == a.time)
            return std::numeric_limits<double>::quiet_NaN();
        return norm(a, b);
    };
    check(!solver.step(state, H, system, output, cache, invalid_result_norm,
                       options) &&
              output == sentinel &&
              starting_guesses(solver, state, H, cache) == expected,
          "invalid Newton result changed output or cache");
    auto invalid = options;
    invalid.difference_step = 0;
    check(!solver.step(state, H, system, output, cache, norm, invalid) &&
              output == sentinel &&
              starting_guesses(solver, state, H, cache) == expected,
          "invalid Newton options changed output or cache");
    double h = H;
    check(!solver.step_adaptive(state, h, 2 * H, system, output, cache, norm,
                                norm, {1e-30, 1e-8, 1}, options) &&
              h == H && output == sentinel &&
              starting_guesses(solver, state, H, cache) == expected,
          "rejected Newton adaptive trial changed output or cache");
    auto fixed_cache = cache;
    State fixed, newton;
    check(
        solver.step(state, H, system, fixed, fixed_cache, norm, {1e-14, 100}) &&
            solver.step(state, H, system, newton, cache, norm, options) &&
            norm(fixed, newton) < 1e-13,
        "fixed-point and Newton iterations cannot share accepted history");
    cache.reset();
    check(starting_guesses(solver, state, H, cache, false) ==
              starting_guesses(solver, state, H, Cache{}, false),
          "Newton cache reset retained predictor history");
}

struct DegenerateTableau {
    using val_type = double;
    using row_type = std::array<double, 2>;
    using matrix_type = std::array<row_type, 2>;
    static constexpr std::size_t STAGES = 2, ORDER = 1;
    static constexpr bool IMPLICIT = true;
    matrix_type A{};
    row_type b{0, 1}, c{0, 0};
    matrix_type beta{{{1, 0}, {2, 0}}};
};

void test_nullspace() {
    const ode::Integrator solver(DegenerateTableau{});
    ScalarSystem system;
    system.equation = Equation::EXPONENTIAL;
    decltype(solver)::StepCache<State, ScalarSystem> cache;
    decltype(solver)::Stages<State, ScalarSystem> stages;
    State state{1, 0};
    for (int i = 0; i < 2; ++i)
        check(solver.step(state, .1, system, state, cache, stages,
                          ode_test::NewtonNorm<double>{},
                          ode::NewtonOptions<double>{}),
              "degenerate implicit solve failed");
    check(
        std::abs(state.value - 1.21) < 1e-14 &&
            stages.states[1].value == stages.derivatives[1],
        "Newton published arbitrary k instead of fresh F(k) in A's nullspace");
}

template <typename T>
struct TangentSystem : ode_test::NewtonSystem<T> {
    bool evaluate(const ode_test::NewtonState<T>& state, T& derivative) const {
        derivative = 1 + state.value * state.value;
        return std::isfinite(derivative);
    }
};

template <typename Tableau>
void test_nonlinear_order() {
    using T = typename Tableau::val_type;
    const ode::Integrator solver(Tableau{});
    const TangentSystem<T> system;
    const auto error = [&](int steps) {
        typename decltype(solver)::template StepCache<ode_test::NewtonState<T>,
                                                      TangentSystem<T>>
            cache;
        ode_test::NewtonState<T> state;
        for (int i = 0; i < steps; ++i)
            check(solver.step(state, T{1} / T(steps), system, state, cache,
                              ode_test::NewtonNorm<T>{},
                              ode::NewtonOptions<T>{
                                  4 * std::numeric_limits<T>::epsilon(), 30}),
                  "nonlinear Newton solve failed");
        return std::abs(state.value - std::tan(T{1}));
    };
    constexpr int STEPS = Tableau::STAGES >= 5 ? 2 : 4;
    const T coarse = error(STEPS), fine = error(2 * STEPS);
    const T order = std::log2(coarse / fine);
    std::cout << "Newton Gauss " << Tableau::STAGES << ": order " << order
              << '\n';
    check(fine > 64 * std::numeric_limits<T>::epsilon() &&
              std::abs(order - T(Tableau::ORDER)) < T{0.5},
          "Newton iteration lost the RK convergence order");
}

struct OpaqueDerivative {
    double value = 0;
};
using PairState = ode_test::NewtonPairState<double>;

struct OpaqueSeparable {
    using first_derivative_type = OpaqueDerivative;
    using second_derivative_type = OpaqueDerivative;
    static constexpr bool SEPARABLE = true;

    bool evaluate_first(const PairState& state,
                        OpaqueDerivative& derivative) const {
        derivative.value = state.p;
        return true;
    }
    bool evaluate_second(const PairState& state,
                         OpaqueDerivative& derivative) const {
        derivative.value = -state.q;
        return true;
    }
    template <std::size_t N>
    PairState advance(PairState state,
                      const std::array<OpaqueDerivative, N>& first,
                      const std::array<OpaqueDerivative, N>& second,
                      const std::array<double, N>& a,
                      const std::array<double, N>& b,
                      std::size_t count,
                      double h,
                      double c_first,
                      double c_second) const {
        for (std::size_t j = 0; j < count; ++j) {
            state.q += h * a[j] * first[j].value;
            state.p += h * b[j] * second[j].value;
        }
        state.first_time += h * c_first;
        state.second_time += h * c_second;
        return state;
    }
};

struct OpaqueDirect : OpaqueSeparable {
    mutable std::size_t calls = 0;
    bool solve_stage(PairState& state, double a, double b, std::size_t) const {
        ++calls;
        state.p = (state.p - b * state.q) / (1 + a * b);
        state.q += a * state.p;
        return true;
    }
};

struct OpaqueCoupled : OpaqueSeparable {
    static constexpr bool SEPARABLE = false;
};

void test_precedence() {
    const ode_test::NewtonPairNorm<double> norm;
    const ode::NewtonOptions<double> options;
    const ode::PartitionedIntegrator verlet(
        ode::PositionVerletTableau<double>{});
    const ode::PartitionedIntegrator midpoint(
        ode::ImplicitMidpointPairTableau<double>{});
    PairState state;
    check(verlet.step(state, .1, OpaqueSeparable{}, state, norm, options) &&
              std::abs(state.q - .995) < 1e-14 &&
              std::abs(state.p + .1) < 1e-14,
          "Newton options broke unmappable separable systems");
    state = {};
    const OpaqueDirect direct;
    check(midpoint.step(state, 1., direct, state, norm, options) &&
              direct.calls == 1 && std::abs(state.q - .6) < 1e-14 &&
              std::abs(state.p + .8) < 1e-14,
          "Newton options bypassed the direct stage solver");
    const PairState sentinel{91, 92, 93, 94};
    state = sentinel;
    check(!midpoint.step(PairState{}, 1., OpaqueCoupled{}, state, norm,
                         options) &&
              norm(state, sentinel) == 0,
          "unmappable coupled PRK did not fail without changing output");
}

}  // namespace

int main() {
    try {
        test_shared_checks<float>();
        test_shared_checks<double>();
        test_shared_checks<long double>();
        test_transaction(ode::ContinuousOutputPredictor{});
        test_transaction(ode::AdditionalStagePredictor<1>{});
        test_transaction(ode::AdditionalStagePredictor<3>{});
        test_transaction(ode::EquistagePredictor<8>{});
        test_nullspace();
        test_nonlinear_order<ode::Gauss4Tableau<double>>();
        test_nonlinear_order<ode::Gauss12Tableau<long double>>();
        test_precedence();
    } catch (const std::exception& error) {
        std::cerr << "test_newton: " << error.what() << '\n';
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
