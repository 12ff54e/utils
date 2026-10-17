#include "numerics/ode.h"

#include <cmath>
#include <cstdlib>
#include <iostream>
#include <limits>
#include <stdexcept>
#include <vector>

namespace {

template <typename T>
struct ExplicitMidpointTableau {
    using val_type = T;

    static constexpr std::size_t ORDER = 2;
    static constexpr std::size_t STAGES = 2;
    static constexpr val_type HALF = 0.5;

    using row_type = std::array<val_type, STAGES>;
    using matrix_type = std::array<row_type, STAGES>;

    matrix_type A{{
        {0, 0},
        {HALF, 0},
    }};
    row_type b{0, 1};
    row_type c{0, HALF};
};

template <typename T>
struct ImplicitMidpointTableau {
    using val_type = T;

    static constexpr std::size_t ORDER = 2;
    static constexpr std::size_t STAGES = 1;
    static constexpr bool IMPLICIT = true;

    using row_type = std::array<val_type, STAGES>;
    using matrix_type = std::array<row_type, STAGES>;

    matrix_type A{{{val_type{1} / 2}}};
    row_type b{1};
    row_type c{val_type{1} / 2};
    matrix_type beta{{{val_type{3} / 2}}};
};

template <typename T>
struct ExponentialSystem {
    using val_type = T;
    using derivative_type = val_type;

    bool evaluate(val_type state, val_type& derivative) const {
        derivative = state;
        return true;
    }

    template <std::size_t STAGES>
    val_type advance(val_type state,
                     const std::array<val_type, STAGES>& derivatives,
                     const std::array<val_type, STAGES>& weights,
                     std::size_t active_stages,
                     val_type time_step,
                     val_type time_fraction) const {
        static_cast<void>(time_fraction);
        for (std::size_t i = 0; i < STAGES; ++i) {
            if (i == active_stages) break;
            state += time_step * weights[i] * derivatives[i];
        }
        return state;
    }
};

template <typename T, typename Tableau = ode::DefaultButcherTableau<T>>
T integrate(T time_step) {
    const ode::Integrator integrator(Tableau{});
    const ExponentialSystem<T> system;
    typename decltype(integrator)::template StepCache<T, ExponentialSystem<T>>
        cache;
    const auto error_norm = [](T lhs, T rhs) { return std::abs(lhs - rhs); };
    T state = 1;
    const int steps = static_cast<int>(std::lround(T{1} / time_step));
    for (int step = 0; step < steps; ++step) {
        T next_state = 0;
        if (!integrator.step(state, time_step, system, next_state, cache,
                             error_norm)) {
            return 0.0;
        }
        state = next_state;
    }
    return state;
}

void check(bool condition, const char* message) {
    if (!condition) throw std::runtime_error(message);
}

template <template <typename> typename Tableau>
void test_implicit_order() {
    const double exact = std::exp(1.0);
    const double coarse =
        std::abs(integrate<double, Tableau<double>>(0.1) - exact);
    const double fine =
        std::abs(integrate<double, Tableau<double>>(0.05) - exact);
    check(coarse / fine > 15.5 && coarse / fine < 16.5,
          "implicit method must converge at fourth order");
    const double midpoint_coarse = std::abs(
        integrate<double, ImplicitMidpointTableau<double>>(0.1) - exact);
    const double midpoint_fine = std::abs(
        integrate<double, ImplicitMidpointTableau<double>>(0.05) - exact);
    check(midpoint_coarse / midpoint_fine > 3.9 &&
              midpoint_coarse / midpoint_fine < 4.1,
          "custom implicit midpoint must converge at second order");
    check(std::abs(integrate<float, Tableau<float>>(0.05F) - std::exp(1.0F)) <
              1.0e-5F,
          "float implicit integration is inaccurate");
}

struct LogisticSystem : ExponentialSystem<double> {
    bool evaluate(double state, double& derivative) const {
        derivative = state * (1.0 - state);
        return std::isfinite(derivative);
    }
};

template <typename Tableau>
void test_nonlinear_order() {
    const ode::Integrator integrator(Tableau{});
    const LogisticSystem system;
    const auto error_norm = [](double a, double b) { return std::abs(a - b); };
    const auto solve = [&](double time_step, bool predict) {
        typename decltype(integrator)::template StepCache<double,
                                                          LogisticSystem>
            cache;
        double state = 0.2;
        const int steps = static_cast<int>(std::lround(1.0 / time_step));
        for (int i = 0; i < steps; ++i) {
            if (!predict) cache.reset();
            check(integrator.step(state, time_step, system, state, cache,
                                  error_norm),
                  "nonlinear implicit step failed");
        }
        return state;
    };
    const double exact = 1.0 / (1.0 + 4.0 * std::exp(-1.0));
    const double coarse = solve(0.1, true);
    const double fine = solve(0.05, true);
    const double ratio = std::abs((coarse - exact) / (fine - exact));
    check(ratio > 15.0 && ratio < 17.0,
          "nonlinear implicit method must converge at fourth order");
    check(std::abs(solve(0.1, false) - coarse) < 1.0e-13,
          "cached and uncached nonlinear solves must agree");
}

template <typename Tableau>
void test_predictor_moments() {
    const Tableau tableau;
    for (double ratio : {1.0, 0.5, 2.0, -1.0}) {
        const auto beta =
            ratio == 1.0 ? tableau.beta : tableau.predictor(ratio);
        for (std::size_t i = 0; i < Tableau::STAGES; ++i) {
            for (std::size_t k = 1; k <= Tableau::STAGES; ++k) {
                double moment = 0;
                for (std::size_t j = 0; j < Tableau::STAGES; ++j) {
                    moment += beta[i][j] * std::pow(tableau.c[j], k - 1);
                }
                const double exact = std::pow(1 + ratio * tableau.c[i], k) / k;
                check(std::abs(moment - exact) < 2e-14,
                      "predictor must integrate each polynomial moment");
            }
        }
    }
}

struct TimedState {
    double value = 0.0;
    double time = 0.0;
};

// y' = t^2 has a cubic solution, exactly reproduced by the stage predictor.
struct PolynomialSystem {
    using derivative_type = double;
    mutable std::vector<TimedState> evaluations;
    bool fail = false;

    bool evaluate(const TimedState& state, double& derivative) const {
        evaluations.push_back(state);
        derivative = state.time * state.time;
        return !fail;
    }

    template <std::size_t STAGES>
    TimedState advance(TimedState state,
                       const std::array<double, STAGES>& derivatives,
                       const std::array<double, STAGES>& weights,
                       std::size_t active_stages,
                       double time_step,
                       double time_fraction) const {
        for (std::size_t i = 0; i < STAGES; ++i) {
            if (i == active_stages) break;
            state.value += time_step * weights[i] * derivatives[i];
        }
        state.time += time_step * time_fraction;
        return state;
    }
};

// y' = (1 + t)y(1 - y), y(0) = 1/5. Tests nonlinear stages and stage times.
struct TimedLogisticSystem : PolynomialSystem {
    bool evaluate(const TimedState& state, double& derivative) const {
        derivative = (1 + state.time) * state.value * (1 - state.value);
        return std::isfinite(derivative);
    }
};

template <template <typename> typename Tableau>
void test_explicit_method(std::size_t order) {
    const ode::Integrator integrator(Tableau<double>{});
    static_assert(!decltype(integrator)::IMPLICIT);
    check(decltype(integrator)::ORDER == order,
          "explicit tableau must declare the expected order");
    const double expected_ratio = std::pow(2.0, order);
    const double exact_exp = std::exp(1.0);
    const double exp_ratio =
        std::abs((integrate<double, Tableau<double>>(0.05) - exact_exp) /
                 (integrate<double, Tableau<double>>(0.025) - exact_exp));
    check(std::abs(exp_ratio / expected_ratio - 1) < 0.05,
          "explicit exponential convergence order");
    check(std::abs(integrate<float, Tableau<float>>(1.0F / 64) -
                   std::exp(1.0F)) < 2e-4F,
          "float explicit integration is inaccurate");

    const TimedLogisticSystem system;
    const auto exact = [](double t) {
        return 1 / (1 + 4 * std::exp(-t - t * t / 2));
    };
    for (bool backward : {false, true}) {
        const auto error = [&](int steps) {
            TimedState state{exact(backward ? 1 : 0), backward ? 1.0 : 0.0};
            const double h = (backward ? -1.0 : 1.0) / steps;
            for (int i = 0; i < steps; ++i) {
                check(integrator.step(state, h, system, state),
                      "explicit time-dependent step failed");
            }
            check(state.time == (backward ? 0 : 1),
                  "explicit integration must advance time with signed steps");
            return std::abs(state.value - exact(state.time));
        };
        const double ratio = error(32) / error(64);
        check(std::abs(ratio / expected_ratio - 1) < 0.1,
              "explicit nonlinear time-dependent convergence order");
        std::cout << "RK" << order << (backward ? " backward" : " forward")
                  << " nonlinear order: " << std::log2(ratio) << '\n';
    }

    TimedState state{exact(0), 0};
    double h = 0.5;
    check(
        integrator.step_adaptive(state, h, 0.5, 1e-9, 1e-11, system,
                                 [](const TimedState& a, const TimedState& b) {
                                     return std::abs(a.value - b.value);
                                 }),
        "explicit step-doubling error control failed");
    check(state.time > 0 && state.time < 0.5 && h > 0 && h <= 0.5 &&
              std::abs(state.value - exact(state.time)) < 5e-9,
          "explicit step-doubling must reduce an inaccurate step");
}

void test_predictor_and_failures() {
    const ode::Integrator integrator(ode::LobattoIIIA4Tableau<double>{});
    PolynomialSystem system;
    decltype(integrator)::StepCache<TimedState, PolynomialSystem> cache;
    const auto error_norm = [](const TimedState& a, const TimedState& b) {
        return std::abs(a.value - b.value) + std::abs(a.time - b.time);
    };
    constexpr double TIME_STEP = 0.125;
    TimedState state{1.0 / 3.0, 1.0};
    check(integrator.step(state, TIME_STEP, system, state, cache, error_norm),
          "first polynomial step failed");
    check(system.evaluations.size() > 3, "first step must start cold");
    for (int step = 0; step < 3; ++step) {
        system.evaluations.clear();
        const double initial_time = state.time;
        check(integrator.step(state, TIME_STEP, system, state, cache,
                              error_norm, {1.0e-13, 1}),
              "exact cubic prediction must converge in one iteration");
        check(system.evaluations.size() == 3,
              "predictor must reuse cached RHS without old-step evaluations");
        for (std::size_t i = 0; i < 3; ++i) {
            const auto& stage = system.evaluations[i];
            check(stage.time == initial_time + TIME_STEP * i / 2,
                  "predicted stage time is incorrect");
            check(std::abs(stage.value - std::pow(stage.time, 3) / 3) < 1.0e-13,
                  "predicted stages must lie on the exact cubic solution");
        }
    }

    TimedState next{-99.0, -99.0};
    system.fail = true;
    decltype(integrator)::Stages<TimedState, PolynomialSystem> recorded{};
    recorded.states[0] = {-77, -88};
    recorded.derivatives[0] = -66;
    check(!integrator.step(state, TIME_STEP, system, next, cache, recorded,
                           error_norm) &&
              recorded.states[0].value == -77 &&
              recorded.derivatives[0] == -66 && next.value == -99,
          "failed recorded implicit steps must preserve stage data and output");
    check(!integrator.step(state, TIME_STEP, system, next, cache, error_norm),
          "failed RHS must fail the step");
    system.fail = false;
    check(!integrator.step(state, TIME_STEP, system, next, cache, error_norm,
                           {1.0e-13, 0}),
          "zero iteration limit must fail the step");
    check(!integrator.step(state, TIME_STEP, system, next, cache, error_norm,
                           {std::numeric_limits<double>::infinity(), 10}),
          "nonfinite tolerance must fail the step");
    check(!integrator.step(state, TIME_STEP, system, next, cache,
                           [](const TimedState&, const TimedState&) {
                               return std::numeric_limits<double>::quiet_NaN();
                           }),
          "nonfinite residual must fail the step");
    check(next.value == -99.0 && next.time == -99.0,
          "failed steps must preserve the output");
    check(integrator.step(state, TIME_STEP, system, state, cache, error_norm,
                          {1.0e-13, 1}),
          "failed steps must preserve the last successful cache");

    for (double h : {TIME_STEP / 2, TIME_STEP * 1.5, -TIME_STEP}) {
        system.evaluations.clear();
        check(integrator.step(state, h, system, state, cache, error_norm,
                              {1.0e-13, 1}),
              "variable-step cubic predictor must converge in one iteration");
        check(system.evaluations.size() == 3,
              "variable-step prediction must reuse the cached derivatives");
        for (const auto& stage : system.evaluations) {
            check(std::abs(stage.value - std::pow(stage.time, 3) / 3) < 1e-13,
                  "variable-step stages must reproduce the cubic solution");
        }
    }
    cache.reset();
    system.evaluations.clear();
    check(
        integrator.step(state, TIME_STEP / 2, system, state, cache, error_norm),
        "reset cache must remain usable");
    check(system.evaluations.size() > 3, "reset must discard stage history");

    const ExponentialSystem<double> exponential;
    double output = -99.0;
    check(!integrator.step(1.0, 10.0, exponential, output,
                           [](double a, double b) { return std::abs(a - b); },
                           {1.0e-13, 8}),
          "divergent fixed-point iteration must fail within its limit");
    check(output == -99.0, "nonconvergence must preserve the output");
}

void test_symmetric_step_selection() {
    const ode::Integrator integrator(ode::LobattoIIIA4Tableau<double>{});
    PolynomialSystem system;
    decltype(integrator)::StepCache<TimedState, PolynomialSystem> cache;
    const auto norm = [](const TimedState& a, const TimedState& b) {
        return std::hypot(a.value - b.value, a.time - b.time);
    };
    // For y'=t^2, trapezoidal quadrature has error h^3/6 and Lobatto is exact.
    constexpr double TARGET_STEP = 0.125;
    const ode::SymmetricStepOptions<double> options{
        TARGET_STEP * TARGET_STEP * TARGET_STEP / 6, 1e-9, 20};
    const ode::FixedPointOptions<double> fixed_point{1e-14, 100};
    TimedState state{1.0 / 3.0, 1.0};
    for (double guess : {0.01, 0.4}) {
        cache.reset();
        double h = guess;
        check(integrator.step_adaptive(state, h, 0.5, system, state, cache,
                                       norm, norm, options, fixed_point),
              "symmetric step selection must solve the defect equation");
        check(std::abs(h - TARGET_STEP) < 1e-10,
              "small guesses must grow to the target, large guesses shrink");
        check(std::abs(state.value - std::pow(state.time, 3) / 3) < 1e-13,
              "adaptive polynomial trajectory must remain exact");
    }
    double h = 0.04;
    check(integrator.step_adaptive(state, h, 0.04, system, state, cache, norm,
                                   norm, options, fixed_point),
          "a capped step may have error below the target");
    check(h == 0.04, "accepted step must obey the maximum step size");

    const TimedState before = state;
    TimedState output{-99, -99};
    h = 0.2;
    auto fail_options = options;
    fail_options.max_iterations = 1;
    check(!integrator.step_adaptive(state, h, 0.5, system, output, cache, norm,
                                    norm, fail_options, fixed_point),
          "exhausted step-size iterations must fail");
    check(state.value == before.value && state.time == before.time &&
              output.value == -99 && output.time == -99 && h == 0.2,
          "rejected step-size trials must preserve state, output and step");
    check(integrator.step(state, 0.05, system, state, cache, norm, {1e-13, 1}),
          "rejected trials must preserve the successful stage history");
    for (double maximum :
         {0.0, -1.0, std::numeric_limits<double>::infinity()}) {
        check(
            !integrator.step_adaptive(state, h, maximum, system, output, cache,
                                      norm, norm, options, fixed_point),
            "invalid maximum steps must fail without accepting a state");
    }
    fail_options = options;
    fail_options.relative_tolerance = 1;
    check(!integrator.step_adaptive(state, h, 0.5, system, output, cache, norm,
                                    norm, fail_options, fixed_point),
          "invalid selector tolerance must be rejected");
    check(!integrator.step_adaptive(
              state, h, 0.5, system, output, cache, norm,
              [](const TimedState&, const TimedState&) {
                  return std::numeric_limits<double>::quiet_NaN();
              },
              options, fixed_point),
          "nonfinite embedded defects must fail");
    check(output.value == -99 && output.time == -99 && h == 0.2,
          "invalid adaptive steps must preserve all outputs");
}

struct PendulumState {
    double q = 0;
    double p = 0;
    double time = 0;
};

struct PendulumSystem {
    using derivative_type = std::array<double, 2>;

    bool evaluate(const PendulumState& state,
                  derivative_type& derivative) const {
        derivative = {state.p, -std::sin(state.q)};
        return std::isfinite(derivative[0]) && std::isfinite(derivative[1]);
    }

    template <std::size_t STAGES>
    PendulumState advance(
        PendulumState state,
        const std::array<derivative_type, STAGES>& derivatives,
        const std::array<double, STAGES>& weights,
        std::size_t active_stages,
        double time_step,
        double time_fraction) const {
        for (std::size_t i = 0; i < active_stages; ++i) {
            state.q += time_step * weights[i] * derivatives[i][0];
            state.p += time_step * weights[i] * derivatives[i][1];
        }
        state.time += time_step * time_fraction;
        return state;
    }
};

double pendulum_norm(const PendulumState& a, const PendulumState& b) {
    return std::hypot(a.q - b.q, a.p - b.p);
}

struct QuadraticInvariantSystem : PendulumSystem {
    bool evaluate(const PendulumState& state,
                  derivative_type& derivative) const {
        derivative = {state.q * state.p, -state.q * state.q};
        return std::isfinite(derivative[0]) && std::isfinite(derivative[1]);
    }
};

void test_gauss_quadratic_invariant() {
    const ode::Integrator integrator(ode::Gauss4Tableau<double>{});
    const QuadraticInvariantSystem system;
    decltype(integrator)::StepCache<PendulumState, QuadraticInvariantSystem>
        cache;
    PendulumState state{1, 0, 0};
    for (int i = 0; i < 100; ++i) {
        check(integrator.step(state, 0.1, system, state, cache, pendulum_norm,
                              {1e-14, 100}),
              "Gauss nonlinear quadratic-invariant orbit must converge");
        check(std::abs(state.q * state.q + state.p * state.p - 1) < 1e-12,
              "Gauss must preserve quadratic invariants to stage accuracy");
    }
    check(std::abs(state.q - 1 / std::cosh(10.0)) < 1e-8 &&
              std::abs(state.p + std::tanh(10.0)) < 1e-8,
          "quadratic-invariant orbit must follow the analytic solution");
}

template <typename Tableau>
void test_fixed_time_reversal() {
    const ode::Integrator integrator(Tableau{});
    const PendulumSystem system;
    typename decltype(integrator)::template StepCache<PendulumState,
                                                      PendulumSystem>
        cache;
    const PendulumState initial{1.3, 0.7, 0};
    PendulumState state = initial;
    for (int i = 0; i < 100; ++i) {
        check(integrator.step(state, 0.1, system, state, cache, pendulum_norm,
                              {1e-14, 100}),
              "forward symmetric orbit must converge");
    }
    cache.reset();
    for (int i = 0; i < 100; ++i) {
        check(integrator.step(state, -0.1, system, state, cache, pendulum_norm,
                              {1e-14, 100}),
              "backward symmetric orbit must converge");
    }
    check(pendulum_norm(initial, state) < 2e-12,
          "symmetric fixed steps must retrace a nonlinear orbit");
}

template <typename Tableau>
void test_adaptive_time_reversal() {
    const ode::Integrator integrator(Tableau{});
    const PendulumSystem system;
    typename decltype(integrator)::template StepCache<PendulumState,
                                                      PendulumSystem>
        cache;
    const ode::SymmetricStepOptions<double> options{1e-4, 1e-9, 30};
    const ode::FixedPointOptions<double> fixed_point{1e-14, 100};
    const PendulumState initial{1.3, 0.7, 0};
    PendulumState forward{};
    double h = 0.3;
    check(integrator.step_adaptive(initial, h, 0.5, system, forward, cache,
                                   pendulum_norm, pendulum_norm, options,
                                   fixed_point),
          "forward adaptive pendulum step must converge");
    check(h < 0.5,
          "time-reversal test must exercise the selector, not its cap");
    cache.reset();
    PendulumState backward{};
    double reverse_h = -0.01;
    check(integrator.step_adaptive(forward, reverse_h, 0.5, system, backward,
                                   cache, pendulum_norm, pendulum_norm, options,
                                   fixed_point),
          "backward adaptive pendulum step must converge");
    check(std::abs(h + reverse_h) < 1e-9 &&
              pendulum_norm(initial, backward) < 2e-9 &&
              std::abs(backward.time) < 1e-9,
          "independent forward/backward selection must retrace the same step");

    PendulumState reflected = forward;
    reflected.p = -reflected.p;
    cache.reset();
    double reflected_h = 0.4;
    check(integrator.step_adaptive(reflected, reflected_h, 0.5, system,
                                   reflected, cache, pendulum_norm,
                                   pendulum_norm, options, fixed_point),
          "reflected adaptive step must converge");
    check(
        std::abs(reflected_h - h) < 1e-9 &&
            std::hypot(reflected.q - initial.q, reflected.p + initial.p) < 2e-9,
        "the variable-step map must preserve pendulum reversibility");
}

template <typename Tableau>
void test_adaptive_pendulum_accuracy() {
    const ode::Integrator integrator(Tableau{});
    const PendulumSystem system;
    // Exact period 4*K(sin(1)^2), where K uses the elliptic parameter m=k^2.
    constexpr double PERIOD = 8.3497529269184947344;
    const auto integrate_pendulum = [&](double tolerance, double end_time) {
        typename decltype(integrator)::template StepCache<PendulumState,
                                                          PendulumSystem>
            cache;
        PendulumState state{2, 0, 0};
        const double initial_energy = 1 - std::cos(state.q);
        double max_energy_error = 0;
        double minimum_h = 0.5;
        double maximum_h = 0;
        std::size_t steps = 0;
        while (end_time - state.time > 1e-12) {
            check(++steps < 500'000, "adaptive pendulum exceeded step budget");
            double h = 0.5;
            const double cap = std::min(h, end_time - state.time);
            const double initial_time = state.time;
            check(integrator.step_adaptive(state, h, cap, system, state, cache,
                                           pendulum_norm, pendulum_norm,
                                           {tolerance, 1e-7, 30}, {1e-14, 100}),
                  "adaptive pendulum orbit must converge");
            check(h > 0 && h <= cap && state.time > initial_time,
                  "adaptive pendulum must advance within its step bound");
            if (cap == 0.5) minimum_h = std::min(minimum_h, h);
            maximum_h = std::max(maximum_h, h);
            const double energy = state.p * state.p / 2 + 1 - std::cos(state.q);
            max_energy_error = std::max(max_energy_error,
                                        std::abs(energy / initial_energy - 1));
        }
        check(maximum_h / minimum_h > 1.2,
              "nonlinear orbit must use meaningfully varying step sizes");
        check(max_energy_error < 1e-4,
              "adaptive pendulum must keep its energy invariant accurate");
        return pendulum_norm(state, PendulumState{2, 0, end_time});
    };
    const double coarse = integrate_pendulum(1e-4, PERIOD);
    const double fine =
        integrate_pendulum(1e-4 / std::pow(2.0, Tableau::ERROR_ORDER), PERIOD);
    // Halve h through the defect's leading power; the orbit stays order four.
    check(coarse / fine > 10 && coarse / fine < 22,
          "adaptive implicit method must retain fourth-order convergence");
    check(integrate_pendulum(1e-5, 100 * PERIOD) < 2e-3,
          "long adaptive pendulum orbit must retain the correct phase");
}

template <template <typename> typename Tableau>
void test_symmetric_method() {
    test_implicit_order<Tableau>();
    test_nonlinear_order<Tableau<double>>();
    test_predictor_moments<Tableau<double>>();
    test_fixed_time_reversal<Tableau<double>>();
    test_adaptive_time_reversal<Tableau<double>>();
    test_adaptive_pendulum_accuracy<Tableau<double>>();
    const ode::Integrator integrator(Tableau<double>{});
    typename decltype(integrator)::template StepCache<double,
                                                      ExponentialSystem<double>>
        cache;
    double state = 1;
    double h = 0.5;
    const auto norm = [](double a, double b) { return std::abs(a - b); };
    check(integrator.step_adaptive(state, h, 0.5, ExponentialSystem<double>{},
                                   state, cache, norm, norm, {1e-6, 1e-6, 30},
                                   {1e-14, 100}) &&
              h < 0.1 && std::abs(state - std::exp(h)) < 1e-9,
          "the embedded defect must control a nonconstant linear solution");
}

struct DecayingSystem : ExponentialSystem<double> {
    bool evaluate(double state, double& derivative) const {
        derivative = -20 * state;
        return std::isfinite(derivative);
    }
};

void test_adaptive_iteration_recovery() {
    const ode::Integrator integrator(ode::LobattoIIIA4Tableau<double>{});
    const DecayingSystem system;
    decltype(integrator)::StepCache<double, DecayingSystem> cache;
    const auto norm = [](double a, double b) { return std::abs(a - b); };
    double state = 1;
    double h = 0.5;
    double failed_output = -99;
    check(!integrator.step(state, h, system, failed_output, norm, {1e-14, 100}),
          "recovery test must begin outside fixed-point convergence");
    check(integrator.step_adaptive(state, h, 0.5, system, state, cache, norm,
                                   norm, {1e-6, 1e-6, 30}, {1e-14, 100}),
          "adaptive selection must recover by reducing an unconverged step");
    check(h < 0.1 && std::abs(state - std::exp(-20 * h)) < 1e-9,
          "recovered adaptive step must follow the analytic decay");
}

}  // namespace

int main() {
    static_assert(ode::Integrator<double>::ORDER == 4);
    static_assert(
        ode::Integrator<double, ode::LobattoIIIA4Tableau<double>>::ORDER == 4);
    static_assert(
        ode::Integrator<double, ode::LobattoIIIA4Tableau<double>>::STAGES == 3);

    const double exact = std::exp(1.0);
    const double coarse_error = std::abs(integrate<double>(0.1) - exact);
    const double fine_error = std::abs(integrate<double>(0.05) - exact);
    if (!(coarse_error / fine_error > 15.0)) {
        std::cerr << "RK4 convergence ratio was " << coarse_error / fine_error
                  << ", expected approximately 16\n";
        return EXIT_FAILURE;
    }

    static_assert(
        ode::Integrator<double, ExplicitMidpointTableau<double>>::ORDER == 2);
    const double midpoint_coarse_error = std::abs(
        integrate<double, ExplicitMidpointTableau<double>>(0.1) - exact);
    const double midpoint_fine_error = std::abs(
        integrate<double, ExplicitMidpointTableau<double>>(0.05) - exact);
    if (!(midpoint_coarse_error / midpoint_fine_error > 3.8) ||
        !(midpoint_coarse_error / midpoint_fine_error < 4.2)) {
        std::cerr << "midpoint convergence ratio was "
                  << midpoint_coarse_error / midpoint_fine_error
                  << ", expected approximately 4\n";
        return EXIT_FAILURE;
    }

    if (std::abs(integrate<float>(0.05F) - std::exp(1.0F)) > 1.0e-5F) {
        std::cerr << "float RK4 integration is inaccurate\n";
        return EXIT_FAILURE;
    }

    const ode::Integrator integrator(ode::DefaultButcherTableau<double>{});
    const ExponentialSystem<double> system;
    double state = 1.0;
    double next_time_step = 0.25;
    if (!integrator.step_adaptive(
            state, next_time_step, 0.25, 1.0e-8, 1.0e-10, system,
            [](double lhs, double rhs) { return std::abs(lhs - rhs); })) {
        std::cerr << "adaptive RK4 step failed\n";
        return EXIT_FAILURE;
    }
    if (!(state > 1.0) || !(state < std::exp(0.25)) ||
        !(next_time_step > 0.0) || !(next_time_step <= 0.25)) {
        std::cerr << "adaptive RK4 step produced state " << state
                  << " and next time step " << next_time_step << '\n';
        return EXIT_FAILURE;
    }

    try {
        test_explicit_method<ode::RungeKutta2Tableau>(2);
        test_explicit_method<ode::RungeKutta3Tableau>(3);
        test_explicit_method<ode::RungeKutta4Tableau>(4);
        test_symmetric_method<ode::LobattoIIIA4Tableau>();
        test_symmetric_method<ode::Gauss4Tableau>();
        test_symmetric_method<ode::LobattoIIIB4Tableau>();
        test_gauss_quadratic_invariant();
        test_predictor_and_failures();
        test_symmetric_step_selection();
        test_adaptive_iteration_recovery();
    } catch (const std::exception& error) {
        std::cerr << "ode_test: " << error.what() << '\n';
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
