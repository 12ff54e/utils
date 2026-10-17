// Adapted from GEPT's tests/newton_checks.hpp.
#ifndef UTILS_ODE_TESTS_NEWTON_CHECKS_HPP_
#define UTILS_ODE_TESTS_NEWTON_CHECKS_HPP_

#include "finite_error.h"
#include "numerics/ode.h"

#if defined(__CUDACC__)
#define UTILS_ODE_NEWTON_HOST_DEVICE __host__ __device__
#else
#define UTILS_ODE_NEWTON_HOST_DEVICE
#endif

namespace ode_test {

template <typename T>
struct NewtonState {
    using val_type = T;
    T value = 0;
    T time = 0;
    bool operator==(const NewtonState&) const = default;
};

template <typename T>
struct NewtonNorm {
    using val_type = T;
    UTILS_ODE_NEWTON_HOST_DEVICE T operator()(const NewtonState<T>& a,
                                              const NewtonState<T>& b) const {
        return ode_test::finite_max(std::abs(a.value - b.value),
                                    std::abs(a.time - b.time));
    }
};

template <typename T, std::size_t S>
struct NewtonResidualNorm : NewtonNorm<T> {
    using val_type = T;
    mutable std::array<T, S + 1> recent{};
    mutable std::size_t calls = 0;

    UTILS_ODE_NEWTON_HOST_DEVICE T operator()(const NewtonState<T>& a,
                                              const NewtonState<T>& b) const {
        const T error = NewtonNorm<T>::operator()(a, b);
        recent[calls++ % recent.size()] = error;
        return error;
    }
    UTILS_ODE_NEWTON_HOST_DEVICE T last_residual() const {
        if (calls < S + 1) return std::numeric_limits<T>::infinity();
        T error = 0;
        // The final self-norm checks the output. The preceding S calls
        // measure the accepted stage residual, independently of that check.
        for (std::size_t j = 0; j < S; ++j)
            error = ode_test::finite_max(
                error, recent[(calls - 2 - j) % recent.size()]);
        return error;
    }
};

enum class NewtonEquation { STIFF, CUBIC, DOMAIN, SINGULAR, EXPONENTIAL };

template <typename T>
struct NewtonSystem {
    using val_type = T;
    using derivative_type = T;
    NewtonEquation equation = NewtonEquation::STIFF;
    mutable std::size_t calls = 0;
    mutable std::size_t domain_failures = 0;
    std::size_t call_limit = std::numeric_limits<std::size_t>::max();

    UTILS_ODE_NEWTON_HOST_DEVICE bool evaluate(const NewtonState<T>& state,
                                               T& derivative) const {
        if (calls++ >= call_limit) return false;
        switch (equation) {
            case NewtonEquation::STIFF:
                derivative = 1 - 32 * (state.value - state.time);
                break;
            case NewtonEquation::CUBIC:
                derivative = -state.value * state.value * state.value;
                break;
            case NewtonEquation::DOMAIN:
                if (state.value > 0) {
                    ++domain_failures;
                    return false;
                }
                derivative = 1 - 2 * state.value;
                break;
            case NewtonEquation::SINGULAR:
                derivative = 2 * state.value;
                break;
            case NewtonEquation::EXPONENTIAL:
                derivative = state.value;
                break;
        }
        return std::isfinite(derivative);
    }
    template <std::size_t N>
    UTILS_ODE_NEWTON_HOST_DEVICE NewtonState<T> advance(
        NewtonState<T> state,
        const std::array<T, N>& derivatives,
        const std::array<T, N>& weights,
        std::size_t count,
        T h,
        T fraction) const {
        for (std::size_t j = 0; j < count; ++j)
            state.value += h * weights[j] * derivatives[j];
        state.time += h * fraction;
        return state;
    }
};

template <typename T, bool MIDPOINT = false>
struct NewtonOneStage {
    using val_type = T;
    using row_type = std::array<T, 1>;
    using matrix_type = std::array<row_type, 1>;
    static constexpr std::size_t STAGES = 1;
    static constexpr std::size_t ORDER = MIDPOINT ? 2 : 1;
    static constexpr bool IMPLICIT = true;
    matrix_type A{{{MIDPOINT ? T{0.5} : T{1}}}};
    row_type b{1};
    row_type c{MIDPOINT ? T{0.5} : T{1}};
    matrix_type beta{{{MIDPOINT ? T{1.5} : T{2}}}};
};

struct NewtonChecks {
    unsigned failures = 0;
    double value = 0;
    double residual = 0;
};

template <typename T>
UTILS_ODE_NEWTON_HOST_DEVICE T pade(T z, std::size_t degree) {
    T numerator = 1, denominator = 1, term = 1;
    for (std::size_t j = 1; j <= degree; ++j) {
        term *= z * T(degree - j + 1) / T(j * (2 * degree - j + 1));
        numerator += term;
        denominator += j % 2 == 0 ? term : -term;
    }
    return numerator / denominator;
}

template <typename Tableau, typename Predictor>
UTILS_ODE_NEWTON_HOST_DEVICE NewtonChecks check_newton_linear() {
    using T = typename Tableau::val_type;
    using State = NewtonState<T>;
    using System = NewtonSystem<T>;
    constexpr T EPSILON = std::numeric_limits<T>::epsilon();
    const ode::NewtonOptions<T> options{32 * EPSILON, 20};
    // The stiff residual subtracts quantities amplified by 32. Allow that
    // roundoff floor while still checking the resulting discrete solution.
    const ode::NewtonOptions<T> stiff_options{128 * EPSILON, 20};
    const ode::Integrator solver(Tableau{}, Predictor{});
    const NewtonResidualNorm<T, Tableau::STAGES> norm{};
    const System system{};
    typename decltype(solver)::template StepCache<State, System> cache;
    typename decltype(solver)::template Stages<State, System> stages;
    const State initial{T{1.25}, T{0.25}};
    State state = initial;
    NewtonChecks checks;
    // y'=1-32(y-t): y-t is multiplied by the Padé stability function.
    if (solver.step(initial, T{1}, system, state, cache, norm,
                    {16 * EPSILON, 30}))
        checks.failures |= 1;
    if (!solver.step(initial, T{1}, system, state, cache, stages, norm,
                     stiff_options)) {
        checks.failures |= 2;
        return checks;
    }
    const T exact = T{1.25} + pade(T{-32}, Tableau::ORDER / 2);
    if (!(std::abs(state.value - exact) <= 512 * EPSILON) ||
        state.time != T{1.25})
        checks.failures |= 4;
    if (!(norm.last_residual() <= stiff_options.tolerance))
        checks.failures |= 1024;
    checks.value = static_cast<double>(state.value);
    checks.residual = static_cast<double>(norm.last_residual());
    const Tableau tableau{};
    for (std::size_t i = 0; i < Tableau::STAGES; ++i)
        if (!(std::abs(stages.states[i].time - (initial.time + tableau.c[i])) <=
              8 * EPSILON))
            checks.failures |= 8;
    // Warm a full C8 ring and the B3 endpoint cache, then change signs/sizes.
    for (int i = 0; i < 12; ++i)
        if (!solver.step(state, T{0.03125}, system, state, cache, norm,
                         options))
            checks.failures |= 16;
    const State before = state;
    for (T h :
         std::array<T, 5>{T{0.0625}, T{-0.0625}, T{0}, T{-0.03125}, T{0.03125}})
        if (!solver.step(state, h, system, state, cache, norm, options))
            checks.failures |= 32;
    if (!(norm(before, state) <= 2048 * EPSILON)) checks.failures |= 64;
    T h = 1;
    State adaptive;
    typename decltype(solver)::template StepCache<State, System> adaptive_cache;
    // A loose defect target accepts the full stiff step. A fixed-point
    // fallback would shrink it instead, so this checks option forwarding.
    if (!solver.step_adaptive(initial, h, T{1}, system, adaptive,
                              adaptive_cache, norm, norm, {T{1e6}, T{0.01}, 40},
                              stiff_options)) {
        checks.failures |= 128;
    } else {
        if (h != 1 || !(std::abs(adaptive.value - exact) <= 512 * EPSILON))
            checks.failures |= 256;
    }
    cache.reset();
    State reset_result, fresh_result;
    if (!solver.step(state, T{0.125}, system, reset_result, cache, norm,
                     options) ||
        !solver.step(state, T{0.125}, system, fresh_result, norm, options) ||
        norm(reset_result, fresh_result) != 0)
        checks.failures |= 512;
    return checks;
}

template <typename T>
UTILS_ODE_NEWTON_HOST_DEVICE NewtonChecks check_newton_edges() {
    using State = NewtonState<T>;
    constexpr T EPSILON = std::numeric_limits<T>::epsilon();
    const ode::Integrator solver(NewtonOneStage<T>{});
    const ode::Integrator midpoint(NewtonOneStage<T, true>{});
    const NewtonNorm<T> norm{};
    const State sentinel{91, 92};
    NewtonSystem<T> system;
    system.equation = NewtonEquation::CUBIC;
    NewtonChecks checks;
    State output = sentinel;
    auto options = ode::NewtonOptions<T>{};
    options.max_backtracks = 0;
    if (solver.step(State{1, 0}, T{1}, system, output, norm, options) ||
        norm(output, sentinel) != 0)
        checks.failures |= 1;
    options.max_backtracks = 8;
    if (!solver.step(State{1, 0}, T{1}, system, output, norm, options) ||
        !(std::abs(output.value + output.value * output.value * output.value -
                   1) <= 128 * EPSILON))
        checks.failures |= 2;
    checks.value = static_cast<double>(output.value);
    system.equation = NewtonEquation::DOMAIN;
    // The initial derivative puts the first residual exactly at y=0, also
    // with fused arithmetic; the positive difference probe leaves the domain.
    if (!solver.step(State{T{-0.5}, 0}, T{0.25}, system, output, norm,
                     options) ||
        !(std::abs(output.value + T{1} / 6) <= 128 * EPSILON) ||
        system.domain_failures == 0)
        checks.failures |= 4;
    system.equation = NewtonEquation::SINGULAR;
    output = sentinel;
    if (midpoint.step(State{1, 0}, T{1}, system, output, norm, options) ||
        norm(output, sentinel) != 0)
        checks.failures |= 8;
    system.equation = NewtonEquation::CUBIC;
    system.calls = 0;
    system.call_limit = 2;
    if (solver.step(State{1, 0}, T{1}, system, output, norm, options) ||
        norm(output, sentinel) != 0)
        checks.failures |= 16;
    system.call_limit = std::numeric_limits<std::size_t>::max();
    const auto invalid_norm = [](const State&, const State&) {
        return std::numeric_limits<T>::quiet_NaN();
    };
    if (solver.step(State{1, 0}, T{1}, system, output, invalid_norm, options) ||
        norm(output, sentinel) != 0)
        checks.failures |= 32;
    // One permitted correction must include its following residual check.
    system.equation = NewtonEquation::SINGULAR;
    if (!midpoint.step(State{1, 0}, T{-2}, system, output, norm,
                       ode::NewtonOptions<T>{32 * EPSILON, 1}) ||
        !(std::abs(output.value + T{1} / 3) <= 64 * EPSILON))
        checks.failures |= 64;
    return checks;
}

template <typename T>
struct NewtonPairState {
    using val_type = T;
    T q = 1;
    T p = 0;
    T first_time = 0;
    T second_time = 0;
};

template <typename T>
struct NewtonPairNorm {
    using val_type = T;
    UTILS_ODE_NEWTON_HOST_DEVICE T
    operator()(const NewtonPairState<T>& a, const NewtonPairState<T>& b) const {
        return ode_test::finite_max(std::abs(a.q - b.q), std::abs(a.p - b.p));
    }
};

template <typename T>
struct NewtonPairSystem {
    using val_type = T;
    using first_derivative_type = T;
    using second_derivative_type = T;
    T frequency = 10;

    UTILS_ODE_NEWTON_HOST_DEVICE bool evaluate_first(
        const NewtonPairState<T>& state,
        T& derivative) const {
        derivative = state.p;
        return std::isfinite(derivative);
    }
    UTILS_ODE_NEWTON_HOST_DEVICE bool evaluate_second(
        const NewtonPairState<T>& state,
        T& derivative) const {
        derivative = -frequency * frequency * state.q;
        return std::isfinite(derivative);
    }
    template <std::size_t N>
    UTILS_ODE_NEWTON_HOST_DEVICE NewtonPairState<T> advance(
        NewtonPairState<T> state,
        const std::array<T, N>& first,
        const std::array<T, N>& second,
        const std::array<T, N>& a,
        const std::array<T, N>& b,
        std::size_t count,
        T h,
        T first_fraction,
        T second_fraction) const {
        for (std::size_t j = 0; j < count; ++j) {
            state.q += h * a[j] * first[j];
            state.p += h * b[j] * second[j];
        }
        state.first_time += h * first_fraction;
        state.second_time += h * second_fraction;
        return state;
    }
};

template <typename T>
UTILS_ODE_NEWTON_HOST_DEVICE NewtonChecks check_newton_pair() {
    const ode::PartitionedIntegrator solver(
        ode::ImplicitMidpointPairTableau<T>{});
    const NewtonPairSystem<T> system{};
    const NewtonPairNorm<T> norm{};
    constexpr T EPSILON = std::numeric_limits<T>::epsilon();
    const ode::NewtonOptions<T> options{16 * EPSILON, 20};
    NewtonPairState<T> state;
    NewtonChecks checks;
    if (solver.step(state, T{1}, system, state, norm, {16 * EPSILON, 30}))
        checks.failures |= 1;
    typename decltype(solver)::template Stages<NewtonPairState<T>,
                                               NewtonPairSystem<T>>
        stages;
    if (!solver.step(state, T{1}, system, state, stages, norm, options)) {
        checks.failures |= 2;
        return checks;
    }
    if (!(std::abs(state.q + T{12} / 13) <= 512 * EPSILON) ||
        !(std::abs(state.p + T{50} / 13) <= 512 * EPSILON) ||
        stages.states[0].first_time != T{0.5} ||
        stages.states[0].second_time != T{0.5})
        checks.failures |= 4;
    checks.value = static_cast<double>(state.q);
    if (!solver.step(state, T{-1}, system, state, norm, options) ||
        !(norm(state, NewtonPairState<T>{}) <= 1024 * EPSILON))
        checks.failures |= 8;
    checks.residual = static_cast<double>(norm(state, NewtonPairState<T>{}));
    return checks;
}

template <typename T>
struct NewtonVectorState {
    using val_type = T;
    T x = 0;
    T y = 0;
    T time = 0;
    unsigned marker = 73;
};

template <typename T>
struct NewtonDerivative {
    using val_type = T;
    T x = 0;
    T y = 0;
};

template <typename T, bool CUSTOM>
struct NewtonVectorSystem {
    using val_type = T;
    using derivative_type =
        std::conditional_t<CUSTOM, NewtonDerivative<T>, std::array<T, 2>>;
    static constexpr T X_SCALE = CUSTOM ? T{1e6} : T{1};
    static constexpr T Y_SCALE = CUSTOM ? T{1e-6} : T{1};

    UTILS_ODE_NEWTON_HOST_DEVICE std::array<T, 2> newton_components(
        const derivative_type& d) const
        requires(CUSTOM)
    {
        return {d.x / X_SCALE, d.y / Y_SCALE};
    }
    UTILS_ODE_NEWTON_HOST_DEVICE void newton_assign(
        derivative_type& d,
        const std::array<T, 2>& values) const
        requires(CUSTOM)
    {
        d = {X_SCALE * values[0], Y_SCALE * values[1]};
    }
    UTILS_ODE_NEWTON_HOST_DEVICE bool evaluate(
        const NewtonVectorState<T>& state,
        derivative_type& derivative) const {
        derivative = {-8 * state.x + 4 * X_SCALE * (state.y / Y_SCALE),
                      -4 * state.y};
        return true;
    }
    template <std::size_t N>
    UTILS_ODE_NEWTON_HOST_DEVICE NewtonVectorState<T> advance(
        NewtonVectorState<T> state,
        const std::array<derivative_type, N>& derivatives,
        const std::array<T, N>& weights,
        std::size_t count,
        T h,
        T fraction) const {
        for (std::size_t j = 0; j < count; ++j) {
            if constexpr (CUSTOM) {
                state.x += h * weights[j] * derivatives[j].x;
                state.y += h * weights[j] * derivatives[j].y;
            } else {
                state.x += h * weights[j] * derivatives[j][0];
                state.y += h * weights[j] * derivatives[j][1];
            }
        }
        state.time += h * fraction;
        return state;
    }
};

struct NewtonMissingAssignment {
    UTILS_ODE_NEWTON_HOST_DEVICE std::array<double, 1> newton_components(
        const NewtonDerivative<double>& d) const {
        return {d.x};
    }
};

struct NewtonNonvoidAssignment : NewtonMissingAssignment {
    UTILS_ODE_NEWTON_HOST_DEVICE bool newton_assign(
        NewtonDerivative<double>& d,
        const std::array<double, 1>& values) const {
        d.x = values[0];
        return true;
    }
};

static_assert(ode::detail::NEWTON_MAPPABLE<double,
                                           NewtonVectorSystem<double, true>,
                                           NewtonDerivative<double>>);
static_assert(ode::detail::NEWTON_MAPPABLE<double,
                                           NewtonVectorSystem<double, false>,
                                           std::array<double, 2>>);
static_assert(!ode::detail::NEWTON_MAPPABLE<double,
                                            NewtonMissingAssignment,
                                            NewtonDerivative<double>>);
static_assert(!ode::detail::NEWTON_MAPPABLE<double,
                                            NewtonNonvoidAssignment,
                                            NewtonDerivative<double>>);

template <typename T, bool CUSTOM>
UTILS_ODE_NEWTON_HOST_DEVICE NewtonChecks check_newton_vector() {
    const ode::Integrator solver(ode::Gauss4Tableau<T>{});
    using System = NewtonVectorSystem<T, CUSTOM>;
    using State = NewtonVectorState<T>;
    const System system{};
    constexpr T EPSILON = std::numeric_limits<T>::epsilon();
    const auto norm = [](const State& a, const State& b) {
        return ode_test::finite_max(std::abs((a.x - b.x) / System::X_SCALE),
                                    std::abs((a.y - b.y) / System::Y_SCALE));
    };
    const State initial{2 * System::X_SCALE, System::Y_SCALE, T{0.125}};
    State state;
    NewtonChecks checks;
    if (!solver.step(initial, T{1}, system, state, norm,
                     ode::NewtonOptions<T>{16 * EPSILON, 20})) {
        checks.failures = 1;
        return checks;
    }
    const T x = state.x / System::X_SCALE, y = state.y / System::Y_SCALE;
    if (!(std::abs(x - (T{7} / 31 + T{1} / 13)) <= 512 * EPSILON) ||
        !(std::abs(y - T{1} / 13) <= 512 * EPSILON) || state.time != T{1.125} ||
        state.marker != initial.marker)
        checks.failures |= 2;
    checks.value = static_cast<double>(x);
    checks.residual = static_cast<double>(y);
    return checks;
}

// Only column zero of the first partition and column one of the second
// partition are used. The cross-stage coupling requires a Newton solve.
template <typename T>
struct NewtonUnusedPair {
    using val_type = T;
    using row_type = std::array<T, 2>;
    using matrix_type = std::array<row_type, 2>;
    static constexpr std::size_t STAGES = 2;
    static constexpr std::size_t ORDER = 1;
    static constexpr bool IMPLICIT = true;
    matrix_type A_first{{{T{0.5}, 0}, {T{0.25}, 0}}};
    matrix_type A_second{{{0, T{0.25}}, {0, T{0.5}}}};
    row_type b_first{1, 0}, b_second{0, 1};
    row_type c_first{T{0.5}, T{0.25}}, c_second{T{0.25}, T{0.5}};
};

template <typename T>
struct NewtonUnusedSystem {
    using val_type = T;
    using first_derivative_type = NewtonDerivative<T>;
    using second_derivative_type = std::array<T, 1>;

    UTILS_ODE_NEWTON_HOST_DEVICE std::array<T, 1> newton_components(
        const first_derivative_type& d) const {
        return {d.x};
    }
    UTILS_ODE_NEWTON_HOST_DEVICE void newton_assign(
        first_derivative_type& d,
        const std::array<T, 1>& values) const {
        d.x = values[0];
    }
    UTILS_ODE_NEWTON_HOST_DEVICE bool evaluate_first(
        const NewtonPairState<T>& state,
        first_derivative_type& derivative) const {
        derivative = {state.p, 0};
        return state.first_time == T{0.5};
    }
    UTILS_ODE_NEWTON_HOST_DEVICE bool evaluate_second(
        const NewtonPairState<T>& state,
        second_derivative_type& derivative) const {
        derivative = {-16 * state.q};
        return state.second_time == T{0.5};
    }
    template <std::size_t N>
    UTILS_ODE_NEWTON_HOST_DEVICE NewtonPairState<T> advance(
        NewtonPairState<T> state,
        const std::array<first_derivative_type, N>& first,
        const std::array<second_derivative_type, N>& second,
        const std::array<T, N>& a,
        const std::array<T, N>& b,
        std::size_t count,
        T h,
        T first_fraction,
        T second_fraction) const {
        for (std::size_t j = 0; j < count; ++j) {
            state.q += h * a[j] * first[j].x;
            state.p += h * b[j] * second[j][0];
        }
        state.first_time += h * first_fraction;
        state.second_time += h * second_fraction;
        return state;
    }
};

template <typename T>
UTILS_ODE_NEWTON_HOST_DEVICE NewtonChecks check_newton_unused() {
    const ode::PartitionedIntegrator solver(NewtonUnusedPair<T>{});
    const NewtonUnusedSystem<T> system{};
    const NewtonPairNorm<T> norm{};
    constexpr T EPSILON = std::numeric_limits<T>::epsilon();
    NewtonPairState<T> state;
    typename decltype(solver)::template Stages<NewtonPairState<T>,
                                               NewtonUnusedSystem<T>>
        stages;
    NewtonChecks checks;
    if (!solver.step(state, T{1}, system, state, stages, norm,
                     ode::NewtonOptions<T>{16 * EPSILON, 20})) {
        checks.failures = 1;
        return checks;
    }
    if (!(std::abs(state.q + 1) <= 512 * EPSILON) ||
        !(std::abs(state.p + 8) <= 512 * EPSILON) ||
        stages.first_evaluated[1] || stages.second_evaluated[0] ||
        stages.first[1].x != 0 || stages.first[1].y != 0 ||
        stages.second[0][0] != 0)
        checks.failures |= 2;
    checks.value = static_cast<double>(state.q);
    checks.residual = static_cast<double>(state.p);
    return checks;
}

using NewtonResults = std::array<NewtonChecks, 13>;

template <typename T, typename Run>
void for_each_newton_check(const Run& run) {
    run.template
    operator()<ode::Gauss4Tableau<T>, ode::ContinuousOutputPredictor>(0);
    run.template
    operator()<ode::Gauss4Tableau<T>, ode::AdditionalStagePredictor<1>>(1);
    run.template
    operator()<ode::Gauss4Tableau<T>, ode::AdditionalStagePredictor<3>>(2);
    run.template operator()<ode::Gauss4Tableau<T>, ode::EquistagePredictor<8>>(
        3);
    run.template
    operator()<ode::Gauss12Tableau<T>, ode::AdditionalStagePredictor<3>>(4);
    run.template operator()<ode::Gauss12Tableau<T>, ode::EquistagePredictor<1>>(
        5);
    run.template operator()<ode::LobattoIIIA4Tableau<T>,
                            ode::AdditionalStagePredictor<1>>(6);
    run.template
    operator()<ode::LobattoIIIB4Tableau<T>, ode::EquistagePredictor<8>>(7);
}

template <typename T>
NewtonResults check_newton() {
    NewtonResults results{};
    for_each_newton_check<T>(
        [&]<typename Tableau, typename Predictor>(std::size_t i) {
            results[i] = check_newton_linear<Tableau, Predictor>();
        });
    results[8] = check_newton_edges<T>();
    results[9] = check_newton_pair<T>();
    results[10] = check_newton_vector<T, false>();
    results[11] = check_newton_vector<T, true>();
    results[12] = check_newton_unused<T>();
    return results;
}

}  // namespace ode_test

#undef UTILS_ODE_NEWTON_HOST_DEVICE

#endif  // UTILS_ODE_TESTS_NEWTON_CHECKS_HPP_
