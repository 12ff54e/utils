// Adapted from GEPT's tests/explicit_adaptive_checks.hpp.
#ifndef UTILS_ODE_TESTS_EXPLICIT_ADAPTIVE_CHECKS_HPP_
#define UTILS_ODE_TESTS_EXPLICIT_ADAPTIVE_CHECKS_HPP_

#include "finite_error.h"
#include "numerics/ode.h"

#if defined(__CUDACC__)
#define UTILS_ODE_ADAPTIVE_HOST_DEVICE __host__ __device__
#else
#define UTILS_ODE_ADAPTIVE_HOST_DEVICE
#endif

namespace ode_test {

template <typename T>
struct ExplicitAdaptiveState {
    using val_type = T;
    val_type value = 1;
    val_type time = 0;
};

template <typename T>
struct ExplicitAdaptiveSystem {
    using val_type = T;
    using derivative_type = val_type;
    val_type rate = 1;
    bool timed = false;
    bool constant = false;
    mutable std::size_t calls = 0;
    std::size_t limit = std::numeric_limits<std::size_t>::max();

    UTILS_ODE_ADAPTIVE_HOST_DEVICE bool evaluate(
        const ExplicitAdaptiveState<T>& s,
        T& derivative) const {
        if (++calls > limit) return false;
        derivative =
            constant ? rate : rate * (timed ? 1 + s.time : 1) * s.value;
        return std::isfinite(derivative);
    }
    template <std::size_t S>
    UTILS_ODE_ADAPTIVE_HOST_DEVICE ExplicitAdaptiveState<T> advance(
        ExplicitAdaptiveState<T> state,
        const std::array<T, S>& derivatives,
        const std::array<T, S>& weights,
        std::size_t count,
        T h,
        T fraction) const {
        for (std::size_t i = 0; i < S; ++i) {
            if (i == count) break;
            state.value += h * weights[i] * derivatives[i];
        }
        state.time += h * fraction;
        return state;
    }
};

template <typename T>
struct ExplicitAdaptiveNorm {
    using val_type = T;
    UTILS_ODE_ADAPTIVE_HOST_DEVICE T
    operator()(const ExplicitAdaptiveState<T>& a,
               const ExplicitAdaptiveState<T>& b) const {
        if (!std::isfinite(a.time) || !std::isfinite(b.time))
            return std::numeric_limits<T>::quiet_NaN();
        return std::abs(a.value - b.value);
    }
};

struct ExplicitAdaptiveChecks {
    unsigned failures = 0;
    // Forward value/time, backward value/time, scaled value/normalized time.
    std::array<double, 6> values{};
};

template <typename T>
UTILS_ODE_ADAPTIVE_HOST_DEVICE bool same_adaptive_state(
    const ExplicitAdaptiveState<T>& a,
    const ExplicitAdaptiveState<T>& b) {
    return a.value == b.value && a.time == b.time;
}

template <typename Solver>
UTILS_ODE_ADAPTIVE_HOST_DEVICE bool rejects_adaptive_options(
    const Solver& solver,
    typename Solver::val_type h,
    typename Solver::val_type cap,
    typename Solver::val_type upper,
    typename Solver::val_type lower) {
    using T = typename Solver::val_type;
    const ExplicitAdaptiveState<T> initial{T{1.25}, T{0.5}};
    auto state = initial;
    const T old_h = h;
    const ExplicitAdaptiveSystem<T> system{};
    bool rejected = false;
#if defined(__CUDA_ARCH__)
    rejected = !solver.step_adaptive(state, h, cap, upper, lower, system,
                                     ExplicitAdaptiveNorm<T>{});
#else
    try {
        solver.step_adaptive(state, h, cap, upper, lower, system,
                             ExplicitAdaptiveNorm<T>{});
    } catch (const std::invalid_argument&) { rejected = true; }
#endif
    return rejected && same_adaptive_state(state, initial) &&
           (h == old_h || (std::isnan(h) && std::isnan(old_h))) &&
           system.calls == 0;
}

template <typename Tableau>
UTILS_ODE_ADAPTIVE_HOST_DEVICE ExplicitAdaptiveChecks
check_explicit_adaptive(typename Tableau::val_type offset = 0) {
    using T = typename Tableau::val_type;
    using State = ExplicitAdaptiveState<T>;
    using System = ExplicitAdaptiveSystem<T>;
    constexpr T EPSILON = std::numeric_limits<T>::epsilon();
    constexpr T UPPER =
        std::numeric_limits<T>::digits <= 24 ? T{1e-5} : T{1e-9};
    constexpr T LOWER = UPPER / 100;
    constexpr T NAN_VALUE = std::numeric_limits<T>::quiet_NaN();
    constexpr T INFINITY_VALUE = std::numeric_limits<T>::infinity();
    constexpr T DENORMAL = std::numeric_limits<T>::denorm_min();
    const ode::Integrator solver(Tableau{});
    const ExplicitAdaptiveNorm<T> norm;
    const State initial{1 + offset, 0};
    ExplicitAdaptiveChecks result;

    const System timed{1, true};
    auto forward = initial;
    T h = T{0.5};
    if (!solver.step_adaptive(forward, h, T{0.5}, UPPER, LOWER, timed, norm) ||
        !(forward.time > 0 && forward.time < T{0.5}) ||
        !(h > 0 && h <= T{0.5}) ||
        !(std::abs(forward.value -
                   initial.value * std::exp(forward.time +
                                            forward.time * forward.time / 2)) <=
          8 * UPPER + 128 * EPSILON))
        result.failures |= 1;
    result.values[0] = static_cast<double>(forward.value);
    result.values[1] = static_cast<double>(forward.time);

    auto backward = forward;
    T reverse_h = -forward.time;
    if (!(forward.time > 0) ||
        !solver.step_adaptive(backward, reverse_h, forward.time, UPPER, LOWER,
                              timed, norm) ||
        !(backward.time < forward.time && backward.time >= 0) ||
        !(reverse_h < 0 && -reverse_h <= forward.time) ||
        !(std::abs(backward.value -
                   initial.value *
                       std::exp(backward.time +
                                backward.time * backward.time / 2)) <=
          16 * UPPER + 256 * EPSILON))
        result.failures |= 2;
    result.values[2] = static_cast<double>(backward.value);
    result.values[3] = static_cast<double>(backward.time);

    constexpr T RATE = std::numeric_limits<T>::digits <= 24 ? T{1e10} : T{1e20};
    const System ordinary{}, fast{RATE};
    auto normal = initial, scaled = initial;
    T normal_h = 1, scaled_h = 1 / RATE;
    // Cancellation in O(UPPER) distances between O(1) states permits relative
    // step-selection differences of order EPSILON / UPPER.
    constexpr T SCALE_TOLERANCE = 8 * EPSILON / UPPER;
    if (!solver.step_adaptive(normal, normal_h, T{1}, UPPER, LOWER, ordinary,
                              norm) ||
        !solver.step_adaptive(scaled, scaled_h, T{1} / RATE, UPPER, LOWER, fast,
                              norm) ||
        !(scaled.time > 0 && scaled.time < T{1} / RATE) ||
        !(std::abs(normal.time - RATE * scaled.time) <=
          SCALE_TOLERANCE *
              ode_test::finite_max(normal.time, RATE * scaled.time)) ||
        !(std::abs(normal.value - scaled.value) <=
          SCALE_TOLERANCE * ode_test::finite_max(normal.value, scaled.value)) ||
        !(std::abs(normal.value - initial.value * std::exp(normal.time)) <=
          8 * UPPER + 128 * EPSILON) ||
        !(std::abs(scaled.value -
                   initial.value * std::exp(RATE * scaled.time)) <=
          8 * UPPER + 128 * EPSILON))
        result.failures |= 4;
    result.values[4] = static_cast<double>(scaled.value);
    result.values[5] = static_cast<double>(RATE * scaled.time);

    for (T value : {T{0}, NAN_VALUE, INFINITY_VALUE, -INFINITY_VALUE})
        if (!rejects_adaptive_options(solver, value, T{1}, UPPER, LOWER))
            result.failures |= 8;
    for (T value : {T{0}, T{-1}, NAN_VALUE, INFINITY_VALUE, -INFINITY_VALUE}) {
        if (!rejects_adaptive_options(solver, T{0.1}, value, UPPER, LOWER) ||
            !rejects_adaptive_options(solver, T{0.1}, T{1}, value, T{0}))
            result.failures |= 8;
    }
    for (T value :
         {T{-1}, NAN_VALUE, INFINITY_VALUE, -INFINITY_VALUE, 2 * UPPER})
        if (!rejects_adaptive_options(solver, T{0.1}, T{1}, UPPER, value))
            result.failures |= 8;

    for (T value : {T{-1}, -DENORMAL, NAN_VALUE, INFINITY_VALUE}) {
        auto state = initial;
        T trial_h = T{0.1};
        if (solver.step_adaptive(
                state, trial_h, T{1}, UPPER, LOWER, ordinary,
                [=](const State&, const State&) { return value; }) ||
            !same_adaptive_state(state, initial) || trial_h != T{0.1})
            result.failures |= 16;
    }

    for (T value : {NAN_VALUE, INFINITY_VALUE, -INFINITY_VALUE}) {
        const System constant{1, false, true};
        const State sentinel{91, 92};
        auto output = sentinel;
        typename decltype(solver)::template ExplicitStages<State, System>
            stages;
        for (auto& state : stages.states) state = sentinel;
        for (auto& derivative : stages.derivatives) derivative = 93;
        if (solver.step(initial, value, constant, output, stages) ||
            !same_adaptive_state(output, sentinel) || constant.calls != 0)
            result.failures |= 32;
        for (const auto& state : stages.states)
            if (!same_adaptive_state(state, sentinel)) result.failures |= 32;
        for (T derivative : stages.derivatives)
            if (derivative != 93) result.failures |= 32;
    }

    for (T value : {DENORMAL, 3 * DENORMAL}) {
        const System zero{0, false, true};
        auto state = initial;
        T trial_h = value;
        if (solver.step_adaptive(state, trial_h, value, UPPER, LOWER, zero,
                                 norm) ||
            !same_adaptive_state(state, initial) || trial_h != value ||
            zero.calls != 0)
            result.failures |= 64;
    }
    {
        const System zero{0, false, true};
        auto state = initial;
        T trial_h = 2 * DENORMAL;
        if (!solver.step_adaptive(state, trial_h, trial_h, UPPER, LOWER, zero,
                                  norm) ||
            state.time != 2 * DENORMAL || state.value != initial.value ||
            trial_h != 2 * DENORMAL)
            result.failures |= 64;
    }

    // Prescribed finite estimates exercise growth at the largest finite cap,
    // including division/multiplication rounding for noninteger factors.
    for (T factor : {T{1.5}, T{3}}) {
        T raw_error = 1, error_scale = 1;
        for (std::size_t i = 0; i < Tableau::ORDER + 1; ++i)
            raw_error *= T{0.9} / factor;
        for (std::size_t i = 0; i < Tableau::ORDER; ++i) error_scale *= 2;
        raw_error *= error_scale - 1;
        for (T sign : {T{1}, T{-1}}) {
            const System zero{0, false, true};
            auto state = initial;
            constexpr T CAP = std::numeric_limits<T>::max();
            const T length = CAP / factor;
            T trial_h = sign * length;
            if (!solver.step_adaptive(
                    state, trial_h, CAP, T{1}, T{1}, zero,
                    [=](const State&, const State&) { return raw_error; }) ||
                !std::isfinite(trial_h) || sign * trial_h <= 0 ||
                sign * trial_h > CAP || state.time != sign * length ||
                state.value != initial.value ||
                !(std::abs(sign * trial_h / CAP - 1) <= 64 * EPSILON))
                result.failures |= 128;
        }
    }

    {
        const System failing{1, false, false, 0, Tableau::STAGES + 1};
        auto state = initial;
        T trial_h = T{0.1};
        if (solver.step_adaptive(state, trial_h, T{1}, UPPER, LOWER, failing,
                                 norm) ||
            !same_adaptive_state(state, initial) || trial_h != T{0.1})
            result.failures |= 256;
    }
    {
        const System zero{0, false, true};
        auto state = initial;
        T trial_h = std::numeric_limits<T>::min();
        const T old_h = trial_h;
        if (solver.step_adaptive(
                state, trial_h, trial_h, T{0.01}, T{0.001}, zero,
                [](const State&, const State&) { return T{1}; }) ||
            !same_adaptive_state(state, initial) || trial_h != old_h)
            result.failures |= 256;
        if (!solver.step(initial, T{0}, zero, state) ||
            !same_adaptive_state(state, initial))
            result.failures |= 512;
    }
    return result;
}

}  // namespace ode_test

#undef UTILS_ODE_ADAPTIVE_HOST_DEVICE

#endif  // UTILS_ODE_TESTS_EXPLICIT_ADAPTIVE_CHECKS_HPP_
