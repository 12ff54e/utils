// Adapted from GEPT's tests/partitioned_capability_checks.hpp.
#ifndef UTILS_ODE_TESTS_PARTITIONED_CAPABILITY_CHECKS_HPP_
#define UTILS_ODE_TESTS_PARTITIONED_CAPABILITY_CHECKS_HPP_

#include "finite_error.h"
#include "numerics/ode.h"

#if defined(__CUDACC__)
#define UTILS_ODE_PARTITIONED_HOST_DEVICE __host__ __device__
#else
#define UTILS_ODE_PARTITIONED_HOST_DEVICE
#endif

namespace ode_test {

template <typename T>
struct PartitionedState {
    using val_type = T;
    T q = 0, p = 0, first_time = 0, second_time = 0;
};

template <typename T>
struct PartitionedSystem {
    using val_type = T;
    using first_derivative_type = T;
    using second_derivative_type = T;
    mutable std::size_t first_calls = 0, second_calls = 0;
    mutable std::array<PartitionedState<T>, 16> samples{};
    std::size_t limit = std::numeric_limits<std::size_t>::max();
    T rate = 1;
    bool timed = false;
    T time_ceiling = std::numeric_limits<T>::infinity();
    std::size_t second_limit = std::numeric_limits<std::size_t>::max();

    UTILS_ODE_PARTITIONED_HOST_DEVICE bool evaluate_first(
        const PartitionedState<T>& state,
        T& derivative) const {
        if (first_calls < samples.size()) samples[first_calls] = state;
        if (++first_calls > limit || state.first_time > time_ceiling)
            return false;
        derivative =
            timed ? state.first_time * state.first_time : rate * state.p;
        return std::isfinite(derivative);
    }
    UTILS_ODE_PARTITIONED_HOST_DEVICE bool evaluate_second(
        const PartitionedState<T>& state,
        T& derivative) const {
        if (++second_calls > limit || second_calls > second_limit) return false;
        derivative =
            timed ? state.second_time * state.second_time : -rate * state.q;
        return std::isfinite(derivative);
    }
    template <std::size_t S>
    UTILS_ODE_PARTITIONED_HOST_DEVICE PartitionedState<T> advance(
        PartitionedState<T> state,
        const std::array<T, S>& first,
        const std::array<T, S>& second,
        const std::array<T, S>& a,
        const std::array<T, S>& b,
        std::size_t count,
        T h,
        T first_fraction,
        T second_fraction) const {
        for (std::size_t i = 0; i < S; ++i) {
            if (i == count) break;
            state.q += h * a[i] * first[i];
            state.p += h * b[i] * second[i];
        }
        state.first_time += h * first_fraction;
        state.second_time += h * second_fraction;
        return state;
    }
};

template <typename T>
struct PartitionedNorm {
    using val_type = T;
    UTILS_ODE_PARTITIONED_HOST_DEVICE T
    operator()(const PartitionedState<T>& a,
               const PartitionedState<T>& b) const {
        if (!std::isfinite(a.first_time) || !std::isfinite(a.second_time) ||
            !std::isfinite(b.first_time) || !std::isfinite(b.second_time))
            return std::numeric_limits<T>::infinity();
        return ode_test::finite_max(std::abs(a.q - b.q), std::abs(a.p - b.p));
    }
};

// Even under simultaneous sign reversal, but not a single-partition reversal.
template <typename T>
struct JointPartitionedNorm {
    using val_type = T;
    UTILS_ODE_PARTITIONED_HOST_DEVICE T
    operator()(const PartitionedState<T>& a,
               const PartitionedState<T>& b) const {
        const T q = a.q - b.q, p = a.p - b.p;
        return std::sqrt(q * q + p * p + q * p);
    }
};

struct PartitionedChecks {
    unsigned failures = 0;
    double value = 0;
    double reverse_error = 0;
    double selected_h = 0;
};

template <typename T>
UTILS_ODE_PARTITIONED_HOST_DEVICE bool same_partitioned_state(
    const PartitionedState<T>& a,
    const PartitionedState<T>& b) {
    return a.q == b.q && a.p == b.p && a.first_time == b.first_time &&
           a.second_time == b.second_time;
}

template <typename Tableau, typename Predictor, bool NEWTON = false>
UTILS_ODE_PARTITIONED_HOST_DEVICE PartitionedChecks
check_partitioned_predictor() {
    using T = typename Tableau::val_type;
    using State = PartitionedState<T>;
    using System = PartitionedSystem<T>;
    constexpr T EPSILON = std::numeric_limits<T>::epsilon();
    constexpr T H = T{1} / 32;
    constexpr T TOLERANCE = 8 * EPSILON;
    using Options = std::conditional_t<NEWTON, ode::NewtonOptions<T>,
                                       ode::FixedPointOptions<T>>;
    Options options;
    options.tolerance = TOLERANCE;
    const ode::PartitionedIntegrator solver(Tableau{}, Predictor{});
    const ode::PartitionedIntegrator reference(Tableau{});
    typename decltype(solver)::template StepCache<State, System> cache;
    typename decltype(solver)::template Stages<State, System> stages;
    const PartitionedNorm<T> norm;
    const State initial{1, T{0.3}, 2, 3};
    State state = initial, expected = initial;
    PartitionedChecks result;
    for (int i = 0; i < 12; ++i) {
        const System system{};
        if (!solver.step(state, H, system, state, cache, stages, norm,
                         options) ||
            !reference.step(expected, H, system, expected, norm, options))
            result.failures |= 1;
    }
    if (!(norm(state, expected) <= 512 * EPSILON) ||
        !(std::abs(state.q -
                   (std::cos(12 * H) + initial.p * std::sin(12 * H))) <=
          4 * std::pow(H, static_cast<int>(Tableau::ORDER)) + 512 * EPSILON))
        result.failures |= 1;

    // Reject one iteration to observe guesses while preserving the cache.
    const auto reject = [](const State&, const State&) { return T{1}; };
    const System sample{};
    const State sentinel{91, 92, 93, 94};
    State output = sentinel;
    auto saved_stages = stages;
    if (solver.step(state, H, sample, output, cache, stages, reject,
                    ode::FixedPointOptions<T>{EPSILON, 1}) ||
        !same_partitioned_state(output, sentinel) ||
        !same_partitioned_state(stages.states[0], saved_stages.states[0]))
        result.failures |= 2;
    constexpr std::size_t EXTRA =
        Predictor::EXTRA_STAGES == 3 ? 2 : Predictor::EXTRA_STAGES;
    std::size_t active = 0;
    for (std::size_t i = 0; i < Tableau::STAGES; ++i) {
        if (!saved_stages.first_evaluated[i]) continue;
        const auto& guess = sample.samples[EXTRA + active++];
        if (!(std::abs(guess.first_time -
                       (state.first_time + H * Tableau{}.c_first[i])) <=
              32 * EPSILON) ||
            !(std::abs(guess.second_time -
                       (state.second_time + H * Tableau{}.c_second[i])) <=
              32 * EPSILON))
            result.failures |= 4;
    }
    if (sample.first_calls != EXTRA + active) result.failures |= 4;
    const System failing{0, 0, {}, EXTRA + 1};
    if (solver.step(state, H, failing, output, cache, stages, norm, options) ||
        !same_partitioned_state(output, sentinel))
        result.failures |= 2;
    const System repeated{};
    solver.step(state, H, repeated, output, cache, stages, reject,
                ode::FixedPointOptions<T>{EPSILON, 1});
    for (std::size_t i = 0; i < active; ++i)
        if (!same_partitioned_state(sample.samples[EXTRA + i],
                                    repeated.samples[EXTRA + i]))
            result.failures |= 2;

    for (T h : {H / 2, -H / 2, T{0}, H, H}) {
        const System system{};
        if (!solver.step(state, h, system, state, cache, norm, options) ||
            !reference.step(expected, h, system, expected, norm, options) ||
            !(norm(state, expected) <= 1024 * EPSILON))
            result.failures |= 8;
    }
    result.value = static_cast<double>(state.q);
    cache.reset();
    State reset{}, fresh{};
    const System system{};
    if (!solver.step(state, H, system, reset, cache, norm, options) ||
        !solver.step(state, H, system, fresh, norm, options) ||
        !same_partitioned_state(reset, fresh))
        result.failures |= 16;
    state = reset;
    for (int i = 0; i < 15; ++i)
        if (!solver.step(state, -H, system, state, cache, norm, options))
            result.failures |= 8;
    result.reverse_error = static_cast<double>(norm(state, initial));
    if (!(result.reverse_error <= 4096 * EPSILON)) result.failures |= 8;
    return result;
}

template <typename Tableau, bool NEWTON = false>
UTILS_ODE_PARTITIONED_HOST_DEVICE PartitionedChecks
check_partitioned_adaptive() {
    using T = typename Tableau::val_type;
    using State = PartitionedState<T>;
    using System = PartitionedSystem<T>;
    constexpr T EPSILON = std::numeric_limits<T>::epsilon();
    constexpr bool SINGLE = std::numeric_limits<T>::digits <= 24;
    constexpr T TARGET = SINGLE ? T{2e-3} : T{1e-5};
    constexpr T RESIDUAL = SINGLE ? T{1e-3} : T{1e-6};
    using Options = std::conditional_t<NEWTON, ode::NewtonOptions<T>,
                                       ode::FixedPointOptions<T>>;
    Options options;
    options.tolerance = 8 * EPSILON;
    const ode::PartitionedIntegrator solver(Tableau{});
    using Cache = typename decltype(solver)::template StepCache<State, System>;
    Cache cache;
    const PartitionedNorm<T> norm;
    const JointPartitionedNorm<T> defect_norm;
    const State initial{1, T{0.3}, 0, 1};
    const State sentinel{91, 92, 93, 94};
    const System system{};
    const ode::SymmetricStepOptions<T> selection{TARGET, RESIDUAL, 40};
    State forward{};
    T h = T{0.2};
    PartitionedChecks result;
    if (!solver.step_adaptive(initial, h, T{0.25}, system, forward, cache, norm,
                              defect_norm, selection, options) ||
        !(h > 0 && h < T{0.25}) ||
        !(std::abs(forward.q - std::cos(h) - initial.p * std::sin(h)) <=
          4 * std::pow(h, static_cast<int>(Tableau::ORDER + 1)) +
              512 * EPSILON))
        result.failures |= 1;
    result.selected_h = static_cast<double>(h);
    result.value = static_cast<double>(forward.q);
    typename decltype(solver)::template Stages<State, System> stages;
    State fixed{};
    if (!solver.step(initial, h, system, fixed, stages, norm, options) ||
        !(norm(fixed, forward) <= 512 * EPSILON))
        result.failures |= 1;
    State defect{};
    if constexpr (ode::detail::HAS_PARTITIONED_ENDPOINT_DEFECT<Tableau>) {
        // Independent endpoint difference for q'=p, p'=-q.
        defect.q = h * (forward.p - initial.p) / 2;
        defect.p = -h * (forward.q - initial.q) / 2;
    } else {
        for (std::size_t i = 0; i < Tableau::STAGES; ++i) {
            defect.q += h * Tableau{}.error_weights_first[i] * stages.first[i];
            defect.p +=
                h * Tableau{}.error_weights_second[i] * stages.second[i];
        }
    }
    if (!(std::abs(defect_norm(State{}, defect) / TARGET - 1) <=
          2 * RESIDUAL + 256 * EPSILON))
        result.failures |= 2;

    Cache backward_cache;
    State backward{};
    T reverse_h = T{-0.11};
    if (!solver.step_adaptive(forward, reverse_h, T{0.25}, system, backward,
                              backward_cache, norm, defect_norm, selection,
                              options) ||
        !(std::abs(h + reverse_h) <= 4 * RESIDUAL * h + 1024 * EPSILON) ||
        !(norm(backward, initial) <= 8 * RESIDUAL * h + 2048 * EPSILON))
        result.failures |= 4;
    result.reverse_error = static_cast<double>(norm(backward, initial));

    const System baseline{};
    auto baseline_cache = cache;
    State baseline_state{};
    if (!solver.step(forward, h, baseline, baseline_state, baseline_cache, norm,
                     options))
        result.failures |= 1;
    constexpr auto ENDPOINTS =
        ode::detail::HAS_PARTITIONED_ENDPOINT_DEFECT<Tableau> ? 2 : 0;
    // Invalid stage/defect norms abort, unlike a recoverable failed RHS.
    for (bool invalid_stage : {false, true}) {
        const System counted{};
        auto trial_cache = cache;
        State output = sentinel;
        T trial_h = h;
        const auto bad_norm = [&](const State& a, const State& b) {
            return invalid_stage ? T{-1} : norm(a, b);
        };
        const auto bad_defect = [&](const State& a, const State& b) {
            return invalid_stage ? defect_norm(a, b) : T{-1};
        };
        if (solver.step_adaptive(forward, trial_h, T{0.25}, counted, output,
                                 trial_cache, bad_norm, bad_defect, selection,
                                 options) ||
            !same_partitioned_state(output, sentinel) || trial_h != h ||
            counted.first_calls > baseline.first_calls + ENDPOINTS)
            result.failures |= 8;
    }
    if constexpr (NEWTON) {
        Cache fresh;
        State output = sentinel;
        T trial_h = T{0.2};
        std::size_t calls = 0;
        const auto bad_probe = [&](const State& a, const State& b) {
            return calls++ == Tableau::STAGES
                       ? std::numeric_limits<T>::quiet_NaN()
                       : norm(a, b);
        };
        if (solver.step_adaptive(initial, trial_h, T{0.25}, system, output,
                                 fresh, bad_probe, defect_norm, selection,
                                 options) ||
            !same_partitioned_state(output, sentinel) || trial_h != T{0.2} ||
            calls <= Tableau::STAGES + 1)
            result.failures |= 8;
    }
    {
        System bounded{};
        bounded.time_ceiling = T{0.125};
        Cache fresh;
        State output{};
        T trial_h = T{0.2};
        if (!solver.step_adaptive(initial, trial_h, T{0.25}, bounded, output,
                                  fresh, norm, defect_norm, selection,
                                  options) ||
            trial_h > T{0.125})
            result.failures |= 16;
    }
    // Endpoint defects control both components at a Verlet turning point.
    {
        Cache fresh;
        State output{};
        T trial_h = T{0.2};
        if (!solver.step_adaptive(State{1, 0}, trial_h, T{0.25}, system, output,
                                  fresh, norm, defect_norm, selection,
                                  options) ||
            !(trial_h > 0 && trial_h < T{0.25}))
            result.failures |= 32;
    }
    return result;
}

template <typename Tableau>
UTILS_ODE_PARTITIONED_HOST_DEVICE PartitionedChecks
check_partitioned_doubling() {
    using T = typename Tableau::val_type;
    using State = PartitionedState<T>;
    using System = PartitionedSystem<T>;
    using Predictor =
        std::conditional_t<Tableau::IMPLICIT, ode::EquistagePredictor<4>,
                           ode::ContinuousOutputPredictor>;
    const ode::PartitionedIntegrator solver(Tableau{}, Predictor{});
    using Cache = typename decltype(solver)::template StepCache<State, System>;
    constexpr T EPSILON = std::numeric_limits<T>::epsilon();
    constexpr bool SINGLE = std::numeric_limits<T>::digits <= 24;
    constexpr T UPPER = SINGLE ? T{1e-5} : T{1e-9};
    const ode::FixedPointOptions<T> options{8 * EPSILON, 100};
    const PartitionedNorm<T> norm;
    const State initial{1, T{0.3}};
    PartitionedChecks result;
    const System system{};
    Cache cache;
    State state = initial;
    T h = 1;
    if (!solver.step_adaptive(state, h, T{1}, UPPER, T{0}, system, cache, norm,
                              options) ||
        !(h > 0 && h < 1) ||
        !(std::abs(state.q - std::cos(h) - initial.p * std::sin(h)) <=
          8 * UPPER + 512 * EPSILON))
        result.failures |= 1;
    result.value = static_cast<double>(state.q);
    result.selected_h = static_cast<double>(h);

    // Rebuild the accepted composition, excluding its discarded full trial.
    Cache manual_cache;
    State manual = initial;
    if (!solver.step(initial, h / 2, system, manual, manual_cache, norm,
                     options) ||
        !solver.step(manual, h / 2, system, manual, manual_cache, norm,
                     options) ||
        !same_partitioned_state(state, manual))
        result.failures |= 2;
    const auto reject = [](const State&, const State&) { return T{1}; };
    if constexpr (Tableau::IMPLICIT) {
        const System actual{}, expected{};
        State output{};
        auto c = cache, m = manual_cache;
        solver.step(state, h / 2, actual, output, c, reject,
                    ode::FixedPointOptions<T>{EPSILON, 1});
        solver.step(manual, h / 2, expected, output, m, reject,
                    ode::FixedPointOptions<T>{EPSILON, 1});
        for (std::size_t i = 0; i < Tableau::STAGES; ++i)
            if (!same_partitioned_state(actual.samples[i], expected.samples[i]))
                result.failures |= 2;
    }

    // Fail after a successful full trial and first half, during the second.
    const System full_calls{}, half_calls{};
    auto full_cache = cache, half_cache = cache;
    State full{}, first_half{};
    const T length = h / 2;
    if (!solver.step(state, length, full_calls, full, full_cache, norm,
                     options) ||
        !solver.step(state, length / 2, half_calls, first_half, half_cache,
                     norm, options))
        result.failures |= 4;
    System failing{};
    failing.second_limit = full_calls.second_calls + half_calls.second_calls;
    auto saved_cache = cache;
    auto saved = state;
    T failed_h = length;
    if (solver.step_adaptive(saved, failed_h, length, UPPER, T{0}, failing,
                             cache, norm, options) ||
        !same_partitioned_state(saved, state) || failed_h != length ||
        failing.first_calls <= full_calls.first_calls + half_calls.first_calls)
        result.failures |= 4;
    State a{}, b{};
    auto actual_cache = cache, expected_cache = saved_cache;
    if (!solver.step(state, length, system, a, actual_cache, norm, options) ||
        !solver.step(state, length, system, b, expected_cache, norm, options) ||
        !same_partitioned_state(a, b))
        result.failures |= 4;

    // The same dimensionless problem must shrink below machine epsilon in
    // physical time without an absolute time-unit cutoff.
    constexpr T RATE = SINGLE ? T{1e10} : T{1e20};
    System fast{};
    fast.rate = RATE;
    State scaled = initial;
    T scaled_h = 1 / RATE;
    if (!solver.step_adaptive(scaled, scaled_h, T{1} / RATE, UPPER, T{0}, fast,
                              norm, options) ||
        !(scaled_h > 0 && scaled_h < T{1} / RATE) ||
        !(std::abs(scaled.q - std::cos(RATE * scaled_h) -
                   initial.p * std::sin(RATE * scaled_h)) <=
          8 * UPPER + 512 * EPSILON))
        result.failures |= 8;
    T reverse_h = -h;
    if (!solver.step_adaptive(state, reverse_h, h, UPPER, T{0}, system, norm,
                              options) ||
        !(reverse_h < 0) ||
        !(std::abs(state.q - std::cos(h + reverse_h) -
                   initial.p * std::sin(h + reverse_h)) <=
          16 * UPPER + 1024 * EPSILON))
        result.failures |= 16;

    State unchanged = state;
    T bad_h = h;
    bool rejected = false;
#if defined(__CUDA_ARCH__)
    rejected =
        !solver.step_adaptive(state, bad_h, std::numeric_limits<T>::infinity(),
                              UPPER, T{0}, system, norm, options);
#else
    try {
        solver.step_adaptive(state, bad_h, std::numeric_limits<T>::infinity(),
                             UPPER, T{0}, system, norm, options);
    } catch (const std::invalid_argument&) { rejected = true; }
#endif
    if (!rejected || !same_partitioned_state(state, unchanged) || bad_h != h)
        result.failures |= 32;
    return result;
}

template <typename T>
struct DirectPartitionedSystem : PartitionedSystem<T> {
    using val_type = T;
    mutable std::size_t solves = 0;
    UTILS_ODE_PARTITIONED_HOST_DEVICE bool
    solve_stage(PartitionedState<T>& state, T a, T b, std::size_t) const {
        ++solves;
        state.p = (state.p - b * state.q) / (1 + a * b);
        state.q += a * state.p;
        return true;
    }
};

template <typename T>
struct SeparablePartitionedSystem : PartitionedSystem<T> {
    using val_type = T;
    static constexpr bool SEPARABLE = true;
};

template <typename T>
UTILS_ODE_PARTITIONED_HOST_DEVICE PartitionedChecks check_partitioned_direct() {
    const ode::PartitionedIntegrator solver(ode::BorisTableau<T>{},
                                            ode::EquistagePredictor<8>{});
    using State = PartitionedState<T>;
    using System = DirectPartitionedSystem<T>;
    typename decltype(solver)::template StepCache<State, System> cache;
    typename decltype(solver)::template Stages<State, System> stages;
    const State initial{1, T{0.3}, 2, 3};
    State state = initial;
    PartitionedChecks result;
    for (T h : {T{0.125}, T{0.125}, T{-0.125}, T{-0.125}}) {
        const System system{};
        if (!solver.step(state, h, system, state, cache, stages) ||
            system.solves != 3 || system.first_calls != 2 ||
            system.second_calls != 1 || stages.first_evaluated[1] ||
            stages.second_evaluated[0] || stages.second_evaluated[2])
            result.failures |= 1;
    }
    constexpr T EPSILON = std::numeric_limits<T>::epsilon();
    if (!(PartitionedNorm<T>{}(state, initial) <= 32 * EPSILON))
        result.failures |= 2;
    cache.reset();
    const SeparablePartitionedSystem<T> separable{};
    if (!solver.step(initial, T{0.125}, separable, state, cache, stages,
                     PartitionedNorm<T>{}, ode::NewtonOptions<T>{}) ||
        separable.first_calls != 2 || separable.second_calls != 1)
        result.failures |= 4;
    cache.reset();
    const System direct{};
    T h = T{0.1};
    if (!solver.step_adaptive(State{1, T{0.3}}, h, T{0.2}, direct, state, cache,
                              PartitionedNorm<T>{}, JointPartitionedNorm<T>{},
                              {T{1e-3}, T{1e-3}, 40}))
        result.failures |= 8;
    result.value = static_cast<double>(state.q);
    result.selected_h = static_cast<double>(h);
    return result;
}

enum class PartitionedCheck { PREDICTOR, ADAPTIVE, DOUBLING, DIRECT };

template <typename Tableau,
          typename Predictor,
          bool NEWTON,
          PartitionedCheck KIND>
UTILS_ODE_PARTITIONED_HOST_DEVICE PartitionedChecks run_partitioned_check() {
    if constexpr (KIND == PartitionedCheck::PREDICTOR)
        return check_partitioned_predictor<Tableau, Predictor, NEWTON>();
    else if constexpr (KIND == PartitionedCheck::ADAPTIVE)
        return check_partitioned_adaptive<Tableau, NEWTON>();
    else if constexpr (KIND == PartitionedCheck::DOUBLING)
        return check_partitioned_doubling<Tableau>();
    else
        return check_partitioned_direct<typename Tableau::val_type>();
}

inline constexpr std::size_t PARTITIONED_CHECK_COUNT = 24;

template <typename T, typename Run>
void for_each_partitioned_check(const Run& run) {
    using A = ode::ContinuousOutputPredictor;
    using B1 = ode::AdditionalStagePredictor<1>;
    using B3 = ode::AdditionalStagePredictor<3>;
    using C1 = ode::EquistagePredictor<1>;
    using C8 = ode::EquistagePredictor<8>;
    using Gauss4 = ode::PartitionedRungeKuttaTableau<ode::Gauss4Tableau<T>>;
    using Gauss6 = ode::PartitionedRungeKuttaTableau<ode::Gauss6Tableau<T>>;
    using Gauss8 = ode::PartitionedRungeKuttaTableau<ode::Gauss8Tableau<T>>;
    using Gauss12 = ode::PartitionedRungeKuttaTableau<ode::Gauss12Tableau<T>>;
    using IIIA = ode::PartitionedRungeKuttaTableau<ode::LobattoIIIA4Tableau<T>>;
    using Verlet = ode::PositionVerletTableau<T>;
    using Velocity = ode::VelocityVerletTableau<T>;
    using Lobatto = ode::LobattoIIIAIIIB4Tableau<T>;
    using Midpoint = ode::ImplicitMidpointPairTableau<T>;
    using Boris = ode::BorisTableau<T>;
    using enum PartitionedCheck;
    run.template operator()<Lobatto, C8, false, PREDICTOR>(0);
    run.template operator()<Lobatto, A, false, PREDICTOR>(1);
    run.template operator()<Midpoint, C8, true, PREDICTOR>(2);
    run.template operator()<Verlet, C8, false, PREDICTOR>(3);
    run.template operator()<Velocity, C1, true, PREDICTOR>(4);
    run.template operator()<Boris, C8, false, PREDICTOR>(5);
    run.template operator()<Gauss4, B1, false, PREDICTOR>(6);
    run.template operator()<Gauss4, B3, true, PREDICTOR>(7);
    run.template operator()<Gauss6, B3, false, PREDICTOR>(8);
    run.template operator()<Gauss12, B1, false, PREDICTOR>(9);
    run.template operator()<IIIA, B1, false, PREDICTOR>(10);
    run.template operator()<Gauss8, C8, false, PREDICTOR>(11);
    run.template operator()<Verlet, A, false, ADAPTIVE>(12);
    run.template operator()<Velocity, A, false, ADAPTIVE>(13);
    run.template operator()<Lobatto, A, false, ADAPTIVE>(14);
    run.template operator()<Midpoint, A, true, ADAPTIVE>(15);
    run.template operator()<Boris, A, true, ADAPTIVE>(16);
    run.template operator()<Gauss4, A, true, ADAPTIVE>(17);
    run.template
    operator()<ode::PartitionedRungeKuttaTableau<ode::RungeKutta4Tableau<T>>, A,
               false, DOUBLING>(18);
    run.template operator()<Verlet, A, false, DOUBLING>(19);
    run.template operator()<Lobatto, A, false, DOUBLING>(20);
    run.template operator()<Midpoint, A, false, DOUBLING>(21);
    run.template operator()<Boris, A, false, DOUBLING>(22);
    run.template operator()<Boris, C8, false, DIRECT>(23);
}

}  // namespace ode_test

#undef UTILS_ODE_PARTITIONED_HOST_DEVICE

#endif  // UTILS_ODE_TESTS_PARTITIONED_CAPABILITY_CHECKS_HPP_
