// Adapted from GEPT's tests/implicit_predictor_checks.hpp.
#ifndef UTILS_ODE_TESTS_IMPLICIT_PREDICTOR_CHECKS_HPP_
#define UTILS_ODE_TESTS_IMPLICIT_PREDICTOR_CHECKS_HPP_

#include "finite_error.h"
#include "numerics/ode.h"

#if defined(__CUDACC__)
#define UTILS_ODE_PREDICTOR_HOST_DEVICE __host__ __device__
#else
#define UTILS_ODE_PREDICTOR_HOST_DEVICE
#endif

namespace ode_test {

template <typename T>
struct PredictorState {
    using val_type = T;
    T value = T{0.7};
    T time = T{0.25};
};

template <typename T>
struct PredictorNorm {
    using val_type = T;
    UTILS_ODE_PREDICTOR_HOST_DEVICE T
    operator()(const PredictorState<T>& a, const PredictorState<T>& b) const {
        return ode_test::finite_max(std::abs(a.value - b.value),
                                    std::abs(a.time - b.time));
    }
};

template <typename T, std::size_t S>
struct PredictorSystem {
    using val_type = T;
    using derivative_type = T;
    mutable std::size_t calls = 0;
    mutable std::array<PredictorState<T>, S + 2> samples{};
    std::size_t fail_at = std::numeric_limits<std::size_t>::max();

    UTILS_ODE_PREDICTOR_HOST_DEVICE bool evaluate(
        const PredictorState<T>& state,
        T& derivative) const {
        if (calls < samples.size()) samples[calls] = state;
        if (calls++ == fail_at) return false;
        derivative = state.time - state.value * state.value / 4;
        return true;
    }
    template <std::size_t N>
    UTILS_ODE_PREDICTOR_HOST_DEVICE PredictorState<T> advance(
        PredictorState<T> state,
        const std::array<T, N>& derivatives,
        const std::array<T, N>& weights,
        std::size_t count,
        T h,
        T fraction) const {
        for (std::size_t i = 0; i < count; ++i)
            state.value += h * weights[i] * derivatives[i];
        state.time += h * fraction;
        return state;
    }
};

struct ImplicitPredictorChecks {
    bool orbit = false;
    bool clocks = false;
    bool atomicity = false;
    bool adaptive = false;
    bool reset = false;
    double value = 0;
    double starting_value = 0;
    double reverse_error = 0;

    UTILS_ODE_PREDICTOR_HOST_DEVICE bool passed() const {
        return orbit && clocks && atomicity && adaptive && reset;
    }
};

// Nonlinear, nonautonomous solves with opaque state types exercise the same
// predictor and cache paths on the CPU and on the device.
template <typename Tableau, typename Predictor>
UTILS_ODE_PREDICTOR_HOST_DEVICE ImplicitPredictorChecks
check_implicit_predictor() {
    using T = typename Tableau::val_type;
    using State = PredictorState<T>;
    using System = PredictorSystem<T, Tableau::STAGES>;
    const Tableau tableau{};
    const ode::Integrator solver(tableau, Predictor{});
    const ode::Integrator reference(tableau);
    typename decltype(solver)::template StepCache<State, System> cache;
    typename decltype(reference)::template StepCache<State, System>
        reference_cache;
    const System system{};
    const PredictorNorm<T> norm;
    constexpr T EPSILON = std::numeric_limits<T>::epsilon();
    constexpr T H = T{1} / 32;
    constexpr ode::FixedPointOptions<T> OPTIONS{8 * EPSILON, 100};
    constexpr std::size_t EXTRA =
        Predictor::EXTRA_STAGES == 3 ? 2 : Predictor::EXTRA_STAGES;
    ImplicitPredictorChecks checks;
    State state, expected;
    for (int i = 0; i < 12; ++i)
        if (!solver.step(state, H, system, state, cache, norm, OPTIONS) ||
            !reference.step(expected, H, system, expected, reference_cache,
                            norm, OPTIONS))
            return checks;

    // One forced, rejected iteration exposes the starting guesses without
    // changing the successful-step cache.
    const auto reject = [](const State&, const State&) { return T{1}; };
    State output{91, 92};
    system.calls = 0;
    if (solver.step(state, H, system, output, cache, reject, {EPSILON, 1}))
        return checks;
    const auto before = system.samples;
    checks.starting_value =
        static_cast<double>(before[EXTRA + Tableau::STAGES - 1].value);
    checks.clocks = system.calls == EXTRA + Tableau::STAGES;
    for (std::size_t i = 0; i < Tableau::STAGES; ++i)
        checks.clocks &=
            std::abs(before[EXTRA + i].time - (state.time + H * tableau.c[i])) <
            8 * EPSILON;
    if constexpr (Predictor::EXTRA_STAGES > 0) {
        const auto coefficients =
            tableau.template additional_predictor<Predictor::EXTRA_STAGES>(
                T{1});
        constexpr std::size_t LAST = EXTRA - 1;
        checks.clocks &=
            std::abs(before[LAST].time -
                     (state.time - H + H * coefficients.c.back())) <
            8 * EPSILON;
        if constexpr (Predictor::EXTRA_STAGES == 3)
            checks.clocks &= before[0].time == state.time;
    }

    System failing;
    failing.fail_at = EXTRA + 1;
    checks.atomicity =
        !solver.step(state, 2 * H, failing, output, cache, norm, OPTIONS) &&
        output.value == 91 && output.time == 92;
    system.calls = 0;
    solver.step(state, H, system, output, cache, reject, {EPSILON, 1});
    for (std::size_t i = 0; i < Tableau::STAGES; ++i)
        checks.atomicity &=
            norm(before[EXTRA + i], system.samples[EXTRA + i]) == 0;

    T adaptive_h = H;
    checks.atomicity &=
        !solver.step_adaptive(state, adaptive_h, 2 * H, system, output, cache,
                              norm, norm, {T{1e-8}, T{1e-3}, 1}, {EPSILON, 1});
    checks.atomicity &=
        adaptive_h == H && output.value == 91 && output.time == 92;
    system.calls = 0;
    solver.step(state, H, system, output, cache, reject, {EPSILON, 1});
    for (std::size_t i = 0; i < Tableau::STAGES; ++i)
        checks.atomicity &=
            norm(before[EXTRA + i], system.samples[EXTRA + i]) == 0;

    // Successful adaptive selection is checked against a fresh fixed-step
    // solve at the selected duration, independently of the predictor.
    auto adaptive_cache = cache;
    State adaptive_state;
    checks.adaptive = solver.step_adaptive(
        state, adaptive_h, 2 * H, system, adaptive_state, adaptive_cache, norm,
        norm, {T{1e-4}, T{1e-3}, 40}, OPTIONS);
    State fixed_state;
    checks.adaptive &=
        reference.step(state, adaptive_h, system, fixed_state, norm, OPTIONS) &&
        norm(fixed_state, adaptive_state) < 256 * EPSILON;

    // Signed changes exercise ratio-dependent B coefficients and C fallback.
    constexpr std::array<T, 5> CHANGES{H / 2, -H / 2, 0, H, H};
    checks.orbit = true;
    for (T h : CHANGES)
        checks.orbit &=
            solver.step(state, h, system, state, cache, norm, OPTIONS) &&
            reference.step(expected, h, system, expected, reference_cache, norm,
                           OPTIONS);
    checks.orbit &= norm(state, expected) < 256 * EPSILON;
    checks.value = static_cast<double>(state.value);
    for (std::size_t i = CHANGES.size(); i > 0; --i)
        checks.orbit &= solver.step(state, -CHANGES[i - 1], system, state,
                                    cache, norm, OPTIONS);
    for (int i = 0; i < 12; ++i)
        checks.orbit &=
            solver.step(state, -H, system, state, cache, norm, OPTIONS);
    checks.reverse_error = static_cast<double>(norm(state, State{}));
    checks.orbit &= checks.reverse_error < 512 * EPSILON;

    cache.reset();
    State reset_result, fresh_result;
    checks.reset =
        solver.step(state, H, system, reset_result, cache, norm, OPTIONS) &&
        solver.step(state, H, system, fresh_result, norm, OPTIONS) &&
        norm(reset_result, fresh_result) == 0;
    return checks;
}

using ImplicitPredictorResults = std::array<ImplicitPredictorChecks, 23>;

// Keep enumeration on the host so each CUDA case can use a small kernel.
template <typename T, typename Run>
void for_each_implicit_predictor(const Run& run) {
    using B1 = ode::AdditionalStagePredictor<1>;
    using B3 = ode::AdditionalStagePredictor<3>;
    using C1 = ode::EquistagePredictor<1>;
    using C8 = ode::EquistagePredictor<8>;
    run.template operator()<ode::Gauss4Tableau<T>, B1>(0);
    run.template operator()<ode::Gauss4Tableau<T>, B3>(1);
    run.template operator()<ode::Gauss4Tableau<T>, C1>(2);
    run.template operator()<ode::Gauss4Tableau<T>, C8>(3);
    run.template operator()<ode::Gauss6Tableau<T>, B1>(4);
    run.template operator()<ode::Gauss6Tableau<T>, B3>(5);
    run.template operator()<ode::Gauss6Tableau<T>, C1>(6);
    run.template operator()<ode::Gauss6Tableau<T>, C8>(7);
    run.template operator()<ode::Gauss8Tableau<T>, B1>(8);
    run.template operator()<ode::Gauss8Tableau<T>, B3>(9);
    run.template operator()<ode::Gauss8Tableau<T>, C1>(10);
    run.template operator()<ode::Gauss8Tableau<T>, C8>(11);
    run.template operator()<ode::Gauss10Tableau<T>, B1>(12);
    run.template operator()<ode::Gauss10Tableau<T>, B3>(13);
    run.template operator()<ode::Gauss10Tableau<T>, C1>(14);
    run.template operator()<ode::Gauss10Tableau<T>, C8>(15);
    run.template operator()<ode::Gauss12Tableau<T>, B1>(16);
    run.template operator()<ode::Gauss12Tableau<T>, B3>(17);
    run.template operator()<ode::Gauss12Tableau<T>, C1>(18);
    run.template operator()<ode::Gauss12Tableau<T>, C8>(19);
    run.template operator()<ode::LobattoIIIA4Tableau<T>, B1>(20);
    run.template operator()<ode::LobattoIIIA4Tableau<T>, C8>(21);
    run.template operator()<ode::LobattoIIIB4Tableau<T>, C8>(22);
}

template <typename T>
ImplicitPredictorResults check_implicit_predictors() {
    ImplicitPredictorResults results;
    for_each_implicit_predictor<T>(
        [&]<typename Tableau, typename Predictor>(std::size_t i) {
            results[i] = check_implicit_predictor<Tableau, Predictor>();
        });
    return results;
}

}  // namespace ode_test

#undef UTILS_ODE_PREDICTOR_HOST_DEVICE

#endif  // UTILS_ODE_TESTS_IMPLICIT_PREDICTOR_CHECKS_HPP_
