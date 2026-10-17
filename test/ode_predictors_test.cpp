// Adapted from GEPT's tests/test_implicit_predictors.cpp.
#include "numerics/ode.h"
#include "ode/finite_error.h"
#include "ode/implicit_predictor_checks.h"

#include <cstdlib>
#include <iostream>
#include <vector>

namespace {

void check(bool condition, const char* message) {
    if (!condition) throw std::runtime_error(message);
}

template <typename T>
T power(T x, std::size_t n) {
    T result = 1;
    for (std::size_t i = 0; i < n; ++i) result *= x;
    return result;
}

template <typename T>
struct State {
    T x = 0;
    T y = 0;
    T time = 0;
    bool operator==(const State&) const = default;
};

enum class Equation { POLYNOMIAL, BRANCH, LINEAR, NONLINEAR };

template <typename T>
struct System {
    using val_type = T;
    using derivative_type = std::array<T, 2>;

    Equation equation = Equation::NONLINEAR;
    std::size_t degree = 0;
    mutable std::vector<State<T>> samples;
    std::size_t fail_at = std::numeric_limits<std::size_t>::max();

    bool evaluate(const State<T>& state, derivative_type& derivative) const {
        samples.push_back(state);
        if (samples.size() - 1 == fail_at) return false;
        switch (equation) {
            case Equation::POLYNOMIAL:
                derivative = {power(state.time, degree), 0};
                break;
            case Equation::BRANCH:
                derivative = {power(state.time, degree), state.x};
                break;
            case Equation::LINEAR:
                derivative = {state.x, state.y};
                break;
            case Equation::NONLINEAR:
                derivative = {1 + state.x * state.x + state.time / 2, -state.y};
                break;
        }
        return true;
    }
    template <std::size_t N>
    State<T> advance(State<T> state,
                     const std::array<derivative_type, N>& derivatives,
                     const std::array<T, N>& weights,
                     std::size_t count,
                     T h,
                     T fraction) const {
        for (std::size_t i = 0; i < count; ++i) {
            state.x += h * weights[i] * derivatives[i][0];
            state.y += h * weights[i] * derivatives[i][1];
        }
        state.time += h * fraction;
        return state;
    }
};

template <typename T>
T distance(const State<T>& a, const State<T>& b) {
    return ode_test::finite_max(
        {std::abs(a.x - b.x), std::abs(a.y - b.y), std::abs(a.time - b.time)});
}

template <typename Solver>
auto guesses(
    const Solver& solver,
    const State<typename Solver::val_type>& initial,
    typename Solver::val_type h,
    const System<typename Solver::val_type>& system,
    typename Solver::template StepCache<State<typename Solver::val_type>,
                                        System<typename Solver::val_type>>
        cache,
    std::size_t extra_calls) {
    using T = typename Solver::val_type;
    system.samples.clear();
    State<T> output;
    solver.step(initial, h, system, output, cache, distance<T>,
                {std::numeric_limits<T>::epsilon(), 1});
    check(system.samples.size() == extra_calls + Solver::STAGES,
          "predictor used an unexpected number of RHS evaluations");
    std::array<State<T>, Solver::STAGES> result{};
    for (std::size_t i = 0; i < Solver::STAGES; ++i)
        result[i] = system.samples[extra_calls + i];
    return result;
}

template <typename Tableau, std::size_t M>
void test_moments() {
    using T = typename Tableau::val_type;
    constexpr auto S = Tableau::STAGES;
    constexpr T EPSILON = std::numeric_limits<T>::epsilon();
    const Tableau tableau;
    for (T ratio : {T{0}, T{1}, T{0.5}, T{1.5}, T{-1}, T{-0.5}}) {
        const auto coefficients =
            tableau.template additional_predictor<M>(ratio);
        for (std::size_t i = 0; i < S; ++i)
            for (std::size_t k = 0; k < S + (M == 1 ? 1 : 2); ++k) {
                T actual = 0, expected = 0, scale = 1;
                for (std::size_t j = 0; j < S; ++j) {
                    const T term =
                        coefficients.alpha[i][j] * power(tableau.c[j], k);
                    actual += term;
                    scale += std::abs(term);
                    expected += ratio * tableau.A[i][j] *
                                power(1 + ratio * tableau.c[j], k);
                }
                for (std::size_t j = 0; j < M; ++j) {
                    const T term =
                        coefficients.nu[i][j] * power(coefficients.c[j], k);
                    actual += term;
                    scale += std::abs(term);
                }
                check(std::abs(actual - expected) < 256 * EPSILON * scale,
                      "additional-stage predictor violates HLW (6.9)");
            }
        // Check C(s), including the two endpoint derivatives of B3's last
        // extra stage. The higher moments check its chosen C(s+2) extension.
        for (std::size_t i = 0; i < M; ++i)
            for (std::size_t k = 0; k < S + (M == 3 && i == 2 ? 2 : 0); ++k) {
                T actual = 0, scale = 1;
                for (std::size_t j = 0; j < S + i; ++j) {
                    const T node = j < S ? tableau.c[j] : coefficients.c[j - S];
                    const T term = coefficients.mu[i][j] * power(node, k);
                    actual += term;
                    scale += std::abs(term);
                }
                const T expected = power(coefficients.c[i], k + 1) / T(k + 1);
                check(std::abs(actual - expected) < 256 * EPSILON * scale,
                      "additional stages violate their collocation moments");
            }
    }
}

template <typename Tableau, std::size_t M>
void test_additional_order() {
    using T = typename Tableau::val_type;
    constexpr auto S = Tableau::STAGES;
    constexpr std::size_t DEGREE = S + (M == 1 ? 1 : 2);
    const ode::Integrator solver(Tableau{}, ode::AdditionalStagePredictor<M>{});
    System<T> system;
    system.equation = Equation::POLYNOMIAL;
    system.degree = DEGREE;
    const Tableau tableau;
    const auto error = [&](T h, T ratio) {
        typename decltype(solver)::template StepCache<State<T>, System<T>>
            cache;
        State<T> state;
        check(solver.step(state, h, system, state, cache, distance<T>),
              "additional-stage warmup failed");
        const auto predicted =
            guesses(solver, state, ratio * h, system, cache, M == 1 ? 1 : 2);
        T result = 0;
        for (std::size_t i = 0; i < S; ++i) {
            // These are discrete RK stages, not exact solution samples.
            T exact = state.x;
            for (std::size_t j = 0; j < S; ++j)
                exact += ratio * h * tableau.A[i][j] *
                         power(state.time + ratio * h * tableau.c[j], DEGREE);
            result =
                ode_test::finite_max(result, std::abs(predicted[i].x - exact));
            check(std::abs(predicted[i].time -
                           (state.time + ratio * h * tableau.c[i])) <
                      16 * std::numeric_limits<T>::epsilon(),
                  "additional-stage predictor advanced a stage clock twice");
        }
        return result;
    };
    for (T ratio : {T{1}, T{0.5}, T{-0.5}}) {
        const T coarse = error(T{0.25}, ratio);
        const T fine = error(T{0.125}, ratio);
        const T order = std::log2(coarse / fine);
        check(std::abs(order - T(DEGREE + 1)) < T{0.01},
              "additional-stage guesses have the wrong error order");
    }
    if constexpr (M == 3) {
        system.equation = Equation::BRANCH;
        system.degree = S;
        for (T ratio : {T{1}, T{0.5}, T{-0.5}}) {
            typename decltype(solver)::template StepCache<State<T>, System<T>>
                cache;
            State<T> state;
            const T h = T{0.25};
            check(solver.step(state, h, system, state, cache, distance<T>),
                  "branched-tree warmup failed");
            const auto predicted =
                guesses(solver, state, ratio * h, system, cache, 2);
            std::array<T, S> x{};
            for (std::size_t i = 0; i < S; ++i) {
                x[i] = state.x;
                for (std::size_t j = 0; j < S; ++j)
                    x[i] += ratio * h * tableau.A[i][j] *
                            power(state.time + ratio * h * tableau.c[j], S);
            }
            for (std::size_t i = 0; i < S; ++i) {
                T y = state.y;
                for (std::size_t j = 0; j < S; ++j)
                    y += ratio * h * tableau.A[i][j] * x[j];
                check(std::abs(predicted[i].y - y) <
                          4096 * std::numeric_limits<T>::epsilon() *
                              power(h, S + 2),
                      "B3 violates the branched-tree condition (6.10)");
            }
        }
    }
}

template <std::size_t K>
void test_equistage_order() {
    using T = long double;
    const ode::Integrator solver(ode::Gauss4Tableau<T>{},
                                 ode::EquistagePredictor<K>{});
    const ode::Integrator reference(ode::Gauss4Tableau<T>{});
    System<T> system;
    system.equation = Equation::LINEAR;
    const auto error = [&](T h) {
        typename decltype(solver)::template StepCache<State<T>, System<T>>
            cache;
        State<T> state{1, 0, 0};
        for (std::size_t i = 0; i < K + 3; ++i)
            check(solver.step(state, h, system, state, cache, distance<T>,
                              {4 * std::numeric_limits<T>::epsilon(), 100}),
                  "equistage warmup failed");
        const auto predicted = guesses(solver, state, h, system, cache, 0);
        typename decltype(reference)::template StepCache<State<T>, System<T>>
            fresh;
        typename decltype(reference)::template Stages<State<T>, System<T>>
            stages;
        State<T> output;
        check(
            reference.step(state, h, system, output, fresh, stages, distance<T>,
                           {4 * std::numeric_limits<T>::epsilon(), 100}),
            "reference discrete stage solve failed");
        T result = 0;
        for (std::size_t i = 0; i < solver.STAGES; ++i)
            result = ode_test::finite_max(
                result, std::abs(predicted[i].x - stages.states[i].x));
        return result / state.x;
    };
    const T order = std::log2(error(.08L) / error(.04L));
    std::cout << "Equistage k=" << K << ": guess error order " << order << '\n';
    check(std::abs(order - T(K + 1)) < .45L,
          "equistage guesses have the wrong error order");
}

template <typename Predictor>
void test_failure_and_reset(Predictor predictor) {
    const ode::Integrator solver(ode::Gauss4Tableau<double>{}, predictor);
    using Cache = typename decltype(solver)::template StepCache<State<double>,
                                                                System<double>>;
    using Stages = typename decltype(solver)::template Stages<State<double>,
                                                              System<double>>;
    System<double> system;
    Cache cache;
    State<double> state{.1, .8, 2};
    constexpr double H = .025;
    for (int i = 0; i < 12; ++i)
        check(solver.step(state, H, system, state, cache, distance<double>,
                          {1e-15, 100}),
              "nonlinear predictor warmup failed");
    constexpr auto EXTRA =
        Predictor::EXTRA_STAGES == 3 ? 2 : Predictor::EXTRA_STAGES;
    const auto expected = guesses(solver, state, H, system, cache, EXTRA);
    const State<double> sentinel{91, 92, 93};
    for (std::size_t fail_at : {std::size_t{0}, std::size_t{1}, EXTRA + 1}) {
        System<double> failing;
        failing.fail_at = fail_at;
        State<double> output = sentinel;
        Stages stages;
        for (auto& stage : stages.states) stage = sentinel;
        for (auto& derivative : stages.derivatives) derivative = {94, 95};
        check(!solver.step(state, H * 2, failing, output, cache, stages,
                           distance<double>) &&
                  output == sentinel,
              "failed RHS changed the output");
        for (const auto& stage : stages.states)
            check(stage == sentinel, "failed RHS changed recorded stages");
        for (const auto& derivative : stages.derivatives)
            check(derivative == std::array{94., 95.},
                  "failed RHS changed recorded derivatives");
        check(guesses(solver, state, H, system, cache, EXTRA) == expected,
              "failed changed-step solve corrupted predictor history");
    }
    for (bool invalid : {false, true}) {
        State<double> output = sentinel;
        const auto norm = [invalid](const auto&, const auto&) {
            return invalid ? std::numeric_limits<double>::quiet_NaN() : 1.;
        };
        check(!solver.step(state, H, system, output, cache, norm, {1e-15, 1}) &&
                  output == sentinel &&
                  guesses(solver, state, H, system, cache, EXTRA) == expected,
              "failed iteration changed output or predictor history");
    }
    double h = H;
    State<double> output = sentinel;
    check(!solver.step_adaptive(state, h, .3, system, output, cache,
                                distance<double>, distance<double>,
                                {1e-25, 1e-12, 1}) &&
              h == H && output == sentinel &&
              guesses(solver, state, H, system, cache, EXTRA) == expected,
          "rejected adaptive trial changed predictor history");
    cache.reset();
    const auto cold_extra = Predictor::EXTRA_STAGES == 3 ? 1 : 0;
    System<double> failing;
    failing.fail_at = 0;
    output = sentinel;
    check(!solver.step(state, H, failing, output, cache, distance<double>) &&
              output == sentinel,
          "failed cold predictor changed the output");
    check(guesses(solver, state, H, system, cache, cold_extra) ==
              guesses(solver, state, H, system, Cache{}, cold_extra),
          "reset retained predictor history");
}

void test_equistage_history() {
    const ode::Gauss4Tableau<double> tableau;
    const ode::Integrator solver(tableau, ode::EquistagePredictor<8>{});
    const ode::Integrator fallback(tableau);
    const ode::Integrator one_step(tableau, ode::EquistagePredictor<1>{});
    System<double> system;
    using State = State<double>;
    using System = System<double>;
    decltype(solver)::StepCache<State, System> cache;
    decltype(fallback)::StepCache<State, System> fallback_cache;
    std::vector<std::array<double, 2>> increments;
    State state{.1, .8, 1};
    constexpr double H = .025;
    for (int step = 0; step < 14; ++step) {
        if (!increments.empty()) {
            const auto predicted = guesses(solver, state, H, system, cache, 0);
            const std::size_t k = std::min(increments.size(), std::size_t{8});
            for (std::size_t i = 0; i < solver.STAGES; ++i) {
                double expected = state.x;
                // Evaluate the interpolation polynomial at zero through the
                // old increments at -1,...,-k, independently of binomials.
                for (std::size_t j = 0; j < k; ++j) {
                    double weight = 1;
                    for (std::size_t l = 0; l < k; ++l)
                        if (l != j)
                            weight *= double(l + 1) / (double(l) - double(j));
                    expected +=
                        weight * increments[increments.size() - 1 - j][i];
                }
                check(std::abs(predicted[i].x - expected) < 1e-13 &&
                          std::abs(predicted[i].time -
                                   (state.time + H * tableau.c[i])) < 1e-15,
                      "equistage startup or ring history is incorrect");
            }
        }
        decltype(solver)::Stages<State, System> stages;
        State next, other;
        check(solver.step(state, H, system, next, cache, stages,
                          distance<double>, {1e-15, 100}) &&
                  fallback.step(state, H, system, other, fallback_cache,
                                distance<double>, {1e-15, 100}),
              "equistage history warmup failed");
        increments.push_back(
            {stages.states[0].x - state.x, stages.states[1].x - state.x});
        state = next;
    }
    for (double h : {H / 2, -H}) {
        const auto predicted = guesses(solver, state, h, system, cache, 0);
        const auto continuous =
            guesses(fallback, state, h, system, fallback_cache, 0);
        for (std::size_t i = 0; i < solver.STAGES; ++i)
            check(distance(predicted[i], continuous[i]) < 1e-13,
                  "changed signed step must fall back to continuous output");
    }
    // A successful changed step starts a new sequence with one old increment.
    for (double h : {H / 2, -H, 0.}) {
        auto changed = cache;
        decltype(one_step)::StepCache<State, System> fresh;
        State next, other;
        check(solver.step(state, h, system, next, changed, distance<double>,
                          {1e-15, 100}) &&
                  one_step.step(state, h, system, other, fresh,
                                distance<double>, {1e-15, 100}),
              "changed or zero step failed");
        const auto predicted =
            guesses(solver, next, h == 0 ? H : h, system, changed, 0);
        const auto restarted =
            guesses(one_step, next, h == 0 ? H : h, system, fresh, 0);
        for (std::size_t i = 0; i < solver.STAGES; ++i)
            check(distance(predicted[i], restarted[i]) < 1e-13,
                  "accepted changed step retained an incompatible history");
    }
}

template <template <typename> typename Tableau>
void test_tableau() {
    test_moments<Tableau<float>, 1>();
    test_moments<Tableau<double>, 1>();
    test_moments<Tableau<long double>, 1>();
    test_additional_order<Tableau<long double>, 1>();
    if constexpr (requires {
                      Tableau<double>{}.template additional_predictor<3>(1);
                  }) {
        test_moments<Tableau<float>, 3>();
        test_moments<Tableau<double>, 3>();
        test_moments<Tableau<long double>, 3>();
        test_additional_order<Tableau<long double>, 3>();
    }
}

template <typename T>
void test_shared_checks() {
    const auto results = ode_test::check_implicit_predictors<T>();
    for (std::size_t i = 0; i < results.size(); ++i) {
        const auto result = results[i];
        if (result.passed()) continue;
        std::cerr << "Predictor " << i << ", scalar bytes=" << sizeof(T)
                  << ": orbit=" << result.orbit << ", clocks=" << result.clocks
                  << ", atomicity=" << result.atomicity
                  << ", adaptive=" << result.adaptive
                  << ", reset=" << result.reset
                  << ", reverse error=" << result.reverse_error << '\n';
        check(false, "shared predictor checks failed");
    }
}

}  // namespace

int main() {
    try {
        test_tableau<ode::Gauss4Tableau>();
        test_tableau<ode::Gauss6Tableau>();
        test_tableau<ode::Gauss8Tableau>();
        test_tableau<ode::Gauss10Tableau>();
        test_tableau<ode::Gauss12Tableau>();
        test_tableau<ode::LobattoIIIA4Tableau>();
        test_equistage_order<1>();
        test_equistage_order<2>();
        test_equistage_order<4>();
        test_equistage_order<8>();
        test_equistage_history();
        test_failure_and_reset(ode::AdditionalStagePredictor<1>{});
        test_failure_and_reset(ode::AdditionalStagePredictor<3>{});
        test_failure_and_reset(ode::EquistagePredictor<1>{});
        test_failure_and_reset(ode::EquistagePredictor<8>{});
        test_shared_checks<float>();
        test_shared_checks<double>();
    } catch (const std::exception& error) {
        std::cerr << "test_implicit_predictors: " << error.what() << '\n';
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
