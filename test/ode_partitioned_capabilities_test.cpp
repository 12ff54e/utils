// Adapted from GEPT's tests/test_partitioned_capabilities.cpp.
#include "ode/finite_error.h"
#include "ode/partitioned_capability_checks.h"

#include <iostream>

namespace {

void check(bool value, const char* message) {
    if (!value) throw std::runtime_error(message);
}

template <typename T>
struct PolynomialSystem : ode_test::PartitionedSystem<T> {
    using val_type = T;
    std::size_t degree = 0;
    bool branch = false;
    bool evaluate_first(const ode_test::PartitionedState<T>& state,
                        T& derivative) const {
        if (this->first_calls < this->samples.size())
            this->samples[this->first_calls] = state;
        ++this->first_calls;
        derivative = std::pow(state.first_time, static_cast<int>(degree));
        return true;
    }
    bool evaluate_second(const ode_test::PartitionedState<T>& state,
                         T& derivative) const {
        derivative =
            branch ? state.q
                   : 2 * std::pow(state.second_time, static_cast<int>(degree));
        return true;
    }
};

template <typename Tableau, std::size_t M>
void check_additional_order() {
    using T = long double;
    using State = ode_test::PartitionedState<T>;
    using System = PolynomialSystem<T>;
    using Pair = ode::PartitionedRungeKuttaTableau<Tableau>;
    const Pair tableau;
    const ode::PartitionedIntegrator solver(tableau,
                                            ode::AdditionalStagePredictor<M>{});
    const ode_test::PartitionedNorm<T> norm;
    constexpr auto S = Tableau::STAGES;
    constexpr auto DEGREE = S + (M == 1 ? 1 : 2);
    const auto error = [&](T h, T ratio) {
        typename decltype(solver)::template StepCache<State, System> cache;
        System system;
        system.degree = DEGREE;
        State state{0, 0, 0, h / 4};
        check(solver.step(state, h, system, state, cache, norm),
              "polynomial predictor warmup failed");
        system.first_calls = 0;
        State output{};
        solver.step(
            state, ratio * h, system, output, cache,
            [](const State&, const State&) { return T{1}; },
            ode::FixedPointOptions<T>{1e-18L, 1});
        constexpr auto EXTRA = M == 1 ? 1 : 2;
        check(system.first_calls == EXTRA + S,
              "additional predictor sampling count differs");
        T result = 0;
        for (std::size_t i = 0; i < S; ++i) {
            State exact = state;
            for (std::size_t j = 0; j < S; ++j) {
                exact.q +=
                    ratio * h * tableau.A_first[i][j] *
                    std::pow(state.first_time + ratio * h * tableau.c_first[j],
                             int(DEGREE));
                exact.p += ratio * h * tableau.A_second[i][j] * 2 *
                           std::pow(state.second_time +
                                        ratio * h * tableau.c_second[j],
                                    int(DEGREE));
            }
            result = ode_test::finite_max(
                result, norm(exact, system.samples[EXTRA + i]));
        }
        return result;
    };
    for (T ratio : {1.L, .5L, -.5L}) {
        const T order = std::log2(error(.25L, ratio) / error(.125L, ratio));
        check(std::abs(order - T(DEGREE + 1)) < .02L,
              "partitioned additional predictor has the wrong order");
    }
    if constexpr (M == 3) {
        for (T ratio : {1.L, .5L, -.5L}) {
            typename decltype(solver)::template StepCache<State, System> cache;
            System system;
            system.degree = S;
            system.branch = true;
            State state{};
            constexpr T H = .25L;
            check(solver.step(state, H, system, state, cache, norm),
                  "coupled polynomial predictor warmup failed");
            system.first_calls = 0;
            State output{};
            solver.step(
                state, ratio * H, system, output, cache,
                [](const State&, const State&) { return T{1}; },
                ode::FixedPointOptions<T>{1e-18L, 1});
            std::array<T, S> q{};
            for (std::size_t i = 0; i < S; ++i) {
                q[i] = state.q;
                for (std::size_t j = 0; j < S; ++j)
                    q[i] += ratio * H * tableau.A_first[i][j] *
                            std::pow(state.first_time +
                                         ratio * H * tableau.c_first[j],
                                     int(S));
            }
            for (std::size_t i = 0; i < S; ++i) {
                T expected = state.p;
                for (std::size_t j = 0; j < S; ++j)
                    expected += ratio * H * tableau.A_second[i][j] * q[j];
                check(std::abs(system.samples[2 + i].p - expected) <
                          4096 * std::numeric_limits<T>::epsilon() *
                              std::pow(H, int(S + 2)),
                      "partitioned B3 violates the coupled order condition");
            }
        }
    }
}

template <std::size_t K>
void check_equistage_order() {
    using T = long double;
    using State = ode_test::PartitionedState<T>;
    using System = ode_test::PartitionedSystem<T>;
    const ode::LobattoIIIAIIIB4Tableau<T> tableau;
    const ode::PartitionedIntegrator solver(tableau,
                                            ode::EquistagePredictor<K>{});
    const ode::PartitionedIntegrator reference(tableau);
    const ode_test::PartitionedNorm<T> norm;
    constexpr T EPSILON = std::numeric_limits<T>::epsilon();
    const ode::FixedPointOptions<T> options{4 * EPSILON, 100};
    const auto error = [&](T h) {
        typename decltype(solver)::template StepCache<State, System> cache;
        State state{1, .3L};
        const System system{};
        for (std::size_t i = 0; i < K + 3; ++i)
            check(solver.step(state, h, system, state, cache, norm, options),
                  "partitioned history warmup failed");
        State output{};
        const System sampled{};
        solver.step(
            state, h, sampled, output, cache,
            [](const State&, const State&) { return T{1}; },
            ode::FixedPointOptions<T>{EPSILON, 1});
        typename decltype(reference)::template Stages<State, System> stages;
        check(reference.step(state, h, system, output, stages, norm, options),
              "partitioned reference stage solve failed");
        T result = 0;
        for (std::size_t i = 0; i < solver.STAGES; ++i) {
            const T q = sampled.samples[i].q - stages.states[i].q;
            const T p = sampled.samples[i].p - stages.states[i].p;
            result = ode_test::finite_max(result, std::sqrt(q * q + p * p));
        }
        return result;
    };
    const T order = std::log2(error(.08L) / error(.04L));
    std::cout << "Partitioned equistage k=" << K << ": guess order " << order
              << '\n';
    check(std::abs(order - T(K + 1)) < .45L,
          "partitioned equistage predictor has the wrong order");
}

// Ordinary explicit RK does not require IMPLICIT or coefficient aliases.
struct BareRK {
    using val_type = double;
    static constexpr std::size_t ORDER = 1, STAGES = 1;
    std::array<std::array<double, 1>, 1> A{{{0}}};
    std::array<double, 1> b{1}, c{0};
};

struct BarePair {
    using val_type = double;
    using row_type = std::array<double, 1>;
    static constexpr std::size_t ORDER = 1, STAGES = 1;
    static constexpr bool IMPLICIT = false;
    std::array<row_type, 1> A_first{{{0}}}, A_second = A_first;
    row_type b_first{1}, b_second{1}, c_first{0}, c_second{0};
};

struct AuxiliaryMidpoint {
    using val_type = double;
    static constexpr std::size_t ORDER = 2, STAGES = 2;
    static constexpr bool IMPLICIT = true, SYMMETRIC = true;
    static constexpr std::size_t ERROR_ORDER = 2;
    std::array<std::array<double, 2>, 2> A{{{.5, 0}, {1, 0}}};
    std::array<double, 2> b{1, 0}, c{.5, 1};
    std::array<std::array<double, 2>, 2> beta{{{.75, .75}, {0, 2}}};
};

using CustomState = ode_test::PartitionedState<double>;
using CustomSystem = ode_test::PartitionedSystem<double>;
template <typename Tableau>
constexpr bool HAS_SYMMETRIC_ADAPTATION =
    requires(const ode::PartitionedIntegrator<double, Tableau>& solver,
             CustomState& state,
             double& h,
             typename ode::PartitionedIntegrator<double, Tableau>::
                 template StepCache<CustomState, CustomSystem>& cache) {
        solver.step_adaptive(state, h, 1., CustomSystem{}, state, cache,
                             ode_test::PartitionedNorm<double>{},
                             ode_test::JointPartitionedNorm<double>{});
    };
static_assert(!ode::PartitionedRungeKuttaTableau<BareRK>::IMPLICIT);
static_assert(!HAS_SYMMETRIC_ADAPTATION<
              ode::PartitionedRungeKuttaTableau<AuxiliaryMidpoint>>);

struct ConstantSystem : CustomSystem {
    bool evaluate_first(const CustomState& state, double& derivative) const {
        if (first_calls < samples.size()) samples[first_calls] = state;
        ++first_calls;
        derivative = 1;
        return true;
    }
    bool evaluate_second(const CustomState&, double& derivative) const {
        ++second_calls;
        derivative = 2;
        return true;
    }
};

struct SkippedAuxiliaryPair
    : ode::PartitionedRungeKuttaTableau<AuxiliaryMidpoint> {
    static constexpr bool SAMPLE_ALL_STAGES = false;
};

void check_custom_tableaux() {
    const ode::PartitionedIntegrator bare(BarePair{});
    const ode::PartitionedIntegrator lifted(
        ode::PartitionedRungeKuttaTableau<BareRK>{});
    const ode_test::PartitionedNorm<double> norm;
    const CustomState initial{1, .3};
    CustomState a{}, b{};
    check(bare.step(initial, .1, CustomSystem{}, a) &&
              lifted.step(initial, .1, CustomSystem{}, b) &&
              ode_test::same_partitioned_state(a, b),
          "minimal custom tableaux lost source compatibility");
    const ode::PartitionedIntegrator solver(
        ode::PartitionedRungeKuttaTableau<AuxiliaryMidpoint>{});
    decltype(solver)::StepCache<CustomState, ConstantSystem> cache;
    CustomState state{};
    check(solver.step(state, .125, ConstantSystem{}, state, cache, norm),
          "auxiliary-stage warmup failed");
    const ConstantSystem counted{};
    check(solver.step(state, .125, counted, state, cache, norm, {1e-14, 1}) &&
              counted.first_calls == 2 && counted.second_calls == 2,
          "lifted RK predictors must retain unused auxiliary derivatives");
    const ode::PartitionedIntegrator sparse(SkippedAuxiliaryPair{});
    decltype(sparse)::StepCache<CustomState, ConstantSystem> sparse_cache;
    state = {};
    check(sparse.step(state, .125, ConstantSystem{}, state, sparse_cache, norm),
          "sparse custom pair warmup failed");
    const ConstantSystem sampled{};
    sparse.step(state, .125, sampled, a, sparse_cache,
                [](const CustomState&, const CustomState&) { return 1.; },
                {1e-14, 1});
    check(sampled.samples[0].q == state.q && sampled.samples[0].p == state.p,
          "missing cached columns must fall back to a cold guess");
}

struct ScalarDecay {
    using derivative_type = double;
    bool evaluate(double state, double& derivative) const {
        derivative = -state;
        return true;
    }
    template <std::size_t S>
    double advance(double state,
                   const std::array<double, S>& derivatives,
                   const std::array<double, S>& weights,
                   std::size_t count,
                   double h,
                   double) const {
        for (std::size_t i = 0; i < count; ++i)
            state += h * weights[i] * derivatives[i];
        return state;
    }
};

void check_rk_probe_rejection() {
    const ode::Integrator solver(ode::Gauss4Tableau<double>{});
    decltype(solver)::StepCache<double, ScalarDecay> cache;
    const auto norm = [](double a, double b) { return std::abs(a - b); };
    std::size_t calls = 0;
    const auto bad_probe = [&](double a, double b) {
        return calls++ == solver.STAGES
                   ? std::numeric_limits<double>::quiet_NaN()
                   : norm(a, b);
    };
    const ode::NewtonOptions<double> newton{1e-12, 20};
    double h = .2, output = 91;
    check(!solver.step_adaptive(1., h, .5, ScalarDecay{}, output, cache,
                                bad_probe, norm, {1e-5, 1e-3, 40}, newton) &&
              output == 91 && h == .2 && calls > solver.STAGES + 1,
          "RK adaptation must reject a recovered invalid Newton probe norm");
}

}  // namespace

template <typename T>
bool check_shared() {
    bool passed = true;
    ode_test::for_each_partitioned_check<T>(
        [&]<typename Tableau, typename Predictor, bool NEWTON,
            ode_test::PartitionedCheck KIND>(std::size_t i) {
            const auto result =
                ode_test::run_partitioned_check<Tableau, Predictor, NEWTON,
                                                KIND>();
            if (result.failures == 0) return;
            std::cerr << "PRK case " << i << ", scalar bytes=" << sizeof(T)
                      << ", mask=" << result.failures << '\n';
            passed = false;
        });
    return passed;
}

int main() {
    try {
        check_custom_tableaux();
        check_rk_probe_rejection();
        check_additional_order<ode::Gauss4Tableau<long double>, 1>();
        check_additional_order<ode::Gauss4Tableau<long double>, 3>();
        check_additional_order<ode::Gauss6Tableau<long double>, 3>();
        check_additional_order<ode::LobattoIIIA4Tableau<long double>, 1>();
        check_equistage_order<1>();
        check_equistage_order<2>();
        check_equistage_order<4>();
        check_equistage_order<8>();
        return check_shared<float>() && check_shared<double>() &&
                       check_shared<long double>()
                   ? 0
                   : 1;
    } catch (const std::exception& error) {
        std::cerr << "test_partitioned_capabilities: " << error.what() << '\n';
        return 1;
    }
}
