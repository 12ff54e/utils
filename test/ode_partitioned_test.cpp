// Adapted from GEPT's tests/test_partitioned_integrator.cpp.
#include "numerics/ode.h"
#include "ode/finite_error.h"

#include <cstdlib>
#include <iostream>
#include <string>

namespace {

void check(bool condition, const char* message) {
    if (!condition) throw std::runtime_error(message);
}

struct State {
    double x = 0;
    double v = 0;
    double first_time = 0;
    double second_time = 0;

    bool operator==(const State&) const = default;
};

struct Oscillator {
    using first_derivative_type = double;
    using second_derivative_type = double;

    bool fail = false;

    bool evaluate_first(const State& state, double& derivative) const {
        derivative = state.v;
        return std::isfinite(derivative);
    }
    bool evaluate_second(const State& state, double& derivative) const {
        derivative = -state.x;
        return !fail && std::isfinite(derivative);
    }
    template <std::size_t N, typename T>
    State advance(const State& initial,
                  const std::array<double, N>& first,
                  const std::array<double, N>& second,
                  const std::array<T, N>& first_weights,
                  const std::array<T, N>& second_weights,
                  std::size_t count,
                  T h,
                  T first_fraction,
                  T second_fraction) const {
        State state = initial;
        for (std::size_t i = 0; i < count; ++i) {
            state.x += h * first_weights[i] * first[i];
            state.v += h * second_weights[i] * second[i];
        }
        state.first_time += h * first_fraction;
        state.second_time += h * second_fraction;
        return state;
    }
};

double distance(const State& a, const State& b) {
    return ode_test::finite_max(std::abs(a.x - b.x), std::abs(a.v - b.v));
}

template <typename Tableau, std::size_t STAGES, std::size_t ORDER>
void test_coefficients() {
    using T = typename Tableau::val_type;
    constexpr Tableau tableau;
    static_assert(Tableau::STAGES == STAGES && Tableau::ORDER == ORDER);
    static_assert(Tableau::IMPLICIT && Tableau::SYMMETRIC);
    constexpr T TOLERANCE = 64 * std::numeric_limits<T>::epsilon();
    T first_coupling = 0, second_coupling = 0;
    for (std::size_t i = 0; i < STAGES; ++i) {
        check(tableau.b_first[i] > 0 && tableau.b_second[i] > 0,
              "Hamiltonian PRK weights must be positive");
        T first_sum = 0, second_sum = 0;
        for (std::size_t j = 0; j < STAGES; ++j) {
            first_sum += tableau.A_first[i][j];
            second_sum += tableau.A_second[i][j];
            check(
                std::abs(tableau.b_first[i] * tableau.A_second[i][j] +
                         tableau.b_second[j] * tableau.A_first[j][i] -
                         tableau.b_first[i] * tableau.b_second[j]) < TOLERANCE,
                "PRK pair must satisfy the symplectic cross identity");
            check(
                std::abs(tableau.A_first[i][j] +
                         tableau.A_first[STAGES - 1 - i][STAGES - 1 - j] -
                         tableau.b_first[j]) < TOLERANCE &&
                    std::abs(tableau.A_second[i][j] +
                             tableau.A_second[STAGES - 1 - i][STAGES - 1 - j] -
                             tableau.b_second[j]) < TOLERANCE,
                "PRK coefficients must reverse under stage reversal");
        }
        check(std::abs(first_sum - tableau.c_first[i]) < TOLERANCE &&
                  std::abs(second_sum - tableau.c_second[i]) < TOLERANCE &&
                  std::abs(tableau.c_first[i] +
                           tableau.c_first[STAGES - 1 - i] - 1) < TOLERANCE &&
                  std::abs(tableau.c_second[i] +
                           tableau.c_second[STAGES - 1 - i] - 1) < TOLERANCE &&
                  std::abs(tableau.b_first[i] -
                           tableau.b_first[STAGES - 1 - i]) < TOLERANCE &&
                  std::abs(tableau.b_second[i] -
                           tableau.b_second[STAGES - 1 - i]) < TOLERANCE,
              "PRK times, row sums, and weights must be consistent and "
              "symmetric");
        first_coupling += tableau.b_first[i] * tableau.c_second[i];
        second_coupling += tableau.b_second[i] * tableau.c_first[i];
    }
    check(std::abs(first_coupling - T{1} / 2) < TOLERANCE &&
              std::abs(second_coupling - T{1} / 2) < TOLERANCE,
          "PRK pair must satisfy the order-two coupling conditions");
    for (std::size_t k = 0; k < ORDER; ++k) {
        T first = 0, second = 0;
        for (std::size_t i = 0; i < STAGES; ++i) {
            first += tableau.b_first[i] * std::pow(tableau.c_first[i], int(k));
            second +=
                tableau.b_second[i] * std::pow(tableau.c_second[i], int(k));
        }
        check(std::abs(first - T{1} / T(k + 1)) < TOLERANCE &&
                  std::abs(second - T{1} / T(k + 1)) < TOLERANCE,
              "PRK quadrature moments must match the declared order");
    }
}

template <template <typename> typename Tableau, std::size_t S, std::size_t P>
void test_coefficient_family() {
    test_coefficients<Tableau<float>, S, P>();
    test_coefficients<Tableau<double>, S, P>();
    test_coefficients<Tableau<long double>, S, P>();
}

// H=(q*p)^2/2 has the exact solution q=q0*exp(C*t), p=p0*exp(-C*t),
// C=q0*p0. Both nonlinear derivatives depend on both partitions.
struct ProductHamiltonianSystem : Oscillator {
    bool evaluate_first(const State& state, double& derivative) const {
        derivative = state.x * state.x * state.v;
        return std::isfinite(derivative);
    }
    bool evaluate_second(const State& state, double& derivative) const {
        derivative = -state.x * state.v * state.v;
        return std::isfinite(derivative);
    }
};

template <typename Tableau,
          typename IterationOptions = ode::FixedPointOptions<double>>
void test_nonlinear_coupled(IterationOptions options = {1e-14, 100}) {
    const ode::PartitionedIntegrator solver(Tableau{});
    const ProductHamiltonianSystem system;
    const State initial{1.1, 0.7};
    const double product = initial.x * initial.v;
    const State exact{initial.x * std::exp(product),
                      initial.v * std::exp(-product)};
    double previous_error = 0;
    for (int steps : {10, 20, 40}) {
        State state = initial;
        const double h = 1.0 / steps;
        for (int i = 0; i < steps; ++i)
            check(solver.step(state, h, system, state, distance, options),
                  "nonlinear coupled PRK step failed");
        const double error = distance(state, exact);
        check(std::isfinite(error) && error > 0,
              "convergence error must be finite and positive");
        if (previous_error > 0) {
            const double expected = std::pow(2.0, Tableau::ORDER);
            check(previous_error / error > 0.95 * expected &&
                      previous_error / error < 1.05 * expected,
                  "nonlinear coupled PRK must reach its declared order");
        }
        previous_error = error;
        check(std::abs(state.x * state.v - product) < 2e-12,
              "symplectic PRK must preserve this bilinear first integral");
        for (int i = 0; i < steps; ++i)
            check(solver.step(state, -h, system, state, distance, options),
                  "reverse nonlinear coupled PRK step failed");
        check(distance(state, initial) < 5e-12,
              "symmetric nonlinear PRK steps must retrace the orbit");
    }
}

struct MidpointHeunPair {
    using val_type = double;
    using row_type = std::array<val_type, 2>;
    using matrix_type = std::array<row_type, 2>;

    static constexpr std::size_t ORDER = 2;
    static constexpr std::size_t STAGES = 2;
    static constexpr bool IMPLICIT = false;

    matrix_type A_first{{{0, 0}, {0.5, 0}}};
    matrix_type A_second{{{0, 0}, {1, 0}}};
    row_type b_first{0, 1};
    row_type b_second{0.5, 0.5};
    row_type c_first{0, 0.5};
    row_type c_second{0, 1};
};

struct ClockSystem : Oscillator {
    bool evaluate_first(const State& state, double& derivative) const {
        derivative = state.first_time;
        return true;
    }
    bool evaluate_second(const State& state, double& derivative) const {
        derivative = state.second_time;
        return true;
    }
};

struct SeparableClockSystem : ClockSystem {
    static constexpr bool SEPARABLE = true;
};

struct SeparableOscillator : Oscillator {
    static constexpr bool SEPARABLE = true;
    mutable std::string order;

    bool evaluate_first(const State& state, double& derivative) const {
        order += 'F';
        return Oscillator::evaluate_first(state, derivative);
    }
    bool evaluate_second(const State& state, double& derivative) const {
        order += 'S';
        return Oscillator::evaluate_second(state, derivative);
    }
};

struct OptOutOscillator : SeparableOscillator {
    static constexpr bool SEPARABLE = false;
};

template <typename System,
          typename Tableau = ode::PositionVerletTableau<double>>
constexpr bool HAS_NO_NORM_STEP =
    requires(const ode::PartitionedIntegrator<double, Tableau>& solver,
             const System& system,
             State& state) { solver.step(state, 0.1, system, state); };

static_assert(HAS_NO_NORM_STEP<SeparableOscillator>);
static_assert(!HAS_NO_NORM_STEP<Oscillator>);
static_assert(!HAS_NO_NORM_STEP<OptOutOscillator>);

// A mixed q*p term makes this canonical Hamiltonian nonseparable.
struct CoupledQuadraticSystem : Oscillator {
    static constexpr double KAPPA = 0.3;

    bool evaluate_first(const State& state, double& derivative) const {
        derivative = state.v + KAPPA * state.x;
        return true;
    }
    bool evaluate_second(const State& state, double& derivative) const {
        derivative = -state.x - KAPPA * state.v;
        return true;
    }
};

struct DirectQuadraticSystem : CoupledQuadraticSystem {
    mutable int calls = 0;

    bool solve_stage(State& state, double a, double b, std::size_t) const {
        ++calls;
        const double q = state.x, p = state.v;
        const double determinant = (1 - KAPPA * a) * (1 + KAPPA * b) + a * b;
        state.x = ((1 + KAPPA * b) * q + a * p) / determinant;
        state.v = ((1 - KAPPA * a) * p - b * q) / determinant;
        return true;
    }
};

static_assert(HAS_NO_NORM_STEP<DirectQuadraticSystem,
                               ode::ImplicitMidpointPairTableau<double>>);
static_assert(!HAS_NO_NORM_STEP<CoupledQuadraticSystem,
                                ode::ImplicitMidpointPairTableau<double>>);
static_assert(!HAS_NO_NORM_STEP<DirectQuadraticSystem,
                                ode::LobattoIIIAIIIB4Tableau<double>>);

void test_midpoint_invariant() {
    const ode::PartitionedIntegrator solver(
        ode::ImplicitMidpointPairTableau<double>{});
    const State initial{1.2, 0.7};
    const auto energy = [](const State& state) {
        return (state.x * state.x + state.v * state.v) / 2 +
               CoupledQuadraticSystem::KAPPA * state.x * state.v;
    };
    const double initial_energy = energy(initial);
    State state = initial;
    for (int i = 0; i < 100; ++i)
        check(solver.step(state, 0.1, CoupledQuadraticSystem{}, state, distance,
                          {1e-14, 100}) &&
                  std::abs(energy(state) - initial_energy) < 5e-12,
              "implicit midpoint must preserve the quadratic Hamiltonian");
    DirectQuadraticSystem direct;
    state = initial;
    // This step lies beyond the oscillator's Picard stability limit.
    for (int i = 0; i < 20; ++i)
        check(solver.step(state, 3, direct, state) &&
                  std::abs(energy(state) - initial_energy) < 1e-13,
              "direct midpoint must preserve energy at large steps");
    for (int i = 0; i < 20; ++i)
        check(solver.step(state, -3, direct, state),
              "reverse direct midpoint step failed");
    check(direct.calls == 40 && distance(state, initial) < 2e-13,
          "nonseparable direct midpoint steps must reverse without iteration");
}

// Two half steps of symplectic Euler have consecutive first-diagonal stages.
struct RepeatedDriftKickPair : ode::PositionVerletTableau<double> {
    static constexpr std::size_t ORDER = 1;
    matrix_type A_first{{{0.5, 0}, {0.5, 0.5}}};
    matrix_type A_second{{{0, 0}, {0.5, 0}}};
    row_type c_first{0.5, 1};
    row_type c_second{0, 0.5};
};

struct ExplicitCyclePair : ode::ImplicitMidpointPairTableau<double> {
    static constexpr bool IMPLICIT = false;
};

struct LateCyclePair : ode::PositionVerletTableau<double> {
    static constexpr std::size_t ORDER = 1;
    matrix_type A_first{{{0.5, 0}, {0.5, 0.5}}};
    row_type c_first{0.5, 1};
};

template <typename Tableau>
void test_separable_stages(const char* expected_order) {
    const Tableau tableau;
    const ode::PartitionedIntegrator solver(tableau);
    SeparableOscillator system;
    const State initial{1, 0.7, 2, 3};
    State output;
    typename decltype(solver)::template Stages<State, SeparableOscillator>
        stages;
    check(solver.step(initial, 0.2, system, output, stages) &&
              system.order == expected_order,
          "separable stages must follow their derivative dependencies");
    for (std::size_t i = 0; i < Tableau::STAGES; ++i) {
        check((!stages.first_evaluated[i] ||
               stages.first[i] == stages.states[i].v) &&
                  (!stages.second_evaluated[i] ||
                   stages.second[i] == -stages.states[i].x) &&
                  stages.states[i].first_time ==
                      initial.first_time + 0.2 * tableau.c_first[i] &&
                  stages.states[i].second_time ==
                      initial.second_time + 0.2 * tableau.c_second[i],
              "separable stage records must contain complete states and times");
    }
    system.order.clear();
    int norm_calls = 0;
    check(solver.step(initial, 0.2, system, output, stages,
                      [&](const State& a, const State& b) {
                          ++norm_calls;
                          return distance(a, b);
                      },
                      {1e-14, 1}) &&
              norm_calls == 1 && system.order == expected_order,
          "separable execution must not iterate even with a supplied norm");
    const auto saved = output;
    const auto saved_stages = stages;
    system.fail = true;
    check(!solver.step(initial, 0.2, system, output, stages) &&
              output == saved && stages.states == saved_stages.states &&
              stages.first == saved_stages.first &&
              stages.second == saved_stages.second &&
              stages.first_evaluated == saved_stages.first_evaluated &&
              stages.second_evaluated == saved_stages.second_evaluated,
          "failed separable stages must leave all outputs unchanged");
}

template <typename Tableau>
void test_separable_convergence() {
    const ode::PartitionedIntegrator solver(Tableau{});
    const SeparableOscillator system;
    double previous_error = 0;
    for (int steps : {20, 40, 80}) {
        State state{1, 0};
        const double h = 1.0 / steps;
        for (int i = 0; i < steps; ++i)
            check(solver.step(state, h, system, state),
                  "separable oscillator step failed");
        const double error = distance(state, {std::cos(1.0), -std::sin(1.0)});
        const double ratio = std::pow(2.0, Tableau::ORDER);
        check(std::isfinite(error) && error > 0,
              "convergence error must be finite and positive");
        if (previous_error > 0)
            check(
                previous_error / error > 0.98 * ratio &&
                    previous_error / error < 1.02 * ratio,
                "separable execution must retain the pair's convergence order");
        previous_error = error;
        if constexpr (Tableau::ORDER == 2) {
            for (int i = 0; i < steps; ++i)
                check(solver.step(state, -h, system, state),
                      "reverse separable oscillator step failed");
            check(distance(state, {1, 0}) < 2e-13,
                  "separable Verlet steps must retrace the orbit");
        }
    }
}

struct QuarticSystem : Oscillator {
    static constexpr bool SEPARABLE = true;

    bool evaluate_first(const State& state, double& derivative) const {
        derivative = state.v * state.v * state.v;
        return true;
    }
    bool evaluate_second(const State& state, double& derivative) const {
        derivative = -state.x * state.x * state.x;
        return true;
    }
};

template <typename Tableau>
void test_nonlinear_separable() {
    const ode::PartitionedIntegrator solver(Tableau{});
    const QuarticSystem system;
    const State initial{1, 0.7};
    const auto energy = [](const State& state) {
        return (std::pow(state.x, 4) + std::pow(state.v, 4)) / 4;
    };
    double previous_error = 0;
    for (int steps : {40, 80, 160}) {
        State state = initial;
        const double h = 1.0 / steps;
        for (int i = 0; i < steps; ++i)
            check(solver.step(state, h, system, state),
                  "nonlinear separable step failed");
        const double error = std::abs(energy(state) - energy(initial));
        check(std::isfinite(error) && error > 0,
              "convergence error must be finite and positive");
        if (previous_error > 0)
            check(previous_error / error > 3.8 && previous_error / error < 4.2,
                  "quartic Hamiltonian error must decrease quadratically");
        previous_error = error;
        for (int i = 0; i < steps; ++i)
            check(solver.step(state, -h, system, state),
                  "reverse nonlinear separable step failed");
        check(distance(state, initial) < 2e-12,
              "nonlinear separable Verlet steps must reverse");
    }
}

template <typename Tableau>
void test_separable_cycle() {
    const ode::PartitionedIntegrator solver(Tableau{});
    const SeparableOscillator system;
    const State initial{1, 0};
    State output{-99, -88};
    typename decltype(solver)::template Stages<State, SeparableOscillator>
        stages;
    stages.states[0] = output;
    check(!solver.step(initial, 0.2, system, output, stages) &&
              system.order.empty() && output == State{-99, -88} &&
              stages.states[0] == output,
          "no-norm cyclic steps must reject before any evaluation or output");
    if constexpr (Tableau::IMPLICIT) {
        check(solver.step(initial, 0.2, system, output, stages, distance,
                          {1e-14, 100}) &&
                  system.order.size() > 2 * Tableau::STAGES &&
                  distance(output, {std::cos(0.2), -std::sin(0.2)}) <
                      0.1 * std::pow(0.2, Tableau::ORDER + 1),
              "separable dependency cycles must retain the implicit solve");
    } else {
        check(!solver.step(initial, 0.2, system, output, stages, distance) &&
                  system.order.empty() && output == State{-99, -88} &&
                  stages.states[0] == output,
              "explicit tableaus must reject cycles even with a norm");
    }
}

struct SeparableStageSolver : SeparableOscillator {
    mutable std::array<int, 3> calls{};
    bool reject = false;

    bool solve_stage(State& state,
                     double h_first,
                     double h_second,
                     std::size_t stage) const {
        ++calls[stage];
        if (reject) return false;
        state.v = (state.v - h_second * state.x) / (1 + h_first * h_second);
        state.x += h_first * state.v;
        return true;
    }
};

void test_separable_override() {
    const ode::PartitionedIntegrator<double> solver;
    SeparableStageSolver system;
    State state{1, 0.7};
    check(solver.step(state, 0.2, system, state) &&
              system.calls == std::array{1, 1, 1},
          "custom solvers must override separability at all stages");
    const auto saved = state;
    system.reject = true;
    check(!solver.step(state, 0.2, system, state) && state == saved &&
              system.calls == std::array{2, 1, 1},
          "zero-diagonal custom solver failures must remain visible");
    const ode::PartitionedIntegrator explicit_solver(MidpointHeunPair{});
    check(explicit_solver.step(state, 0.2, system, state) &&
              system.calls == std::array{2, 1, 1},
          "explicit tableaus must continue to ignore custom stage solvers");
    const ode::PartitionedIntegrator verlet(
        ode::PositionVerletTableau<double>{});
    const OptOutOscillator opt_out;
    check(verlet.step(state, 0.2, opt_out, state, distance) &&
              opt_out.order.size() > 4,
          "SEPARABLE=false must retain general implicit execution");
    const ode::PartitionedIntegrator midpoint(
        ode::ImplicitMidpointPairTableau<double>{});
    SeparableStageSolver direct;
    check(midpoint.step(state, 0.2, direct, state) && direct.calls[0] == 1,
          "a direct solver must also handle coupled separable diagonals");
    const ode::PartitionedIntegrator dense(
        ode::LobattoIIIAIIIB4Tableau<double>{});
    direct.calls = {};
    direct.order.clear();
    const auto before = state;
    check(!dense.step(state, 0.2, direct, state) && state == before &&
              direct.order.empty() && direct.calls == std::array{0, 0, 0},
          "no-norm dense steps must reject before any custom solver call");
}

template <typename Tableau>
void test_separable_times() {
    const ode::PartitionedIntegrator solver(Tableau{});
    State state{0, 0, 2, 3};
    for (double h : {0.0, 0.25, -0.25}) {
        const auto initial = state;
        check(solver.step(state, h, SeparableClockSystem{}, state) &&
                  std::abs(state.x - initial.x -
                           h * (initial.first_time + h / 2)) < 1e-15 &&
                  std::abs(state.v - initial.v -
                           h * (initial.second_time + h / 2)) < 1e-15,
              "partial stage reconstruction must preserve each stage clock");
    }
    check(state == State{0, 0, 2, 3},
          "separable steps must advance each clock exactly once");
}

void test_coupled_pair() {
    const ode::PartitionedIntegrator solver(
        ode::LobattoIIIAIIIB4Tableau<double>{});
    const Oscillator system;
    double previous_error = 0;
    for (int steps : {10, 20, 40}) {
        State state{1, 0};
        const double h = 1.0 / steps;
        for (int i = 0; i < steps; ++i)
            check(solver.step(state, h, system, state, distance, {1e-14, 100}),
                  "coupled Lobatto IIIA/IIIB solve failed");
        const double error = distance(state, {std::cos(1.0), -std::sin(1.0)});
        check(std::isfinite(error) && error > 0,
              "convergence error must be finite and positive");
        if (previous_error > 0)
            check(
                previous_error / error > 15.5 && previous_error / error < 16.5,
                "Lobatto IIIA/IIIB must have fourth-order convergence");
        previous_error = error;
        for (int i = 0; i < steps; ++i)
            check(solver.step(state, -h, system, state, distance, {1e-14, 100}),
                  "backward partitioned solve failed");
        check(distance(state, {1, 0}) < 2e-12,
              "symmetric partitioned pair must retrace the orbit");
    }
    const ode::PartitionedIntegrator float_solver(
        ode::LobattoIIIAIIIB4Tableau<float>{});
    State state{1, 0};
    for (int i = 0; i < 10; ++i)
        check(float_solver.step(state, 0.1F, system, state, distance),
              "float partitioned coefficients must be supported");
    check(distance(state, {std::cos(1.0), -std::sin(1.0)}) < 1e-5,
          "float partitioned integration is inaccurate");
    State output{-99, -88};
    check(!solver.step(State{1, 0}, 1, system, output, distance, {1e-14, 1}) &&
              output.x == -99 && output.v == -88,
          "unconverged coupled stages must preserve the output");
    check(!solver.step(State{1, 0}, 0.1, system, output,
                       [](const State&, const State&) { return -1.0; }),
          "negative partitioned norms must fail");
}

void test_partition_times() {
    const ode::PartitionedIntegrator solver(MidpointHeunPair{});
    State state{0, 0, 2, 3};
    for (double h : {0.25, -0.25}) {
        const auto initial = state;
        check(solver.step(state, h, ClockSystem{}, state),
              "explicit partitioned clock solve failed");
        check(std::abs(state.x - initial.x - h * (initial.first_time + h / 2)) <
                      1e-15 &&
                  std::abs(state.v - initial.v -
                           h * (initial.second_time + h / 2)) < 1e-15,
              "each partition must use its own stage abscissae");
    }
    check(distance(state, {}) < 1e-15 && state.first_time == 2 &&
              state.second_time == 3,
          "signed partitioned steps must advance both clocks");
}

struct MagneticState {
    std::array<double, 2> x{};
    std::array<double, 2> v{};
    double time = 0;
};

struct MagneticSystem {
    using first_derivative_type = std::array<double, 2>;
    using second_derivative_type = first_derivative_type;

    double omega = 1;
    mutable std::size_t first_calls = 0;
    mutable std::size_t second_calls = 0;
    bool fail = false;

    bool evaluate_first(const MagneticState& state,
                        first_derivative_type& derivative) const {
        ++first_calls;
        derivative = state.v;
        return true;
    }
    bool evaluate_second(const MagneticState& state,
                         second_derivative_type& derivative) const {
        ++second_calls;
        derivative = {omega * state.v[1], -omega * state.v[0]};
        return !fail;
    }
    template <std::size_t N>
    MagneticState advance(const MagneticState& initial,
                          const std::array<first_derivative_type, N>& first,
                          const std::array<second_derivative_type, N>& second,
                          const std::array<double, N>& first_weights,
                          const std::array<double, N>& second_weights,
                          std::size_t count,
                          double h,
                          double first_fraction,
                          double) const {
        auto state = initial;
        for (std::size_t i = 0; i < count; ++i)
            for (std::size_t j = 0; j < 2; ++j) {
                state.x[j] += h * first_weights[i] * first[i][j];
                state.v[j] += h * second_weights[i] * second[i][j];
            }
        state.time += h * first_fraction;
        return state;
    }
    bool solve_stage(MagneticState& state,
                     double h_first,
                     double h_second,
                     std::size_t) const {
        const auto v = state.v;
        const double t = h_second * omega;
        state.v = {(v[0] + t * v[1]) / (1 + t * t),
                   (v[1] - t * v[0]) / (1 + t * t)};
        for (std::size_t j = 0; j < 2; ++j) state.x[j] += h_first * state.v[j];
        return true;
    }
};

double magnetic_error(const MagneticState& state, double omega, double time) {
    const std::array exact_x{std::sin(omega * time) / omega,
                             (std::cos(omega * time) - 1) / omega};
    const std::array exact_v{std::cos(omega * time), -std::sin(omega * time)};
    double error = 0;
    for (std::size_t j = 0; j < 2; ++j)
        error += std::abs(state.x[j] - exact_x[j]) +
                 std::abs(state.v[j] - exact_v[j]);
    return error;
}

void test_boris_pair() {
    const ode::PartitionedIntegrator<double> solver;
    for (double omega : {-2.0, 2.0}) {
        const MagneticSystem system{omega};
        double previous_error = 0;
        for (int steps : {20, 40, 80}) {
            MagneticState state{{}, {1, 0}};
            const double h = 1.0 / steps;
            for (int i = 0; i < steps; ++i)
                check(solver.step(state, h, system, state),
                      "direct Boris stage solve failed");
            const double error = magnetic_error(state, omega, 1);
            check(std::isfinite(error) && error > 0,
                  "convergence error must be finite and positive");
            if (previous_error > 0)
                check(previous_error / error > 3.9 &&
                          previous_error / error < 4.1,
                      "Boris PRK must have second-order convergence");
            previous_error = error;
        }
        // h*|omega|/2 > 1 defeats naive midpoint fixed-point iteration.
        MagneticState state{{}, {1, 0}};
        const auto initial = state;
        for (int i = 0; i < 20; ++i) {
            check(solver.step(state, 2, system, state),
                  "algebraic Boris solve must work beyond Picard stability");
            check(std::abs(state.v[0] * state.v[0] + state.v[1] * state.v[1] -
                           1) < 2e-14,
                  "Boris must preserve magnetic kinetic energy");
        }
        for (int i = 0; i < 20; ++i)
            check(solver.step(state, -2, system, state),
                  "reverse Boris solve failed");
        check(magnetic_error(state, omega, 0) < 2e-13,
              "Boris PRK must be time reversible");
        check(state.time == initial.time, "Boris time must reverse");
    }
    MagneticSystem system;
    MagneticState state{{}, {1, 0}}, output{{-99, 0}, {0, 0}};
    decltype(solver)::Stages<MagneticState, MagneticSystem> stages;
    check(solver.step(state, 0.1, system, output, stages),
          "recorded Boris solve failed");
    check(system.first_calls == 2 && system.second_calls == 1 &&
              stages.first_evaluated == std::array{true, false, true} &&
              stages.second_evaluated == std::array{false, true, false},
          "Boris must skip unused endpoint force samples");
    const auto saved = output;
    const auto saved_stages = stages;
    system.fail = true;
    check(!solver.step(state, 0.1, system, output, stages) &&
              output.x == saved.x && output.v == saved.v &&
              stages.states.back().x == saved_stages.states.back().x,
          "failed partitioned steps must preserve outputs and recorded stages");
    check(!solver.step(state, std::numeric_limits<double>::infinity(), system,
                       output),
          "nonfinite PRK time steps must fail");
}

void test_root_events() {
    const ode::PartitionedIntegrator<double> solver;
    const MagneticSystem system;
    const MagneticState initial{{}, {1, 0}};
    MagneticState next;
    decltype(solver)::Stages<MagneticState, MagneticSystem> stages;
    check(solver.step(initial, 0.2, system, next, stages),
          "event test requires a successful partitioned trial");
    std::array<double, 2> times{};
    std::size_t count = 0;
    const std::array events{
        ode::Event<MagneticState>{
            [](const auto& state) { return state.time - 0.15; },
            [&](const auto& state) {
                times[count++] = state.time;
                return ode::EventAction::STOP;
            },
            ode::EventDirection::RISING},
        ode::Event<MagneticState>{
            [](const auto& state) { return state.time - 0.05; },
            [&](const auto& state) { times[count++] = state.time; }}};
    ode::EventDispatcher<MagneticState> dispatcher(events);
    const auto stop = dispatcher.advance(initial, next, [&](double fraction) {
        MagneticState state;
        if (!solver.step(initial, 0.2 * fraction, system, state))
            throw std::runtime_error("shortened partitioned event step failed");
        return state;
    });
    check(stop && stop->event_index == 0 && count == 2 &&
              std::abs(times[0] - 0.05) < 1e-10 &&
              std::abs(times[1] - 0.15) < 1e-10,
          "standalone root events must localize and dispatch in time order");
    MagneticState expected;
    check(solver.step(initial, 0.15, system, expected) &&
              std::abs(stop->state.x[0] - expected.x[0]) < 1e-10 &&
              std::abs(stop->state.v[1] - expected.v[1]) < 1e-10,
          "root events must use the supplied trajectory evaluator");
    check(next.time == 0.2 && stages.states.back().time == 0.2,
          "root localization must leave the recorded trial unchanged");
}

}  // namespace

int main() {
    try {
        test_coefficient_family<ode::PositionVerletTableau, 2, 2>();
        test_coefficient_family<ode::VelocityVerletTableau, 2, 2>();
        test_coefficient_family<ode::LobattoIIIAIIIB4Tableau, 3, 4>();
        test_coefficient_family<ode::ImplicitMidpointPairTableau, 1, 2>();
        test_nonlinear_coupled<ode::PositionVerletTableau<double>>();
        test_nonlinear_coupled<ode::VelocityVerletTableau<double>>();
        test_nonlinear_coupled<ode::LobattoIIIAIIIB4Tableau<double>>();
        test_nonlinear_coupled<ode::ImplicitMidpointPairTableau<double>>();
        test_nonlinear_coupled<ode::LobattoIIIAIIIB4Tableau<double>>(
            ode::NewtonOptions<double>{1e-14, 20});
        test_nonlinear_coupled<ode::ImplicitMidpointPairTableau<double>>(
            ode::NewtonOptions<double>{1e-14, 20});
        test_midpoint_invariant();
        test_coupled_pair();
        test_partition_times();
        test_separable_stages<ode::PositionVerletTableau<double>>("FSSF");
        test_separable_stages<ode::VelocityVerletTableau<double>>("SFFS");
        test_separable_stages<ode::BorisTableau<double>>("FSF");
        test_separable_stages<RepeatedDriftKickPair>("FSFS");
        test_separable_convergence<ode::PositionVerletTableau<double>>();
        test_separable_convergence<ode::VelocityVerletTableau<double>>();
        test_separable_convergence<ode::PositionVerletTableau<float>>();
        test_separable_convergence<ode::VelocityVerletTableau<float>>();
        test_separable_convergence<ode::BorisTableau<double>>();
        test_separable_convergence<RepeatedDriftKickPair>();
        test_nonlinear_separable<ode::PositionVerletTableau<double>>();
        test_nonlinear_separable<ode::VelocityVerletTableau<double>>();
        test_nonlinear_separable<ode::BorisTableau<double>>();
        test_separable_cycle<ode::ImplicitMidpointPairTableau<double>>();
        test_separable_cycle<ode::LobattoIIIAIIIB4Tableau<double>>();
        test_separable_cycle<ExplicitCyclePair>();
        test_separable_cycle<LateCyclePair>();
        test_separable_override();
        test_separable_times<ode::PositionVerletTableau<double>>();
        test_separable_times<ode::VelocityVerletTableau<double>>();
        test_separable_times<ode::BorisTableau<double>>();
        test_boris_pair();
        test_root_events();
    } catch (const std::exception& error) {
        std::cerr << "test_partitioned_integrator: " << error.what() << '\n';
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
