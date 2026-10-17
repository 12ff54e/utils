// Adapted from GEPT's tests/test_gauss_tableau.cpp.
#include "numerics/ode.h"

#include <cstdlib>
#include <iostream>

namespace {

void check(bool condition, const char* message) {
    if (!condition) throw std::runtime_error(message);
}

template <typename T>
T power(T value, std::size_t exponent) {
    T result = 1;
    for (std::size_t k = 0; k < exponent; ++k) result *= value;
    return result;
}

template <typename Tableau>
void test_coefficients() {
    using T = typename Tableau::val_type;
    constexpr Tableau tableau;
    constexpr std::size_t STAGES = Tableau::STAGES;
    constexpr T EPSILON = std::numeric_limits<T>::epsilon();
    static_assert(Tableau::ORDER == 2 * STAGES);
    static_assert(Tableau::IMPLICIT && Tableau::SYMMETRIC);
    static_assert(Tableau::ERROR_ORDER == 2);
    for (std::size_t i = 0; i < STAGES; ++i) {
        check(tableau.c[i] > 0 && tableau.c[i] < 1 && tableau.b[i] > 0,
              "Gauss nodes must be interior and weights positive");
        if (i != 0)
            check(tableau.c[i] > tableau.c[i - 1],
                  "Gauss nodes must be ordered");
        check(std::abs(tableau.c[i] + tableau.c[STAGES - 1 - i] - 1) <
                      16 * EPSILON &&
                  tableau.b[i] == tableau.b[STAGES - 1 - i],
              "Gauss nodes and weights must reflect about the midpoint");
        for (std::size_t j = 0; j < STAGES; ++j) {
            check(std::abs(tableau.A[i][j] +
                           tableau.A[STAGES - 1 - i][STAGES - 1 - j] -
                           tableau.b[j]) < 64 * EPSILON,
                  "Gauss coefficients must satisfy time symmetry");
            check(std::abs(tableau.b[i] * tableau.A[i][j] +
                           tableau.b[j] * tableau.A[j][i] -
                           tableau.b[i] * tableau.b[j]) < 64 * EPSILON,
                  "Gauss coefficients must satisfy the symplectic identity");
        }
        for (std::size_t k = 1; k <= STAGES; ++k) {
            T moment = 0;
            for (std::size_t j = 0; j < STAGES; ++j)
                moment += tableau.A[i][j] * power(tableau.c[j], k - 1);
            check(
                std::abs(moment - power(tableau.c[i], k) / T(k)) < 64 * EPSILON,
                "Gauss collocation must integrate stage moments");
        }
    }
    for (std::size_t k = 0; k < 2 * STAGES; ++k) {
        T moment = 0;
        for (std::size_t j = 0; j < STAGES; ++j)
            moment += tableau.b[j] * power(tableau.c[j], k);
        check(std::abs(moment - T{1} / T(k + 1)) < 64 * EPSILON,
              "Gauss quadrature must be exact through degree 2*s-1");
    }
    for (T ratio : {T{0}, T{1}, T{0.5}, T{2}, T{-1}, T{-0.5}}) {
        const auto weights =
            ratio == 1 ? tableau.beta : tableau.predictor(ratio);
        for (std::size_t i = 0; i < STAGES; ++i)
            for (std::size_t k = 1; k <= STAGES; ++k) {
                T moment = 0, scale = 1;
                for (std::size_t j = 0; j < STAGES; ++j) {
                    const T term = weights[i][j] * power(tableau.c[j], k - 1);
                    moment += term;
                    scale += std::abs(term);
                }
                const T exact = power(1 + ratio * tableau.c[i], k) / T(k);
                scale += std::abs(exact);
                check(std::abs(moment - exact) < 64 * EPSILON * scale,
                      "Gauss predictor must integrate polynomial moments");
            }
    }
}

template <typename T>
struct TangentSystem {
    using val_type = T;
    using derivative_type = val_type;

    bool evaluate(val_type state, val_type& derivative) const {
        derivative = 1 + state * state;
        return std::isfinite(derivative);
    }
    template <std::size_t N>
    val_type advance(val_type state,
                     const std::array<val_type, N>& derivatives,
                     const std::array<val_type, N>& weights,
                     std::size_t count,
                     val_type h,
                     val_type) const {
        for (std::size_t i = 0; i < count; ++i)
            state += h * weights[i] * derivatives[i];
        return state;
    }
};

template <typename Tableau>
void test_nonlinear_order() {
    using T = typename Tableau::val_type;
    const ode::Integrator solver(Tableau{});
    const TangentSystem<T> system;
    const auto norm = [](T a, T b) { return std::abs(a - b); };
    const auto error = [&](int steps) {
        typename decltype(solver)::template StepCache<T, TangentSystem<T>>
            cache;
        T state = 0;
        for (int i = 0; i < steps; ++i)
            check(solver.step(state, T{1} / steps, system, state, cache, norm,
                              {4 * std::numeric_limits<T>::epsilon(), 300}),
                  "high-order nonlinear Gauss solve failed");
        return std::abs(state - std::tan(T{1}));
    };
    // Keep truncation error above roundoff, including when long double has
    // only binary64 precision. Smaller steps would hide the highest orders.
    constexpr int STEPS = Tableau::STAGES == 3   ? 8
                          : Tableau::STAGES >= 5 ? 2
                                                 : 4;
    const T coarse = error(STEPS), fine = error(2 * STEPS);
    check(fine > 64 * std::numeric_limits<T>::epsilon(),
          "Gauss convergence test must resolve error above roundoff");
    const T observed = std::log2(coarse / fine);
    std::cout << "Gauss " << Tableau::STAGES << " stages: order " << observed
              << ", fine error " << fine << '\n';
    check(std::abs(observed - T(Tableau::ORDER)) < T{0.5},
          "Gauss nonlinear convergence must reach order 2*s");
}

struct State {
    double q = 0;
    double p = 0;
};

struct Oscillator {
    using derivative_type = std::array<double, 2>;

    bool evaluate(const State& state, derivative_type& derivative) const {
        derivative = {state.p, -state.q};
        return true;
    }
    template <std::size_t N>
    State advance(State state,
                  const std::array<derivative_type, N>& derivatives,
                  const std::array<double, N>& weights,
                  std::size_t count,
                  double h,
                  double) const {
        for (std::size_t i = 0; i < count; ++i) {
            state.q += h * weights[i] * derivatives[i][0];
            state.p += h * weights[i] * derivatives[i][1];
        }
        return state;
    }
};

double distance(const State& a, const State& b) {
    return std::hypot(a.q - b.q, a.p - b.p);
}

template <typename Tableau>
void test_reversal_and_invariant() {
    const ode::Integrator solver(Tableau{});
    const Oscillator system;
    typename decltype(solver)::template StepCache<State, Oscillator> cache;
    const State initial{1.3, 0.7};
    State state = initial;
    const double energy = initial.q * initial.q + initial.p * initial.p;
    for (int i = 0; i < 100; ++i) {
        check(solver.step(state, 0.2, system, state, cache, distance,
                          {1e-14, 100}),
              "Gauss oscillator step failed");
        check(std::abs(state.q * state.q + state.p * state.p - energy) < 5e-12,
              "Gauss must preserve quadratic invariants to solve accuracy");
    }
    cache.reset();
    for (int i = 0; i < 100; ++i)
        check(solver.step(state, -0.2, system, state, cache, distance,
                          {1e-14, 100}),
              "reverse Gauss oscillator step failed");
    check(distance(state, initial) < 5e-12,
          "Gauss fixed steps must retrace the numerical orbit");

    cache.reset();
    double h = 0.3;
    State forward;
    check(
        solver.step_adaptive(initial, h, 0.5, system, forward, cache, distance,
                             distance, {1e-4, 1e-9, 30}, {1e-14, 100}) &&
            h < 0.5,
        "Gauss symmetric defect must select the current step");
    cache.reset();
    double reverse_h = -0.01;
    State backward;
    check(solver.step_adaptive(forward, reverse_h, 0.5, system, backward, cache,
                               distance, distance, {1e-4, 1e-9, 30},
                               {1e-14, 100}) &&
              std::abs(h + reverse_h) < 1e-9 &&
              distance(initial, backward) < 2e-9,
          "independent adaptive Gauss steps must reverse");
}

template <typename Tableau>
void test_scalar_precision() {
    using T = typename Tableau::val_type;
    const ode::Integrator solver(Tableau{});
    const TangentSystem<T> system;
    typename decltype(solver)::template StepCache<T, TangentSystem<T>> cache;
    T state = 0;
    for (int i = 0; i < 100; ++i)
        check(solver.step(state, T{1} / 100, system, state, cache,
                          [](T a, T b) { return std::abs(a - b); }),
              "Gauss scalar-precision solve failed");
    check(std::abs(state - std::tan(T{1})) <
              1000 * std::numeric_limits<T>::epsilon(),
          "Gauss scalar-precision integration is inaccurate");
}

template <template <typename> typename Tableau>
void test_gauss() {
    test_coefficients<Tableau<float>>();
    test_coefficients<Tableau<double>>();
    test_coefficients<Tableau<long double>>();
    test_nonlinear_order<Tableau<double>>();
    test_nonlinear_order<Tableau<long double>>();
    test_reversal_and_invariant<Tableau<double>>();
    test_scalar_precision<Tableau<float>>();
    test_scalar_precision<Tableau<double>>();
}

}  // namespace

int main() {
    try {
        test_gauss<ode::Gauss6Tableau>();
        test_gauss<ode::Gauss8Tableau>();
        test_gauss<ode::Gauss10Tableau>();
        test_gauss<ode::Gauss12Tableau>();
    } catch (const std::exception& error) {
        std::cerr << "test_gauss_tableau: " << error.what() << '\n';
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
