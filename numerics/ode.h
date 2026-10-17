/*
 * ode.h - standalone C++20 ODE integration and root events.
 * Include this file; only the C++ standard library is required. All public
 * types live in namespace ode. No GEPT headers or linked library are needed.
 *
 * Features:
 * - Integrator<T, Tableau, Predictor> with caller-defined states, systems,
 *   scalar types, and Butcher tableaux. Built-in methods: explicit RK2/3/4,
 *   fourth-order Lobatto IIIA and IIIB, and 2--6-stage Gauss-Legendre methods
 *   of orders 4/6/8/10/12.
 * - PartitionedIntegrator<T, Tableau, Predictor> with separate coefficients
 *   and stage times for two partitions. Includes position/velocity Verlet,
 *   fourth-order Lobatto IIIA--IIIB, implicit midpoint, and Boris tableaux.
 *   Supports alternating separable stages and direct solve_stage hooks.
 *   PartitionedRungeKuttaTableau<Tableau> applies an ordinary RK method to
 *   both partitions while retaining its order and supported predictors.
 * - Implicit solves with FixedPointOptions<T> (default) or NewtonOptions<T>.
 *   Newton uses finite differences and damping; scalar and flat std::array
 *   derivatives work directly, and custom types can supply component maps.
 * - ContinuousOutputPredictor, AdditionalStagePredictor<1> or <3> (B1/B3,
 *   where the tableau supplies coefficients), and EquistagePredictor<K>.
 * - Fixed steps in either time direction; adaptive step doubling for explicit
 *   RK and partitioned RK; Stoffer's symmetric step selection for implicit RK
 *   or partitioned methods with a declared symmetric defect. Doubling updates
 *   h to the next proposal; symmetric selection sets h to the accepted step.
 * - Optional Stages and per-trajectory StepCache storage. Failed steps leave
 *   outputs and caches unchanged. Reset after external state/system/tableau
 *   changes.
 * - Root events with direction filters, bracket tolerances, ordered callbacks,
 *   and continue/stop actions. make_event<State> and
 *   make_event_dispatcher<State> keep concrete callable types in tuple/array
 *   collections with fixed scratch storage. EventDispatcher<State> provides a
 *   host-only runtime span adapter.
 *
 * Start with ode::Integrator solver(ode::RungeKutta4Tableau<double>{}); then
 * call solver.step(initial, h, system, next). The system defines derivative
 * types, evaluates the RHS, and applies weighted increments through advance().
 * It also owns any time metadata. Iterative solves and adaptive calls take
 * caller-defined error norms. Symmetric selection needs fixed scales, an even
 * defect norm, and State{} representing a zero increment. Events need
 * state.time in seconds and caller-supplied interpolation. Dispatch only after
 * accepting a forward segment. Detection finds sign changes; tangencies and
 * multiple roots within one segment may be missed.
 *
 * Integration and typed events run on CPU or CUDA. Device systems, norms, and
 * event/interpolation functions must be device-callable. With NVCC, compile as
 * C++20 with --expt-relaxed-constexpr for std::array access.
 */

#ifndef ZQ_ODE
#define ZQ_ODE

#include <algorithm>
#include <array>
#include <cmath>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <limits>
#include <numeric>
#include <optional>
#include <span>
#include <stdexcept>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#if defined(__CUDACC__)
#define ZQ_ODE_HOST_DEVICE __host__ __device__
#else
#define ZQ_ODE_HOST_DEVICE
#endif

namespace ode {

// HLW VIII.6.1(B): old-stage and additional-stage derivative weights for Z^0.
template <typename T, std::size_t S, std::size_t M>
struct AdditionalPredictorCoefficients {
    using val_type = T;
    using row_type = std::array<val_type, S>;
    using extra_row_type = std::array<val_type, M>;
    using extended_row_type = std::array<val_type, S + M>;

    extra_row_type c{};
    std::array<extended_row_type, M> mu{};
    std::array<row_type, S> alpha{};
    std::array<extra_row_type, S> nu{};
};

namespace detail {

template <std::size_t M, typename Tableau>
ZQ_ODE_HOST_DEVICE auto additional_predictor(const Tableau& tableau,
                                             typename Tableau::val_type ratio)
    -> AdditionalPredictorCoefficients<typename Tableau::val_type,
                                       Tableau::STAGES,
                                       M>;

}  // namespace detail

// Explicit midpoint method.
template <typename T>
struct RungeKutta2Tableau {
    using val_type = T;

    static constexpr std::size_t ORDER = 2;
    static constexpr std::size_t STAGES = 2;
    static constexpr bool IMPLICIT = false;

    using row_type = std::array<val_type, STAGES>;
    using matrix_type = std::array<row_type, STAGES>;

    matrix_type A{{
        {0, 0},
        {val_type{1} / 2, 0},
    }};
    row_type b{0, 1};
    row_type c{0, val_type{1} / 2};
};

// Classical third-order Kutta method.
template <typename T>
struct RungeKutta3Tableau {
    using val_type = T;

    static constexpr std::size_t ORDER = 3;
    static constexpr std::size_t STAGES = 3;
    static constexpr bool IMPLICIT = false;

    using row_type = std::array<val_type, STAGES>;
    using matrix_type = std::array<row_type, STAGES>;

    matrix_type A{{
        {0, 0, 0},
        {val_type{1} / 2, 0, 0},
        {-1, 2, 0},
    }};
    row_type b{val_type{1} / 6, val_type{2} / 3, val_type{1} / 6};
    row_type c{0, val_type{1} / 2, 1};
};

template <typename T>
struct RungeKutta4Tableau {
    using val_type = T;

    static constexpr std::size_t ORDER = 4;
    static constexpr std::size_t STAGES = 4;
    static constexpr bool IMPLICIT = false;
    static constexpr val_type HALF = 0.5;
    static constexpr val_type THIRD = 1.0 / 3.0;
    static constexpr val_type SIXTH = 1.0 / 6.0;

    using row_type = std::array<val_type, STAGES>;
    using matrix_type = std::array<row_type, STAGES>;

    matrix_type A{{
        {0, 0, 0, 0},
        {HALF, 0, 0, 0},
        {0, HALF, 0, 0},
        {0, 0, 1, 0},
    }};
    row_type b{SIXTH, THIRD, THIRD, SIXTH};
    row_type c{0, HALF, HALF, 1};
};

namespace detail {

// Shared quadrature nodes and extrapolation for fourth-order Lobatto methods.
template <typename T>
struct Lobatto4Coefficients {
    using val_type = T;

    static constexpr std::size_t ORDER = 4;
    static constexpr std::size_t STAGES = 3;
    static constexpr bool IMPLICIT = true;
    static constexpr bool SYMMETRIC = true;

    using row_type = std::array<val_type, STAGES>;
    using matrix_type = std::array<row_type, STAGES>;

    row_type b{val_type{1} / 6, val_type{2} / 3, val_type{1} / 6};
    row_type c{0, val_type{1} / 2, 1};

    // Integral of each Lagrange basis polynomial from 0 to 1 + c_i:
    // sum_j beta_ij * c_j^(k-1) = (1 + c_i)^k / k, k = 1, ..., STAGES.
    matrix_type beta{{
        {val_type{1} / 6, val_type{2} / 3, val_type{1} / 6},
        {val_type{3} / 8, 0, val_type{9} / 8},
        {val_type{4} / 3, -val_type{8} / 3, val_type{10} / 3},
    }};

    ZQ_ODE_HOST_DEVICE matrix_type predictor(val_type step_ratio) const {
        matrix_type weights{};
        for (std::size_t i = 0; i < STAGES; ++i) {
            // Integrate the old-step Lagrange basis to 1 + c_i*h_new/h_old.
            const val_type u = 1 + c[i] * step_ratio;
            const val_type u2 = u * u;
            const val_type u3 = u2 * u;
            weights[i] = {u - val_type{3} / 2 * u2 + val_type{2} / 3 * u3,
                          2 * u2 - val_type{4} / 3 * u3,
                          -u2 / 2 + val_type{2} / 3 * u3};
        }
        return weights;
    }
};

}  // namespace detail

template <typename T>
struct LobattoIIIA4Tableau : detail::Lobatto4Coefficients<T> {
    using val_type = T;
    using row_type = typename detail::Lobatto4Coefficients<T>::row_type;
    using matrix_type = typename detail::Lobatto4Coefficients<T>::matrix_type;

    static constexpr std::size_t ERROR_ORDER = 3;

    matrix_type A{{
        {0, 0, 0},
        {val_type{5} / 24, val_type{1} / 3, -val_type{1} / 24},
        {val_type{1} / 6, val_type{2} / 3, val_type{1} / 6},
    }};

    // b - b_hat for the embedded trapezoidal rule, b_hat = (1/2, 0, 1/2).
    // Reversing the stages preserves these weights; the defect is O(h^3).
    row_type error_weights{-val_type{1} / 3, val_type{2} / 3, -val_type{1} / 3};

    template <std::size_t M>
    ZQ_ODE_HOST_DEVICE auto additional_predictor(val_type ratio) const
        requires(M == 1)
    {
        return detail::additional_predictor<M>(*this, ratio);
    }
};

template <typename T>
struct LobattoIIIB4Tableau : detail::Lobatto4Coefficients<T> {
    using val_type = T;
    using row_type = typename detail::Lobatto4Coefficients<T>::row_type;
    using matrix_type = typename detail::Lobatto4Coefficients<T>::matrix_type;

    static constexpr std::size_t ERROR_ORDER = 2;

    // Standalone IIIB is symmetric. Symplecticity requires the partitioned
    // IIIA/IIIB pair; it does not hold for either standalone method.
    matrix_type A{{
        {val_type{1} / 6, -val_type{1} / 6, 0},
        {val_type{1} / 6, val_type{1} / 3, 0},
        {val_type{1} / 6, val_type{5} / 6, 0},
    }};

    // h*(f(Y_3)-f(Y_1))/2 is O(h^2) with a reversal-invariant norm.
    // IIIA's trapezoidal defect vanishes on linear systems for IIIB because
    // Y_2 = (Y_1 + Y_3)/2, so it cannot control their step size.
    row_type error_weights{-val_type{1} / 2, 0, val_type{1} / 2};
};

template <typename T>
struct Gauss4Tableau {
    using val_type = T;

    static constexpr std::size_t ORDER = 4;
    static constexpr std::size_t STAGES = 2;
    static constexpr bool IMPLICIT = true;
    static constexpr bool SYMMETRIC = true;
    static constexpr std::size_t ERROR_ORDER = 2;
    static constexpr val_type SQRT_THREE =
        static_cast<val_type>(1.732050807568877293527446341505872367L);

    using row_type = std::array<val_type, STAGES>;
    using matrix_type = std::array<row_type, STAGES>;

    matrix_type A{{
        {val_type{1} / 4, val_type{1} / 4 - SQRT_THREE / 6},
        {val_type{1} / 4 + SQRT_THREE / 6, val_type{1} / 4},
    }};
    row_type b{val_type{1} / 2, val_type{1} / 2};
    row_type c{val_type{1} / 2 - SQRT_THREE / 6,
               val_type{1} / 2 + SQRT_THREE / 6};

    // h*(f(Y_2)-f(Y_1))/2 is O(h^2). Its weights change sign under stage
    // reversal, so an even norm gives Stoffer's symmetric step selector.
    row_type error_weights{-val_type{1} / 2, val_type{1} / 2};

    matrix_type beta{{
        {val_type{5} / 4 - SQRT_THREE / 2, val_type{1} / 4 + SQRT_THREE / 3},
        {val_type{1} / 4 - SQRT_THREE / 3, val_type{5} / 4 + SQRT_THREE / 2},
    }};

    ZQ_ODE_HOST_DEVICE matrix_type predictor(val_type step_ratio) const {
        matrix_type weights{};
        for (std::size_t i = 0; i < STAGES; ++i) {
            const val_type u = 1 + c[i] * step_ratio;
            weights[i] = {(c[1] * u - u * u / 2) / (c[1] - c[0]),
                          (u * u / 2 - c[0] * u) / (c[1] - c[0])};
        }
        return weights;
    }

    template <std::size_t M>
    ZQ_ODE_HOST_DEVICE auto additional_predictor(val_type ratio) const
        requires(M == 1 || M == 3)
    {
        return detail::additional_predictor<M>(*this, ratio);
    }
};

namespace detail {

// Gauss--Legendre nodes and weights on [0,1]. The decimal literals retain
// long-double precision. Nodes reflect about 1/2; weights are symmetric.
template <std::size_t N>
ZQ_ODE_HOST_DEVICE constexpr std::array<long double, N> gauss_nodes() {
    if constexpr (N == 3) {
        return {0.11270166537925831148207346002176003892L, 0.5L,
                0.88729833462074168851792653997823996108L};
    } else if constexpr (N == 4) {
        return {0.069431844202973712388026755553595247452L,
                0.33000947820757186759866712044837765640L,
                0.66999052179242813240133287955162234360L,
                0.93056815579702628761197324444640475255L};
    } else if constexpr (N == 5) {
        return {0.046910077030668003601186560850303517437L,
                0.23076534494715845448184278964989559752L, 0.5L,
                0.76923465505284154551815721035010440248L,
                0.95308992296933199639881343914969648256L};
    } else {
        static_assert(N == 6);
        return {0.033765242898423986093849222753002695433L,
                0.16939530676686774316930020249004732650L,
                0.38069040695840154568474913915964403229L,
                0.61930959304159845431525086084035596771L,
                0.83060469323313225683069979750995267350L,
                0.96623475710157601390615077724699730457L};
    }
}

template <std::size_t N>
ZQ_ODE_HOST_DEVICE constexpr std::array<long double, N> gauss_weights() {
    if constexpr (N == 3) {
        return {5.0L / 18, 4.0L / 9, 5.0L / 18};
    } else if constexpr (N == 4) {
        return {0.17392742256872692868653197461099970362L,
                0.32607257743127307131346802538900029638L,
                0.32607257743127307131346802538900029638L,
                0.17392742256872692868653197461099970362L};
    } else if constexpr (N == 5) {
        return {0.11846344252809454375713202035995868132L,
                0.23931433524968323402064575741781909646L, 64.0L / 225,
                0.23931433524968323402064575741781909646L,
                0.11846344252809454375713202035995868132L};
    } else {
        static_assert(N == 6);
        return {0.085662246189585172520148071086366446763L,
                0.18038078652406930378491675691885805583L,
                0.23395696728634552369493517199477549741L,
                0.23395696728634552369493517199477549741L,
                0.18038078652406930378491675691885805583L,
                0.085662246189585172520148071086366446763L};
    }
}

template <typename T, std::size_t N>
ZQ_ODE_HOST_DEVICE constexpr std::array<T, N> gauss_cast(
    const std::array<long double, N>& coefficients) {
    std::array<T, N> result{};
    for (std::size_t i = 0; i < N; ++i)
        result[i] = static_cast<T>(coefficients[i]);
    return result;
}

template <typename T, std::size_t N>
ZQ_ODE_HOST_DEVICE constexpr std::array<T, N + 1> legendre_values(T x) {
    std::array<T, N + 1> values{};
    values[0] = 1;
    if constexpr (N > 0) values[1] = x;
    for (std::size_t k = 2; k <= N; ++k)
        values[k] = (static_cast<T>(2 * k - 1) * x * values[k - 1] -
                     static_cast<T>(k - 1) * values[k - 2]) /
                    static_cast<T>(k);
    return values;
}

}  // namespace detail

// Gauss collocation with 3--6 stages and order 2*N. Integrating the Lagrange
// basis in Legendre form avoids expanding ill-conditioned monomial weights.
// The existing two-stage Gauss4Tableau keeps its original coefficients.
template <typename T, std::size_t N>
struct GaussLegendreTableau {
    using val_type = T;

    static constexpr std::size_t ORDER = 2 * N;
    static constexpr std::size_t STAGES = N;
    static constexpr bool IMPLICIT = true;
    static constexpr bool SYMMETRIC = true;
    // This symmetric endpoint defect is O(h^2), independent of method order.
    static constexpr std::size_t ERROR_ORDER = 2;
    static_assert(STAGES >= 3 && STAGES <= 6,
                  "GaussLegendreTableau supports 3 through 6 stages");

    using row_type = std::array<val_type, STAGES>;
    using matrix_type = std::array<row_type, STAGES>;

    row_type b = detail::gauss_cast<val_type>(detail::gauss_weights<STAGES>());
    row_type c = detail::gauss_cast<val_type>(detail::gauss_nodes<STAGES>());
    matrix_type A = integral_matrix(0, 1);
    row_type error_weights = endpoint_error_weights();
    matrix_type beta = predictor(1);

    ZQ_ODE_HOST_DEVICE constexpr GaussLegendreTableau() = default;

    ZQ_ODE_HOST_DEVICE constexpr matrix_type predictor(
        val_type step_ratio) const {
        return integral_matrix(1, step_ratio);
    }

    template <std::size_t M>
    ZQ_ODE_HOST_DEVICE auto additional_predictor(val_type ratio) const
        requires(M == 1 || M == 3)
    {
        return detail::additional_predictor<M>(*this, ratio);
    }

   private:
    ZQ_ODE_HOST_DEVICE static constexpr row_type endpoint_error_weights() {
        row_type weights{};
        weights.front() = -val_type{1} / 2;
        weights.back() = val_type{1} / 2;
        return weights;
    }

    ZQ_ODE_HOST_DEVICE constexpr matrix_type integral_matrix(
        val_type origin,
        val_type step_ratio) const {
        matrix_type weights{};
        for (std::size_t i = 0; i < STAGES; ++i) {
            const val_type u = origin + step_ratio * c[i];
            const auto endpoint =
                detail::legendre_values<val_type, STAGES>(2 * u - 1);
            for (std::size_t j = 0; j < STAGES; ++j) {
                const auto node =
                    detail::legendre_values<val_type, STAGES>(2 * c[j] - 1);
                val_type integral = u;
                // Integral_0^u l_j(t) dt = b_j * [u + 1/2 sum_{k=1}^{s-1}
                // P_k(2c_j-1) * (P_{k+1}(2u-1) - P_{k-1}(2u-1))].
                for (std::size_t k = 1; k < STAGES; ++k)
                    integral +=
                        node[k] * (endpoint[k + 1] - endpoint[k - 1]) / 2;
                weights[i][j] = b[j] * integral;
            }
        }
        return weights;
    }
};

template <typename T>
using Gauss6Tableau = GaussLegendreTableau<T, 3>;
template <typename T>
using Gauss8Tableau = GaussLegendreTableau<T, 4>;
template <typename T>
using Gauss10Tableau = GaussLegendreTableau<T, 5>;
template <typename T>
using Gauss12Tableau = GaussLegendreTableau<T, 6>;

namespace detail {

template <typename T>
ZQ_ODE_HOST_DEVICE T predictor_power(T value, std::size_t exponent) {
    T result = 1;
    for (std::size_t k = 0; k < exponent; ++k) result *= value;
    return result;
}

template <typename T, std::size_t N>
ZQ_ODE_HOST_DEVICE T cardinal_value(const std::array<T, N>& nodes,
                                    std::size_t j,
                                    T x) {
    T value = 1;
    for (std::size_t k = 0; k < N; ++k)
        if (k != j) value *= (x - nodes[k]) / (nodes[j] - nodes[k]);
    return value;
}

// Gauss quadrature integrates each degree N-1 cardinal polynomial exactly.
template <typename T, std::size_t N>
ZQ_ODE_HOST_DEVICE std::array<T, N> cardinal_integrals(
    const std::array<T, N>& nodes,
    T x) {
    constexpr std::size_t POINTS = (N + 1) / 2;
    std::array<T, POINTS> c{}, b{};
    if constexpr (POINTS == 1) {
        c = {T{1} / 2};
        b = {1};
    } else if constexpr (POINTS == 2) {
        const Gauss4Tableau<T> quadrature{};
        c = quadrature.c;
        b = quadrature.b;
    } else {
        c = gauss_cast<T>(gauss_nodes<POINTS>());
        b = gauss_cast<T>(gauss_weights<POINTS>());
    }
    std::array<T, N> weights{};
    for (std::size_t j = 0; j < N; ++j)
        for (std::size_t k = 0; k < POINTS; ++k)
            weights[j] += x * b[k] * cardinal_value(nodes, j, x * c[k]);
    return weights;
}

// Cardinal form of HLW (6.9)--(6.12), including h_new/h_old. Avoid a
// Vandermonde solve and retain at least double precision during construction.
template <std::size_t M, typename Tableau>
ZQ_ODE_HOST_DEVICE auto additional_predictor(const Tableau& tableau,
                                             typename Tableau::val_type ratio)
    -> AdditionalPredictorCoefficients<typename Tableau::val_type,
                                       Tableau::STAGES,
                                       M> {
    using T = typename Tableau::val_type;
    using Work = std::conditional_t<(std::numeric_limits<T>::digits <
                                     std::numeric_limits<double>::digits),
                                    double, T>;
    constexpr std::size_t S = Tableau::STAGES;
    constexpr std::size_t N = S + (M == 1 ? 1 : 2);
    static_assert(M == 1 || M == 3);
    std::array<Work, S> old_nodes{}, powers{}, branch{};
    for (std::size_t j = 0; j < S; ++j) {
        old_nodes[j] = static_cast<Work>(tableau.c[j]);
        powers[j] = predictor_power(old_nodes[j], S);
    }
    std::array<Work, N> nodes{};
    for (std::size_t j = 0; j < S; ++j) nodes[j] = old_nodes[j];
    // HLW's even-stage choices; odd stages use the next even-stage value.
    const Work u = M == 1 ? (S <= 4 ? Work{7} / 4 : Work{9} / 5)
                          : (S <= 2   ? Work{8} / 5
                             : S <= 4 ? Work{33} / 20
                                      : Work{7} / 4);
    if constexpr (M == 1) {
        nodes[S] = u;
    } else {
        nodes[S] = 0;
        nodes[S + 1] = 1;
    }
    AdditionalPredictorCoefficients<T, S, M> result;
    if constexpr (M == 1) {
        result.c[0] = static_cast<T>(u);
        const auto weights = cardinal_integrals(old_nodes, u);
        for (std::size_t j = 0; j < S; ++j)
            result.mu[0][j] = static_cast<T>(weights[j]);
    } else {
        result.c = {0, 1, static_cast<T>(u)};
        const auto weights = cardinal_integrals(nodes, u);
        for (std::size_t j = 0; j < S; ++j) result.mu[1][j] = tableau.b[j];
        for (std::size_t j = 0; j < N; ++j)
            result.mu[2][j] = static_cast<T>(weights[j]);
    }
    const Work r = static_cast<Work>(ratio);
    Work denominator = 1;
    if constexpr (M == 3) {
        const auto at_u = cardinal_integrals(old_nodes, u);
        denominator = predictor_power(u, S + 1) / static_cast<Work>(S + 1);
        Work endpoint = 0;
        for (std::size_t j = 0; j < S; ++j) {
            denominator -= at_u[j] * powers[j];
            endpoint += static_cast<Work>(tableau.b[j]) * powers[j];
        }
        for (std::size_t k = 0; k < S; ++k) {
            const auto integral =
                cardinal_integrals(old_nodes, 1 + r * old_nodes[k]);
            branch[k] = endpoint;
            for (std::size_t j = 0; j < S; ++j) {
                branch[k] += r * static_cast<Work>(tableau.A[k][j]) *
                             predictor_power(1 + r * old_nodes[j], S);
                branch[k] -= integral[j] * powers[j];
            }
        }
    }
    for (std::size_t i = 0; i < S; ++i) {
        Work nu = 0;
        if constexpr (M == 3) {
            for (std::size_t k = 0; k < S; ++k)
                nu += r * static_cast<Work>(tableau.A[i][k]) * branch[k];
            nu /= denominator;
            result.nu[i][2] = static_cast<T>(nu);
        }
        for (std::size_t j = 0; j < N; ++j) {
            Work weight = 0;
            for (std::size_t k = 0; k < S; ++k)
                weight += r * static_cast<Work>(tableau.A[i][k]) *
                          cardinal_value(nodes, j, 1 + r * old_nodes[k]);
            if constexpr (M == 3) weight -= nu * cardinal_value(nodes, j, u);
            if (j < S)
                result.alpha[i][j] = static_cast<T>(weight);
            else
                result.nu[i][j - S] = static_cast<T>(weight);
        }
    }
    return result;
}

// h*(f(y_end)-f(y_start))/2 is O(h^2) and unchanged on step reversal.
template <typename T>
struct PartitionedEndpointDefect {
    using val_type = T;
    using endpoint_row_type = std::array<val_type, 2>;
    static constexpr std::size_t ERROR_ORDER = 2;
    endpoint_row_type endpoint_error_weights_first{-val_type{1} / 2,
                                                   val_type{1} / 2};
    endpoint_row_type endpoint_error_weights_second =
        endpoint_error_weights_first;
};

template <typename T, std::size_t S>
ZQ_ODE_HOST_DEVICE constexpr auto linear_stage_predictor(
    const std::array<std::array<T, S>, S>& A,
    const std::array<T, S>& b,
    T ratio) {
    auto beta = A;
    for (std::size_t i = 0; i < S; ++i)
        for (std::size_t j = 0; j < S; ++j) beta[i][j] = b[j] + ratio * A[i][j];
    return beta;
}

// IIIB--IIIA gives position Verlet; swapping the pair gives velocity Verlet.
// The midpoint component has two identical rows and stage times 1/2, 1/2.
template <typename T, bool VELOCITY>
struct VerletTableau : PartitionedEndpointDefect<T> {
    using val_type = T;

    static constexpr std::size_t ORDER = 2;
    static constexpr std::size_t STAGES = 2;
    static constexpr bool IMPLICIT = true;
    static constexpr bool DIAGONALLY_IMPLICIT = true;
    static constexpr bool SYMMETRIC = true;

    using row_type = std::array<val_type, STAGES>;
    using matrix_type = std::array<row_type, STAGES>;

    matrix_type A_first =
        VELOCITY ? matrix_type{{{0, 0}, {val_type{1} / 2, val_type{1} / 2}}}
                 : matrix_type{{{val_type{1} / 2, 0}, {val_type{1} / 2, 0}}};
    matrix_type A_second =
        VELOCITY ? matrix_type{{{val_type{1} / 2, 0}, {val_type{1} / 2, 0}}}
                 : matrix_type{{{0, 0}, {val_type{1} / 2, val_type{1} / 2}}};
    row_type b_first{val_type{1} / 2, val_type{1} / 2};
    row_type b_second = b_first;
    row_type c_first =
        VELOCITY ? row_type{0, 1} : row_type{val_type{1} / 2, val_type{1} / 2};
    row_type c_second =
        VELOCITY ? row_type{val_type{1} / 2, val_type{1} / 2} : row_type{0, 1};

    ZQ_ODE_HOST_DEVICE constexpr VerletTableau() = default;

    ZQ_ODE_HOST_DEVICE constexpr matrix_type predictor_first(T ratio) const {
        return linear_stage_predictor(A_first, b_first, ratio);
    }
    ZQ_ODE_HOST_DEVICE constexpr matrix_type predictor_second(T ratio) const {
        return linear_stage_predictor(A_second, b_second, ratio);
    }
};

}  // namespace detail

// Both Verlet pairs are explicit for q'=F(p), p'=G(q) when the system
// declares SEPARABLE=true. General partitioned systems need a stage solve.
template <typename T>
using PositionVerletTableau = detail::VerletTableau<T, false>;
template <typename T>
using VelocityVerletTableau = detail::VerletTableau<T, true>;

// Three-stage Lobatto IIIA--IIIB pair with coupled order four.
template <typename T>
struct LobattoIIIAIIIB4Tableau : detail::PartitionedEndpointDefect<T> {
    using val_type = T;

    static constexpr std::size_t ORDER = 4;
    static constexpr std::size_t STAGES = LobattoIIIA4Tableau<val_type>::STAGES;
    static constexpr bool IMPLICIT = true;
    static constexpr bool SYMMETRIC = true;

    using row_type = typename LobattoIIIA4Tableau<val_type>::row_type;
    using matrix_type = typename LobattoIIIA4Tableau<val_type>::matrix_type;

    matrix_type A_first = LobattoIIIA4Tableau<val_type>{}.A;
    matrix_type A_second = LobattoIIIB4Tableau<val_type>{}.A;
    row_type b_first = LobattoIIIA4Tableau<val_type>{}.b;
    row_type b_second = LobattoIIIB4Tableau<val_type>{}.b;
    row_type c_first = LobattoIIIA4Tableau<val_type>{}.c;
    row_type c_second = LobattoIIIB4Tableau<val_type>{}.c;

    ZQ_ODE_HOST_DEVICE constexpr LobattoIIIAIIIB4Tableau() = default;

    ZQ_ODE_HOST_DEVICE matrix_type predictor_first(T ratio) const {
        return detail::Lobatto4Coefficients<T>{}.predictor(ratio);
    }
    ZQ_ODE_HOST_DEVICE matrix_type predictor_second(T ratio) const {
        return predictor_first(ratio);
    }
};

// Implicit midpoint on both partitions: one coupled stage even for a
// separable system. A direct diagonal solver may replace coupled iteration.
template <typename T>
struct ImplicitMidpointPairTableau : detail::PartitionedEndpointDefect<T> {
    using val_type = T;

    static constexpr std::size_t ORDER = 2;
    static constexpr std::size_t STAGES = 1;
    static constexpr bool IMPLICIT = true;
    static constexpr bool DIAGONALLY_IMPLICIT = true;
    static constexpr bool SYMMETRIC = true;

    using row_type = std::array<val_type, STAGES>;
    using matrix_type = std::array<row_type, STAGES>;

    matrix_type A_first{{{val_type{1} / 2}}};
    matrix_type A_second = A_first;
    row_type b_first{1}, b_second{1};
    row_type c_first{val_type{1} / 2}, c_second{val_type{1} / 2};

    ZQ_ODE_HOST_DEVICE constexpr ImplicitMidpointPairTableau() = default;

    ZQ_ODE_HOST_DEVICE constexpr matrix_type predictor_first(T ratio) const {
        return detail::linear_stage_predictor(A_first, b_first, ratio);
    }
    ZQ_ODE_HOST_DEVICE constexpr matrix_type predictor_second(T ratio) const {
        return predictor_first(ratio);
    }
};

// Stiffly accurate partitioned RK form of drift--rotate--drift Boris.
// The first partition is position; the second is velocity. The middle
// velocity stage is an implicit midpoint solve at the half-drift position.
template <typename T>
struct BorisTableau : detail::PartitionedEndpointDefect<T> {
    using val_type = T;

    static constexpr std::size_t ORDER = 2;
    static constexpr std::size_t STAGES = 3;
    static constexpr bool IMPLICIT = true;
    static constexpr bool DIAGONALLY_IMPLICIT = true;
    static constexpr bool SYMMETRIC = true;

    using row_type = std::array<val_type, STAGES>;
    using matrix_type = std::array<row_type, STAGES>;

    matrix_type A_first{{{0, 0, 0},
                         {val_type{1} / 2, 0, 0},
                         {val_type{1} / 2, 0, val_type{1} / 2}}};
    matrix_type A_second{{{0, 0, 0}, {0, val_type{1} / 2, 0}, {0, 1, 0}}};
    row_type b_first{val_type{1} / 2, 0, val_type{1} / 2};
    row_type b_second{0, 1, 0};
    row_type c_first{0, val_type{1} / 2, 1};
    row_type c_second{0, val_type{1} / 2, 1};

    // Linear stage-increment extrapolation preserves the unused columns.
    ZQ_ODE_HOST_DEVICE constexpr matrix_type predictor_first(T ratio) const {
        return detail::linear_stage_predictor(A_first, b_first, ratio);
    }
    ZQ_ODE_HOST_DEVICE constexpr matrix_type predictor_second(T ratio) const {
        return detail::linear_stage_predictor(A_second, b_second, ratio);
    }
};

namespace detail {

template <typename Tableau>
concept HasStagePredictor =
    requires(const Tableau& tableau, typename Tableau::val_type ratio) {
        tableau.predictor(ratio);
    };

template <typename Tableau, std::size_t M>
concept HasAdditionalPredictor =
    requires(const Tableau& tableau, typename Tableau::val_type ratio) {
        tableau.template additional_predictor<M>(ratio);
    };

template <typename Tableau, std::size_t M>
concept HasPartitionedAdditionalPredictor =
    requires(const Tableau& tableau, typename Tableau::val_type ratio) {
        tableau.template additional_predictor_first<M>(ratio);
        tableau.template additional_predictor_second<M>(ratio);
    };

template <typename Tableau>
concept HasPartitionedStagePredictor =
    requires(const Tableau& tableau, typename Tableau::val_type ratio) {
        tableau.predictor_first(ratio);
        tableau.predictor_second(ratio);
    };

template <typename Tableau, typename = void>
inline constexpr bool HAS_PARTITIONED_BETA = false;
template <typename Tableau>
inline constexpr bool HAS_PARTITIONED_BETA<
    Tableau,
    std::void_t<decltype(std::declval<Tableau>().beta_first),
                decltype(std::declval<Tableau>().beta_second)>> = true;

template <typename Tableau, typename = void>
inline constexpr bool HAS_PARTITIONED_STAGE_DEFECT = false;
template <typename Tableau>
inline constexpr bool HAS_PARTITIONED_STAGE_DEFECT<
    Tableau,
    std::void_t<decltype(std::declval<Tableau>().error_weights_first),
                decltype(std::declval<Tableau>().error_weights_second)>> = true;

template <typename Tableau, typename = void>
inline constexpr bool HAS_PARTITIONED_ENDPOINT_DEFECT = false;
template <typename Tableau>
inline constexpr bool HAS_PARTITIONED_ENDPOINT_DEFECT<
    Tableau,
    std::void_t<
        decltype(std::declval<Tableau>().endpoint_error_weights_first),
        decltype(std::declval<Tableau>().endpoint_error_weights_second)>> =
    true;

// void_t also keeps optional-member detection usable with NVCC 12.1.
template <typename Tableau, typename = void>
struct PartitionedStageDefect {
    ZQ_ODE_HOST_DEVICE explicit constexpr PartitionedStageDefect(
        const Tableau&) {}
};

template <typename Tableau>
struct PartitionedStageDefect<
    Tableau,
    std::void_t<decltype(std::declval<Tableau>().error_weights)>> {
    using val_type = typename Tableau::val_type;
    using row_type = std::remove_cvref_t<decltype(std::declval<Tableau>().b)>;
    row_type error_weights_first{}, error_weights_second{};
    ZQ_ODE_HOST_DEVICE explicit constexpr PartitionedStageDefect(
        const Tableau& t)
        : error_weights_first(t.error_weights),
          error_weights_second(t.error_weights) {}
};

template <typename Tableau, typename = void>
struct PartitionedBeta {
    ZQ_ODE_HOST_DEVICE explicit constexpr PartitionedBeta(const Tableau&) {}
};

template <typename Tableau>
struct PartitionedBeta<Tableau,
                       std::void_t<decltype(std::declval<Tableau>().beta)>> {
    using val_type = typename Tableau::val_type;
    using matrix_type =
        std::remove_cvref_t<decltype(std::declval<Tableau>().A)>;
    matrix_type beta_first{}, beta_second{};
    ZQ_ODE_HOST_DEVICE explicit constexpr PartitionedBeta(const Tableau& t)
        : beta_first(t.beta), beta_second(t.beta) {}
};

}  // namespace detail

// Apply one ordinary RK method to both partitions. Equal coefficients retain
// the source method's order, predictors, and symmetric embedded defect.
template <typename Tableau>
struct PartitionedRungeKuttaTableau : Tableau,
                                      detail::PartitionedStageDefect<Tableau>,
                                      detail::PartitionedBeta<Tableau> {
    using val_type = typename Tableau::val_type;
    using row_type = std::remove_cvref_t<decltype(std::declval<Tableau>().b)>;
    using matrix_type =
        std::remove_cvref_t<decltype(std::declval<Tableau>().A)>;
    static constexpr bool IMPLICIT = [] {
        if constexpr (requires { Tableau::IMPLICIT; })
            return Tableau::IMPLICIT;
        else
            return false;
    }();
    static constexpr bool SAMPLE_ALL_STAGES = true;

    matrix_type A_first = this->A;
    matrix_type A_second = A_first;
    row_type b_first = this->b, b_second = b_first;
    row_type c_first = this->c, c_second = c_first;

    ZQ_ODE_HOST_DEVICE constexpr PartitionedRungeKuttaTableau()
        : PartitionedRungeKuttaTableau(Tableau{}) {}

    ZQ_ODE_HOST_DEVICE explicit constexpr PartitionedRungeKuttaTableau(
        Tableau tableau)
        : Tableau(tableau),
          detail::PartitionedStageDefect<Tableau>(tableau),
          detail::PartitionedBeta<Tableau>(tableau) {}

    ZQ_ODE_HOST_DEVICE matrix_type predictor_first(val_type ratio) const
        requires detail::HasStagePredictor<Tableau>
    {
        return Tableau::predictor(ratio);
    }
    ZQ_ODE_HOST_DEVICE matrix_type predictor_second(val_type ratio) const
        requires detail::HasStagePredictor<Tableau>
    {
        return Tableau::predictor(ratio);
    }
    template <std::size_t M>
    ZQ_ODE_HOST_DEVICE auto additional_predictor_first(val_type ratio) const
        -> AdditionalPredictorCoefficients<val_type, Tableau::STAGES, M>
        requires detail::HasAdditionalPredictor<Tableau, M>
    {
        return Tableau::template additional_predictor<M>(ratio);
    }
    template <std::size_t M>
    ZQ_ODE_HOST_DEVICE auto additional_predictor_second(val_type ratio) const
        -> AdditionalPredictorCoefficients<val_type, Tableau::STAGES, M>
        requires detail::HasAdditionalPredictor<Tableau, M>
    {
        return Tableau::template additional_predictor<M>(ratio);
    }
};

template <typename T>
struct FixedPointOptions {
    using val_type = T;

    // Absolute bound in the caller's error norm, which may itself be scaled.
    val_type tolerance = 32 * std::numeric_limits<val_type>::epsilon();
    std::size_t max_iterations = 100;
};

template <typename T>
struct NewtonOptions {
    using val_type = T;

    // Bound on the RK stage residual in the caller's state norm.
    val_type tolerance = 32 * std::numeric_limits<val_type>::epsilon();
    std::size_t max_iterations = 20;
    // Perturb each derivative component by step * max(1, abs(component)).
    val_type difference_step =
        std::sqrt(std::numeric_limits<val_type>::epsilon());
    std::size_t max_backtracks = 8;
};

namespace detail {

template <typename Options, typename T>
concept ImplicitSolveOptions = std::same_as<Options, FixedPointOptions<T>> ||
                               std::same_as<Options, NewtonOptions<T>>;

template <typename T>
ZQ_ODE_HOST_DEVICE bool newton_finite(T value) {
    constexpr T MAXIMUM = std::numeric_limits<T>::max();
    return value >= -MAXIMUM && value <= MAXIMUM;
}

template <typename T, ImplicitSolveOptions<T> Options>
ZQ_ODE_HOST_DEVICE bool valid_iteration_options(const Options& options) {
    if (!newton_finite(options.tolerance) || options.tolerance <= 0 ||
        options.max_iterations == 0)
        return false;
    if constexpr (std::same_as<Options, NewtonOptions<T>>)
        return newton_finite(options.difference_step) &&
               options.difference_step > 0;
    return true;
}

template <typename T, typename Components>
inline constexpr std::size_t NEWTON_DIMENSION = 0;
template <typename T, std::size_t N>
inline constexpr std::size_t NEWTON_DIMENSION<T, std::array<T, N>> = N;

// Custom derivatives need only a component map; state metadata is never
// flattened. Both hooks may overload on the derivative type for PRK pairs.
template <typename T, typename System, typename Derivative>
ZQ_ODE_HOST_DEVICE auto newton_components(const System& system,
                                          const Derivative& derivative) {
    if constexpr (requires { system.newton_components(derivative); })
        return system.newton_components(derivative);
    else if constexpr (std::same_as<Derivative, T>)
        return std::array<T, 1>{derivative};
    else if constexpr (NEWTON_DIMENSION<T, Derivative> > 0)
        return derivative;
    else
        return std::array<T, 0>{};
}

template <typename T, typename System, typename Derivative>
inline constexpr bool NEWTON_MAPPABLE = [] {
    using Components = decltype(newton_components<T>(
        std::declval<const System&>(), std::declval<const Derivative&>()));
    if constexpr (NEWTON_DIMENSION<T, Components> == 0)
        return false;
    else if constexpr (requires(const System& system, const Derivative& d) {
                           system.newton_components(d);
                       }) {
        if constexpr (requires(const System& system, Derivative& d,
                               const Components& values) {
                          system.newton_assign(d, values);
                      })
            return std::is_same_v<
                decltype(std::declval<const System&>().newton_assign(
                    std::declval<Derivative&>(),
                    std::declval<const Components&>())),
                void>;
        else
            return false;
    } else
        return true;
}();

template <typename T, typename System, typename Derivative, std::size_t N>
ZQ_ODE_HOST_DEVICE void newton_assign(const System& system,
                                      Derivative& derivative,
                                      const std::array<T, N>& values) {
    if constexpr (requires { system.newton_components(derivative); })
        system.newton_assign(derivative, values);
    else if constexpr (std::same_as<Derivative, T>)
        derivative = values[0];
    else
        derivative = values;
}

template <std::size_t OFFSET = 0,
          typename T,
          typename System,
          typename Derivative,
          std::size_t S,
          std::size_t N>
ZQ_ODE_HOST_DEVICE void newton_pack(
    const System& system,
    const std::array<Derivative, S>& derivatives,
    std::array<T, N>& values) {
    using Components = decltype(newton_components<T>(system, derivatives[0]));
    constexpr auto DIMENSION = NEWTON_DIMENSION<T, Components>;
    static_assert(DIMENSION > 0 && OFFSET + S * DIMENSION <= N);
    for (std::size_t i = 0; i < S; ++i) {
        const auto components = newton_components<T>(system, derivatives[i]);
        for (std::size_t j = 0; j < DIMENSION; ++j)
            values[OFFSET + i * DIMENSION + j] = components[j];
    }
}

template <std::size_t OFFSET = 0,
          typename T,
          typename System,
          typename Derivative,
          std::size_t S,
          std::size_t N>
ZQ_ODE_HOST_DEVICE void newton_unpack(const System& system,
                                      const std::array<T, N>& values,
                                      std::array<Derivative, S>& derivatives) {
    using Components = decltype(newton_components<T>(system, derivatives[0]));
    constexpr auto DIMENSION = NEWTON_DIMENSION<T, Components>;
    static_assert(DIMENSION > 0 && OFFSET + S * DIMENSION <= N);
    for (std::size_t i = 0; i < S; ++i) {
        Components components{};
        for (std::size_t j = 0; j < DIMENSION; ++j)
            components[j] = values[OFFSET + i * DIMENSION + j];
        newton_assign(system, derivatives[i], components);
    }
}

// Scaled partial pivoting avoids an absolute pivot cutoff for components
// with different units. On success rhs holds the solution; storage is fixed.
template <typename T, std::size_t N>
ZQ_ODE_HOST_DEVICE bool newton_linear_solve(
    std::array<std::array<T, N>, N>& matrix,
    std::array<T, N>& rhs) {
    std::array<T, N> scales{};
    for (std::size_t i = 0; i < N; ++i) {
        for (T value : matrix[i]) {
            if (!newton_finite(value)) return false;
            scales[i] = std::max(scales[i], std::abs(value));
        }
        if (scales[i] == 0 || !newton_finite(rhs[i])) return false;
    }
    for (std::size_t k = 0; k < N; ++k) {
        std::size_t pivot = k;
        T largest = 0;
        for (std::size_t i = k; i < N; ++i) {
            const T relative = std::abs(matrix[i][k]) / scales[i];
            if (relative > largest) {
                pivot = i;
                largest = relative;
            }
        }
        if (!(largest > std::numeric_limits<T>::epsilon())) return false;
        if (pivot != k) {
            const auto row = matrix[k];
            matrix[k] = matrix[pivot];
            matrix[pivot] = row;
            const T value = rhs[k], scale = scales[k];
            rhs[k] = rhs[pivot];
            rhs[pivot] = value;
            scales[k] = scales[pivot];
            scales[pivot] = scale;
        }
        for (std::size_t i = k + 1; i < N; ++i) {
            const T factor = matrix[i][k] / matrix[k][k];
            for (std::size_t j = k + 1; j < N; ++j)
                matrix[i][j] -= factor * matrix[k][j];
            rhs[i] -= factor * rhs[k];
        }
    }
    for (std::size_t i = N; i-- > 0;) {
        for (std::size_t j = i + 1; j < N; ++j) rhs[i] -= matrix[i][j] * rhs[j];
        rhs[i] /= matrix[i][i];
        if (!newton_finite(rhs[i])) return false;
    }
    return true;
}

// Solve x=F(x), where evaluate returns F and the RK stage residual norm.
// Publish only the final evaluation: Jacobian probes and rejected line-search
// candidates may overwrite the caller's local trial stages and derivatives.
template <typename T, std::size_t N, typename Evaluate>
ZQ_ODE_HOST_DEVICE bool newton_solve(std::array<T, N>& values,
                                     const Evaluate& evaluate,
                                     const NewtonOptions<T>& options) {
    const auto sample = [&](const auto& x, auto& rhs, T& error) {
        for (T value : x)
            if (!newton_finite(value)) return false;
        if (!evaluate(x, rhs, error) || !newton_finite(error) || error < 0)
            return false;
        for (T value : rhs)
            if (!newton_finite(value)) return false;
        return true;
    };
    std::array<T, N> rhs{};
    T error = 0;
    if (!sample(values, rhs, error)) return false;
    if (error <= options.tolerance) return true;
    for (std::size_t iteration = 0; iteration < options.max_iterations;
         ++iteration) {
        std::array<std::array<T, N>, N> matrix{};
        for (std::size_t j = 0; j < N; ++j) {
            const T increment =
                options.difference_step * std::max(T{1}, std::abs(values[j]));
            std::array<T, N> perturbed_rhs{};
            T delta = 0, unused = 0;
            bool sampled = false;
            for (T sign : {T{1}, T{-1}}) {
                auto perturbed = values;
                perturbed[j] += sign * increment;
                delta = perturbed[j] - values[j];
                if (newton_finite(delta) && delta != 0 &&
                    sample(perturbed, perturbed_rhs, unused)) {
                    sampled = true;
                    break;
                }
            }
            if (!sampled) return false;
            for (std::size_t i = 0; i < N; ++i)
                matrix[i][j] = (i == j ? T{1} : T{0}) -
                               (perturbed_rhs[i] - rhs[i]) / delta;
        }
        std::array<T, N> correction{};
        for (std::size_t i = 0; i < N; ++i) correction[i] = rhs[i] - values[i];
        if (!newton_linear_solve(matrix, correction)) return false;
        T damping = 1;
        bool accepted = false;
        for (std::size_t backtrack = 0;; ++backtrack) {
            auto candidate = values;
            for (std::size_t i = 0; i < N; ++i)
                candidate[i] += damping * correction[i];
            T candidate_error = 0;
            std::array<T, N> candidate_rhs{};
            if (sample(candidate, candidate_rhs, candidate_error) &&
                (candidate_error <= options.tolerance ||
                 candidate_error <= (T{1} - T{1e-4} * damping) * error)) {
                values = candidate;
                rhs = candidate_rhs;
                error = candidate_error;
                accepted = true;
                break;
            }
            if (backtrack == options.max_backtracks) break;
            damping /= 2;
        }
        if (!accepted) return false;
        if (error <= options.tolerance) return true;
    }
    return false;
}

}  // namespace detail

template <typename T>
struct SymmetricStepOptions {
    using val_type = T;

    // Target norm of the embedded defect, not a bound on the global error.
    val_type tolerance = 1.0e-6;
    // Relative residual in L(h, y) = tolerance; finite solves approximate
    // Stoffer's time symmetry to this accuracy.
    val_type relative_tolerance = 1.0e-3;
    std::size_t max_iterations = 20;
};

template <typename T>
using DefaultButcherTableau = RungeKutta4Tableau<T>;

struct ContinuousOutputPredictor {
    static constexpr std::size_t EXTRA_STAGES = 0;
    static constexpr std::size_t HISTORY = 0;
};

// HLW VIII.6.1(B): order s+1 with one extra stage, or s+2 with three.
template <std::size_t M = 3>
struct AdditionalStagePredictor {
    static constexpr std::size_t EXTRA_STAGES = M;
    static constexpr std::size_t HISTORY = 0;
    static_assert(M == 1 || M == 3,
                  "AdditionalStagePredictor requires one or three stages");
};

// HLW VIII.6.1(C): k equal-step stage increments give error O(h^(k+1)).
template <std::size_t K = 8>
struct EquistagePredictor {
    static constexpr std::size_t EXTRA_STAGES = 0;
    static constexpr std::size_t HISTORY = K;
    static_assert(K > 0, "EquistagePredictor requires positive history length");
};

namespace detail {

struct EmptyPredictorData {};

template <typename Derivative>
struct PredictorEndpoint {
    Derivative initial{};
};

template <typename Derivative, std::size_t S, std::size_t K>
struct EquistageHistory {
    // The newest derivatives already live in StepCacheData::derivatives_.
    std::array<std::array<Derivative, S>, K - 1> older{};
    std::size_t count = 0;
    std::size_t next = 0;
};

template <typename First, typename Second, std::size_t S, std::size_t K>
struct PartitionedHistory {
    std::array<std::array<First, S>, K - 1> first{};
    std::array<std::array<Second, S>, K - 1> second{};
    std::size_t count = 0;
    std::size_t next = 0;
};

template <typename First, typename Second>
struct PartitionedEndpoint {
    First first{};
    Second second{};
    bool valid = false;
};

template <typename T>
ZQ_ODE_HOST_DEVICE bool valid_doubling_options(T h, T cap, T upper, T lower) {
    if (!newton_finite(h) || h == 0 || !newton_finite(cap) || cap <= 0) {
#if !defined(__CUDA_ARCH__)
        throw std::invalid_argument(
            "Integrator requires a finite nonzero step and a finite "
            "positive maximum step");
#endif
        return false;
    }
    if (!newton_finite(upper) || upper <= 0 || !newton_finite(lower) ||
        lower < 0 || lower > upper) {
#if !defined(__CUDA_ARCH__)
        throw std::invalid_argument(
            "Integrator requires finite error bounds with "
            "0 <= lower <= upper and upper > 0");
#endif
        return false;
    }
    return true;
}

// The callback computes full/fine trials without publishing them and returns
// their raw distance. The caller commits the fine trial only on success.
template <std::size_t ORDER, typename T, typename Trial>
ZQ_ODE_HOST_DEVICE bool step_doubling(T& time_step,
                                      T cap,
                                      T upper,
                                      T lower,
                                      const Trial& trial) {
    if (!valid_doubling_options(time_step, cap, upper, lower)) return false;
    constexpr T SAFETY = T{0.9}, MINIMUM = T{0.1}, MAXIMUM = 5;
    constexpr T SCALE = [] {
        T value = 1;
        for (std::size_t i = 0; i < ORDER; ++i) value *= 2;
        return value - 1;
    }();
    constexpr T EXPONENT = T{1} / static_cast<T>(ORDER + 1);
    const T direction = time_step > 0 ? T{1} : T{-1};
    T h = std::min(direction * time_step, cap);
    while (true) {
        const T half = h / 2;
        if (half == 0 || half + half != h) return false;
        T distance = 0;
        if (!trial(direction * h, direction * half, distance) ||
            !newton_finite(distance) || distance < 0)
            return false;
        const T error = distance / SCALE;
        if (error <= upper) {
            T proposal = h;
            if (error < lower) {
                const T factor =
                    error == 0
                        ? MAXIMUM
                        : std::clamp(SAFETY * std::pow(upper / error, EXPONENT),
                                     T{1}, MAXIMUM);
                proposal = h >= cap / factor ? cap : h * factor;
            }
            time_step = direction * proposal;
            return true;
        }
        const T factor = std::clamp(SAFETY * std::pow(upper / error, EXPONENT),
                                    MINIMUM, T{0.5});
        const T reduced = h * factor;
        if (!(reduced > 0 && reduced < h)) return false;
        h = reduced;
    }
}

enum class AdaptiveTrial { SOLVE_FAILED, INVALID_ERROR, SOLVED };

// Shared Stoffer selector. A failed stage solve may shrink; an invalid norm
// aborts. Only acceptance changes h; trial state and cache stay with the
// caller.
template <std::size_t ERROR_ORDER, typename T, typename Trial>
ZQ_ODE_HOST_DEVICE bool symmetric_step(T& time_step,
                                       T cap,
                                       T last_h,
                                       T previous_h,
                                       bool valid,
                                       bool history_valid,
                                       SymmetricStepOptions<T> options,
                                       const Trial& trial) {
    if (!newton_finite(time_step) || time_step == 0 || !newton_finite(cap) ||
        cap <= 0 || !newton_finite(options.tolerance) ||
        options.tolerance <= 0 || !newton_finite(options.relative_tolerance) ||
        options.relative_tolerance <= 0 || options.relative_tolerance >= 1 ||
        options.max_iterations == 0)
        return false;
    const T direction = time_step > 0 ? T{1} : T{-1};
    T h = direction * time_step;
    if (valid && direction * last_h > 0) {
        h = direction * last_h;
        if (history_valid && direction * previous_h > 0) {
            const T prediction = 2 * h - direction * previous_h;
            if (newton_finite(prediction) && prediction > 0) h = prediction;
        }
    }
    h = std::min(h, cap);
    T lower = 0, upper = cap, before_h = 0, before_error = 0;
    bool bracketed = false;
    for (std::size_t iteration = 0; iteration < options.max_iterations;
         ++iteration) {
        T error = 0;
        const auto outcome = trial(direction * h, error);
        if (outcome == AdaptiveTrial::INVALID_ERROR) return false;
        if (outcome == AdaptiveTrial::SOLVE_FAILED) {
            upper = h;
            bracketed = true;
            before_error = 0;
            h = lower > 0 ? std::sqrt(lower) * std::sqrt(upper)
                          : T{0.5} * upper;
            if (!newton_finite(h) || h <= lower || h >= upper) return false;
            continue;
        }
        if (!newton_finite(error) || error < 0) return false;
        const T relative_error = error / options.tolerance;
        if ((relative_error >= 1 - options.relative_tolerance &&
             relative_error <= 1 + options.relative_tolerance) ||
            (h == cap && relative_error <= 1)) {
            time_step = direction * h;
            return true;
        }
        if (error < options.tolerance)
            lower = h;
        else {
            upper = h;
            bracketed = true;
        }
        T candidate = 2 * h;
        if (error > 0) {
            T exponent = T{1} / ERROR_ORDER;
            if (before_error > 0 && error != before_error && h != before_h) {
                const T secant =
                    std::log(h / before_h) / std::log(error / before_error);
                if (newton_finite(secant) && secant > 0) exponent = secant;
            }
            const T correction =
                exponent * (std::log(options.tolerance) - std::log(error));
            candidate = h * std::exp(std::clamp(correction, std::log(T{0.2}),
                                                std::log(T{5})));
        }
        candidate = std::min(candidate, cap);
        if (bracketed && (candidate <= lower || candidate >= upper))
            candidate = lower > 0 ? std::sqrt(lower) * std::sqrt(upper)
                                  : T{0.5} * upper;
        if (!newton_finite(candidate) || candidate <= 0 || candidate == h)
            return false;
        before_h = h;
        before_error = error;
        h = candidate;
    }
    return false;
}

}  // namespace detail

// Runge-Kutta integrator driven entirely by Tableau::A, b, and c.
// Implicit tableaus declare IMPLICIT = true and supply predictor weights beta.
// Tableaus without IMPLICIT retain the explicit interface.
// System must provide:
//   using derivative_type = ...;
//   bool evaluate(const State&, derivative_type&) const;
//   State advance(const State&, const array<derivative_type, STAGES>&,
//                 const array<T, STAGES>&, size_t active_stages,
//                 T time_step, T time_fraction) const;
// Implicit and adaptive steps take an error_norm(lhs, rhs) returning a finite,
// nonnegative state distance (nonfinite inputs must give nonfinite error).
// Predictors B/C accumulate increments through repeated advance calls with
// time_fraction=0, then advance the accumulated state's clock once.
template <typename T,
          typename Tableau = DefaultButcherTableau<T>,
          typename Predictor = ContinuousOutputPredictor>
class Integrator {
   public:
    using val_type = T;
    using tableau_type = Tableau;
    using predictor_type = Predictor;

    static constexpr std::size_t ORDER = tableau_type::ORDER;
    static constexpr std::size_t STAGES = tableau_type::STAGES;
    static constexpr bool IMPLICIT = [] {
        if constexpr (requires { tableau_type::IMPLICIT; }) {
            return tableau_type::IMPLICIT;
        } else {
            return false;
        }
    }();
    static_assert(std::is_same_v<typename tableau_type::val_type, val_type>);
    static_assert(ORDER > 0 && STAGES > 0);
    static_assert(IMPLICIT ||
                      std::same_as<predictor_type, ContinuousOutputPredictor>,
                  "starting predictors require an implicit tableau");

    // Keep one cache per trajectory. Reset after externally changing the state,
    // system, or tableau, including collisions. A tableau may supply
    // predictor(h_new/h_old) to reuse stages when the step size changes.
    template <typename State, typename Derivative>
    class StepCacheData {
       public:
        ZQ_ODE_HOST_DEVICE void reset() {
            valid_ = false;
            history_valid_ = false;
            if constexpr (predictor_type::HISTORY > 0) {
                history_.count = 0;
                history_.next = 0;
            }
        }

       private:
        friend class Integrator;
        State initial_state_{};
        std::array<Derivative, STAGES> derivatives_{};
        val_type time_step_ = 0;
        val_type previous_time_step_ = 0;
        bool valid_ = false;
        bool history_valid_ = false;
        [[no_unique_address]] std::conditional_t<
            predictor_type::EXTRA_STAGES == 3,
            detail::PredictorEndpoint<Derivative>,
            detail::EmptyPredictorData> endpoint_{};
        [[no_unique_address]] std::conditional_t<
            (predictor_type::HISTORY > 0),
            detail::
                EquistageHistory<Derivative, STAGES, predictor_type::HISTORY>,
            detail::EmptyPredictorData> history_{};
    };

    // Adapters that restrict field sampling retain the same cache type.
    template <typename State, typename System>
    using StepCache = StepCacheData<State, typename System::derivative_type>;

    ZQ_ODE_HOST_DEVICE explicit Integrator(tableau_type tableau = {},
                                           predictor_type = {})
        : tableau_(tableau),
          additional_(additional_coefficients(tableau_, ONE)) {}

    template <typename State, typename Derivative>
    struct StageData {
        std::array<State, STAGES> states;
        std::array<Derivative, STAGES> derivatives;
    };

    template <typename State, typename System>
    using Stages = StageData<State, typename System::derivative_type>;

    template <typename State, typename System>
    using ExplicitStages = Stages<State, System>;

    template <typename State, typename System>
    ZQ_ODE_HOST_DEVICE bool step(const State& initial_state,
                                 val_type time_step,
                                 const System& system,
                                 State& next_state) const
        requires(!IMPLICIT)
    {
        return explicit_step(initial_state, time_step, system, next_state,
                             [](std::size_t, const State&,
                                const typename System::derivative_type&) {});
    }

    // Stage data supports dense output and discontinuity diagnostics. As with
    // next_state, it is published only after all RHS evaluations succeed.
    template <typename State, typename System>
    ZQ_ODE_HOST_DEVICE bool step(const State& initial_state,
                                 val_type time_step,
                                 const System& system,
                                 State& next_state,
                                 ExplicitStages<State, System>& stages) const
        requires(!IMPLICIT)
    {
        ExplicitStages<State, System> trial{};
        if (!explicit_step(
                initial_state, time_step, system, next_state,
                [&](std::size_t i, const State& state,
                    const typename System::derivative_type& derivative) {
                    trial.states[i] = state;
                    trial.derivatives[i] = derivative;
                }))
            return false;
        stages = trial;
        return true;
    }

    template <typename State,
              typename System,
              typename ErrorNorm,
              typename IterationOptions = FixedPointOptions<val_type>>
    ZQ_ODE_HOST_DEVICE bool step(const State& initial_state,
                                 val_type time_step,
                                 const System& system,
                                 State& next_state,
                                 const ErrorNorm& error_norm,
                                 IterationOptions options = {}) const
        requires detail::ImplicitSolveOptions<IterationOptions, val_type>
    {
        StepCache<State, System> cache;
        return step(initial_state, time_step, system, next_state, cache,
                    error_norm, options);
    }

    // Implicit iteration on Y_i = y_n + Z_i. This form also supports
    // states with time or other metadata through System::advance. Failure
    // leaves both next_state and cache untouched; aliasing next_state with y_n
    // is valid.
    template <typename State,
              typename System,
              typename ErrorNorm,
              typename IterationOptions = FixedPointOptions<val_type>>
    ZQ_ODE_HOST_DEVICE bool step(const State& initial_state,
                                 val_type time_step,
                                 const System& system,
                                 State& next_state,
                                 StepCache<State, System>& cache,
                                 const ErrorNorm& error_norm,
                                 IterationOptions options = {}) const
        requires detail::ImplicitSolveOptions<IterationOptions, val_type>
    {
        if constexpr (!IMPLICIT) {
            static_assert(
                std::same_as<IterationOptions, FixedPointOptions<val_type>>,
                "Newton iteration requires an implicit tableau");
            return step(initial_state, time_step, system, next_state);
        } else {
            if (!detail::valid_iteration_options<val_type>(options) ||
                !finite(time_step)) {
                return false;
            }
            using Derivative = typename System::derivative_type;
            std::array<Derivative, STAGES> derivatives{};
            std::array<State, STAGES> stages{};
            std::conditional_t<predictor_type::EXTRA_STAGES == 3, Derivative,
                               detail::EmptyPredictorData>
                initial_derivative{};
            if (!starting_stages(initial_state, time_step, system, cache,
                                 derivatives, stages, initial_derivative))
                return false;

            bool solved = false;
            if constexpr (std::same_as<IterationOptions,
                                       NewtonOptions<val_type>>) {
                solved = newton_stages(initial_state, time_step, system, stages,
                                       derivatives, error_norm, options);
            } else {
                for (std::size_t iteration = 0;
                     iteration < options.max_iterations; ++iteration) {
                    for (std::size_t i = 0; i < STAGES; ++i) {
                        if (!system.evaluate(stages[i], derivatives[i])) {
                            return false;
                        }
                    }
                    bool converged = true;
                    for (std::size_t i = 0; i < STAGES; ++i) {
                        const State updated = system.advance(
                            initial_state, derivatives, tableau_.A[i], STAGES,
                            time_step, tableau_.c[i]);
                        const val_type error = error_norm(stages[i], updated);
                        if (!finite(error) || error < ZERO) return false;
                        if (error > options.tolerance) converged = false;
                        stages[i] = updated;
                    }
                    if (converged) {
                        solved = true;
                        break;
                    }
                }
            }
            if (!solved) return false;

            const State result = system.advance(
                initial_state, derivatives, tableau_.b, STAGES, time_step, ONE);
            const val_type error = error_norm(result, result);
            if (!finite(error) || error < ZERO) return false;
            if constexpr (predictor_type::HISTORY > 0) {
                auto& history = cache.history_;
                if (time_step == ZERO) {
                    history.count = 0;
                    history.next = 0;
                } else if (cache.valid_ && cache.time_step_ == time_step &&
                           history.count > 0) {
                    if constexpr (predictor_type::HISTORY > 1) {
                        history.older[history.next] = cache.derivatives_;
                        history.next =
                            (history.next + 1) % (predictor_type::HISTORY - 1);
                    }
                    if (history.count < predictor_type::HISTORY)
                        ++history.count;
                } else {
                    history.count = 1;
                    history.next = 0;
                }
            }
            if constexpr (predictor_type::EXTRA_STAGES == 3)
                cache.endpoint_.initial = initial_derivative;
            cache.initial_state_ = initial_state;
            cache.derivatives_ = derivatives;
            cache.previous_time_step_ = cache.time_step_;
            cache.history_valid_ = cache.valid_;
            cache.time_step_ = time_step;
            cache.valid_ = true;
            next_state = result;
            return true;
        }
    }

    // Record a successful implicit solve without changing the ordinary step
    // path. Recorded stage states and derivatives agree to the solve tolerance.
    template <typename State,
              typename System,
              typename ErrorNorm,
              typename IterationOptions = FixedPointOptions<val_type>>
    ZQ_ODE_HOST_DEVICE bool step(const State& initial_state,
                                 val_type time_step,
                                 const System& system,
                                 State& next_state,
                                 StepCache<State, System>& cache,
                                 Stages<State, System>& stages,
                                 const ErrorNorm& error_norm,
                                 IterationOptions options = {}) const
        requires detail::ImplicitSolveOptions<IterationOptions, val_type>
    {
        if constexpr (!IMPLICIT) {
            static_assert(
                std::same_as<IterationOptions, FixedPointOptions<val_type>>,
                "Newton iteration requires an implicit tableau");
            return step(initial_state, time_step, system, next_state, stages);
        } else {
            if (!step(initial_state, time_step, system, next_state, cache,
                      error_norm, options))
                return false;
            stages.derivatives = cache.derivatives_;
            for (std::size_t i = 0; i < STAGES; ++i)
                stages.states[i] = system.advance(
                    cache.initial_state_, cache.derivatives_, tableau_.A[i],
                    STAGES, time_step, tableau_.c[i]);
            return true;
        }
    }

    // Stoffer (1995), Sections 4.2(a), 4.3: solve the symmetric embedded
    // defect equation L(h, y) = tolerance for the CURRENT step. A smaller
    // error is accepted only at maximum_time_step. time_step supplies the
    // initial guess/direction and returns the accepted, signed step length.
    // defect_norm must be an even, translation-invariant norm with fixed
    // scales (normally Euclidean); error_norm controls the stage iteration.
    // Rejected trials leave state, time_step, and successful-step cache intact.
    template <typename State,
              typename System,
              typename ErrorNorm,
              typename DefectNorm,
              typename IterationOptions = FixedPointOptions<val_type>>
    ZQ_ODE_HOST_DEVICE bool step_adaptive(
        const State& initial_state,
        val_type& time_step,
        val_type maximum_time_step,
        const System& system,
        State& next_state,
        StepCache<State, System>& cache,
        const ErrorNorm& error_norm,
        const DefectNorm& defect_norm,
        SymmetricStepOptions<val_type> options = {},
        IterationOptions iteration_options = {}) const
        requires(IMPLICIT &&
                 detail::ImplicitSolveOptions<IterationOptions, val_type> &&
                 requires(tableau_type t) {
                     tableau_type::SYMMETRIC;
                     tableau_type::ERROR_ORDER;
                     t.error_weights;
                 })
    {
        static_assert(tableau_type::SYMMETRIC && tableau_type::ERROR_ORDER > 0);
        if (!detail::valid_iteration_options<val_type>(iteration_options)) {
            return false;
        }
        auto trial_cache = cache;
        State trial_state{};
        const State error_origin{};
        const auto trial = [&](val_type h, val_type& error) {
            trial_cache = cache;
            bool valid_norm = true;
            const auto checked_norm = [&](const State& a, const State& b) {
                const val_type value = error_norm(a, b);
                valid_norm &= finite(value) && value >= ZERO;
                return value;
            };
            const bool solved =
                step(initial_state, h, system, trial_state, trial_cache,
                     checked_norm, iteration_options);
            if (!valid_norm) return detail::AdaptiveTrial::INVALID_ERROR;
            if (!solved) return detail::AdaptiveTrial::SOLVE_FAILED;
            // Accumulate about a default state to avoid subtracting large
            // orbit coordinates when measuring a small embedded defect.
            const State defect =
                system.advance(error_origin, trial_cache.derivatives_,
                               tableau_.error_weights, STAGES, h, ZERO);
            error = defect_norm(error_origin, defect);
            return detail::AdaptiveTrial::SOLVED;
        };
        if (!detail::symmetric_step<tableau_type::ERROR_ORDER>(
                time_step, maximum_time_step, cache.time_step_,
                cache.previous_time_step_, cache.valid_, cache.history_valid_,
                options, trial))
            return false;
        next_state = trial_state;
        cache = trial_cache;
        return true;
    }

    // Accept the two-half-step state and return a signed NEXT-step proposal.
    // maximum_time_step is a positive magnitude. Invalid options throw on the
    // host and return false on a CUDA device; other failures return false.
    // Failure leaves state and time_step untouched.
    template <typename State, typename System, typename ErrorNorm>
    ZQ_ODE_HOST_DEVICE bool step_adaptive(State& state,
                                          val_type& time_step,
                                          val_type maximum_time_step,
                                          val_type upper_error_bound,
                                          val_type lower_error_bound,
                                          const System& system,
                                          const ErrorNorm& error_norm) const
        requires(!IMPLICIT)
    {
        const State initial_state = state;
        State second_half_step{};
        const auto trial = [&](val_type h, val_type half, val_type& distance) {
            State full_step{};
            State first_half_step{};
            if (!step(initial_state, h, system, full_step) ||
                !step(initial_state, half, system, first_half_step) ||
                !step(first_half_step, half, system, second_half_step)) {
                return false;
            }
            distance = error_norm(full_step, second_half_step);
            return true;
        };
        if (!detail::step_doubling<ORDER>(time_step, maximum_time_step,
                                          upper_error_bound, lower_error_bound,
                                          trial))
            return false;
        state = second_half_step;
        return true;
    }

   private:
    using additional_type = std::conditional_t<
        predictor_type::EXTRA_STAGES == 0,
        detail::EmptyPredictorData,
        AdditionalPredictorCoefficients<val_type,
                                        STAGES,
                                        predictor_type::EXTRA_STAGES>>;

    ZQ_ODE_HOST_DEVICE static additional_type additional_coefficients(
        const tableau_type& tableau,
        val_type ratio) {
        if constexpr (predictor_type::EXTRA_STAGES == 0) {
            return {};
        } else if constexpr (requires {
                                 tableau.template additional_predictor<
                                     predictor_type::EXTRA_STAGES>(ratio);
                             }) {
            return tableau
                .template additional_predictor<predictor_type::EXTRA_STAGES>(
                    ratio);
        } else {
            static_assert(predictor_type::EXTRA_STAGES == 0,
                          "tableau does not support this additional-stage "
                          "predictor (B1: Gauss/IIIA; B3: Gauss)");
            return {};
        }
    }

    template <typename State, typename System, typename ErrorNorm>
    ZQ_ODE_HOST_DEVICE bool newton_stages(
        const State& initial,
        val_type h,
        const System& system,
        std::array<State, STAGES>& stages,
        std::array<typename System::derivative_type, STAGES>& derivatives,
        const ErrorNorm& error_norm,
        NewtonOptions<val_type> options) const {
        using Derivative = typename System::derivative_type;
        static_assert(detail::NEWTON_MAPPABLE<val_type, System, Derivative>,
                      "Newton requires scalar/std::array derivatives or "
                      "System::newton_components and newton_assign hooks "
                      "using std::array<val_type, N> with N > 0");
        if constexpr (detail::NEWTON_MAPPABLE<val_type, System, Derivative>) {
            using Components = decltype(detail::newton_components<val_type>(
                system, derivatives[0]));
            constexpr auto DIMENSION = std::tuple_size_v<Components>;
            using Vector = std::array<val_type, STAGES * DIMENSION>;
            Vector values{};
            // Evaluate the chosen stage predictor to start the derivative
            // equations k_i=f(y_n+h sum_j A_ij k_j).
            for (std::size_t i = 0; i < STAGES; ++i) {
                if (!system.evaluate(stages[i], derivatives[i])) return false;
            }
            detail::newton_pack(system, derivatives, values);
            const auto evaluate = [&](const Vector& x, Vector& rhs,
                                      val_type& error) {
                detail::newton_unpack(system, x, derivatives);
                for (std::size_t i = 0; i < STAGES; ++i)
                    stages[i] =
                        system.advance(initial, derivatives, tableau_.A[i],
                                       STAGES, h, tableau_.c[i]);
                for (std::size_t i = 0; i < STAGES; ++i)
                    if (!system.evaluate(stages[i], derivatives[i]))
                        return false;
                error = 0;
                for (std::size_t i = 0; i < STAGES; ++i) {
                    const State updated =
                        system.advance(initial, derivatives, tableau_.A[i],
                                       STAGES, h, tableau_.c[i]);
                    const val_type residual = error_norm(stages[i], updated);
                    if (!finite(residual) || residual < 0) return false;
                    error = std::max(error, residual);
                    stages[i] = updated;
                }
                detail::newton_pack(system, derivatives, rhs);
                return true;
            };
            // Keep fresh F(k), not k: A can have a nullspace, while b still
            // depends on that direction. Stages are reconstructed from F(k).
            return detail::newton_solve(values, evaluate, options);
        }
        return false;
    }

    template <typename State, typename System>
    ZQ_ODE_HOST_DEVICE State
    add_derivative(const State& state,
                   const typename System::derivative_type& derivative,
                   val_type weight,
                   val_type h,
                   const System& system) const {
        std::array<typename System::derivative_type, STAGES> values{};
        typename tableau_type::row_type weights{};
        values[0] = derivative;
        weights[0] = weight;
        return system.advance(state, values, weights, 1, h, ZERO);
    }

    template <typename State, typename System, typename InitialDerivative>
    ZQ_ODE_HOST_DEVICE bool starting_stages(
        const State& initial,
        val_type h,
        const System& system,
        const StepCache<State, System>& cache,
        std::array<typename System::derivative_type, STAGES>& derivatives,
        std::array<State, STAGES>& stages,
        InitialDerivative& initial_derivative) const {
        if constexpr (predictor_type::HISTORY > 0) {
            if (cache.valid_ && cache.time_step_ == h && h != ZERO &&
                cache.history_.count > 0) {
                const auto& history = cache.history_;
                for (std::size_t i = 0; i < STAGES; ++i) {
                    State stage = initial;
                    val_type binomial = ONE;
                    for (std::size_t j = 0; j < history.count; ++j) {
                        // Expand sum_l nabla^l Z_old into old A*k increments.
                        binomial *= -static_cast<val_type>(history.count - j) /
                                    static_cast<val_type>(j + 1);
                        if (!finite(binomial)) return false;
                        auto weights = tableau_.A[i];
                        for (auto& weight : weights) weight *= -binomial;
                        if (j == 0) {
                            stage = system.advance(stage, cache.derivatives_,
                                                   weights, STAGES, h, ZERO);
                        } else if constexpr (predictor_type::HISTORY > 1) {
                            constexpr auto CAPACITY =
                                predictor_type::HISTORY - 1;
                            const auto index =
                                (history.next + CAPACITY - j) % CAPACITY;
                            stage = system.advance(stage, history.older[index],
                                                   weights, STAGES, h, ZERO);
                        }
                    }
                    stages[i] = system.advance(
                        stage, derivatives, tableau_.A[i], 0, h, tableau_.c[i]);
                }
                return true;
            }
        }
        if constexpr (predictor_type::EXTRA_STAGES > 0) {
            constexpr auto M = predictor_type::EXTRA_STAGES;
            if constexpr (M == 3)
                if (!system.evaluate(initial, initial_derivative)) return false;
            if (!cache.valid_ || cache.time_step_ == ZERO) {
                auto weights = typename tableau_type::row_type{};
                if constexpr (M == 3) derivatives[0] = initial_derivative;
                for (std::size_t i = 0; i < STAGES; ++i) {
                    weights[0] = tableau_.c[i];
                    stages[i] =
                        system.advance(initial, derivatives, weights,
                                       M == 3 ? 1 : 0, h, tableau_.c[i]);
                }
                return true;
            }
            const val_type ratio = h / cache.time_step_;
            if (!finite(ratio)) return false;
            const auto coefficients =
                ratio == ONE ? additional_
                             : additional_coefficients(tableau_, ratio);
            for (val_type c : coefficients.c)
                if (!finite(c)) return false;
            for (const auto& row : coefficients.mu)
                for (val_type weight : row)
                    if (!finite(weight)) return false;
            for (const auto& row : coefficients.alpha)
                for (val_type weight : row)
                    if (!finite(weight)) return false;
            for (const auto& row : coefficients.nu)
                for (val_type weight : row)
                    if (!finite(weight)) return false;
            std::array<typename System::derivative_type, M> extra{};
            for (std::size_t j = 0; j < M; ++j) {
                if constexpr (M == 3) {
                    if (j == 0) {
                        extra[j] = cache.endpoint_.initial;
                        continue;
                    }
                    if (j == 1) {
                        extra[j] = initial_derivative;
                        continue;
                    }
                }
                auto weights = typename tableau_type::row_type{};
                for (std::size_t k = 0; k < STAGES; ++k)
                    weights[k] = coefficients.mu[j][k];
                State sample = system.advance(
                    cache.initial_state_, cache.derivatives_, weights, STAGES,
                    cache.time_step_, coefficients.c[j]);
                for (std::size_t k = 0; k < j; ++k)
                    if (coefficients.mu[j][STAGES + k] != ZERO)
                        sample = add_derivative(sample, extra[k],
                                                coefficients.mu[j][STAGES + k],
                                                cache.time_step_, system);
                if (!system.evaluate(sample, extra[j])) return false;
            }
            for (std::size_t i = 0; i < STAGES; ++i) {
                State stage = system.advance(initial, cache.derivatives_,
                                             coefficients.alpha[i], STAGES,
                                             cache.time_step_, ZERO);
                for (std::size_t j = 0; j < M; ++j)
                    if (coefficients.nu[i][j] != ZERO)
                        stage = add_derivative(stage, extra[j],
                                               coefficients.nu[i][j],
                                               cache.time_step_, system);
                stages[i] = system.advance(stage, derivatives, tableau_.A[i], 0,
                                           h, tableau_.c[i]);
            }
            return true;
        }
        bool predict =
            cache.valid_ && cache.time_step_ != ZERO && cache.time_step_ == h;
        auto beta = tableau_.beta;
        if constexpr (detail::HasStagePredictor<tableau_type>) {
            if (cache.valid_ && cache.time_step_ != ZERO &&
                cache.time_step_ != h) {
                const val_type ratio = h / cache.time_step_;
                if (finite(ratio)) {
                    beta = tableau_.predictor(ratio);
                    predict = true;
                }
            }
        }
        for (std::size_t i = 0; i < STAGES; ++i)
            stages[i] =
                predict
                    ? system.advance(cache.initial_state_, cache.derivatives_,
                                     beta[i], STAGES, cache.time_step_,
                                     ONE + tableau_.c[i] * h / cache.time_step_)
                    : system.advance(initial, derivatives, tableau_.A[i], 0, h,
                                     tableau_.c[i]);
        return true;
    }

    template <typename State, typename System, typename Observer>
    ZQ_ODE_HOST_DEVICE bool explicit_step(const State& initial_state,
                                          val_type time_step,
                                          const System& system,
                                          State& next_state,
                                          const Observer& observe) const {
        if (!finite(time_step)) return false;
        std::array<typename System::derivative_type, STAGES> derivatives{};
        for (std::size_t i = 0; i < STAGES; ++i) {
            const State state =
                system.advance(initial_state, derivatives, tableau_.A[i], i,
                               time_step, tableau_.c[i]);
            if (!system.evaluate(state, derivatives[i])) return false;
            observe(i, state, derivatives[i]);
        }
        next_state = system.advance(initial_state, derivatives, tableau_.b,
                                    STAGES, time_step, ONE);
        return true;
    }

    const tableau_type tableau_;
    [[no_unique_address]] const additional_type additional_;

    static constexpr val_type ZERO = 0;
    static constexpr val_type ONE = 1;

    ZQ_ODE_HOST_DEVICE static bool finite(val_type value) {
        constexpr val_type MAXIMUM = std::numeric_limits<val_type>::max();
        return value >= -MAXIMUM && value <= MAXIMUM;
    }
};

template <typename Tableau>
Integrator(Tableau) -> Integrator<typename Tableau::val_type, Tableau>;

template <typename Tableau, typename Predictor>
Integrator(Tableau, Predictor)
    -> Integrator<typename Tableau::val_type, Tableau, Predictor>;

// Two-partition RK with independent coefficients and stage times. System
// supplies first_derivative_type, second_derivative_type, evaluate_first,
// evaluate_second, and:
//   State advance(initial, first_derivatives, second_derivatives,
//                 first_weights, second_weights, count, h, c_first, c_second)
// The caller decides how the two partitions and their times inhabit State.
// System::SEPARABLE = true promises that each evaluator depends only on the
// opposite partition and its own stage time. A lower-triangular pair with at
// most one nonzero diagonal per stage then needs no implicit solve. advance
// reconstructs partial stages from initial, so it must apply only the supplied
// increments and must not require both partitions to be complete.
// A lower-triangular pair may use an algebraic diagonal-stage solver:
//   bool solve_stage(State& base, T h_a_first, T h_a_second, size_t stage)
// base excludes both diagonal increments but carries the target stage times.
// The hook also runs for zero diagonals, allowing geometric metadata updates.
// For implicit pairs this hook takes precedence over separable execution.
// Otherwise unresolved implicit stages use coupled fixed-point or Newton
// iteration, selected by the type of the iteration options.
template <typename T,
          typename Tableau = BorisTableau<T>,
          typename Predictor = ContinuousOutputPredictor>
class PartitionedIntegrator {
    template <typename System>
    static constexpr bool SEPARABLE_SYSTEM = [] {
        if constexpr (requires { System::SEPARABLE; })
            return System::SEPARABLE;
        else
            return false;
    }();

    template <typename State, typename System>
    static constexpr bool HAS_STAGE_SOLVER =
        requires(const System& system, State& state, T h) {
            system.solve_stage(state, h, h, std::size_t{});
        };

   public:
    using val_type = T;
    using tableau_type = Tableau;
    using predictor_type = Predictor;
    using row_type = typename tableau_type::row_type;
    using matrix_type = std::array<row_type, tableau_type::STAGES>;

    static constexpr std::size_t ORDER = tableau_type::ORDER;
    static constexpr std::size_t STAGES = tableau_type::STAGES;
    static constexpr bool IMPLICIT = tableau_type::IMPLICIT;
    static constexpr bool DIAGONALLY_IMPLICIT = [] {
        if constexpr (requires { tableau_type::DIAGONALLY_IMPLICIT; })
            return tableau_type::DIAGONALLY_IMPLICIT;
        else
            return false;
    }();
    static_assert(std::is_same_v<typename tableau_type::val_type, val_type>);
    static_assert(ORDER > 0 && STAGES > 0);
    static_assert(std::tuple_size_v<row_type> == STAGES);
    static_assert(IMPLICIT ||
                      std::same_as<predictor_type, ContinuousOutputPredictor>,
                  "starting predictors require an implicit tableau");

    template <typename State, typename First, typename Second>
    struct StageData {
        std::array<State, STAGES> states{};
        std::array<First, STAGES> first{};
        std::array<Second, STAGES> second{};
        // Unsampled columns retain zero derivatives.
        std::array<bool, STAGES> first_evaluated{};
        std::array<bool, STAGES> second_evaluated{};
    };

    template <typename State, typename System>
    using Stages = StageData<State,
                             typename System::first_derivative_type,
                             typename System::second_derivative_type>;
    template <typename State, typename System>
    using ExplicitStages = Stages<State, System>;

    // One cache per trajectory; reset after state/system/tableau changes.
    // Derivative types determine compatibility across sampling adapters.
    template <typename State, typename First, typename Second>
    class StepCacheData {
       public:
        ZQ_ODE_HOST_DEVICE void reset() {
            valid_ = false;
            history_valid_ = false;
            if constexpr (predictor_type::HISTORY > 0) {
                history_.count = 0;
                history_.next = 0;
            }
            if constexpr (predictor_type::EXTRA_STAGES == 3)
                endpoint_.valid = false;
        }

       private:
        friend class PartitionedIntegrator;
        State initial_state_{};
        std::array<First, STAGES> first_{};
        std::array<Second, STAGES> second_{};
        std::array<bool, STAGES> first_available_{};
        std::array<bool, STAGES> second_available_{};
        val_type time_step_ = 0;
        val_type previous_time_step_ = 0;
        bool valid_ = false;
        bool history_valid_ = false;
        [[no_unique_address]] std::conditional_t<
            predictor_type::EXTRA_STAGES == 3,
            detail::PartitionedEndpoint<First, Second>,
            detail::EmptyPredictorData> endpoint_{};
        [[no_unique_address]] std::conditional_t<
            (predictor_type::HISTORY > 0),
            detail::PartitionedHistory<First,
                                       Second,
                                       STAGES,
                                       predictor_type::HISTORY>,
            detail::EmptyPredictorData> history_{};
    };

    template <typename State, typename System>
    using StepCache = StepCacheData<State,
                                    typename System::first_derivative_type,
                                    typename System::second_derivative_type>;

    ZQ_ODE_HOST_DEVICE explicit PartitionedIntegrator(tableau_type tableau = {},
                                                      predictor_type = {})
        : tableau_(tableau),
          additional_(additional_coefficients(tableau_, val_type{1})) {}

    template <typename State, typename System>
    ZQ_ODE_HOST_DEVICE bool step(const State& initial,
                                 val_type h,
                                 const System& system,
                                 State& next) const
        requires(!IMPLICIT || SEPARABLE_SYSTEM<System> ||
                 (DIAGONALLY_IMPLICIT && HAS_STAGE_SOLVER<State, System>))
    {
        Stages<State, System> stages;
        return step(initial, h, system, next, stages);
    }

    template <typename State, typename System>
    ZQ_ODE_HOST_DEVICE bool step(const State& initial,
                                 val_type h,
                                 const System& system,
                                 State& next,
                                 Stages<State, System>& stages) const
        requires(!IMPLICIT || SEPARABLE_SYSTEM<System> ||
                 (DIAGONALLY_IMPLICIT && HAS_STAGE_SOLVER<State, System>))
    {
        // The zero norm below must never reach the iterative fallback.
        if constexpr (IMPLICIT && HAS_STAGE_SOLVER<State, System>) {
            if (!lower_triangular()) return false;
        } else if (!explicit_stages<System>()) {
            return false;
        }
        return step(initial, h, system, next, stages,
                    [](const State&, const State&) { return val_type{0}; });
    }

    template <typename State, typename System>
    ZQ_ODE_HOST_DEVICE bool step(const State& initial,
                                 val_type h,
                                 const System& system,
                                 State& next,
                                 StepCache<State, System>& cache) const
        requires(!IMPLICIT || SEPARABLE_SYSTEM<System> ||
                 (DIAGONALLY_IMPLICIT && HAS_STAGE_SOLVER<State, System>))
    {
        Stages<State, System> stages;
        return step(initial, h, system, next, cache, stages);
    }

    template <typename State, typename System>
    ZQ_ODE_HOST_DEVICE bool step(const State& initial,
                                 val_type h,
                                 const System& system,
                                 State& next,
                                 StepCache<State, System>& cache,
                                 Stages<State, System>& stages) const
        requires(!IMPLICIT || SEPARABLE_SYSTEM<System> ||
                 (DIAGONALLY_IMPLICIT && HAS_STAGE_SOLVER<State, System>))
    {
        if constexpr (IMPLICIT && HAS_STAGE_SOLVER<State, System>) {
            if (!lower_triangular()) return false;
        } else if (!explicit_stages<System>())
            return false;
        return step(initial, h, system, next, cache, stages,
                    [](const State&, const State&) { return val_type{0}; });
    }

    template <typename State,
              typename System,
              typename ErrorNorm,
              typename IterationOptions = FixedPointOptions<val_type>>
    ZQ_ODE_HOST_DEVICE bool step(const State& initial,
                                 val_type h,
                                 const System& system,
                                 State& next,
                                 const ErrorNorm& error_norm,
                                 IterationOptions options = {}) const
        requires detail::ImplicitSolveOptions<IterationOptions, val_type>
    {
        Stages<State, System> stages;
        return step(initial, h, system, next, stages, error_norm, options);
    }

    template <typename State,
              typename System,
              typename ErrorNorm,
              typename IterationOptions = FixedPointOptions<val_type>>
    ZQ_ODE_HOST_DEVICE bool step(const State& initial,
                                 val_type h,
                                 const System& system,
                                 State& next,
                                 StepCache<State, System>& cache,
                                 const ErrorNorm& error_norm,
                                 IterationOptions options = {}) const
        requires detail::ImplicitSolveOptions<IterationOptions, val_type>
    {
        Stages<State, System> stages;
        return step(initial, h, system, next, cache, stages, error_norm,
                    options);
    }

    // Only a successful solve publishes next and stages. A successful trial
    // may still be rejected by the outer driver; it must dispatch root events
    // only after accepting its deterministic segment.
    template <typename State,
              typename System,
              typename ErrorNorm,
              typename IterationOptions = FixedPointOptions<val_type>>
    ZQ_ODE_HOST_DEVICE bool step(const State& initial,
                                 val_type h,
                                 const System& system,
                                 State& next,
                                 Stages<State, System>& stages,
                                 const ErrorNorm& error_norm,
                                 IterationOptions options = {}) const
        requires detail::ImplicitSolveOptions<IterationOptions, val_type>
    {
        StepCache<State, System> cache;
        return step(initial, h, system, next, cache, stages, error_norm,
                    options);
    }

    template <typename State,
              typename System,
              typename ErrorNorm,
              typename IterationOptions = FixedPointOptions<val_type>>
    ZQ_ODE_HOST_DEVICE bool step(const State& initial,
                                 val_type h,
                                 const System& system,
                                 State& next,
                                 StepCache<State, System>& cache,
                                 Stages<State, System>& stages,
                                 const ErrorNorm& error_norm,
                                 IterationOptions options = {}) const
        requires detail::ImplicitSolveOptions<IterationOptions, val_type>
    {
        if (!finite(h) || !detail::valid_iteration_options<val_type>(options))
            return false;
        Stages<State, System> trial;
        std::conditional_t<predictor_type::EXTRA_STAGES == 3,
                           detail::PartitionedEndpoint<
                               typename System::first_derivative_type,
                               typename System::second_derivative_type>,
                           detail::EmptyPredictorData>
            endpoint{};
        for (std::size_t i = 0; i < STAGES; ++i) {
            trial.first_evaluated[i] = tableau_.b_first[i] != 0;
            trial.second_evaluated[i] = tableau_.b_second[i] != 0;
            for (std::size_t j = 0; j < STAGES; ++j) {
                trial.first_evaluated[i] |= tableau_.A_first[j][i] != 0;
                trial.second_evaluated[i] |= tableau_.A_second[j][i] != 0;
            }
            if constexpr (requires { tableau_type::SAMPLE_ALL_STAGES; }) {
                if constexpr (tableau_type::SAMPLE_ALL_STAGES) {
                    trial.first_evaluated[i] = true;
                    trial.second_evaluated[i] = true;
                }
            }
        }
        const auto evaluate_first = [&](std::size_t i) {
            return (!trial.first_evaluated[i] ||
                    system.evaluate_first(trial.states[i], trial.first[i]));
        };
        const auto evaluate_second = [&](std::size_t i) {
            return (!trial.second_evaluated[i] ||
                    system.evaluate_second(trial.states[i], trial.second[i]));
        };
        const auto evaluate = [&](std::size_t i) {
            return evaluate_first(i) && evaluate_second(i);
        };
        const auto advance = [&](std::size_t i, std::size_t count) {
            return system.advance(initial, trial.first, trial.second,
                                  tableau_.A_first[i], tableau_.A_second[i],
                                  count, h, tableau_.c_first[i],
                                  tableau_.c_second[i]);
        };
        bool solved = false;
        if constexpr (IMPLICIT && HAS_STAGE_SOLVER<State, System>) {
            if (lower_triangular()) {
                for (std::size_t i = 0; i < STAGES; ++i) {
                    trial.states[i] = advance(i, i);
                    if (!system.solve_stage(trial.states[i],
                                            h * tableau_.A_first[i][i],
                                            h * tableau_.A_second[i][i], i) ||
                        !evaluate(i))
                        return false;
                }
                solved = true;
            }
        }
        if (!solved && explicit_stages<System>()) {
            for (std::size_t i = 0; i < STAGES; ++i) {
                trial.states[i] = advance(i, i);
                if constexpr (SEPARABLE_SYSTEM<System>) {
                    if (tableau_.A_first[i][i] != 0) {
                        // The second partition is complete, so its value
                        // determines the first derivative before its update.
                        if (!evaluate_first(i)) return false;
                        trial.states[i] = advance(i, i + 1);
                        if (!evaluate_second(i)) return false;
                        continue;
                    }
                    if (tableau_.A_second[i][i] != 0) {
                        if (!evaluate_second(i)) return false;
                        trial.states[i] = advance(i, i + 1);
                        if (!evaluate_first(i)) return false;
                        continue;
                    }
                }
                if (!evaluate(i)) return false;
            }
            solved = true;
        }
        if (!solved) {
            if constexpr (!IMPLICIT) {
                return false;
            } else {
                if (!starting_stages(initial, h, system, cache, trial,
                                     endpoint))
                    return false;
                if constexpr (std::same_as<IterationOptions,
                                           NewtonOptions<val_type>>) {
                    solved = newton_stages(initial, h, system, trial,
                                           error_norm, options);
                } else {
                    for (std::size_t iteration = 0;
                         iteration < options.max_iterations; ++iteration) {
                        for (std::size_t i = 0; i < STAGES; ++i)
                            if (!evaluate(i)) return false;
                        bool converged = true;
                        for (std::size_t i = 0; i < STAGES; ++i) {
                            const State updated = advance(i, STAGES);
                            const val_type error =
                                error_norm(trial.states[i], updated);
                            if (!finite(error) || error < 0) return false;
                            converged &= error <= options.tolerance;
                            trial.states[i] = updated;
                        }
                        if (converged) {
                            solved = true;
                            break;
                        }
                    }
                }
            }
        }
        if (!solved) return false;
        // Stiffly accurate pairs already hold the final state and its metadata.
        const State result =
            stiffly_accurate()
                ? trial.states.back()
                : system.advance(initial, trial.first, trial.second,
                                 tableau_.b_first, tableau_.b_second, STAGES, h,
                                 val_type{1}, val_type{1});
        const val_type error = error_norm(result, result);
        if (!finite(error) || error < 0) return false;
        update_cache<State, System>(initial, h, trial, endpoint, cache);
        stages = trial;
        next = result;
        return true;
    }

    // Step doubling accepts the two-half-step state and returns a NEXT-step
    // proposal. Its cache contains the accepted second half and its history.
    template <typename State,
              typename System,
              typename ErrorNorm,
              typename IterationOptions = FixedPointOptions<val_type>>
    ZQ_ODE_HOST_DEVICE bool step_adaptive(State& state,
                                          val_type& time_step,
                                          val_type maximum_time_step,
                                          val_type upper_error_bound,
                                          val_type lower_error_bound,
                                          const System& system,
                                          const ErrorNorm& error_norm,
                                          IterationOptions options = {}) const
        requires detail::ImplicitSolveOptions<IterationOptions, val_type>
    {
        StepCache<State, System> cache;
        return step_adaptive(state, time_step, maximum_time_step,
                             upper_error_bound, lower_error_bound, system,
                             cache, error_norm, options);
    }

    template <typename State,
              typename System,
              typename ErrorNorm,
              typename IterationOptions = FixedPointOptions<val_type>>
    ZQ_ODE_HOST_DEVICE bool step_adaptive(State& state,
                                          val_type& time_step,
                                          val_type maximum_time_step,
                                          val_type upper_error_bound,
                                          val_type lower_error_bound,
                                          const System& system,
                                          StepCache<State, System>& cache,
                                          const ErrorNorm& error_norm,
                                          IterationOptions options = {}) const
        requires detail::ImplicitSolveOptions<IterationOptions, val_type>
    {
        if (!detail::valid_iteration_options<val_type>(options)) return false;
        const State initial = state;
        State fine{};
        auto fine_cache = cache;
        const auto trial = [&](val_type h, val_type half, val_type& distance) {
            auto full_cache = cache;
            fine_cache = cache;
            State full{}, first_half{};
            if (!step(initial, h, system, full, full_cache, error_norm,
                      options) ||
                !step(initial, half, system, first_half, fine_cache, error_norm,
                      options) ||
                !step(first_half, half, system, fine, fine_cache, error_norm,
                      options))
                return false;
            distance = error_norm(full, fine);
            return true;
        };
        if (!detail::step_doubling<ORDER>(time_step, maximum_time_step,
                                          upper_error_bound, lower_error_bound,
                                          trial))
            return false;
        state = fine;
        cache = fine_cache;
        return true;
    }

    // Stoffer selection returns the CURRENT accepted step. Norms must have
    // fixed scales; defect_norm must be even and translation invariant.
    // Systems must support repeatable trials and any declared endpoint RHS.
    template <typename State,
              typename System,
              typename ErrorNorm,
              typename DefectNorm,
              typename IterationOptions = FixedPointOptions<val_type>>
    ZQ_ODE_HOST_DEVICE bool step_adaptive(
        const State& initial,
        val_type& time_step,
        val_type maximum_time_step,
        const System& system,
        State& next,
        StepCache<State, System>& cache,
        const ErrorNorm& error_norm,
        const DefectNorm& defect_norm,
        SymmetricStepOptions<val_type> options = {},
        IterationOptions iteration_options = {}) const
        requires(IMPLICIT &&
                 detail::ImplicitSolveOptions<IterationOptions, val_type> &&
                 requires {
                     tableau_type::SYMMETRIC;
                     tableau_type::ERROR_ORDER;
                 } &&
                 (detail::HAS_PARTITIONED_STAGE_DEFECT<tableau_type> ||
                  detail::HAS_PARTITIONED_ENDPOINT_DEFECT<tableau_type>))
    {
        static_assert(tableau_type::SYMMETRIC && tableau_type::ERROR_ORDER > 0);
        if (!detail::valid_iteration_options<val_type>(iteration_options))
            return false;
        auto trial_cache = cache;
        State trial_state{};
        Stages<State, System> stages;
        const auto trial = [&](val_type h, val_type& error) {
            trial_cache = cache;
            bool valid_norm = true;
            const auto checked_norm = [&](const State& a, const State& b) {
                const val_type value = error_norm(a, b);
                valid_norm &= finite(value) && value >= 0;
                return value;
            };
            const bool solved =
                step(initial, h, system, trial_state, trial_cache, stages,
                     checked_norm, iteration_options);
            if (!valid_norm) return detail::AdaptiveTrial::INVALID_ERROR;
            if (!solved) return detail::AdaptiveTrial::SOLVE_FAILED;
            return symmetric_defect(initial, trial_state, h, system, stages,
                                    defect_norm, error);
        };
        if (!detail::symmetric_step<tableau_type::ERROR_ORDER>(
                time_step, maximum_time_step, cache.time_step_,
                cache.previous_time_step_, cache.valid_, cache.history_valid_,
                options, trial))
            return false;
        next = trial_state;
        cache = trial_cache;
        return true;
    }

   private:
    using additional_type =
        std::conditional_t<predictor_type::EXTRA_STAGES == 0,
                           detail::EmptyPredictorData,
                           std::array<AdditionalPredictorCoefficients<
                                          val_type,
                                          STAGES,
                                          predictor_type::EXTRA_STAGES>,
                                      2>>;
    const tableau_type tableau_;
    [[no_unique_address]] const additional_type additional_;

    ZQ_ODE_HOST_DEVICE static additional_type additional_coefficients(
        const tableau_type& tableau,
        val_type ratio) {
        if constexpr (predictor_type::EXTRA_STAGES == 0)
            return {};
        else if constexpr (detail::HasPartitionedAdditionalPredictor<
                               tableau_type, predictor_type::EXTRA_STAGES>) {
            return {tableau.template additional_predictor_first<
                        predictor_type::EXTRA_STAGES>(ratio),
                    tableau.template additional_predictor_second<
                        predictor_type::EXTRA_STAGES>(ratio)};
        } else {
            static_assert(predictor_type::EXTRA_STAGES == 0,
                          "partitioned tableau must declare valid additional "
                          "predictor coefficients for both partitions");
            return {};
        }
    }

    template <typename State, typename System, typename Endpoint>
    ZQ_ODE_HOST_DEVICE void update_cache(
        const State& initial,
        val_type h,
        const Stages<State, System>& stages,
        const Endpoint& endpoint,
        StepCache<State, System>& cache) const {
        if constexpr (predictor_type::HISTORY > 0) {
            auto& history = cache.history_;
            if (h == 0) {
                history.count = 0;
                history.next = 0;
            } else if (cache.valid_ && cache.time_step_ == h &&
                       history.count > 0) {
                if constexpr (predictor_type::HISTORY > 1) {
                    history.first[history.next] = cache.first_;
                    history.second[history.next] = cache.second_;
                    history.next =
                        (history.next + 1) % (predictor_type::HISTORY - 1);
                }
                if (history.count < predictor_type::HISTORY) ++history.count;
            } else {
                history.count = 1;
                history.next = 0;
            }
        }
        if constexpr (predictor_type::EXTRA_STAGES == 3)
            cache.endpoint_ = endpoint;
        cache.initial_state_ = initial;
        cache.first_ = stages.first;
        cache.second_ = stages.second;
        cache.first_available_ = stages.first_evaluated;
        cache.second_available_ = stages.second_evaluated;
        cache.previous_time_step_ = cache.time_step_;
        cache.history_valid_ = cache.valid_;
        cache.time_step_ = h;
        cache.valid_ = true;
    }

    template <typename State, typename System>
    ZQ_ODE_HOST_DEVICE State
    add_derivatives(const State& state,
                    const typename System::first_derivative_type& first,
                    const typename System::second_derivative_type& second,
                    val_type first_weight,
                    val_type second_weight,
                    val_type h,
                    const System& system) const {
        std::array<typename System::first_derivative_type, STAGES> f{};
        std::array<typename System::second_derivative_type, STAGES> g{};
        row_type a{}, b{};
        f[0] = first;
        g[0] = second;
        a[0] = first_weight;
        b[0] = second_weight;
        return system.advance(state, f, g, a, b, 1, h, val_type{0},
                              val_type{0});
    }

    template <typename State, typename System, typename DefectNorm>
    ZQ_ODE_HOST_DEVICE detail::AdaptiveTrial symmetric_defect(
        const State& initial,
        const State& final,
        val_type h,
        const System& system,
        Stages<State, System>& stages,
        const DefectNorm& norm,
        val_type& error) const {
        const State origin{};
        State defect = origin;
        if constexpr (detail::HAS_PARTITIONED_STAGE_DEFECT<tableau_type>) {
            for (std::size_t i = 0; i < STAGES; ++i) {
                if (tableau_.error_weights_first[i] != 0 &&
                    !stages.first_evaluated[i] &&
                    !system.evaluate_first(stages.states[i], stages.first[i]))
                    return detail::AdaptiveTrial::SOLVE_FAILED;
                if (tableau_.error_weights_second[i] != 0 &&
                    !stages.second_evaluated[i] &&
                    !system.evaluate_second(stages.states[i], stages.second[i]))
                    return detail::AdaptiveTrial::SOLVE_FAILED;
            }
            defect = system.advance(origin, stages.first, stages.second,
                                    tableau_.error_weights_first,
                                    tableau_.error_weights_second, STAGES, h,
                                    val_type{0}, val_type{0});
        }
        if constexpr (detail::HAS_PARTITIONED_ENDPOINT_DEFECT<tableau_type>) {
            for (std::size_t i = 0; i < 2; ++i) {
                typename System::first_derivative_type first{};
                typename System::second_derivative_type second{};
                const auto& state = i == 0 ? initial : final;
                const auto a = tableau_.endpoint_error_weights_first[i];
                const auto b = tableau_.endpoint_error_weights_second[i];
                if ((a != 0 && !system.evaluate_first(state, first)) ||
                    (b != 0 && !system.evaluate_second(state, second)))
                    return detail::AdaptiveTrial::SOLVE_FAILED;
                defect =
                    add_derivatives(defect, first, second, a, b, h, system);
            }
        }
        error = norm(origin, defect);
        return detail::AdaptiveTrial::SOLVED;
    }

    template <typename State, typename System, typename Endpoint>
    ZQ_ODE_HOST_DEVICE bool starting_stages(
        const State& initial,
        val_type h,
        const System& system,
        const StepCache<State, System>& cache,
        Stages<State, System>& trial,
        Endpoint& endpoint) const {
        const auto clock = [&](const State& state, std::size_t i) {
            return system.advance(state, trial.first, trial.second,
                                  tableau_.A_first[i], tableau_.A_second[i], 0,
                                  h, tableau_.c_first[i], tableau_.c_second[i]);
        };
        if constexpr (predictor_type::HISTORY > 0) {
            if (cache.valid_ && cache.time_step_ == h && h != 0 &&
                cache.history_.count > 0) {
                const auto& history = cache.history_;
                for (std::size_t i = 0; i < STAGES; ++i) {
                    State stage = initial;
                    val_type binomial = 1;
                    for (std::size_t j = 0; j < history.count; ++j) {
                        binomial *= -static_cast<val_type>(history.count - j) /
                                    static_cast<val_type>(j + 1);
                        if (!finite(binomial)) return false;
                        auto a = tableau_.A_first[i];
                        auto b = tableau_.A_second[i];
                        for (auto& weight : a) weight *= -binomial;
                        for (auto& weight : b) weight *= -binomial;
                        if (j == 0) {
                            stage = system.advance(stage, cache.first_,
                                                   cache.second_, a, b, STAGES,
                                                   h, val_type{0}, val_type{0});
                        } else if constexpr (predictor_type::HISTORY > 1) {
                            constexpr auto CAPACITY =
                                predictor_type::HISTORY - 1;
                            const auto index =
                                (history.next + CAPACITY - j) % CAPACITY;
                            stage = system.advance(stage, history.first[index],
                                                   history.second[index], a, b,
                                                   STAGES, h, val_type{0},
                                                   val_type{0});
                        }
                    }
                    trial.states[i] = clock(stage, i);
                }
                return true;
            }
        }
        if constexpr (predictor_type::EXTRA_STAGES > 0) {
            constexpr auto M = predictor_type::EXTRA_STAGES;
            // Additional-stage formulas may use columns absent from A/b.
            // Only iterative execution reaches this predictor.
            trial.first_evaluated.fill(true);
            trial.second_evaluated.fill(true);
            if constexpr (M == 3) {
                if (!system.evaluate_first(initial, endpoint.first) ||
                    !system.evaluate_second(initial, endpoint.second))
                    return false;
                endpoint.valid = true;
            }
            bool warm = cache.valid_ && cache.time_step_ != 0;
            if constexpr (M == 3) warm &= cache.endpoint_.valid;
            for (std::size_t j = 0; j < STAGES; ++j)
                warm &= cache.first_available_[j] && cache.second_available_[j];
            if (!warm) {
                row_type a{}, b{};
                if constexpr (M == 3) {
                    trial.first[0] = endpoint.first;
                    trial.second[0] = endpoint.second;
                }
                for (std::size_t i = 0; i < STAGES; ++i) {
                    a[0] = tableau_.c_first[i];
                    b[0] = tableau_.c_second[i];
                    trial.states[i] =
                        system.advance(initial, trial.first, trial.second, a, b,
                                       M == 3 ? 1 : 0, h, tableau_.c_first[i],
                                       tableau_.c_second[i]);
                }
                return true;
            }
            const val_type ratio = h / cache.time_step_;
            if (!finite(ratio)) return false;
            const auto coefficients =
                ratio == 1 ? additional_
                           : additional_coefficients(tableau_, ratio);
            for (const auto& weights : coefficients) {
                for (val_type c : weights.c)
                    if (!finite(c)) return false;
                for (const auto& row : weights.mu)
                    for (val_type value : row)
                        if (!finite(value)) return false;
                for (const auto& row : weights.alpha)
                    for (val_type value : row)
                        if (!finite(value)) return false;
                for (const auto& row : weights.nu)
                    for (val_type value : row)
                        if (!finite(value)) return false;
            }
            const auto& f = coefficients[0];
            const auto& g = coefficients[1];
            std::array<typename System::first_derivative_type, M> first{};
            std::array<typename System::second_derivative_type, M> second{};
            for (std::size_t j = 0; j < M; ++j) {
                if constexpr (M == 3) {
                    if (j == 0) {
                        first[j] = cache.endpoint_.first;
                        second[j] = cache.endpoint_.second;
                        continue;
                    }
                    if (j == 1) {
                        first[j] = endpoint.first;
                        second[j] = endpoint.second;
                        continue;
                    }
                }
                row_type a{}, b{};
                for (std::size_t k = 0; k < STAGES; ++k) {
                    a[k] = f.mu[j][k];
                    b[k] = g.mu[j][k];
                }
                State sample = system.advance(
                    cache.initial_state_, cache.first_, cache.second_, a, b,
                    STAGES, cache.time_step_, f.c[j], g.c[j]);
                for (std::size_t k = 0; k < j; ++k)
                    if (f.mu[j][STAGES + k] != 0 || g.mu[j][STAGES + k] != 0)
                        sample = add_derivatives(
                            sample, first[k], second[k], f.mu[j][STAGES + k],
                            g.mu[j][STAGES + k], cache.time_step_, system);
                if (!system.evaluate_first(sample, first[j]) ||
                    !system.evaluate_second(sample, second[j]))
                    return false;
            }
            for (std::size_t i = 0; i < STAGES; ++i) {
                State stage =
                    system.advance(initial, cache.first_, cache.second_,
                                   f.alpha[i], g.alpha[i], STAGES,
                                   cache.time_step_, val_type{0}, val_type{0});
                for (std::size_t j = 0; j < M; ++j)
                    if (f.nu[i][j] != 0 || g.nu[i][j] != 0)
                        stage = add_derivatives(stage, first[j], second[j],
                                                f.nu[i][j], g.nu[i][j],
                                                cache.time_step_, system);
                trial.states[i] = clock(stage, i);
            }
            return true;
        }
        bool predict = false;
        matrix_type first{}, second{};
        if (cache.valid_ && cache.time_step_ != 0) {
            const val_type ratio = h / cache.time_step_;
            if constexpr (detail::HasPartitionedStagePredictor<tableau_type>) {
                if (finite(ratio)) {
                    first = tableau_.predictor_first(ratio);
                    second = tableau_.predictor_second(ratio);
                    predict = true;
                }
            } else if constexpr (detail::HAS_PARTITIONED_BETA<tableau_type>) {
                if (h == cache.time_step_) {
                    first = tableau_.beta_first;
                    second = tableau_.beta_second;
                    predict = true;
                }
            }
        }
        // Custom predictors may refer to columns unused by the step itself.
        // A cold guess is safer than substituting missing cached derivatives.
        for (std::size_t i = 0; i < STAGES && predict; ++i)
            for (std::size_t j = 0; j < STAGES; ++j)
                if ((first[i][j] != 0 && !cache.first_available_[j]) ||
                    (second[i][j] != 0 && !cache.second_available_[j]))
                    predict = false;
        for (std::size_t i = 0; i < STAGES; ++i)
            trial.states[i] =
                predict ? system.advance(cache.initial_state_, cache.first_,
                                         cache.second_, first[i], second[i],
                                         STAGES, cache.time_step_,
                                         val_type{1} + tableau_.c_first[i] * h /
                                                           cache.time_step_,
                                         val_type{1} + tableau_.c_second[i] *
                                                           h / cache.time_step_)
                        : clock(initial, i);
        return true;
    }

    template <typename State, typename System, typename ErrorNorm>
    ZQ_ODE_HOST_DEVICE bool newton_stages(
        const State& initial,
        val_type h,
        const System& system,
        Stages<State, System>& trial,
        const ErrorNorm& error_norm,
        NewtonOptions<val_type> options) const {
        // The direct/separable paths need no component map. Guard this
        // runtime fallback so NewtonOptions also works for those systems.
        if constexpr (detail::NEWTON_MAPPABLE<
                          val_type, System,
                          typename System::first_derivative_type> &&
                      detail::NEWTON_MAPPABLE<
                          val_type, System,
                          typename System::second_derivative_type>) {
            using FirstComponents =
                decltype(detail::newton_components<val_type>(system,
                                                             trial.first[0]));
            using SecondComponents =
                decltype(detail::newton_components<val_type>(system,
                                                             trial.second[0]));
            constexpr auto FIRST_SIZE =
                STAGES * std::tuple_size_v<FirstComponents>;
            constexpr auto SECOND_SIZE =
                STAGES * std::tuple_size_v<SecondComponents>;
            using Vector = std::array<val_type, FIRST_SIZE + SECOND_SIZE>;
            const auto evaluate = [&] {
                for (std::size_t i = 0; i < STAGES; ++i) {
                    if (trial.first_evaluated[i] &&
                        !system.evaluate_first(trial.states[i], trial.first[i]))
                        return false;
                    if (trial.second_evaluated[i] &&
                        !system.evaluate_second(trial.states[i],
                                                trial.second[i]))
                        return false;
                }
                return true;
            };
            const auto advance = [&](std::size_t i) {
                return system.advance(initial, trial.first, trial.second,
                                      tableau_.A_first[i], tableau_.A_second[i],
                                      STAGES, h, tableau_.c_first[i],
                                      tableau_.c_second[i]);
            };
            if (!evaluate()) return false;
            Vector values{};
            detail::newton_pack(system, trial.first, values);
            detail::newton_pack<FIRST_SIZE>(system, trial.second, values);
            const auto residual = [&](const Vector& x, Vector& rhs,
                                      val_type& error) {
                detail::newton_unpack(system, x, trial.first);
                detail::newton_unpack<FIRST_SIZE>(system, x, trial.second);
                for (std::size_t i = 0; i < STAGES; ++i) {
                    if (!trial.first_evaluated[i]) trial.first[i] = {};
                    if (!trial.second_evaluated[i]) trial.second[i] = {};
                }
                for (std::size_t i = 0; i < STAGES; ++i)
                    trial.states[i] = advance(i);
                if (!evaluate()) return false;
                error = 0;
                for (std::size_t i = 0; i < STAGES; ++i) {
                    const State updated = advance(i);
                    const val_type distance =
                        error_norm(trial.states[i], updated);
                    if (!finite(distance) || distance < 0) return false;
                    error = std::max(error, distance);
                    trial.states[i] = updated;
                }
                detail::newton_pack(system, trial.first, rhs);
                detail::newton_pack<FIRST_SIZE>(system, trial.second, rhs);
                return true;
            };
            return detail::newton_solve(values, residual, options);
        }
        return false;
    }

    ZQ_ODE_HOST_DEVICE static bool finite(val_type value) {
        constexpr val_type MAXIMUM = std::numeric_limits<val_type>::max();
        return value >= -MAXIMUM && value <= MAXIMUM;
    }

    ZQ_ODE_HOST_DEVICE bool lower_triangular() const {
        for (std::size_t i = 0; i < STAGES; ++i)
            for (std::size_t j = i + 1; j < STAGES; ++j)
                if (tableau_.A_first[i][j] != 0 || tableau_.A_second[i][j] != 0)
                    return false;
        return true;
    }

    template <typename System>
    ZQ_ODE_HOST_DEVICE bool explicit_stages() const {
        if (!lower_triangular()) return false;
        for (std::size_t i = 0; i < STAGES; ++i) {
            const bool first = tableau_.A_first[i][i] != 0;
            const bool second = tableau_.A_second[i][i] != 0;
            if constexpr (SEPARABLE_SYSTEM<System>) {
                if (first && second) return false;
            } else {
                if (first || second) return false;
            }
        }
        return true;
    }

    ZQ_ODE_HOST_DEVICE bool stiffly_accurate() const {
        if (tableau_.c_first.back() != 1 || tableau_.c_second.back() != 1)
            return false;
        for (std::size_t i = 0; i < STAGES; ++i)
            if (tableau_.A_first.back()[i] != tableau_.b_first[i] ||
                tableau_.A_second.back()[i] != tableau_.b_second[i])
                return false;
        return true;
    }
};

template <typename Tableau>
PartitionedIntegrator(Tableau)
    -> PartitionedIntegrator<typename Tableau::val_type, Tableau>;

template <typename Tableau, typename Predictor>
PartitionedIntegrator(Tableau, Predictor)
    -> PartitionedIntegrator<typename Tableau::val_type, Tableau, Predictor>;

// Root detection is separate from trial-stage observation. The driver
// invokes this dispatcher only after accepting a deterministic segment.
enum class EventDirection : std::uint8_t {
    ANY,
    RISING,
    FALLING,
};

enum class EventAction : std::uint8_t {
    CONTINUE,
    STOP,
};

// Preserve void observers while allowing a callback to stop conditionally.
template <typename State>
class EventCallback {
   public:
    EventCallback() = default;
    EventCallback(std::nullptr_t) {}

    template <typename Function>
        requires std::invocable<Function&, const State&> &&
                 (!std::same_as<std::remove_cvref_t<Function>,
                                EventCallback>) &&
                 (std::same_as<std::invoke_result_t<Function&, const State&>,
                               void> ||
                  std::same_as<std::invoke_result_t<Function&, const State&>,
                               EventAction>)
    EventCallback(Function&& function) {
        using result_type = std::invoke_result_t<Function&, const State&>;
        std::function<result_type(const State&)> callback(
            std::forward<Function>(function));
        if constexpr (std::same_as<result_type, EventAction>) {
            callback_ = std::move(callback);
        } else if (callback) {
            callback_ = [callback = std::move(callback)](const State& state) {
                callback(state);
                return EventAction::CONTINUE;
            };
        }
    }

    explicit operator bool() const { return static_cast<bool>(callback_); }
    EventAction operator()(const State& state) const {
        return callback_(state);
    }

   private:
    std::function<EventAction(const State&)> callback_;
};

template <typename State>
struct EventStop {
    std::size_t event_index;
    State state;
};

// Conditions must be continuous along a segment and free of side effects.
// Callbacks receive a borrowed localized state; exceptions propagate.
// Returning STOP ends the trajectory at this state; void means CONTINUE.
template <typename State,
          typename Condition = std::function<double(const State&)>,
          typename Callback = EventCallback<State>>
struct Event {
    Condition condition;
    Callback callback;
    EventDirection direction = EventDirection::ANY;
    double time_tolerance = 1e-14;      // Absolute bracket width in seconds.
    double relative_tolerance = 1e-10;  // Fraction of the accepted step.
    std::size_t max_iterations = 64;
};

// Store concrete callable types, including move-only and mutable lambdas.
template <typename State, typename Condition, typename Callback>
ZQ_ODE_HOST_DEVICE auto make_event(
    Condition&& condition,
    Callback&& callback,
    EventDirection direction = EventDirection::ANY) {
    static_assert(
        std::is_invocable_r_v<double, std::decay_t<Condition>&, const State&>);
    using result_type =
        std::invoke_result_t<std::decay_t<Callback>&, const State&>;
    static_assert(std::same_as<result_type, void> ||
                  std::same_as<result_type, EventAction>);
    return Event<State, std::decay_t<Condition>, std::decay_t<Callback>>{
        std::forward<Condition>(condition), std::forward<Callback>(callback),
        direction};
}

enum class EventStatus : std::uint8_t {
    SUCCESS,
    STOP,
    INVALID_EVENT,
    INVALID_OPTIONS,
    INVALID_DIRECTION,
    INVALID_SEGMENT,
    NONFINITE_CONDITION,
    INVALID_INTERPOLATION,
    LOCALIZATION_FAILURE,
    INVALID_ACTION,
};

template <typename State>
struct EventResult {
    EventStatus status = EventStatus::SUCCESS;
    // Engaged only for STOP. Error statuses never contain a partial stop.
    std::optional<EventStop<State>> stop;
};

namespace detail {

ZQ_ODE_HOST_DEVICE inline bool event_finite(double value) {
    constexpr double MAXIMUM = std::numeric_limits<double>::max();
    return value >= -MAXIMUM && value <= MAXIMUM;
}

template <typename EventType>
ZQ_ODE_HOST_DEVICE EventStatus validate_event(const EventType& event) {
    if constexpr (std::is_pointer_v<decltype(event.condition)> ||
                  requires { event.condition.operator bool(); })
        if (!static_cast<bool>(event.condition))
            return EventStatus::INVALID_EVENT;
    if constexpr (std::is_pointer_v<decltype(event.callback)> ||
                  requires { event.callback.operator bool(); })
        if (!static_cast<bool>(event.callback))
            return EventStatus::INVALID_EVENT;
    if (!event_finite(event.time_tolerance) || event.time_tolerance <= 0 ||
        !event_finite(event.relative_tolerance) ||
        event.relative_tolerance < 0 || event.relative_tolerance >= 1 ||
        event.max_iterations == 0)
        return EventStatus::INVALID_OPTIONS;
    if (event.direction != EventDirection::ANY &&
        event.direction != EventDirection::RISING &&
        event.direction != EventDirection::FALLING)
        return EventStatus::INVALID_DIRECTION;
    return EventStatus::SUCCESS;
}

template <typename State, typename EventType, typename Interpolate>
ZQ_ODE_HOST_DEVICE EventStatus locate_event(EventType& event,
                                            const State& previous,
                                            const State& next,
                                            Interpolate& interpolate,
                                            std::optional<State>& root) {
    const double first = event.condition(previous);
    if (!event_finite(first)) return EventStatus::NONFINITE_CONDITION;
    const double last = event.condition(next);
    if (!event_finite(last)) return EventStatus::NONFINITE_CONDITION;
    const bool rising = first < 0 && last >= 0;
    const bool falling = first > 0 && last <= 0;
    if ((!rising && !falling) ||
        (rising && event.direction == EventDirection::FALLING) ||
        (falling && event.direction == EventDirection::RISING))
        return EventStatus::SUCCESS;
    if (last == 0) {
        root = next;
        return EventStatus::SUCCESS;
    }
    const double duration = next.time - previous.time;
    const double tolerance =
        std::max(event.time_tolerance, event.relative_tolerance * duration);
    double lower = 0, upper = 1;
    for (std::size_t i = 0; i < event.max_iterations; ++i) {
        const double fraction = std::midpoint(lower, upper);
        State state = previous;
        if constexpr (requires {
                          {
                              interpolate(fraction, state)
                          } -> std::same_as<bool>;
                      }) {
            if (!interpolate(fraction, state))
                return EventStatus::INVALID_INTERPOLATION;
        } else {
            state = interpolate(fraction);
        }
        if (!event_finite(state.time) || state.time < previous.time ||
            state.time > next.time)
            return EventStatus::INVALID_INTERPOLATION;
        const double value = event.condition(state);
        if (!event_finite(value)) return EventStatus::NONFINITE_CONDITION;
        if (value == 0 || (upper - lower) * duration <= tolerance) {
            root = std::move(state);
            return EventStatus::SUCCESS;
        }
        if ((value < 0) == (first < 0))
            lower = fraction;
        else
            upper = fraction;
    }
    return EventStatus::LOCALIZATION_FAILURE;
}

template <typename State>
struct EventOccurrence {
    std::size_t index;
    State state;
};

// Visit supplies the concrete event type for a runtime registration index.
// Scratch holds at most one occurrence per event and is reused each segment.
template <typename State, typename Visit, typename Interpolate>
ZQ_ODE_HOST_DEVICE EventResult<State> dispatch_events(
    std::size_t event_count,
    Visit visit,
    std::span<std::optional<EventOccurrence<State>>> scratch,
    const State& previous,
    const State& next,
    Interpolate& interpolate) {
    if (event_count == 0) return {};
    for (std::size_t i = 0; i < event_count; ++i) {
        const auto status =
            visit(i, [](auto& event) { return validate_event(event); });
        if (status != EventStatus::SUCCESS) return {status, std::nullopt};
    }
    const double duration = next.time - previous.time;
    if (!event_finite(previous.time) || !event_finite(next.time) ||
        !event_finite(duration) || duration <= 0)
        return {EventStatus::INVALID_SEGMENT, std::nullopt};

    for (auto& occurrence : scratch) occurrence.reset();
    std::size_t count = 0;
    for (std::size_t i = 0; i < event_count; ++i) {
        std::optional<State> root;
        const auto status = visit(i, [&](auto& event) {
            return locate_event(event, previous, next, interpolate, root);
        });
        if (status != EventStatus::SUCCESS) return {status, std::nullopt};
        if (!root) continue;
        EventOccurrence<State> occurrence{i, std::move(*root)};
        std::size_t j = count++;
        // Stable insertion keeps registration order for equal-time roots.
        while (j > 0 && occurrence.state.time < scratch[j - 1]->state.time) {
            scratch[j] = std::move(scratch[j - 1]);
            --j;
        }
        scratch[j] = std::move(occurrence);
    }

    // Locate every root before callbacks, including roots after an early STOP.
    for (std::size_t i = 0; i < count; ++i) {
        const auto& occurrence = *scratch[i];
        const auto status = visit(occurrence.index, [&](auto& event) {
            using result_type =
                std::invoke_result_t<decltype((event.callback)), const State&>;
            if constexpr (std::same_as<result_type, void>) {
                event.callback(occurrence.state);
                return EventStatus::SUCCESS;
            } else {
                static_assert(std::same_as<result_type, EventAction>);
                const auto action = event.callback(occurrence.state);
                if (action == EventAction::STOP) return EventStatus::STOP;
                if (action == EventAction::CONTINUE)
                    return EventStatus::SUCCESS;
                return EventStatus::INVALID_ACTION;
            }
        });
        if (status == EventStatus::STOP)
            return {status,
                    EventStop<State>{occurrence.index, occurrence.state}};
        if (status != EventStatus::SUCCESS) return {status, std::nullopt};
    }
    return {};
}

template <std::size_t I = 0, typename Events, typename Function>
ZQ_ODE_HOST_DEVICE EventStatus visit_event(Events& events,
                                           std::size_t index,
                                           Function& function) {
    if constexpr (I < std::tuple_size_v<std::remove_cv_t<Events>>) {
        if (index == I) return function(std::get<I>(events));
        return visit_event<I + 1>(events, index, function);
    } else {
        return EventStatus::INVALID_EVENT;
    }
}

inline void check_event_status(EventStatus status) {
    switch (status) {
        case EventStatus::SUCCESS:
        case EventStatus::STOP:
            return;
        case EventStatus::INVALID_EVENT:
            throw std::invalid_argument(
                "event requires a condition and callback");
        case EventStatus::INVALID_OPTIONS:
            throw std::invalid_argument("invalid event localization options");
        case EventStatus::INVALID_DIRECTION:
            throw std::invalid_argument("invalid event direction");
        case EventStatus::INVALID_SEGMENT:
            throw std::invalid_argument(
                "event segment must have finite increasing times");
        case EventStatus::NONFINITE_CONDITION:
            throw std::runtime_error(
                "event condition must return a finite value");
        case EventStatus::INVALID_INTERPOLATION:
            throw std::runtime_error(
                "event interpolation failed or its time lies outside the "
                "segment");
        case EventStatus::LOCALIZATION_FAILURE:
            throw std::runtime_error(
                "event localization exceeded max_iterations");
        case EventStatus::INVALID_ACTION:
            throw std::runtime_error("invalid event callback action");
    }
}

}  // namespace detail

// Fixed-size host/device dispatcher for a tuple or array of typed events.
// The collection is borrowed and must outlive the dispatcher. Keep it mutable
// for mutable callables; const collections require const-invocable callables.
// Each trajectory needs its own collection, callback state, and dispatcher.
// advance() runs only on accepted segments. Interpolate returns State, or
// bool(fraction, State&) to report failure without exceptions. Device use also
// requires device-callable conditions, callbacks, and interpolation.
template <typename State, typename Events>
class StaticEventDispatcher {
   public:
    static constexpr std::size_t EVENT_COUNT =
        std::tuple_size_v<std::remove_cv_t<Events>>;

    ZQ_ODE_HOST_DEVICE explicit StaticEventDispatcher(Events& events)
        : events_(events) {}
    StaticEventDispatcher(Events&&) = delete;

    template <typename Interpolate>
    ZQ_ODE_HOST_DEVICE EventResult<State> advance(const State& previous,
                                                  const State& next,
                                                  Interpolate interpolate) {
        const auto visit = [&](std::size_t index, auto&& function) {
            return detail::visit_event(events_, index, function);
        };
        return detail::dispatch_events<State>(EVENT_COUNT, visit, occurrences_,
                                              previous, next, interpolate);
    }

   private:
    Events& events_;
    std::array<std::optional<detail::EventOccurrence<State>>, EVENT_COUNT>
        occurrences_{};
};

template <typename State, typename Events>
ZQ_ODE_HOST_DEVICE auto make_event_dispatcher(Events& events) {
    return StaticEventDispatcher<State, Events>(events);
}

template <typename State, typename Events>
void make_event_dispatcher(const Events&&) = delete;

// Compatibility host API for a runtime span of type-erased Event<State>.
// It shares localization and ordering with StaticEventDispatcher, translates
// errors to exceptions, and returns an optional stop. Both APIs exclude an
// initial endpoint root and include a final endpoint root. Sign changes only:
// tangencies and multiple roots within one segment are not resolved.
template <typename State>
class EventDispatcher {
   public:
    explicit EventDispatcher(std::span<const Event<State>> events)
        : events_(events), occurrences_(events.size()) {
#if !defined(__CUDA_ARCH__)
        // Keep host callables out of NVCC's device template instantiations.
        for (const auto& event : events_)
            detail::check_event_status(detail::validate_event(event));
#endif
    }

    // Sign changes only: tangencies and multiple roots within one step are
    // not resolved. A root at the initial endpoint is excluded; a root at
    // the final endpoint is included, preventing duplicate endpoint events.
    // All roots are located before callbacks, in time order (registration
    // order for ties). The first STOP suppresses all subsequent callbacks,
    // including equal-time callbacks registered later. Localization errors
    // never publish a partial segment, even if an earlier callback could stop.
    template <typename Interpolate>
    std::optional<EventStop<State>> advance(const State& previous,
                                            const State& next,
                                            Interpolate interpolate) {
#if !defined(__CUDA_ARCH__)
        const auto visit = [&](std::size_t index, auto&& function) {
            return function(events_[index]);
        };
        auto result = detail::dispatch_events<State>(
            events_.size(), visit, occurrences_, previous, next, interpolate);
        detail::check_event_status(result.status);
        return std::move(result.stop);
#else
        return std::nullopt;
#endif
    }

   private:
    std::span<const Event<State>> events_;
    std::vector<std::optional<detail::EventOccurrence<State>>> occurrences_;
};

}  // namespace ode

#undef ZQ_ODE_HOST_DEVICE

#endif  // ZQ_ODE
