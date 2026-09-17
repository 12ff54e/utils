#ifndef ZQ_ODE
#define ZQ_ODE

#include <algorithm>
#include <array>
#include <cmath>
#include <cstddef>
#include <limits>
#include <stdexcept>
#include <type_traits>

// C++20 Runge-Kutta integration for caller-defined states and systems.
// Fixed steps and symmetric implicit adaptivity also support CUDA device code.
#if defined(__CUDACC__)
#define ZQ_ODE_HOST_DEVICE __host__ __device__
#else
#define ZQ_ODE_HOST_DEVICE
#endif

namespace ode {

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
};

template <typename T>
struct FixedPointOptions {
    using val_type = T;

    // Absolute bound in the caller's error norm, which may itself be scaled.
    val_type tolerance = 32 * std::numeric_limits<val_type>::epsilon();
    std::size_t max_iterations = 100;
};

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

// Runge-Kutta integrator driven entirely by Tableau::A, b, and c.
// Implicit tableaus declare IMPLICIT = true and supply predictor weights beta.
// Tableaus without IMPLICIT retain the explicit interface.
// System must provide:
//   using derivative_type = ...;
//   bool evaluate(const State&, derivative_type&) const;
//   State advance(const State&, const array<derivative_type, STAGES>&,
//                 const array<T, STAGES>&, size_t active_stages,
//                 T time_step, T time_fraction) const;
// Implicit steps additionally take an error_norm(lhs, rhs) returning a finite,
// nonnegative stage-state distance (nonfinite inputs must give nonfinite
// error).
template <typename T, typename Tableau = DefaultButcherTableau<T>>
class Integrator {
   public:
    using val_type = T;
    using tableau_type = Tableau;

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

    // Keep one cache per trajectory. Reset after externally changing the state,
    // system, or tableau, including collisions. A tableau may supply
    // predictor(h_new/h_old) to reuse stages when the step size changes.
    template <typename State, typename Derivative>
    class StepCacheData {
       public:
        ZQ_ODE_HOST_DEVICE void reset() {
            valid_ = false;
            history_valid_ = false;
        }

       private:
        friend class Integrator;
        State initial_state_{};
        std::array<Derivative, STAGES> derivatives_{};
        val_type time_step_ = 0;
        val_type previous_time_step_ = 0;
        bool valid_ = false;
        bool history_valid_ = false;
    };

    // Adapters that restrict field sampling retain the same cache type.
    template <typename State, typename System>
    using StepCache = StepCacheData<State, typename System::derivative_type>;

    ZQ_ODE_HOST_DEVICE explicit Integrator(tableau_type tableau = {})
        : tableau_(tableau) {}

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

    template <typename State, typename System, typename ErrorNorm>
    ZQ_ODE_HOST_DEVICE bool step(
        const State& initial_state,
        val_type time_step,
        const System& system,
        State& next_state,
        const ErrorNorm& error_norm,
        FixedPointOptions<val_type> options = {}) const {
        StepCache<State, System> cache;
        return step(initial_state, time_step, system, next_state, cache,
                    error_norm, options);
    }

    // Jacobi fixed-point iteration on Y_i = y_n + Z_i. This form also supports
    // states with time or other metadata through System::advance. Failure
    // leaves both next_state and cache untouched; aliasing next_state with y_n
    // is valid.
    template <typename State, typename System, typename ErrorNorm>
    ZQ_ODE_HOST_DEVICE bool step(
        const State& initial_state,
        val_type time_step,
        const System& system,
        State& next_state,
        StepCache<State, System>& cache,
        const ErrorNorm& error_norm,
        FixedPointOptions<val_type> options = {}) const {
        if constexpr (!IMPLICIT) {
            return step(initial_state, time_step, system, next_state);
        } else {
            if (!valid_fixed_point_options(options) || !finite(time_step)) {
                return false;
            }
            using Derivative = typename System::derivative_type;
            std::array<Derivative, STAGES> derivatives{};
            std::array<State, STAGES> stages{};
            bool predict = cache.valid_ && cache.time_step_ != ZERO &&
                           cache.time_step_ == time_step;
            auto beta = tableau_.beta;
            if constexpr (requires { tableau_.predictor(ONE); }) {
                if (cache.valid_ && cache.time_step_ != ZERO &&
                    cache.time_step_ != time_step) {
                    const val_type ratio = time_step / cache.time_step_;
                    if (finite(ratio)) {
                        beta = tableau_.predictor(ratio);
                        predict = true;
                    }
                }
            }
            for (std::size_t i = 0; i < STAGES; ++i) {
                // Y_i^(0) = y_previous + h_old * sum_j beta_ij * f_old,j.
                stages[i] =
                    predict ? system.advance(cache.initial_state_,
                                             cache.derivatives_, beta[i],
                                             STAGES, cache.time_step_,
                                             ONE + tableau_.c[i] * time_step /
                                                       cache.time_step_)
                            : system.advance(initial_state, derivatives,
                                             tableau_.A[i], 0, time_step,
                                             tableau_.c[i]);
            }

            for (std::size_t iteration = 0; iteration < options.max_iterations;
                 ++iteration) {
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
                if (!converged) continue;

                const State result =
                    system.advance(initial_state, derivatives, tableau_.b,
                                   STAGES, time_step, ONE);
                const val_type error = error_norm(result, result);
                if (!finite(error) || error < ZERO) return false;
                cache.initial_state_ = initial_state;
                cache.derivatives_ = derivatives;
                cache.previous_time_step_ = cache.time_step_;
                cache.history_valid_ = cache.valid_;
                cache.time_step_ = time_step;
                cache.valid_ = true;
                next_state = result;
                return true;
            }
            return false;
        }
    }

    // Record a successful implicit solve without changing the ordinary step
    // path. Recorded stage states and derivatives agree to the solve tolerance.
    template <typename State, typename System, typename ErrorNorm>
    ZQ_ODE_HOST_DEVICE bool step(
        const State& initial_state,
        val_type time_step,
        const System& system,
        State& next_state,
        StepCache<State, System>& cache,
        Stages<State, System>& stages,
        const ErrorNorm& error_norm,
        FixedPointOptions<val_type> options = {}) const {
        if constexpr (!IMPLICIT) {
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
              typename DefectNorm>
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
        FixedPointOptions<val_type> fixed_point = {}) const
        requires(IMPLICIT &&
                 requires(tableau_type t) {
                     tableau_type::SYMMETRIC;
                     tableau_type::ERROR_ORDER;
                     t.error_weights;
                 })
    {
        static_assert(tableau_type::SYMMETRIC && tableau_type::ERROR_ORDER > 0);
        if (!finite(time_step) || time_step == ZERO ||
            !finite(maximum_time_step) || maximum_time_step <= ZERO ||
            !valid_symmetric_step_options(options) ||
            !valid_fixed_point_options(fixed_point)) {
            return false;
        }
        const val_type direction = time_step > ZERO ? ONE : -ONE;
        val_type h = direction * time_step;
        if (cache.valid_ && direction * cache.time_step_ > ZERO) {
            h = direction * cache.time_step_;
            if (cache.history_valid_ &&
                direction * cache.previous_time_step_ > ZERO) {
                const val_type prediction =
                    TWO * h - direction * cache.previous_time_step_;
                if (finite(prediction) && prediction > ZERO) h = prediction;
            }
        }
        h = std::min(h, maximum_time_step);
        val_type lower = ZERO;
        val_type upper = maximum_time_step;
        bool bracketed = false;
        val_type previous_h = ZERO;
        val_type previous_error = ZERO;
        const State error_origin{};

        for (std::size_t iteration = 0; iteration < options.max_iterations;
             ++iteration) {
            auto trial_cache = cache;
            State trial_state{};
            if (!step(initial_state, direction * h, system, trial_state,
                      trial_cache, error_norm, fixed_point)) {
                upper = h;
                bracketed = true;
                previous_error = ZERO;
                h = lower > ZERO ? std::sqrt(lower) * std::sqrt(upper)
                                 : HALF * upper;
                if (!finite(h) || h <= lower || h >= upper) return false;
                continue;
            }
            // Accumulate about a default state to avoid subtracting large
            // orbit coordinates when measuring a small embedded defect.
            const State defect = system.advance(
                error_origin, trial_cache.derivatives_, tableau_.error_weights,
                STAGES, direction * h, ZERO);
            const val_type error = defect_norm(error_origin, defect);
            if (!finite(error) || error < ZERO) return false;
            const val_type relative_error = error / options.tolerance;
            if ((relative_error >= ONE - options.relative_tolerance &&
                 relative_error <= ONE + options.relative_tolerance) ||
                (h == maximum_time_step && relative_error <= ONE)) {
                next_state = trial_state;
                cache = trial_cache;
                time_step = direction * h;
                return true;
            }

            if (error < options.tolerance) {
                lower = h;
            } else {
                upper = h;
                bracketed = true;
            }
            val_type candidate = TWO * h;
            if (error > ZERO) {
                val_type exponent = ONE / tableau_type::ERROR_ORDER;
                if (previous_error > ZERO && error != previous_error &&
                    h != previous_h) {
                    const val_type secant = std::log(h / previous_h) /
                                            std::log(error / previous_error);
                    if (finite(secant) && secant > ZERO) exponent = secant;
                }
                // Power-law first correction, then secants in (log h, log L).
                // Bounds safeguard guesses; they do not relax acceptance.
                const val_type correction =
                    exponent * (std::log(options.tolerance) - std::log(error));
                candidate =
                    h * std::exp(std::clamp(correction, std::log(val_type{0.2}),
                                            std::log(val_type{5})));
            }
            candidate = std::min(candidate, maximum_time_step);
            if (bracketed && (candidate <= lower || candidate >= upper)) {
                candidate = lower > ZERO ? std::sqrt(lower) * std::sqrt(upper)
                                         : HALF * upper;
            }
            if (!finite(candidate) || candidate <= ZERO || candidate == h) {
                return false;
            }
            previous_h = h;
            previous_error = error;
            h = candidate;
        }
        return false;
    }

    template <typename State, typename System, typename ErrorNorm>
    bool step_adaptive(State& state,
                       val_type& time_step,
                       val_type maximum_time_step,
                       val_type upper_error_bound,
                       val_type lower_error_bound,
                       const System& system,
                       const ErrorNorm& error_norm) const
        requires(!IMPLICIT)
    {
        validate_adaptive_options(time_step, maximum_time_step,
                                  upper_error_bound, lower_error_bound);

        constexpr val_type SAFETY_FACTOR = 0.9;
        constexpr val_type MINIMUM_FACTOR = 0.1;
        constexpr val_type MAXIMUM_FACTOR = 5;
        constexpr val_type ERROR_SCALE = integer_power(TWO, ORDER) - ONE;
        constexpr val_type ERROR_EXPONENT =
            ONE / static_cast<val_type>(ORDER + 1);

        const State initial_state = state;
        val_type attempted_time_step = std::min(time_step, maximum_time_step);
        while (true) {
            State full_step{};
            State first_half_step{};
            State second_half_step{};
            const val_type half_time_step = attempted_time_step / TWO;
            if (!step(initial_state, attempted_time_step, system, full_step) ||
                !step(initial_state, half_time_step, system, first_half_step) ||
                !step(first_half_step, half_time_step, system,
                      second_half_step)) {
                return false;
            }

            const val_type error =
                error_norm(full_step, second_half_step) / ERROR_SCALE;
            if (std::isfinite(error) && error <= upper_error_bound) {
                state = second_half_step;
                if (error < lower_error_bound) {
                    const val_type factor =
                        error == ZERO
                            ? MAXIMUM_FACTOR
                            : std::clamp(SAFETY_FACTOR *
                                             std::pow(upper_error_bound / error,
                                                      ERROR_EXPONENT),
                                         ONE, MAXIMUM_FACTOR);
                    time_step = std::min(maximum_time_step,
                                         attempted_time_step * factor);
                } else {
                    time_step = attempted_time_step;
                }
                return true;
            }

            const val_type factor =
                std::isfinite(error)
                    ? std::clamp(
                          SAFETY_FACTOR * std::pow(upper_error_bound / error,
                                                   ERROR_EXPONENT),
                          MINIMUM_FACTOR, HALF)
                    : MINIMUM_FACTOR;
            attempted_time_step *= factor;
            if (attempted_time_step <=
                std::numeric_limits<val_type>::epsilon()) {
                return false;
            }
        }
    }

   private:
    template <typename State, typename System, typename Observer>
    ZQ_ODE_HOST_DEVICE bool explicit_step(const State& initial_state,
                                          val_type time_step,
                                          const System& system,
                                          State& next_state,
                                          const Observer& observe) const {
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

    static constexpr val_type ZERO = 0;
    static constexpr val_type ONE = 1;
    static constexpr val_type TWO = 2;
    static constexpr val_type HALF = 0.5;

    ZQ_ODE_HOST_DEVICE static bool finite(val_type value) {
        constexpr val_type MAXIMUM = std::numeric_limits<val_type>::max();
        return value >= -MAXIMUM && value <= MAXIMUM;
    }

    ZQ_ODE_HOST_DEVICE static bool valid_fixed_point_options(
        FixedPointOptions<val_type> options) {
        return finite(options.tolerance) && options.tolerance > ZERO &&
               options.max_iterations > 0;
    }

    ZQ_ODE_HOST_DEVICE static bool valid_symmetric_step_options(
        SymmetricStepOptions<val_type> options) {
        return finite(options.tolerance) && options.tolerance > ZERO &&
               finite(options.relative_tolerance) &&
               options.relative_tolerance > ZERO &&
               options.relative_tolerance < ONE && options.max_iterations > 0;
    }

    static consteval val_type integer_power(val_type base,
                                            std::size_t exponent) {
        val_type result = ONE;
        for (std::size_t i = 0; i < exponent; ++i) { result *= base; }
        return result;
    }

    static void validate_adaptive_options(val_type time_step,
                                          val_type maximum_time_step,
                                          val_type upper_error_bound,
                                          val_type lower_error_bound) {
        if (!(time_step > ZERO) || !(maximum_time_step > ZERO)) {
            throw std::invalid_argument(
                "Integrator time steps must be positive");
        }
        if (!(upper_error_bound > ZERO) || lower_error_bound < ZERO ||
            lower_error_bound > upper_error_bound) {
            throw std::invalid_argument("Integrator error bounds are invalid");
        }
    }
};

template <typename Tableau>
Integrator(Tableau) -> Integrator<typename Tableau::val_type, Tableau>;

}  // namespace ode

#undef ZQ_ODE_HOST_DEVICE

#endif  // ZQ_ODE
