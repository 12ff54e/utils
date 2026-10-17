STB style header only utility library, including:

* [`misc/Timer.h`](misc/Timer.h): named timers
* [`io/JsonParser.h`](io/JsonParser.h): JSON parser
* [`io/clap.h`](io/clap.h): **C**ommand **L**ine **A**rgument **P**arser
* [`numerics/ode.h`](numerics/ode.h): [ODE solver](#ode-solver)

## Tests

Build and run the CPU tests with `make -C test check`. Tests use C++20 and the
standard library. Select a library with `check-json`, `check-clap`,
`check-timer`, or `check-ode`. Override `CXX`, `CPPFLAGS`, `CXXFLAGS`,
`LDFLAGS`, `LDLIBS`, or `BLD` as needed; build files go under `test/build`
by default. The `cuda`
target builds optional device tests; `check-ode-cuda` also runs them and
needs a working CUDA device.

## ODE solver

[`numerics/ode.h`](numerics/ode.h) is a standalone C++20 header extracted from
GEPT. Copy it into your include path and use namespace `ode`. It needs only
the C++ standard library: no GEPT, build system, linked library, or
implementation macro.

A single header suits this library: the numerical code is templated over the
scalar, tableau, state, and system, so its definitions must be visible to
callers. Keeping the small engine and its coefficients together makes copying
and CUDA compilation straightforward. The tradeoff is parsing the header in
each translation unit; splitting it would mainly help navigation if it grows.

| Tableau | Order | Stages | Implicit |
| --- | ---: | ---: | --- |
| `RungeKutta2Tableau<T>` (explicit midpoint) | 2 | 2 | No |
| `RungeKutta3Tableau<T>` (Kutta) | 3 | 3 | No |
| `RungeKutta4Tableau<T>` (default) | 4 | 4 | No |
| `LobattoIIIA4Tableau<T>` | 4 | 3 | Yes |
| `LobattoIIIB4Tableau<T>` | 4 | 3 | Yes |
| `Gauss4Tableau<T>` | 4 | 2 | Yes |
| `Gauss6Tableau<T>` through `Gauss12Tableau<T>` | 6, 8, 10, 12 | 3–6 | Yes |

The header also provides partitioned integrators (Verlet, Lobatto IIIA–IIIB,
implicit midpoint, and Boris), Newton iteration, stage predictors, and root
events. These APIs share the same header and have no GEPT dependency.

### Fixed steps

The caller supplies `evaluate(state, derivative)` and `advance(...)`. This
allows scalars, arrays, and custom states containing time or other metadata.
For example, integrate `y' = -y`, `y(0) = 1` to `t = 1`:

```cpp
#include "ode.h"
#include <cmath>

struct Decay {
    using derivative_type = double;

    bool evaluate(double y, double& dy) const {
        dy = -y;
        return std::isfinite(dy);
    }

    template <std::size_t N>
    double advance(double y, const std::array<double, N>& derivatives,
                   const std::array<double, N>& weights,
                   std::size_t active_stages, double h,
                   double /* time_fraction */) const {
        for (std::size_t i = 0; i < active_stages; ++i)
            y += h * weights[i] * derivatives[i];
        return y;
    }
};

int main() {
    const ode::Integrator<double> solver;
    double y = 1;
    for (int i = 0; i < 100; ++i)
        if (!solver.step(y, 0.01, Decay{}, y)) return 1;
    return std::abs(y - std::exp(-1.0)) < 1e-9 ? 0 : 1;
}
```

`advance` returns the initial state plus
`h * sum(weights[i] * derivatives[i])` over **only** `active_stages` entries.
For a time-dependent system, store time in the state and advance it by
`h * time_fraction`; use that stage time in `evaluate`. The state and derivative
types must support default construction, copying, and assignment. The solver
uses fixed-size arrays and performs no heap allocation itself.

For implicit methods, supply a distance between two states and optionally a
cache and fixed-point options:

```cpp
const ode::Integrator solver(ode::Gauss4Tableau<double>{});
decltype(solver)::StepCache<double, Decay> cache;
const auto norm = [](double a, double b) { return std::abs(a - b); };
double y = 1;
bool accepted = solver.step(y, 0.1, Decay{}, y, cache, norm,
                            ode::FixedPointOptions<double>{1e-13, 100});
```

The implicit solve uses fixed-point iteration by default: even an implicit
tableau can require small steps for convergence on stiff problems.
Its default tolerance is `32 * epsilon(T)` with at most 100 iterations.
Pass `ode::NewtonOptions<T>` in place of `ode::FixedPointOptions<T>` to use
damped Newton iteration with a finite-difference Jacobian.
The norm must return a finite, nonnegative distance for valid states and a
nonfinite result for invalid states. Choose its scales and tolerance for your
problem. Omitting the cache starts each step cold. Keep one cache per trajectory
and call `cache.reset()` after externally changing the state, system, or tableau.

Use floating-point scalars and finite step sizes. Fixed steps accept either
sign of `h`; `evaluate` reports invalid stage states by returning `false`.
A failed RHS evaluation or implicit solve returns `false`, preserving the
output state, stage output, and cache.
The output may alias the initial state. Optional `Solver::Stages<State, System>`
records successful stages: pass it after the output for explicit methods, or
after the cache for implicit methods. No rollback is performed on side effects
inside caller-supplied systems or norms.

### Adaptive steps

Explicit methods provide step doubling on CPU and CUDA:

```cpp
double h = 0.25;
bool accepted = ode::Integrator<double>{}.step_adaptive(
    y, h, 0.25, 1e-8, 1e-10, Decay{}, norm);
```

The arguments after `h` are the maximum step and upper/lower error bounds.
Use a finite, nonzero step, a positive maximum step and upper error bound,
and `0 <= lower <= upper`. Negative steps integrate backward.
On success, the state is the result of two half steps and **`h` is the next
proposed step**, which may differ from the distance just advanced. Track time
in the state if it matters. Invalid options throw `std::invalid_argument` on
CPU and return `false` on CUDA. Failed trials leave the caller's state and
`h` unchanged.

The built-in implicit Runge–Kutta methods provide Stoffer's symmetric step
selection:

```cpp
double h = 0.1;  // Initial guess; negative selects backward integration.
bool accepted = solver.step_adaptive(
    y, h, 0.5, Decay{}, y, cache, norm, norm,
    ode::SymmetricStepOptions<double>{1e-6, 1e-3, 20},
    ode::FixedPointOptions<double>{1e-13, 100});
```

Here **`h` is the accepted signed step**. The positive maximum step bounds its
magnitude. The two norms control stage convergence and the embedded defect,
respectively. The defect norm must be even and translation invariant, using
fixed scales. `advance` must support accumulating a defect about `State{}` with
zero time increment. The selector solves for the current step's defect target;
a smaller defect is accepted only at the maximum step. This target is not a
global error bound. Invalid options or exhausted iterations return `false`,
preserving the output, `h`, and cache. The algorithm follows D. Stoffer,
*Variable Steps for Reversible Integration Methods*, Computing 55 (1995),
Sections 4.2(a) and 4.3; reversibility depends on stage and selection tolerances.

Custom tableaux provide `val_type`, `ORDER`, `STAGES`, `A`, `b`, and `c`.
Implicit tableaux additionally declare `IMPLICIT = true` and supply `beta`
for extrapolating cached stages. An optional `predictor(h_new / h_old)` extends
that predictor to unequal steps. Symmetric adaptivity also requires
`SYMMETRIC = true`, `ERROR_ORDER`, and `error_weights` defining the embedded
defect. See the built-in tableaux for their storage and coefficient conventions.

### CUDA and tests

Fixed steps, stage recording, caching, and adaptive steps
are callable from CUDA kernels. Mark the system and norm functions
`__host__ __device__` and compile with C++20 and `--expt-relaxed-constexpr`
for standard-library array access. CPU compilation needs no CUDA toolkit.
Invalid explicit adaptive options return `false` in device code.

Run the standalone numerical tests with `make -C test check-ode`; optional
device tests use `make -C test check-ode-cuda` (override `NVCC`, `CUDAFLAGS`,
or `BLD` as needed). Tests cover convergence, nonautonomous systems, float and
double, custom tableaux, cached predictors, reversibility, invariants, and
failure preservation. The CUDA tests compare device results with CPU results.
