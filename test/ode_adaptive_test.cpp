#include "numerics/ode.h"
#include "ode/explicit_adaptive_checks.h"
#include "ode/implicit_predictor_checks.h"
#include "ode/newton_checks.h"
#include "ode/partitioned_capability_checks.h"

#include "test_support.h"

namespace {

void test_invalid_norms() {
    const double nan = std::numeric_limits<double>::quiet_NaN();
    const double infinity = std::numeric_limits<double>::infinity();
    for (double invalid : {nan, infinity}) {
        CHECK(!std::isfinite(ode_test::finite_max(0., invalid)));
        CHECK(!std::isfinite(ode_test::finite_max(invalid, 0.)));
        CHECK(!std::isfinite(ode_test::finite_max({0., invalid, 0.})));
        CHECK(!std::isfinite(
            ode_test::ExplicitAdaptiveNorm<double>{}({1, invalid}, {1, 0})));
        CHECK(!std::isfinite(
            ode_test::PredictorNorm<double>{}({1, invalid}, {1, 0})));
        CHECK(!std::isfinite(
            ode_test::NewtonNorm<double>{}({1, invalid}, {1, 0})));
        CHECK(!std::isfinite(
            ode_test::NewtonPairNorm<double>{}({1, invalid}, {1, 0})));
        CHECK(!std::isfinite(
            ode_test::PartitionedNorm<double>{}({1, invalid}, {1, 0})));
    }
}

template <typename Tableau>
void check_adaptive() {
    using T = typename Tableau::val_type;
    for (T offset : {T{0}, T{0.125}, T{2}}) {
        const auto result = ode_test::check_explicit_adaptive<Tableau>(offset);
        if (result.failures != 0)
            throw std::runtime_error(
                "RK" + std::to_string(Tableau::ORDER) +
                ", scalar bytes=" + std::to_string(sizeof(T)) +
                ", failures=" + std::to_string(result.failures));
    }
}

template <typename T>
void check_precision() {
    check_adaptive<ode::RungeKutta2Tableau<T>>();
    check_adaptive<ode::RungeKutta3Tableau<T>>();
    check_adaptive<ode::RungeKutta4Tableau<T>>();
}

}  // namespace

int main() {
    return test::run("ode_adaptive_test", [] {
        test_invalid_norms();
        check_precision<float>();
        check_precision<double>();
        check_precision<long double>();
    });
}
