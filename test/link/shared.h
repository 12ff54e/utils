#ifndef UTILS_TEST_LINK_SHARED_H
#define UTILS_TEST_LINK_SHARED_H

#include "io/JsonParser.h"
#include "io/clap.h"
#include "misc/Timer.h"
#include "numerics/ode.h"

struct LinkOptions {
    int count = 0;
};

struct LinkDecay {
    using derivative_type = double;

    bool evaluate(double state, double& derivative) const {
        derivative = -state;
        return true;
    }

    template <std::size_t N>
    double advance(double state,
                   const std::array<double, N>& derivatives,
                   const std::array<double, N>& weights,
                   std::size_t count,
                   double h,
                   double) const {
        for (std::size_t i = 0; i < count; ++i)
            state += h * weights[i] * derivatives[i];
        return state;
    }
};

json::Value value_from_other_file();
Timer& timer_from_other_file();
int count_from_other_file();
double step_from_other_file();

#endif
