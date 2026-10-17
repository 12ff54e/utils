#include "numerics/ode.h"

#include "shared.h"

double step_from_other_file() {
    double output = 0;
    if (!ode::Integrator<double>{}.step(1., .1, LinkDecay{}, output))
        return std::numeric_limits<double>::quiet_NaN();
    return output;
}
