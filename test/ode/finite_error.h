#ifndef UTILS_TEST_ODE_FINITE_ERROR_H
#define UTILS_TEST_ODE_FINITE_ERROR_H

#include <cmath>
#include <initializer_list>
#include <limits>

#if defined(__CUDACC__)
#define UTILS_TEST_ERROR_HOST_DEVICE __host__ __device__
#else
#define UTILS_TEST_ERROR_HOST_DEVICE
#endif

namespace ode_test {

// A nonfinite component must invalidate the whole norm or error reduction.
template <typename T>
UTILS_TEST_ERROR_HOST_DEVICE T finite_max(std::initializer_list<T> values) {
    T result = std::numeric_limits<T>::lowest();
    for (T value : values) {
        if (!std::isfinite(value)) return std::numeric_limits<T>::quiet_NaN();
        if (result < value) result = value;
    }
    return result;
}

template <typename T>
UTILS_TEST_ERROR_HOST_DEVICE T finite_max(T first, T second) {
    return finite_max<T>({first, second});
}

}  // namespace ode_test

#undef UTILS_TEST_ERROR_HOST_DEVICE

#endif
