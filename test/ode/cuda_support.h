#ifndef UTILS_TEST_ODE_CUDA_SUPPORT_H
#define UTILS_TEST_ODE_CUDA_SUPPORT_H

#include <cuda_runtime.h>

#include <iostream>

namespace ode_test {

inline bool require_cuda_device() {
    int count = 0;
    const auto status = cudaGetDeviceCount(&count);
    if (status != cudaSuccess) {
        std::cerr << "CUDA device discovery failed: "
                  << cudaGetErrorString(status) << '\n';
        return false;
    }
    if (count == 0) {
        std::cerr << "CUDA tests require a device; none was found\n";
        return false;
    }
    return true;
}

}  // namespace ode_test

#endif
