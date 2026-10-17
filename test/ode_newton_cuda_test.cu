// Adapted from GEPT's tests/test_cuda_newton.cu.
#include "ode/newton_checks.h"

#include "ode/cuda_support.h"

#include <iostream>
#include <memory>

namespace {

using Results = ode_test::NewtonResults;

template <typename Tableau, typename Predictor>
__global__ void check_linear(Results* results, std::size_t index) {
    (*results)[index] = ode_test::check_newton_linear<Tableau, Predictor>();
}

template <typename T, std::size_t INDEX>
__global__ void check_special(Results* results) {
    if constexpr (INDEX == 8)
        (*results)[INDEX] = ode_test::check_newton_edges<T>();
    else if constexpr (INDEX == 9)
        (*results)[INDEX] = ode_test::check_newton_pair<T>();
    else if constexpr (INDEX == 10)
        (*results)[INDEX] = ode_test::check_newton_vector<T, false>();
    else if constexpr (INDEX == 11)
        (*results)[INDEX] = ode_test::check_newton_vector<T, true>();
    else
        (*results)[INDEX] = ode_test::check_newton_unused<T>();
}

bool checked(cudaError_t status) {
    if (status == cudaSuccess) return true;
    std::cerr << cudaGetErrorString(status) << '\n';
    return false;
}

template <typename T>
bool test_newton() {
    Results* d_results = nullptr;
    if (!checked(cudaMalloc(&d_results, sizeof(Results)))) return false;
    Results results{};
    bool success = true, freed = false;
    {
        const auto release = [&](Results* pointer) {
            freed = checked(cudaFree(pointer));
        };
        const std::unique_ptr<Results, decltype(release)> allocation(d_results,
                                                                     release);
        ode_test::for_each_newton_check<T>(
            [&]<typename Tableau, typename Predictor>(std::size_t index) {
                if (!success) return;
                check_linear<Tableau, Predictor>
                    <<<1, 1>>>(allocation.get(), index);
                success = checked(cudaGetLastError());
            });
        const auto launch_special = [&]<std::size_t INDEX>() {
            if (!success) return;
            check_special<T, INDEX><<<1, 1>>>(allocation.get());
            success = checked(cudaGetLastError());
        };
        launch_special.template operator()<8>();
        launch_special.template operator()<9>();
        launch_special.template operator()<10>();
        launch_special.template operator()<11>();
        launch_special.template operator()<12>();
        success = success &&
                  checked(cudaMemcpy(&results, allocation.get(),
                                     sizeof(Results), cudaMemcpyDeviceToHost));
    }
    if (!success || !freed) return false;
    const auto reference = ode_test::check_newton<T>();
    constexpr double EPSILON = std::numeric_limits<T>::epsilon();
    for (std::size_t i = 0; i < results.size(); ++i) {
        const auto actual = results[i], expected = reference[i];
        if (actual.failures == 0 && expected.failures == 0 &&
            std::abs(actual.value - expected.value) < 1024 * EPSILON &&
            std::abs(actual.residual - expected.residual) < 1024 * EPSILON)
            continue;
        std::cerr << "CUDA Newton case " << i << ", scalar bytes=" << sizeof(T)
                  << ": failures=" << actual.failures
                  << ", CPU failures=" << expected.failures
                  << ", CPU value=" << expected.value
                  << ", GPU value=" << actual.value
                  << ", CPU residual=" << expected.residual
                  << ", GPU residual=" << actual.residual << '\n';
        return false;
    }
    return true;
}

}  // namespace

int main() {
    if (!ode_test::require_cuda_device()) return EXIT_FAILURE;
    return test_newton<float>() && test_newton<double>() ? 0 : 1;
}
