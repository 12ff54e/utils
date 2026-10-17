// Adapted from GEPT's tests/test_cuda_partitioned.cu.
#include "ode/partitioned_capability_checks.h"

#include "ode/cuda_support.h"

#include <iostream>
#include <memory>

namespace {

using Results =
    std::array<ode_test::PartitionedChecks, ode_test::PARTITIONED_CHECK_COUNT>;

template <typename Tableau,
          typename Predictor,
          bool NEWTON,
          ode_test::PartitionedCheck KIND>
__global__ void check_partitioned(Results* results, std::size_t i) {
    (*results)[i] =
        ode_test::run_partitioned_check<Tableau, Predictor, NEWTON, KIND>();
}

bool checked(cudaError_t status) {
    if (status == cudaSuccess) return true;
    std::cerr << cudaGetErrorString(status) << '\n';
    return false;
}

template <typename T>
bool test_partitioned() {
    Results* d_results = nullptr;
    if (!checked(cudaMalloc(&d_results, sizeof(Results)))) return false;
    Results results;
    bool success = true, freed = false;
    {
        const auto release = [&](Results* pointer) {
            freed = checked(cudaFree(pointer));
        };
        const std::unique_ptr<Results, decltype(release)> allocation(d_results,
                                                                     release);
        ode_test::for_each_partitioned_check<T>(
            [&]<typename Tableau, typename Predictor, bool NEWTON,
                ode_test::PartitionedCheck KIND>(std::size_t i) {
                if (!success) return;
                check_partitioned<Tableau, Predictor, NEWTON, KIND>
                    <<<1, 1>>>(allocation.get(), i);
                success = checked(cudaGetLastError());
            });
        success = success &&
                  checked(cudaMemcpy(&results, allocation.get(),
                                     sizeof(Results), cudaMemcpyDeviceToHost));
    }
    if (!success || !freed) return false;
    constexpr T EPSILON = std::numeric_limits<T>::epsilon();
    ode_test::for_each_partitioned_check<
        T>([&]<typename Tableau, typename Predictor, bool NEWTON,
               ode_test::PartitionedCheck KIND>(std::size_t i) {
        const auto expected =
            ode_test::run_partitioned_check<Tableau, Predictor, NEWTON, KIND>();
        const auto& actual = results[i];
        const double tolerance =
            1024 * EPSILON +
            (std::numeric_limits<T>::digits <= 24 ? 4e-3 : 4e-6) *
                std::abs(expected.selected_h);
        if (actual.failures == 0 && expected.failures == 0 &&
            std::abs(actual.value - expected.value) <= tolerance &&
            std::abs(actual.selected_h - expected.selected_h) <= tolerance &&
            std::abs(actual.reverse_error - expected.reverse_error) <=
                tolerance)
            return;
        std::cerr << "CUDA PRK case " << i << ", scalar bytes=" << sizeof(T)
                  << ", GPU mask=" << actual.failures
                  << ", CPU mask=" << expected.failures
                  << ", GPU value=" << actual.value
                  << ", CPU value=" << expected.value << '\n';
        success = false;
    });
    return success;
}

}  // namespace

int main() {
    if (!ode_test::require_cuda_device()) return EXIT_FAILURE;
    return test_partitioned<float>() && test_partitioned<double>() ? 0 : 1;
}
