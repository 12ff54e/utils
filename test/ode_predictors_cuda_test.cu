// Adapted from GEPT's tests/test_cuda_implicit_predictors.cu.
#include "ode/implicit_predictor_checks.h"

#include "ode/cuda_support.h"

#include <iostream>
#include <memory>

namespace {

using Results = ode_test::ImplicitPredictorResults;

template <typename Tableau, typename Predictor>
__global__ void check_predictor(Results* results, std::size_t i) {
    (*results)[i] = ode_test::check_implicit_predictor<Tableau, Predictor>();
}

bool checked(cudaError_t status) {
    if (status == cudaSuccess) return true;
    std::cerr << cudaGetErrorString(status) << '\n';
    return false;
}

template <typename T>
bool test_predictors() {
    Results* d_results = nullptr;
    if (!checked(cudaMalloc(&d_results, sizeof(Results)))) return false;
    Results results;
    bool success, freed = false;
    {
        const auto release = [&](Results* pointer) {
            freed = checked(cudaFree(pointer));
        };
        const std::unique_ptr<Results, decltype(release)> allocation(d_results,
                                                                     release);
        success = true;
        ode_test::for_each_implicit_predictor<T>(
            [&]<typename Tableau, typename Predictor>(std::size_t i) {
                if (!success) return;
                check_predictor<Tableau, Predictor>
                    <<<1, 1>>>(allocation.get(), i);
                success = checked(cudaGetLastError());
            });
        success = success &&
                  checked(cudaMemcpy(&results, allocation.get(),
                                     sizeof(Results), cudaMemcpyDeviceToHost));
    }
    if (!success || !freed) return false;
    const auto reference = ode_test::check_implicit_predictors<T>();
    constexpr double EPSILON = std::numeric_limits<T>::epsilon();
    for (std::size_t i = 0; i < results.size(); ++i) {
        const auto actual = results[i], expected = reference[i];
        if (actual.passed() && expected.passed() &&
            std::abs(actual.value - expected.value) < 512 * EPSILON &&
            std::abs(actual.starting_value - expected.starting_value) <
                256 * EPSILON &&
            std::abs(actual.reverse_error - expected.reverse_error) <
                512 * EPSILON)
            continue;
        std::cerr << "CUDA predictor " << i << ", scalar bytes=" << sizeof(T)
                  << ": orbit=" << actual.orbit << ", clocks=" << actual.clocks
                  << ", atomicity=" << actual.atomicity
                  << ", adaptive=" << actual.adaptive
                  << ", reset=" << actual.reset
                  << ", reverse error=" << actual.reverse_error
                  << ", CPU value=" << expected.value
                  << ", GPU value=" << actual.value
                  << ", CPU starting value=" << expected.starting_value
                  << ", GPU starting value=" << actual.starting_value << '\n';
        return false;
    }
    return true;
}

}  // namespace

int main() {
    if (!ode_test::require_cuda_device()) return EXIT_FAILURE;
    return test_predictors<float>() && test_predictors<double>() ? 0 : 1;
}
