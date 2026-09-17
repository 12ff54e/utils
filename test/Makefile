CXXFLAGS = -std=c++20 -Wall
NVCC ?= nvcc
CUDAFLAGS ?= -std=c++20 --expt-relaxed-constexpr -O2

BLD = build
TEST_SRCS = $(wildcard *.cpp)
TESTS = $(addprefix $(BLD)/, $(basename $(TEST_SRCS)))

all: $(TESTS)

.PHONY: all check-ode check-ode-cuda

check-ode: $(BLD)/ode_test
	$(BLD)/ode_test

check-ode-cuda: $(BLD)/ode_cuda_test
	$(BLD)/ode_cuda_test

$(BLD)/ode_test: ../ode.h

$(BLD)/ode_cuda_test: ode_cuda_test.cu ../ode.h | $(BLD)
	$(NVCC) $(CUDAFLAGS) $< -o $@

$(TESTS):$(BLD)/%: %.cpp | $(BLD)
	$(CXX) $(CXXFLAGS) $< -o $@

$(BLD):
	mkdir -p $@
