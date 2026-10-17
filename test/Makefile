CXXFLAGS ?= -std=c++20 -O2 -Wall -Wextra -Wpedantic
NVCC ?= nvcc
CUDAFLAGS ?= -std=c++20 --expt-relaxed-constexpr -O2

BLD ?= build
INCLUDES = -I.. -DTEST_SOURCE_DIR='"$(CURDIR)"'
TEST_SRCS = $(wildcard *.cpp)
TESTS = $(addprefix $(BLD)/, $(basename $(TEST_SRCS)))
ODE_TESTS = $(addprefix $(BLD)/, $(basename $(wildcard ode*_test.cpp)))
CUDA_TESTS = $(addprefix $(BLD)/, $(basename $(wildcard *_test.cu)))
DEPS = $(addsuffix .d, $(TESTS) $(CUDA_TESTS))

all: $(TESTS)

.PHONY: all check check-json check-timer check-clap check-clap-help check-ode cuda \
        check-ode-cuda clean

check: $(TESTS) check-clap-help
	@set -e; for test in $(TESTS); do \
	    printf 'Running %s\n' "$$test"; "$$test"; \
	done

check-json: $(BLD)/json_test
	$<

check-timer: $(BLD)/timer_test
	$<

check-clap: $(BLD)/clap_test check-clap-help
	$<

check-clap-help: $(BLD)/clap_test
	$< --help > $(BLD)/clap_help.txt
	$< --check-help $(BLD)/clap_help.txt

check-ode: $(ODE_TESTS)
	@set -e; for test in $(ODE_TESTS); do \
	    printf 'Running %s\n' "$$test"; "$$test"; \
	done

cuda: $(CUDA_TESTS)

check-ode-cuda: cuda
	@set -e; for test in $(CUDA_TESTS); do \
	    printf 'Running %s\n' "$$test"; "$$test"; \
	done

$(BLD)/%: %.cpp | $(BLD)
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) $(INCLUDES) -MMD -MP -MF $@.d \
	    -MT $@ $< $(LDFLAGS) $(LDLIBS) -o $@

$(BLD)/%: %.cu | $(BLD)
	$(NVCC) $(CPPFLAGS) $(CUDAFLAGS) $(INCLUDES) -MMD -MF $@.d \
	    $< $(LDFLAGS) $(LDLIBS) -o $@

$(BLD):
	mkdir -p $@

clean:
	$(RM) $(TESTS) $(CUDA_TESTS) $(DEPS) $(BLD)/clap_help.txt

-include $(DEPS)
