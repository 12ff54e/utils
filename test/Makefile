CXXFLAGS = -std=c++20 -Wall

BLD = build
TEST_SRCS = $(wildcard *.cpp)
TESTS = $(addprefix $(BLD)/, $(basename $(TEST_SRCS)))

all: $(TESTS)

$(TESTS):$(BLD)/%: %.cpp | $(BLD)
	$(CXX) $(CXXFLAGS) $< -o $@

$(BLD):
	mkdir -p $@

