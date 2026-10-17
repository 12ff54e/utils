#define ZQ_TIMER_IMPLEMENTATION
#include "misc/Timer.h"

#include "test_support.h"

#include <cmath>
#include <thread>

namespace {

std::string report(Timer& timer) {
    test::CaptureStream capture(std::cout);
    timer.print();
    return capture.str();
}

double elapsed(const std::string& output, const std::string& name) {
    std::istringstream lines(output);
    std::string line;
    while (std::getline(lines, line)) {
        if (line.starts_with("| " + name + " ")) {
            const auto separator = line.find('|', 2);
            const double milliseconds = std::stod(line.substr(separator + 1));
            CHECK(std::isfinite(milliseconds) && milliseconds >= 0);
            CHECK(line.find("ms |") != std::string::npos);
            return milliseconds;
        }
    }
    throw std::runtime_error("Missing timer entry: " + name);
}

void wait_for_clock() {
    const auto initial = std::chrono::high_resolution_clock::now();
    do {
        std::this_thread::sleep_for(std::chrono::milliseconds(5));
    } while (std::chrono::high_resolution_clock::now() <= initial);
}

void test_timer() {
    auto& timer = Timer::get_timer();
    CHECK(&timer == &Timer::get_timer());
    timer.reset();
    CHECK(report(timer).find("Time consumption") != std::string::npos);

    timer.start("total");
    timer.start("first");
    wait_for_clock();
    timer.pause_last_and_start_next("second");
    wait_for_clock();
    timer.pause();
    timer.pause("total");
    const auto output = report(timer);
    const double first = elapsed(output, "first");
    const double second = elapsed(output, "second");
    const double total = elapsed(output, "total");
    CHECK(first > 0 && second > 0 && total >= first && total >= second);
    CHECK(output.find("| total ") < output.find("| first "));
    CHECK(output.find("| first ") < output.find("| second "));
    wait_for_clock();
    CHECK(report(timer) == output);

    timer.start("first");
    wait_for_clock();
    timer.pause("first");
    const auto accumulated = report(timer);
    CHECK(elapsed(accumulated, "first") > first);
    CHECK(elapsed(accumulated, "second") == second);
    const auto position = accumulated.find("| first ");
    CHECK(accumulated.find("| first ", position + 1) == std::string::npos);

    timer.reset();
    const auto cleared = report(timer);
    CHECK(cleared.find("| first ") == std::string::npos);
    CHECK(cleared.find("| total ") == std::string::npos);
    timer.start("after reset");
    timer.pause();
    CHECK(elapsed(report(timer), "after reset") >= 0);
    timer.reset();
}

}  // namespace

int main() {
    return test::run("timer_test", test_timer);
}
