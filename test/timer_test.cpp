#define ZQ_TIMER_IMPLEMENTATION
#include "../Timer.h"

int main() {
    auto& timer = Timer::get_timer();

    volatile int a = 0;
    timer.start("total");
    timer.start("loop 1");
    for (int i = 0; i < 1 << 20; ++i) { a = a + i; }
    timer.pause_last_and_start_next("loop 2");
    for (int i = 0; i < 1 << 20; ++i) { a = a + i; }
    timer.pause();
    // This loop is not timed
    for (int i = 0; i < 1 << 20; ++i) { a = a + i; }
    timer.pause("total");
    timer.print();

    return 0;
}
