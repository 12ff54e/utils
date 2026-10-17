#define ZQ_TIMER_IMPLEMENTATION
#include "misc/Timer.h"

#include "shared.h"

Timer& timer_from_other_file() {
    return Timer::get_timer();
}
