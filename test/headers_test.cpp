#include "numerics/ode.h"

#include "link/shared.h"
#include "test_support.h"

int main() {
    return test::run("headers_test", [] {
        const auto value = value_from_other_file();
        CHECK(value["count"] == 7 && value["ok"].as_boolean());
        CHECK(json::parse(value.dump()) == value);
        CHECK(&Timer::get_timer() == &timer_from_other_file());
        CHECK(count_from_other_file() == 7);
        CHECK(CLAP<LinkOptions>::get_options().contains("--count"));
        double local = 0;
        CHECK(ode::Integrator<double>{}.step(1., .1, LinkDecay{}, local));
        CHECK(local == step_from_other_file());
        CHECK(std::abs(local - std::exp(-.1)) < 1e-7);
    });
}
