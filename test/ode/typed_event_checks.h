// Adapted from GEPT's tests/typed_event_checks.hpp.
#ifndef UTILS_ODE_TESTS_TYPED_EVENT_CHECKS_HPP_
#define UTILS_ODE_TESTS_TYPED_EVENT_CHECKS_HPP_

#include "numerics/ode.h"

#include <array>
#include <cmath>
#include <limits>
#include <tuple>

#if defined(__CUDACC__)
#define UTILS_ODE_TEST_HOST_DEVICE __host__ __device__
#else
#define UTILS_ODE_TEST_HOST_DEVICE
#endif

namespace ode_test {

struct EventTestState {
    double time;
    double position = 0;
};

struct TypedEventChecks {
    bool ordering = false;
    bool stopping = false;
    bool reuse = false;
    bool nonlinear = false;
    bool statuses = false;
    bool atomicity = false;
    bool empty = false;
    double stop_time = 0;

    UTILS_ODE_TEST_HOST_DEVICE bool passed() const {
        return ordering && stopping && reuse && nonlinear && statuses &&
               atomicity && empty;
    }
};

// Run the same contracts on the host and in independent device threads.
UTILS_ODE_TEST_HOST_DEVICE inline TypedEventChecks check_typed_events(
    double offset) {
    using State = EventTestState;
    using Status = ode::EventStatus;
    TypedEventChecks checks;
    const State initial{offset}, next{offset + 1, 1};
    const auto interpolate = [offset](double t, State& state) {
        state = {offset + t, t * t};
        return true;
    };
    const auto condition = [offset](double time) {
        return [offset, time](const State& state) {
            return state.time - offset - time;
        };
    };
    int count = 0, mutable_calls = 0;
    std::array<int, 8> order{};
    const auto record = [&](int index) {
        if (count < static_cast<int>(order.size())) order[count] = index;
        ++count;
    };
    bool stop = false;
    auto events = std::tuple{
        ode::make_event<State>(condition(.75),
                               [&](const State&) { record(0); }),
        ode::make_event<State>(condition(.25),
                               [&, calls = 0](const State&) mutable {
                                   mutable_calls = ++calls;
                                   record(1);
                               }),
        ode::make_event<State>(condition(.5),
                               [&](const State&) {
                                   record(2);
                                   return stop ? ode::EventAction::STOP
                                               : ode::EventAction::CONTINUE;
                               }),
        ode::make_event<State>(condition(.5), [&](const State&) { record(3); }),
        ode::make_event<State>(condition(0), [&](const State&) { record(4); }),
        ode::make_event<State>(condition(1), [&](const State&) { record(5); }),
        ode::make_event<State>(
            condition(.125), [&](const State&) { record(6); },
            ode::EventDirection::FALLING),
        ode::make_event<State>(
            [offset](const State& state) {
                return .375 - (state.time - offset);
            },
            [&](const State&) { record(7); }, ode::EventDirection::FALLING)};
    auto dispatcher = ode::make_event_dispatcher<State>(events);
    const auto first = dispatcher.advance(initial, next, interpolate);
    checks.ordering = first.status == Status::SUCCESS && !first.stop &&
                      count == 6 && mutable_calls == 1 && order[0] == 1 &&
                      order[1] == 7 && order[2] == 2 && order[3] == 3 &&
                      order[4] == 0 && order[5] == 5;
    stop = true;
    count = 0;
    const auto second = dispatcher.advance(initial, next, interpolate);
    checks.stopping = second.status == Status::STOP && second.stop &&
                      second.stop->event_index == 2 &&
                      second.stop->state.time == offset + .5 &&
                      second.stop->state.position == .25 && count == 3 &&
                      order[0] == 1 && order[1] == 7 && order[2] == 2;
    const auto after =
        dispatcher.advance(next, State{offset + 2, 4}, [offset](double t) {
            return State{offset + 1 + t, (1 + t) * (1 + t)};
        });
    checks.reuse = after.status == Status::SUCCESS && !after.stop &&
                   mutable_calls == 2 && count == 3;

    auto nonlinear_events = std::tuple{ode::make_event<State>(
        [](const State& state) { return state.position - .2; },
        [](const State&) { return ode::EventAction::STOP; })};
    auto nonlinear = ode::make_event_dispatcher<State>(nonlinear_events);
    const auto root = nonlinear.advance(initial, next, interpolate);
    if (root.stop) checks.stop_time = root.stop->state.time;
    checks.nonlinear =
        root.status == Status::STOP && root.stop &&
        std::abs(root.stop->state.time - offset - std::sqrt(.2)) < 1e-10 &&
        std::abs(root.stop->state.position - .2) < 1e-10;

    int callbacks = 0, mode = 0, invalid_evaluations = 0;
    auto failing_events = std::tuple{
        ode::make_event<State>(condition(.125),
                               [&](const State&) {
                                   ++callbacks;
                                   return mode == 2 ? ode::EventAction::CONTINUE
                                                    : ode::EventAction::STOP;
                               }),
        ode::make_event<State>(
            [&, offset](const State& state) {
                if (mode == 1) {
                    ++invalid_evaluations;
                    return std::numeric_limits<double>::quiet_NaN();
                }
                return state.time - offset - .3;
            },
            [&](const State&) {
                ++callbacks;
                return mode == 2 ? static_cast<ode::EventAction>(99)
                                 : ode::EventAction::CONTINUE;
            })};
    auto failing = ode::make_event_dispatcher<State>(failing_events);
    const auto expect = [&](Status status, auto sample) {
        callbacks = 0;
        const auto result = failing.advance(initial, next, sample);
        return result.status == status && !result.stop && callbacks == 0;
    };
    checks.atomicity = expect(Status::INVALID_INTERPOLATION,
                              [](double, State&) { return false; });
    auto& options = std::get<1>(failing_events);
    options.max_iterations = 1;
    checks.atomicity &= expect(Status::LOCALIZATION_FAILURE, interpolate);
    options.max_iterations = 64;
    mode = 1;
    checks.statuses = expect(Status::NONFINITE_CONDITION, interpolate) &&
                      invalid_evaluations == 1;
    mode = 0;
    options.time_tolerance = 0;
    checks.statuses &= expect(Status::INVALID_OPTIONS, interpolate);
    options.time_tolerance = 1e-14;
    options.direction = static_cast<ode::EventDirection>(99);
    checks.statuses &= expect(Status::INVALID_DIRECTION, interpolate);
    options.direction = ode::EventDirection::ANY;
    checks.statuses &= expect(Status::INVALID_INTERPOLATION,
                              [offset](double) { return State{offset - 1}; });
    checks.statuses &= expect(Status::INVALID_INTERPOLATION, [](double) {
        return State{std::numeric_limits<double>::quiet_NaN()};
    });
    callbacks = 0;
    const auto invalid_segment = failing.advance(next, initial, interpolate);
    checks.statuses &= invalid_segment.status == Status::INVALID_SEGMENT &&
                       !invalid_segment.stop && callbacks == 0;
    mode = 2;
    const auto invalid_action = failing.advance(initial, next, interpolate);
    checks.statuses &= invalid_action.status == Status::INVALID_ACTION &&
                       !invalid_action.stop && callbacks == 2;
    mode = 0;
    callbacks = 0;
    const auto recovered = failing.advance(initial, next, interpolate);
    checks.atomicity &= recovered.status == Status::STOP && recovered.stop &&
                        recovered.stop->state.time == offset + .125 &&
                        callbacks == 1;

    auto missing_events = std::tuple{ode::make_event<State>(
        static_cast<double (*)(const State&)>(nullptr), [](const State&) {})};
    auto missing = ode::make_event_dispatcher<State>(missing_events);
    checks.statuses &= missing.advance(initial, next, interpolate).status ==
                       Status::INVALID_EVENT;
    const std::tuple<> no_events;
    auto empty = ode::make_event_dispatcher<State>(no_events);
    const auto none =
        empty.advance(initial, next, [](double, State&) { return false; });
    checks.empty = none.status == Status::SUCCESS && !none.stop;
    return checks;
}

}  // namespace ode_test

#undef UTILS_ODE_TEST_HOST_DEVICE

#endif  // UTILS_ODE_TESTS_TYPED_EVENT_CHECKS_HPP_
