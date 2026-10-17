// Standalone dispatcher cases adapted from GEPT's tests/test_events.cpp.
#include "numerics/ode.h"
#include "ode/typed_event_checks.h"

#include "test_support.h"

#include <memory>

namespace {

using State = ode_test::EventTestState;
using Event = ode::Event<State>;

template <typename StateType, typename Events>
concept CanBorrowEvents = requires(Events&& events) {
    ode::make_event_dispatcher<StateType>(std::forward<Events>(events));
};

void check(bool value, const char* message) {
    if (!value) throw std::runtime_error(message);
}

void close(double a, double b, double tolerance, const char* message) {
    check(std::isfinite(a) && std::abs(a - b) <= tolerance, message);
}

template <typename Exception, typename Function>
void throws(Function function, const char* message) {
    try {
        function();
    } catch (const Exception&) { return; }
    throw std::runtime_error(message);
}

void test_dispatcher() {
    struct ClockState {
        double time;
    };
    using ClockEvent = ode::Event<ClockState>;
    std::vector<int> order;
    const auto event = [&](double time, int id, ode::EventDirection direction) {
        return ClockEvent{[time](const auto& s) { return s.time - time; },
                          [&, id, time](const auto& s) {
                              close(s.time, time, 1e-10, "linear root time");
                              order.push_back(id);
                          },
                          direction};
    };
    const std::array events{event(.75, 0, ode::EventDirection::ANY),
                            event(.25, 1, ode::EventDirection::RISING),
                            event(.25, 2, ode::EventDirection::ANY),
                            event(.5, 3, ode::EventDirection::FALLING),
                            event(0, 4, ode::EventDirection::ANY),
                            event(1, 5, ode::EventDirection::ANY)};
    ode::EventDispatcher<ClockState> dispatcher(events);
    dispatcher.advance({0}, {1}, [](double t) { return ClockState{t}; });
    check(order == std::vector<int>({1, 2, 0, 5}),
          "direction filtering, initial exclusion and chronological tie order");
    dispatcher.advance({1}, {2}, [](double t) { return ClockState{1 + t}; });
    check(order.size() == 4, "endpoint root is reported only once");

    const std::array falling{
        ClockEvent{[](const auto& s) { return .3 - s.time; },
                   [&](const auto& s) {
                       close(s.time, .3, 1e-10, "falling root");
                       order.push_back(6);
                   },
                   ode::EventDirection::FALLING}};
    ode::EventDispatcher<ClockState>(falling).advance(
        {0}, {1}, [](double t) { return ClockState{t}; });
    check(order.back() == 6, "falling direction accepted");

    int partial = 0;
    const std::array invalid{
        ClockEvent{[](const auto& s) { return s.time - 1; },
                   [&](const auto&) { ++partial; }},
        ClockEvent{[](const auto& s) { return s.time - .3; },
                   [&](const auto&) { ++partial; }}};
    throws<std::runtime_error>(
        [&] {
            ode::EventDispatcher<ClockState>(invalid).advance(
                {0}, {1}, [](double) {
                    return ClockState{std::numeric_limits<double>::quiet_NaN()};
                });
        },
        "invalid interpolated times are diagnosed");
    check(partial == 0,
          "localization failure suppresses all callbacks for that segment");
}

void test_stop_dispatcher() {
    std::vector<int> order;
    const auto condition = [](double time) {
        return [time](const State& state) { return state.time - time; };
    };
    const std::array events{
        Event{condition(.75),
              [&](const State&) {
                  order.push_back(0);
                  return ode::EventAction::STOP;
              }},
        Event{condition(.25), [&](const State&) { order.push_back(1); }},
        Event{condition(.5),
              [&](const State&) {
                  order.push_back(2);
                  return ode::EventAction::STOP;
              }},
        Event{condition(.5), [&](const State&) { order.push_back(3); }}};
    State initial{0}, next{1};
    const auto interpolate = [](double t) {
        State state;
        state.time = t;
        return state;
    };
    const auto stop =
        ode::EventDispatcher<State>(events).advance(initial, next, interpolate);
    check(stop && stop->event_index == 2 && stop->state.time == .5,
          "dispatcher returns the first stopping event and localized state");
    check(order == std::vector<int>({1, 2}),
          "STOP suppresses later callbacks, including registration-order ties");

    std::array invalid{Event{condition(.5), [](const State&) {
                                 return static_cast<ode::EventAction>(99);
                             }}};
    throws<std::runtime_error>(
        [&] {
            ode::EventDispatcher<State>(invalid).advance(initial, next,
                                                         interpolate);
        },
        "invalid callback actions are diagnosed");
    invalid[0].callback = std::function<void(const State&)>{};
    throws<std::invalid_argument>(
        [&] { ode::EventDispatcher<State> dispatcher(invalid); },
        "empty void functions remain empty through the callback adapter");
    invalid[0].callback = std::function<ode::EventAction(const State&)>{};
    throws<std::invalid_argument>(
        [&] { ode::EventDispatcher<State> dispatcher(invalid); },
        "empty action functions are rejected");
}

void test_typed_dispatcher() {
    for (double offset : {0., 4., 64.}) {
        const auto checks = ode_test::check_typed_events(offset);
        check(checks.ordering, "typed events preserve root and tie ordering");
        check(checks.stopping, "typed STOP suppresses later callbacks");
        check(checks.reuse, "mutable callbacks retain state without copying");
        check(checks.nonlinear, "typed events localize a nonlinear path");
        check(checks.statuses,
              "typed dispatcher returns distinct error statuses");
        check(checks.atomicity,
              "localization failure suppresses an earlier STOP");
        check(checks.empty, "an empty tuple emits no event");
    }

    struct ClockState {
        ClockState() = delete;
        explicit ClockState(double value) : time(value) {}
        double time;
    };
    int calls = 0;
    auto events = std::tuple{ode::make_event<ClockState>(
        [](const ClockState& state) { return state.time - .5; },
        [&, count = std::make_unique<int>(0)](const ClockState&) mutable {
            calls = ++*count;
        })};
    using Events = decltype(events);
    static_assert(!std::is_copy_constructible_v<Events>);
    static_assert(CanBorrowEvents<ClockState, Events&>);
    static_assert(CanBorrowEvents<ClockState, const Events&>);
    static_assert(!CanBorrowEvents<ClockState, Events>);
    static_assert(!CanBorrowEvents<ClockState, const Events>);
    static_assert(
        !std::is_constructible_v<ode::StaticEventDispatcher<ClockState, Events>,
                                 Events&&>);
    static_assert(!std::is_constructible_v<
                  ode::StaticEventDispatcher<ClockState, const Events>,
                  const Events&&>);
    auto dispatcher = ode::make_event_dispatcher<ClockState>(events);
    const auto interpolate = [](double t, ClockState& state) {
        state.time = t;
        return true;
    };
    const auto first =
        dispatcher.advance(ClockState(0), ClockState(1), interpolate);
    const auto second =
        dispatcher.advance(ClockState(0), ClockState(1), interpolate);
    check(first.status == ode::EventStatus::SUCCESS &&
              second.status == ode::EventStatus::SUCCESS && calls == 2,
          "move-only mutable callbacks and non-default states work together");

    const std::array const_events{ode::make_event<ClockState>(
        [](const ClockState& state) { return state.time - 1; },
        [&](const ClockState&) { ++calls; })};
    auto const_dispatcher =
        ode::make_event_dispatcher<ClockState>(const_events);
    const auto endpoint =
        const_dispatcher.advance(ClockState(0), ClockState(1),
                                 [](double, ClockState&) { return false; });
    check(endpoint.status == ode::EventStatus::SUCCESS && calls == 3,
          "const arrays and endpoint roots need no interpolated sample");

    const std::array legacy_events{ode::Event<ClockState>{
        [](const ClockState& state) { return state.time - .5; },
        [&](const ClockState&) { ++calls; }}};
    check(!ode::EventDispatcher<ClockState>(legacy_events)
                  .advance(ClockState(0), ClockState(1), interpolate) &&
              calls == 4,
          "the host adapter supports checked interpolation and non-default "
          "states");

    auto throwing_events = std::tuple{ode::make_event<ClockState>(
        [](const ClockState&) -> double {
            throw std::logic_error("condition");
        },
        [](const ClockState&) {})};
    auto throwing = ode::make_event_dispatcher<ClockState>(throwing_events);
    throws<std::logic_error>(
        [&] { throwing.advance(ClockState(0), ClockState(1), interpolate); },
        "typed user exceptions propagate on the host");
}

}  // namespace

int main() {
    return test::run("ode_events_test", [] {
        test_dispatcher();
        test_stop_dispatcher();
        test_typed_dispatcher();
    });
}
