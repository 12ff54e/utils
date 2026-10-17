#define ZQ_JSON_PARSER_IMPLEMENTATION
#include "io/JsonParser.h"

#include "test_support.h"

#include <array>
#include <cstring>
#include <optional>

namespace {

struct ResourceStats {
    std::size_t allocations{};
    std::size_t deallocations{};
    std::size_t live_bytes{};
    bool alive{};
};

class TrackingResource : public std::pmr::memory_resource {
   public:
    explicit TrackingResource(ResourceStats& stats) : stats_(stats) {
        stats_.alive = true;
    }
    ~TrackingResource() override { stats_.alive = false; }

    void allow_allocations(std::size_t n) {
        fail_after_ = stats_.allocations + n;
    }
    void allow_all() { fail_after_ = std::numeric_limits<std::size_t>::max(); }

   private:
    ResourceStats& stats_;
    std::size_t fail_after_ = std::numeric_limits<std::size_t>::max();
    std::unordered_map<void*, std::pair<std::size_t, std::size_t>> blocks_;

    void* do_allocate(std::size_t bytes, std::size_t alignment) override {
        if (stats_.allocations == fail_after_) { throw std::bad_alloc(); }
        auto* data =
            std::pmr::new_delete_resource()->allocate(bytes, alignment);
        try {
            blocks_.emplace(data, std::pair{bytes, alignment});
        } catch (...) {
            std::pmr::new_delete_resource()->deallocate(data, bytes, alignment);
            throw;
        }
        ++stats_.allocations;
        stats_.live_bytes += bytes;
        return data;
    }
    void do_deallocate(void* data,
                       std::size_t bytes,
                       std::size_t alignment) override {
        const auto found = blocks_.find(data);
        CHECK(found != blocks_.end());
        CHECK(found->second == std::pair{bytes, alignment});
        blocks_.erase(found);
        ++stats_.deallocations;
        stats_.live_bytes -= bytes;
        std::pmr::new_delete_resource()->deallocate(data, bytes, alignment);
    }
    bool do_is_equal(
        const std::pmr::memory_resource& other) const noexcept override {
        return this == &other;
    }
};

// Distinct resources may compare equal when they share a backing allocator.
class EquivalentResource : public std::pmr::memory_resource {
   public:
    EquivalentResource(std::shared_ptr<TrackingResource> upstream, bool& alive)
        : upstream_(std::move(upstream)), alive_(alive) {
        alive_ = true;
    }
    ~EquivalentResource() override { alive_ = false; }

   private:
    std::shared_ptr<TrackingResource> upstream_;
    bool& alive_;

    void* do_allocate(std::size_t bytes, std::size_t alignment) override {
        return upstream_->allocate(bytes, alignment);
    }
    void do_deallocate(void* data,
                       std::size_t bytes,
                       std::size_t alignment) override {
        upstream_->deallocate(data, bytes, alignment);
    }
    bool do_is_equal(
        const std::pmr::memory_resource& other) const noexcept override {
        const auto* equivalent =
            dynamic_cast<const EquivalentResource*>(&other);
        return equivalent && upstream_ == equivalent->upstream_;
    }
};

void test_arena() {
    static_assert(!std::is_copy_constructible_v<json::Arena>);
    static_assert(!std::is_move_constructible_v<json::Arena>);
    ResourceStats stats;
    TrackingResource upstream(stats);
    {
        json::Arena arena(1024, &upstream);
        CHECK(arena.upstream_resource() == &upstream);
        std::array<void*, 40> pointers{};
        for (auto& pointer : pointers) {
            pointer = arena.allocate(8, 8);
            CHECK(reinterpret_cast<std::uintptr_t>(pointer) % 8 == 0);
            std::memset(pointer, 0x5a, 8);
        }
        CHECK(stats.allocations == 1);
        for (auto* pointer : pointers) {
            CHECK(*static_cast<unsigned char*>(pointer) == 0x5a);
            arena.deallocate(pointer, 8, 8);
        }
        CHECK(stats.deallocations == 0);
        CHECK(arena.allocate(0) != arena.allocate(0));
        for (const std::size_t alignment : {1, 8, 64, 256, 4096}) {
            const auto bytes = alignment + 5;
            auto* pointer = arena.allocate(bytes, alignment);
            CHECK(reinterpret_cast<std::uintptr_t>(pointer) % alignment == 0);
            std::memset(pointer, 0x6b, bytes);
        }
        auto* large = arena.allocate(1024 * 1024, 256);
        std::memset(large, 0x7c, 1024 * 1024);
        CHECK(stats.live_bytes > 1024 * 1024);
        arena.release();
        CHECK(stats.live_bytes == 0);
        CHECK(stats.allocations == stats.deallocations);
        auto* reused = arena.allocate(32);
        std::memset(reused, 0, 32);
        test::throws<std::bad_alloc>([&] {
            (void)arena.allocate(std::numeric_limits<std::size_t>::max(), 64);
        });
        upstream.allow_allocations(0);
        test::throws<std::bad_alloc>([&] { (void)arena.allocate(2048); });
        upstream.allow_all();
        std::memset(arena.allocate(2048), 0, 2048);
    }
    CHECK(stats.live_bytes == 0);
    CHECK(stats.allocations == stats.deallocations);
    test::throws<std::invalid_argument>([] { json::Arena arena(0); });
    test::throws<std::invalid_argument>([] { json::Arena arena(8, nullptr); });
}

void test_allocator() {
    using String =
        std::basic_string<char, std::char_traits<char>, json::Allocator<char>>;
    using Strings = std::vector<String, json::Allocator<String>>;
    ResourceStats stats;
    TrackingResource resource(stats);
    {
        json::Allocator<int> allocator(&resource);
        json::Allocator<char> rebound(allocator);
        CHECK(allocator == rebound);
        auto moved = std::move(rebound);
        CHECK(rebound == moved);
        auto* data = rebound.allocate(5);
        rebound.deallocate(data, 5);
        test::throws<std::bad_array_new_length>([&] {
            (void)allocator.allocate(std::numeric_limits<std::size_t>::max());
        });

        Strings strings{json::Allocator<String>(&resource)};
        strings.emplace_back(120, 'x');
        strings.resize(4);
        for (const auto& string : strings) {
            CHECK(string.get_allocator().resource() == &resource);
        }
        auto copy = strings;
        CHECK(copy == strings);
        CHECK(copy.get_allocator().resource() == &resource);

        using Entry = std::pair<const int, String>;
        std::unordered_map<int, String, std::hash<int>, std::equal_to<int>,
                           json::Allocator<Entry>>
            table{json::Allocator<Entry>(&resource)};
        table.try_emplace(1, 120, 'y');
        table[2] = "a string long enough to exceed inline storage";
        CHECK(table.at(1).get_allocator().resource() == &resource);
        CHECK(table.at(2).get_allocator().resource() == &resource);
    }
    CHECK(stats.live_bytes == 0);
    CHECK(stats.allocations == stats.deallocations);

    ResourceStats owned_stats;
    {
        auto owned = std::make_shared<TrackingResource>(owned_stats);
        Strings strings{json::Allocator<String>(owned)};
        owned.reset();
        CHECK(owned_stats.alive);
        strings.emplace_back(120, 'z');
        strings.clear();
        CHECK(owned_stats.alive);
    }
    CHECK(!owned_stats.alive);
    CHECK(owned_stats.live_bytes == 0);
    test::throws<std::invalid_argument>(
        [] { json::Allocator<int> allocator(nullptr); });
    test::throws<std::invalid_argument>([] {
        json::Allocator<int> allocator(std::shared_ptr<TrackingResource>{});
    });
    json::Allocator<int> default_allocator;
    CHECK(dynamic_cast<json::Arena*>(default_allocator.resource()));
}

constexpr char document[] = R"({
    "a long object key that cannot fit in inline string storage": {
        "text": "a long string value that also needs allocated storage",
        "values": [null, true, false, 17, -3.5, {}, [], "short"]
    },
    "tail": [1, 2, 3]
})";

void check_resource(const json::Value& value,
                    std::pmr::memory_resource* resource) {
    CHECK(value.get_allocator().resource() == resource);
    if (value.is_string()) {
        CHECK(value.as_string().get_allocator().resource() == resource);
    } else if (value.is_object()) {
        CHECK(value.as_object().get_allocator().resource() == resource);
        for (const auto& [key, child] : value.as_object()) {
            CHECK(key.get_allocator().resource() == resource);
            check_resource(child, resource);
        }
    } else if (value.is_array()) {
        CHECK(value.as_array().get_allocator().resource() == resource);
        for (const auto& child : value.as_array()) {
            check_resource(child, resource);
        }
    } else if (value.value_category() ==
               json::ValueCategory::TypedArrayComplexFloat) {
        CHECK(value.as_typed_array<std::complex<float>>()
                  .get_allocator()
                  .resource() == resource);
    } else if (value.is_typed_array()) {
        CHECK(value.as_typed_array<std::complex<double>>()
                  .get_allocator()
                  .resource() == resource);
    }
}

void add_typed_arrays(json::Value& value) {
    value["complex float"] = json::Value::create_typed_array(
        std::vector<std::complex<float>>{{1.f, 2.f}, {3.f, 4.f}});
    value["complex double"] = json::Value::create_typed_array(
        std::vector<std::complex<double>>{{5., 6.}, {7., 8.}});
}

void test_parser_resources() {
    auto default_value = json::parse(document);
    auto* default_arena = default_value.get_allocator().resource();
    CHECK(dynamic_cast<json::Arena*>(default_arena));
    check_resource(default_value, default_arena);

    ResourceStats stats;
    TrackingResource resource(stats);
    {
        auto value = json::parse(document, &resource);
        CHECK(value == default_value);
        check_resource(value, &resource);
        CHECK(stats.allocations > 10);
        value["new"] = json::Value::create_object();
        value["new"]["number"] = 5;
        value["new"]["number"] += 0.25;
        value["new"]["number"] -= 1;
        CHECK(value["new"]["number"] == 4.25);
        value["new"]["number"] = "now a string with allocated storage";
        value["new"]["number"] = true;
        value["new"]["number"] = 9;
        value["new"]["number"] = 1.5;
        value["new"]["number"] = std::string_view("a new string");
        value["new"]["number"] = value["new"]["number"].as_string();

        value["array"] = json::Value::create_array(3);
        value["array"].as_array().resize(8);
        value["array"].as_array().emplace_back(19);
        value["array"].as_array().emplace_back("an inserted string");
        value.as_object().try_emplace("a directly inserted key", 23);
        value.as_object()[json::string_type("a default mapped value")] = 24;
        add_typed_arrays(value);
        value["complex float"]
            .as_typed_array<std::complex<float>>()
            .emplace_back(9.f, 10.f);
        check_resource(value, &resource);

        const std::string text = "a new string";
        const auto& string = value["new"]["number"].as_string();
        CHECK(string == text && text == string);
        CHECK(string == "a new string" && "a new string" == string);
        CHECK(string == std::string_view(text));
        CHECK(std::string_view(text) == string);
        CHECK(std::string(value["new"]["number"]) == text);
        CHECK(std::string(string) == text);

        std::istringstream input(document);
        check_resource(json::parse(input, &resource), &resource);
        std::istringstream parser_input(document);
        json::JsonParser parser(json::JsonLexer(parser_input), &resource);
        check_resource(parser.parse(), &resource);
        check_resource(
            json::parse_file(std::string(TEST_SOURCE_DIR) + "/test.json",
                             &resource),
            &resource);

        auto array = json::Value::create_array(4, &resource);
        check_resource(array, &resource);
        auto typed =
            json::Value::create_typed_array<std::complex<float>>(&resource);
        typed.as_typed_array<std::complex<float>>().resize(10);
        check_resource(typed, &resource);
        auto empty = json::Value::create_object(&resource);
        check_resource(empty, &resource);
        const auto before = stats.allocations;
        json::Value null(&resource);
        CHECK(null.is_null() && stats.allocations == before);
        json::Value scalar(42, &resource);
        CHECK(scalar == 42 && stats.allocations == before + 1);
    }
    CHECK(stats.live_bytes == 0);
    CHECK(stats.allocations == stats.deallocations);
}

void test_value_assignment() {
    ResourceStats source_stats, target_stats;
    TrackingResource source_resource(source_stats),
        target_resource(target_stats);
    {
        auto source = json::parse(document, &source_resource);
        add_typed_arrays(source);
        auto copy = source;
        check_resource(copy, &source_resource);
        copy["tail"][0] = 99;
        CHECK(source["tail"][0] == 1);
        auto clone = source.clone(&target_resource);
        CHECK(clone == source);
        check_resource(clone, &target_resource);
        json::Value target(0, &target_resource);
        target = source;
        check_resource(target, &target_resource);
        CHECK(target == source);
        target = json::parse(document);
        check_resource(target, &target_resource);
        target = std::move(source);
        CHECK(source.is_null());
        CHECK(target == clone);
        check_resource(target, &target_resource);
        source = 7;
        CHECK(source == 7);
        check_resource(source, &source_resource);

        json::Value moved(std::move(target));
        CHECK(target.is_null());
        check_resource(moved, &target_resource);
        target = true;
        check_resource(target, &target_resource);

        auto parent =
            json::parse(R"({"child":{"answer":42}})", &source_resource);
        parent = std::move(parent["child"]);
        CHECK(parent["answer"] == 42);
        check_resource(parent, &source_resource);
        auto self_copy = &parent;
        parent = *self_copy;
        parent = std::move(*self_copy);
        CHECK(parent["answer"] == 42);
    }
    CHECK(source_stats.live_bytes == 0 && target_stats.live_bytes == 0);
}

void test_container_assignment() {
    ResourceStats first_stats, second_stats;
    TrackingResource first(first_stats), second(second_stats);
    {
        auto a = json::parse(document, &first);
        auto b = json::Value::create_object(&second);
        b.as_object() = a.as_object();
        check_resource(b, &second);
        b.as_object() = std::move(a.as_object());
        check_resource(b, &second);
        a.as_object().swap(b.as_object());
        CHECK(a.as_object().get_allocator().resource() == &second);
        for (const auto& [key, value] : a.as_object()) {
            CHECK(key.get_allocator().resource() == &second);
            check_resource(value, &second);
        }

        auto x = json::parse(R"([1,"a long string in a vector",{}])", &first);
        auto y = json::Value::create_array(1, &second);
        y.as_array() = x.as_array();
        check_resource(y, &second);
        y.as_array() = std::move(x.as_array());
        check_resource(y, &second);
        x.as_array().swap(y.as_array());
        CHECK(x.as_array().get_allocator().resource() == &second);
        for (const auto& value : x.as_array()) {
            check_resource(value, &second);
        }
        x.as_array().resize(8);
        check_resource(x[7], &second);
    }
    CHECK(first_stats.live_bytes == 0 && second_stats.live_bytes == 0);
}

void test_equivalent_resources() {
    ResourceStats stats;
    auto upstream = std::make_shared<TrackingResource>(stats);
    for (const bool assignment : {false, true}) {
        bool source_alive{}, target_alive{};
        {
            auto target_resource =
                std::make_shared<EquivalentResource>(upstream, target_alive);
            json::Value target(0, target_resource);
            {
                auto source_resource = std::make_shared<EquivalentResource>(
                    upstream, source_alive);
                CHECK(*source_resource == *target_resource);
                json::Value source(73, source_resource);
                if (assignment) {
                    target = std::move(source);
                } else {
                    json::Value moved(std::move(source), target_resource);
                    target = std::move(moved);
                }
                CHECK(source.is_null());
            }
            CHECK(!source_alive);
            CHECK(target == 73);
            CHECK(target.get_allocator().resource() == target_resource.get());
            target_resource.reset();
            CHECK(target_alive);
        }
        CHECK(!target_alive);
        CHECK(stats.live_bytes == 0);
    }
}

void test_escaped_storage() {
    auto child = [] {
        auto value = json::parse(R"({"child":["escaped child value",1]})");
        return json::Value(std::move(value["child"]));
    }();
    CHECK(child[0].as_string() == "escaped child value");
    child.as_array().emplace_back(2);
    check_resource(child, child.get_allocator().resource());
    auto string = [] {
        auto value = json::parse(R"("a string extracted from a temporary")");
        return std::move(value.as_string());
    }();
    string.append(100, '!');
    CHECK(string.size() > 100);
    auto array = [] {
        auto value = json::parse(R"([1,"an escaped array element"])");
        return std::move(value.as_array());
    }();
    array.emplace_back(3);
    CHECK(array[1].as_string() == "an escaped array element");
    auto object = [] {
        auto value = json::parse(R"({"key":"an escaped object element"})");
        return std::move(value.as_object());
    }();
    CHECK(object.at("key").as_string() == "an escaped object element");
    object.try_emplace("other", 4);
    auto typed = [] {
        auto value = json::Value::create_typed_array(
            std::vector<std::complex<double>>{{1., 2.}});
        return std::move(value.as_typed_array<std::complex<double>>());
    }();
    typed.emplace_back(3., 4.);
    CHECK(typed[0] == std::complex<double>(1., 2.));

    ResourceStats stats;
    TrackingResource upstream(stats);
    std::weak_ptr<json::Arena> weak;
    {
        auto escaped = [&] {
            auto arena = std::make_shared<json::Arena>(1024, &upstream);
            weak = arena;
            auto value = json::parse(document, arena);
            return json::Value(std::move(value["tail"]));
        }();
        CHECK(!weak.expired());
        CHECK(stats.live_bytes > 0);
        CHECK(escaped[2] == 3);
    }
    CHECK(weak.expired());
    CHECK(stats.live_bytes == 0);

    ResourceStats owned_stats;
    std::optional<json::Value> escaped;
    std::optional<json::string_type> escaped_string;
    std::optional<json::Value::array_container_type> escaped_array;
    {
        auto resource = std::make_shared<TrackingResource>(owned_stats);
        auto value = json::parse(R"({"text":"shared resource string","a":[1]})",
                                 resource);
        escaped.emplace(value);
        escaped_string.emplace(std::move(value["text"].as_string()));
        escaped_array.emplace(std::move(value["a"].as_array()));
    }
    CHECK(owned_stats.alive);
    escaped.reset();
    escaped_string.reset();
    CHECK(owned_stats.alive);
    escaped_array->emplace_back(2);
    escaped_array.reset();
    CHECK(!owned_stats.alive);
    CHECK(owned_stats.live_bytes == 0);
}

void test_descendant_assignment() {
    std::size_t allocations;
    {
        ResourceStats stats;
        TrackingResource resource(stats);
        auto source = json::parse(document);
        auto copy = source.clone(&resource);
        allocations = stats.allocations;
    }
    for (std::size_t limit = 0; limit <= allocations; ++limit) {
        ResourceStats source_stats, target_stats;
        TrackingResource source(source_stats), target(target_stats);
        {
            auto parent = json::Value::create_array(0, &target);
            auto foreign = json::Value::create_array(0, &source);
            foreign.as_array().push_back(json::parse(document, &source));
            // Swapping exposed containers can put a different resource in a
            // child. The parent payload still belongs to target.
            parent.as_array().swap(foreign.as_array());
            CHECK(parent[0].get_allocator().resource() == &source);
            const auto before = parent.dump();
            target.allow_allocations(limit);
            try {
                parent = std::move(parent[0]);
                CHECK(limit == allocations);
                CHECK(parent == json::parse(document));
                check_resource(parent, &target);
            } catch (const std::bad_alloc&) {
                CHECK(limit < allocations);
                CHECK(parent.dump() == before);
                check_resource(parent[0], &source);
            }
        }
        CHECK(source_stats.live_bytes == 0 && target_stats.live_bytes == 0);
    }
}

void test_allocation_failures() {
    std::size_t parse_allocations;
    {
        ResourceStats stats;
        TrackingResource resource(stats);
        auto value = json::parse(document, &resource);
        parse_allocations = stats.allocations;
    }
    for (std::size_t limit = 0; limit <= parse_allocations; ++limit) {
        ResourceStats stats;
        TrackingResource resource(stats);
        resource.allow_allocations(limit);
        try {
            auto value = json::parse(document, &resource);
            CHECK(limit == parse_allocations);
            check_resource(value, &resource);
        } catch (const std::bad_alloc&) { CHECK(limit < parse_allocations); }
        CHECK(stats.live_bytes == 0);
        CHECK(stats.allocations == stats.deallocations);
    }
    for (const auto* invalid :
         {R"({"x":[1,2,]})", R"({"x":{"y":1,}})", R"({"x":[1,2]} trailing)"}) {
        ResourceStats stats;
        TrackingResource resource(stats);
        test::throws<std::runtime_error>(
            [&] { (void)json::parse(invalid, &resource); });
        CHECK(stats.live_bytes == 0);
    }

    ResourceStats source_stats;
    TrackingResource source_resource(source_stats);
    {
        auto source = json::parse(document, &source_resource);
        add_typed_arrays(source);
        const auto original = source.dump();
        source_resource.allow_allocations(0);
        std::size_t clone_allocations;
        {
            ResourceStats stats;
            TrackingResource resource(stats);
            auto copy = source.clone(&resource);
            CHECK(copy == source);
            clone_allocations = stats.allocations;
        }
        for (std::size_t limit = 0; limit <= clone_allocations; ++limit) {
            ResourceStats stats;
            TrackingResource resource(stats);
            resource.allow_allocations(limit);
            try {
                auto copy = source.clone(&resource);
                CHECK(limit == clone_allocations);
                CHECK(copy == source);
            } catch (const std::bad_alloc&) {
                CHECK(limit < clone_allocations);
            }
            CHECK(stats.live_bytes == 0);
            CHECK(stats.allocations == stats.deallocations);

            resource.allow_all();
            for (const bool move : {false, true}) {
                auto input = source.clone(json::Value::allocator_type{});
                {
                    auto target =
                        json::parse(R"({"keep":[17,"old value"]})", &resource);
                    const auto before = target.dump();
                    resource.allow_allocations(limit);
                    try {
                        if (move) {
                            target = std::move(input);
                        } else {
                            target = input;
                        }
                        CHECK(limit == clone_allocations);
                        CHECK(target == source);
                        check_resource(target, &resource);
                        CHECK(!move || input.is_null());
                    } catch (const std::bad_alloc&) {
                        CHECK(limit < clone_allocations);
                        CHECK(target.dump() == before);
                        CHECK(input == source);
                    }
                    resource.allow_all();
                }
                CHECK(stats.live_bytes == 0);
                CHECK(stats.allocations == stats.deallocations);
            }
            CHECK(source.dump() == original);
        }
    }
    CHECK(source_stats.live_bytes == 0);
}

}  // namespace

int main() {
    return test::run("JSON allocator", [] {
        test_arena();
        test_allocator();
        test_parser_resources();
        test_value_assignment();
        test_container_assignment();
        test_equivalent_resources();
        test_escaped_storage();
        test_descendant_assignment();
        test_allocation_failures();
    });
}
