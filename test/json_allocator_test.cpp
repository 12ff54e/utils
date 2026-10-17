#define ZQ_JSON_PARSER_IMPLEMENTATION
#include "io/JsonParser.h"

#include "test_support.h"

#include <array>
#include <cstring>

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

}  // namespace

int main() {
    return test::run("JSON allocator", [] {
        test_arena();
        test_allocator();
    });
}
