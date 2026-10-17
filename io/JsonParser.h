#ifndef ZQ_JSON_PARSER
#define ZQ_JSON_PARSER

#include <algorithm>
#include <complex>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <iterator>
#include <limits>
#include <memory>  // unique_ptr
#include <memory_resource>
#include <new>
#include <ostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

namespace json {

// Bump allocation in blocks. Individual deallocations do nothing; destruction
// or release() returns the blocks to upstream. Upstream must outlive this
// arena.
class Arena : public std::pmr::memory_resource {
   public:
    explicit Arena(
        std::size_t block_size = 4096,
        std::pmr::memory_resource* upstream = std::pmr::get_default_resource());
    ~Arena() override;
    Arena(const Arena&) = delete;
    Arena& operator=(const Arena&) = delete;
    Arena(Arena&&) = delete;
    Arena& operator=(Arena&&) = delete;

    // All objects using this arena's storage must be destroyed before release.
    void release() noexcept;
    std::pmr::memory_resource* upstream_resource() const { return upstream_; }

   private:
    struct Block {
        Block* next;
        std::size_t bytes;
        void* cursor;
        std::size_t available;
    };
    Block* head_{};
    std::size_t block_size_;
    std::pmr::memory_resource* upstream_;

    void* do_allocate(std::size_t bytes, std::size_t alignment) override;
    void do_deallocate(void*, std::size_t, std::size_t) override {}
    bool do_is_equal(
        const std::pmr::memory_resource& other) const noexcept override {
        return this == &other;
    }
};

// STL allocator backed by a memory_resource. The default owns an Arena; a raw
// resource pointer borrows it, and a shared_ptr shares ownership with the
// caller.
template <typename T>
class Allocator {
   public:
    using value_type = T;
    using propagate_on_container_copy_assignment = std::false_type;
    using propagate_on_container_move_assignment = std::false_type;
    using propagate_on_container_swap = std::true_type;
    using is_always_equal = std::false_type;

    Allocator() : resource_(std::make_shared<Arena>()) {}
    Allocator(std::pmr::memory_resource* resource)
        : resource_(std::shared_ptr<std::pmr::memory_resource>{}, resource) {
        if (!resource) { throw std::invalid_argument("Null JSON resource"); }
    }
    template <std::derived_from<std::pmr::memory_resource> Resource>
    Allocator(std::shared_ptr<Resource> resource)
        : resource_(std::move(resource)) {
        if (!resource_) { throw std::invalid_argument("Null JSON resource"); }
    }
    Allocator(const Allocator&) noexcept = default;
    Allocator& operator=(const Allocator&) noexcept = default;
    // Keep moved-from containers and values usable.
    Allocator(Allocator&& other) noexcept : resource_(other.resource_) {}
    Allocator& operator=(Allocator&& other) noexcept {
        resource_ = other.resource_;
        return *this;
    }
    template <typename U>
    Allocator(const Allocator<U>& other) noexcept
        : resource_(other.resource_) {}

    T* allocate(std::size_t n) {
        if (n > std::numeric_limits<std::size_t>::max() / sizeof(T)) {
            throw std::bad_array_new_length();
        }
        return static_cast<T*>(resource_->allocate(n * sizeof(T), alignof(T)));
    }
    void deallocate(T* data, std::size_t n) {
        resource_->deallocate(data, n * sizeof(T), alignof(T));
    }
    template <typename U, typename... Args>
    void construct(U* data, Args&&... args) {
        std::uninitialized_construct_using_allocator(
            data, *this, std::forward<Args>(args)...);
    }
    std::pmr::memory_resource* resource() const noexcept {
        return resource_.get();
    }

    template <typename U>
    bool operator==(const Allocator<U>& other) const noexcept {
        return *resource_ == *other.resource_;
    }

   private:
    template <typename>
    friend class Allocator;
    std::shared_ptr<std::pmr::memory_resource> resource_;
};

enum class ValueCategory {
    Null = 0,
    NumberInt,
    NumberFloat,
    Boolean,
    String,
    Array,
    Object,
    TypedArrayComplexDouble,
    TypedArrayComplexFloat
};

const char* get_value_category_name(ValueCategory);

struct Null {};
struct NumberInt {
    int64_t content;
};
struct NumberFloat {
    double content;
};
struct Boolean {
    bool content;
};
using string_type =
    std::basic_string<char, std::char_traits<char>, Allocator<char>>;

// Allow comparisons with std::string as well as literals and string_view.
inline bool operator==(const string_type& lhs, std::string_view rhs) noexcept {
    return std::string_view(lhs) == rhs;
}

struct StringHash {
    using is_transparent = void;
    std::size_t operator()(std::string_view key) const noexcept {
        return std::hash<std::string_view>{}(key);
    }
};
struct StringEqual {
    using is_transparent = void;
    bool operator()(std::string_view lhs, std::string_view rhs) const noexcept {
        return lhs == rhs;
    }
};

struct String {
    string_type content;
};
template <typename T>
struct TypedArray {
    using value_type = T;
    using container_type = std::vector<T, Allocator<T>>;
    container_type content;
};

/**
 * @brief Stores either a JSON object, array, number or string
 *
 */
struct Value {
   private:
    template <typename... Ts>
    void expected_cat(Ts... cats) const
        requires(std::same_as<Ts, ValueCategory>&&...) {
        if (value_cat == ValueCategory::Null) {
            throw std::runtime_error(
                "Undefined property (value is Null; accessing a missing key "
                "via operator[] or a default-constructed Value)");
        }
        if (((value_cat != cats) && ...)) {
            std::ostringstream oss;
            if constexpr (sizeof...(Ts) == 1) {
                oss << "Incorrect JSON type, requires: ";
            } else {
                oss << "Incorrect JSON type, requires one of: ";
            }
            ((oss << get_value_category_name(cats) << ", "), ...)
                << "actually: " << get_value_category_name(value_cat);
            throw std::runtime_error(oss.str());
        }
    }

   public:
    using allocator_type = Allocator<std::byte>;
    using string_type = json::string_type;
    using object_container_type =
        std::unordered_map<string_type,
                           Value,
                           StringHash,
                           StringEqual,
                           Allocator<std::pair<const string_type, Value>>>;
    using array_container_type = std::vector<Value, Allocator<Value>>;
    Value();
    explicit Value(allocator_type);
    explicit Value(std::pmr::memory_resource* resource)
        : Value(allocator_type(resource)) {}

    template <std::integral T>
    Value(T val, allocator_type allocator = {})
        : Value(make_node_<NumberInt>(ValueCategory::NumberInt,
                                      allocator,
                                      NumberInt{static_cast<int64_t>(val)})) {}

    template <std::floating_point T>
    Value(T val, allocator_type allocator = {})
        : Value(
              make_node_<NumberFloat>(ValueCategory::NumberFloat,
                                      allocator,
                                      NumberFloat{static_cast<double>(val)})) {}

    // bool needs its own overload because it also satisfies std::integral.
    Value(bool, allocator_type = {});
    Value(std::string, allocator_type = {});
    Value(std::string_view, allocator_type = {});
    Value(const char*, allocator_type = {});

    // Copies share the resource, but own independent contents.
    Value clone() const;
    Value clone(allocator_type) const;
    allocator_type get_allocator() const noexcept { return allocator_; }

    Value(const Value&);
    Value(const Value&, allocator_type);
    Value(Value&&) noexcept;
    Value(Value&&, allocator_type);
    // Assignments keep the destination allocator.
    Value& operator=(const Value&);
    // The move source must not contain the destination.
    Value& operator=(Value&&);

    // Used by allocator-aware containers, including default array elements.
    Value(std::allocator_arg_t, allocator_type allocator)
        : Value(std::move(allocator)) {}
    template <typename T>
    Value(std::allocator_arg_t, allocator_type allocator, T&& val)
        : Value(std::forward<T>(val), std::move(allocator)) {}

    // NOTE: Can not cast to any integer type due to an annoying C builtin
    // operator[](ptrdiff_t, const char*), as far as I still want to support
    // operator[] for getting object property

    operator double() const;
    operator std::string() const;
    // without this, `if (v)` would go through operator double() and throw
    // for non-numbers; truthy means "holds a value" (i.e. not Null).
    // explicit so that `bool b = v;` still requires as_boolean()
    explicit operator bool() const;

    bool as_boolean() const;

    template <typename T>
    T as_number()
        const requires std::is_arithmetic_v<std::remove_reference_t<T>> {
        expected_cat(ValueCategory::NumberFloat, ValueCategory::NumberInt);
        if (value_cat == ValueCategory::NumberFloat) {
            return static_cast<NumberFloat*>(ptr.get())->content;
        } else {
            return static_cast<NumberInt*>(ptr.get())->content;
        }
        return T{};
    }

    string_type& as_string();
    const string_type& as_string() const;

    template <typename T>
    bool operator<(
        T val) const requires std::is_arithmetic_v<std::remove_reference_t<T>> {
        expected_cat(ValueCategory::NumberFloat, ValueCategory::NumberInt);
        return as_number<double>() < val;
    }

    template <typename T>
    bool operator==(
        T val) const requires std::is_arithmetic_v<std::remove_reference_t<T>> {
        expected_cat(ValueCategory::NumberFloat, ValueCategory::NumberInt);
        return as_number<double>() == val;
    }

    // category and content equality; operator!= is synthesized from it.
    // The non-template overload wins for exact matches, so `v == 5` still
    // resolves to the arithmetic template above
    bool operator==(const Value&) const;

#define define_compond_assignment_operator(op, a_op)                          \
    do {                                                                      \
        expected_cat(ValueCategory::NumberFloat, ValueCategory::NumberInt);   \
        if (value_cat == ValueCategory::NumberFloat) {                        \
            static_cast<NumberFloat*>(ptr.get())->content a_op val;           \
        } else {                                                              \
            if constexpr (std::is_integral_v<std::remove_reference_t<T>>) {   \
                static_cast<NumberInt*>(ptr.get())                            \
                    ->content a_op static_cast<int64_t>(val);                 \
            } else {                                                          \
                *this =                                                       \
                    Value{static_cast<NumberInt*>(ptr.get())->content op val, \
                          allocator_};                                        \
            }                                                                 \
        }                                                                     \
    } while (0)

    template <typename T>
    Value& operator+=(const T& val) requires std::convertible_to<T, double> {
        define_compond_assignment_operator(+, +=);
        return *this;
    }
    template <typename T>
    Value& operator-=(const T& val) requires std::convertible_to<T, double> {
        define_compond_assignment_operator(-, -=);
        return *this;
    }
#undef define_compond_assignment_operator

    template <typename T>
    decltype(auto) operator=(T val) requires std::convertible_to<T, double> {
        if (value_cat == ValueCategory::NumberInt) {
            if constexpr (std::is_integral_v<std::remove_reference_t<T>>) {
                static_cast<NumberInt*>(ptr.get())->content = val;
            } else {
                *this = Value{static_cast<double>(val), allocator_};
            }
        } else if (value_cat == ValueCategory::NumberFloat) {
            static_cast<NumberFloat*>(ptr.get())->content = val;
        } else {
            if constexpr (std::is_integral_v<std::remove_reference_t<T>>) {
                *this = Value{static_cast<int64_t>(val), allocator_};
            } else {
                *this = Value{static_cast<double>(val), allocator_};
            }
        }
        return *this;
    }

    template <typename T>
    Value& operator=(const T& val)
        requires(std::convertible_to<const T&, std::string_view> ||
                 std::convertible_to<const T&, std::string>)
    {
        if constexpr (std::convertible_to<const T&, std::string_view>) {
            assign_string_(std::string_view(val));
        } else {
            assign_string_(std::string(val));
        }
        return *this;
    }

    // bool is convertible to double, so the arithmetic operator= above would
    // store `true` as the number 1; this overload wins for exact matches
    Value& operator=(bool);

    decltype(auto) operator[](std::integral auto idx) const {
        // checked like the key overload: out-of-range throws instead of UB
        try {
            return as_array().at(idx);
        } catch (const std::out_of_range&) {
            throw std::runtime_error("Failed to access index: " +
                                     std::to_string(idx));
        }
    }
    decltype(auto) operator[](std::integral auto idx) {
        try {
            return as_array().at(idx);
        } catch (const std::out_of_range&) {
            throw std::runtime_error("Failed to access index: " +
                                     std::to_string(idx));
        }
    }

    Value& operator[](std::string_view);
    Value& operator[](const char*);
    // on a const Value an object key cannot be auto-created; a missing key
    // throws instead (same as at())
    const Value& operator[](std::string_view) const;
    const Value& operator[](const char*) const;

    const Value& at(std::string_view) const;
    const Value& at(std::size_t) const;

    Value& at(std::string_view);
    Value& at(std::size_t);

    const object_container_type& as_object() const;
    const array_container_type& as_array() const;

    object_container_type& as_object();
    array_container_type& as_array();

    template <typename T>
    const typename TypedArray<T>::container_type& as_typed_array() const {
        static_assert(
            std::is_same_v<T, std::complex<float>> ||
                std::is_same_v<T, std::complex<double>>,
            "as_typed_array only supports std::complex<float> and "
            "std::complex<double>");
        expected_cat(std::is_same_v<T, std::complex<float>>
                         ? ValueCategory::TypedArrayComplexFloat
                         : ValueCategory::TypedArrayComplexDouble);
        return static_cast<TypedArray<T>*>(ptr.get())->content;
    }

    template <typename T>
    typename TypedArray<T>::container_type& as_typed_array() {
        static_assert(
            std::is_same_v<T, std::complex<float>> ||
                std::is_same_v<T, std::complex<double>>,
            "as_typed_array only supports std::complex<float> and "
            "std::complex<double>");
        expected_cat(std::is_same_v<T, std::complex<float>>
                         ? ValueCategory::TypedArrayComplexFloat
                         : ValueCategory::TypedArrayComplexDouble);
        return static_cast<TypedArray<T>*>(ptr.get())->content;
    }

    // queries

    bool is_object() const;
    bool is_array() const;
    bool is_number() const;
    bool is_string() const;
    bool is_boolean() const;
    bool is_null() const;
    bool is_typed_array() const;
    // false when the value is not an object
    bool contains(std::string_view key) const;

    std::size_t size() const;
    bool empty() const;

    ValueCategory value_category() const;

    // unformatted output
    std::string dump() const;

    // formatted output
    std::string pretty_print(std::size_t = 0) const;

    static Value create_object(allocator_type = {});
    static Value create_array(std::size_t n = 0, allocator_type = {});

    template <typename T>
    static Value create_typed_array(allocator_type allocator = {}) {
        static_assert(
            std::is_same_v<T, std::complex<float>> ||
                std::is_same_v<T, std::complex<double>>,
            "create_typed_array only supports std::complex<float> and "
            "std::complex<double>");
        return make_node_<TypedArray<T>>(
            std::is_same_v<T, std::complex<float>>
                ? ValueCategory::TypedArrayComplexFloat
                : ValueCategory::TypedArrayComplexDouble,
            allocator,
            TypedArray<T>{typename TypedArray<T>::container_type(allocator)});
    }

    template <typename T, typename SourceAllocator>
    static Value create_typed_array(std::vector<T, SourceAllocator> vec,
                                    allocator_type allocator = {}) {
        static_assert(
            std::is_same_v<T, std::complex<float>> ||
                std::is_same_v<T, std::complex<double>>,
            "create_typed_array only supports std::complex<float> and "
            "std::complex<double>");
        Value result = create_typed_array<T>(allocator);
        auto& content = result.as_typed_array<T>();
        if constexpr (std::same_as<SourceAllocator, Allocator<T>>) {
            content = std::move(vec);
        } else {
            content.assign(std::make_move_iterator(vec.begin()),
                           std::make_move_iterator(vec.end()));
        }
        return result;
    }

   private:
    struct NodeDeleter {
        std::pmr::memory_resource* resource{};
        void (*destroy)(void*, std::pmr::memory_resource*) noexcept {};
        void operator()(void* data) const noexcept { destroy(data, resource); }
    };

    template <typename T, typename... Args>
    static Value make_node_(ValueCategory cat,
                            allocator_type allocator,
                            Args&&... args) {
        Value result(allocator);
        Allocator<T> node_allocator(allocator);
        auto* node = node_allocator.allocate(1);
        try {
            std::construct_at(node, std::forward<Args>(args)...);
        } catch (...) {
            node_allocator.deallocate(node, 1);
            throw;
        }
        result.ptr = std::unique_ptr<void, NodeDeleter>(
            node,
            NodeDeleter{
                allocator.resource(),
                [](void* data, std::pmr::memory_resource* resource) noexcept {
                    auto* typed = static_cast<T*>(data);
                    std::destroy_at(typed);
                    resource->deallocate(typed, sizeof(T), alignof(T));
                }});
        result.value_cat = cat;
        return result;
    }

    friend struct JsonParser;

    // The resource owner must outlive payload and container destruction.
    allocator_type allocator_;
    std::unique_ptr<void, NodeDeleter> ptr{nullptr, NodeDeleter{}};
    ValueCategory value_cat = ValueCategory::Null;

    void assign_string_(std::string_view);
    void take_payload_(Value&) noexcept;
    void swap_payload_(Value&) noexcept;
    void clear_() noexcept;

    static void print_space(std::ostream&, std::size_t);
    static void dump_string(std::ostream&, std::string_view);
    // shortest round-trip representation, e.g. "0.1" not "0.10000000000000001"
    static void dump_double(std::ostream&, double);
};

struct Object {
    Value::object_container_type content;
};
struct Array {
    Value::array_container_type content;
};

struct JsonLexer {
    enum class TokenName {
        END_OF_FILE,
        STRING,
        INTEGER,
        FLOAT,
        PRIMITIVE,
        BRACE_LEFT = '{',
        BRACE_RIGHT = '}',
        BRACKET_LEFT = '[',
        BRACKET_RIGHT = ']',
        COLON = ':',
        COMMA = ',',
    };
    struct Token {
        TokenName name;
        std::string content;
        int row;
        int col;
    };

    JsonLexer(std::istream&, std::string = {});

    Token get_token();
    Token peek_token();

    const std::string& get_filename() const;

    operator bool() const;

   private:
    std::istream& is_;
    std::string filename;
    Token buffer{};
    int row;
    int col;
    bool is_buffer_full{};
    bool is_buffer_output{};

    void read_token_to_buffer();
    // reads the token body of a string (the opening quote is already
    // consumed); unescapes escapes, so the token content is the decoded value
    void read_string_token();
    // any char that can be in a float number
    static bool is_digit(char c);
    static bool is_digit_start(char c);
    // tab, lf, cr or space
    static bool is_whitespace(char c);

    void report_lexical_error() const;
    void report_lexical_error(const std::string& message) const;
    static void append_utf8(std::string&, uint32_t);
};

std::ostream& operator<<(std::ostream& os, const JsonLexer::Token& token);

struct JsonParser {
    using allocator_type = Value::allocator_type;
    JsonParser(JsonLexer&&, allocator_type = {});
    Value parse();

   private:
    JsonLexer lexer;
    allocator_type allocator_;
    // guards against stack overflow on hostile/degenerate input
    static constexpr std::size_t MAX_NESTING_DEPTH = 512;
    std::size_t depth{};

    Value parse_value();
    Value parse_string(const JsonLexer::Token&);
    Value parse_int(const JsonLexer::Token&);
    Value parse_float(const JsonLexer::Token&);
    Value parse_primitive(const JsonLexer::Token&);
    Value parse_object();
    Value parse_array();

    JsonLexer::Token try_get_and_check(JsonLexer::TokenName);
    JsonLexer::Token try_get_from_lexer(bool = false);
    JsonLexer::Token try_peek_from_lexer();
    void report_syntax_error(const JsonLexer::Token& = {});
};

Value parse(std::string, Value::allocator_type = {});
Value parse(std::istream&, Value::allocator_type = {});
Value parse_file(std::string, Value::allocator_type = {});

}  // namespace json

#ifdef ZQ_JSON_PARSER_IMPLEMENTATION

#include <cctype>
#include <cerrno>
#include <charconv>
#include <cmath>
#include <cstdlib>  // strtod, strtoll
#include <fstream>  // ifstream

namespace json {

Arena::Arena(std::size_t block_size, std::pmr::memory_resource* upstream)
    : block_size_(block_size), upstream_(upstream) {
    if (!block_size || !upstream) {
        throw std::invalid_argument("Arena needs a block size and upstream");
    }
}

Arena::~Arena() {
    release();
}

void Arena::release() noexcept {
    while (head_) {
        auto* block = head_;
        head_ = block->next;
        const auto bytes = block->bytes;
        std::destroy_at(block);
        upstream_->deallocate(block, bytes, alignof(Block));
    }
}

void* Arena::do_allocate(std::size_t bytes, std::size_t alignment) {
    bytes = std::max(bytes, std::size_t{1});
    const auto try_block = [&]() -> void* {
        if (!head_) { return nullptr; }
        if (std::align(alignment, bytes, head_->cursor, head_->available)) {
            auto* result = static_cast<std::byte*>(head_->cursor);
            head_->cursor = result + bytes;
            head_->available -= bytes;
            return result;
        }
        return nullptr;
    };
    if (auto* result = try_block()) { return result; }

    const auto capacity = std::max(bytes, block_size_);
    constexpr auto max_size = std::numeric_limits<std::size_t>::max();
    if (alignment - 1 > max_size - sizeof(Block) ||
        capacity > max_size - sizeof(Block) - (alignment - 1)) {
        throw std::bad_alloc();
    }
    const auto total = sizeof(Block) + capacity + alignment - 1;
    auto* storage =
        static_cast<std::byte*>(upstream_->allocate(total, alignof(Block)));
    head_ = std::construct_at(reinterpret_cast<Block*>(storage), head_, total,
                              storage + sizeof(Block), total - sizeof(Block));
    return try_block();
}

// Shared by the lexer (unescaping "\b", "\n", ...) and Value::dump_string
// (escaping the same characters back): `letter` is the character following
// the backslash, `decoded` the character it stands for
constexpr struct StringEscape {
    char letter;
    char decoded;
} string_escape_table[] = {
    {'b', '\b'}, {'f', '\f'}, {'n', '\n'}, {'r', '\r'}, {'t', '\t'},
};

const char* get_value_category_name(ValueCategory val_cat) {
#define PROCESS_CAT_NAME(p) \
    case (p):               \
        return #p;          \
        break;
    switch (val_cat) {
        PROCESS_CAT_NAME(ValueCategory::Null)
        PROCESS_CAT_NAME(ValueCategory::NumberInt)
        PROCESS_CAT_NAME(ValueCategory::NumberFloat)
        PROCESS_CAT_NAME(ValueCategory::Boolean)
        PROCESS_CAT_NAME(ValueCategory::String)
        PROCESS_CAT_NAME(ValueCategory::Array)
        PROCESS_CAT_NAME(ValueCategory::Object)
        PROCESS_CAT_NAME(ValueCategory::TypedArrayComplexFloat)
        PROCESS_CAT_NAME(ValueCategory::TypedArrayComplexDouble)
    }
#undef PROCESS_CAT_NAME
    return "";  // unreachable
}

Value::Value() : Value(allocator_type{}) {}
Value::Value(allocator_type allocator) : allocator_(std::move(allocator)) {}

Value::Value(const Value& other) : Value(other.clone()) {}
Value::Value(const Value& other, allocator_type allocator)
    : Value(other.clone(std::move(allocator))) {}

Value::Value(Value&& other) noexcept
    : allocator_(other.allocator_),
      ptr(std::move(other.ptr)),
      value_cat(std::exchange(other.value_cat, ValueCategory::Null)) {}

Value::Value(Value&& other, allocator_type allocator)
    : Value(std::move(allocator)) {
    if (allocator_ == other.allocator_) {
        take_payload_(other);
    } else {
        auto copy = other.clone(allocator_);
        other.clear_();
        swap_payload_(copy);
    }
}

Value& Value::operator=(const Value& other) {
    if (this != &other) {
        auto copy = other.clone(allocator_);
        swap_payload_(copy);
    }
    return *this;
}

Value& Value::operator=(Value&& other) {
    if (this != &other) {
        // Stage the source before destroying this payload: it may be a child.
        Value moved(std::move(other), allocator_);
        swap_payload_(moved);
    }
    return *this;
}

void Value::clear_() noexcept {
    ptr.reset();
    value_cat = ValueCategory::Null;
}

void Value::take_payload_(Value& other) noexcept {
    ptr = std::move(other.ptr);
    ptr.get_deleter().resource = allocator_.resource();
    value_cat = std::exchange(other.value_cat, ValueCategory::Null);
}

void Value::swap_payload_(Value& other) noexcept {
    ptr.swap(other.ptr);
    std::swap(value_cat, other.value_cat);
}

Value::Value(bool val, allocator_type allocator)
    : Value(make_node_<Boolean>(ValueCategory::Boolean,
                                allocator,
                                Boolean{val})) {}
Value::Value(std::string val, allocator_type allocator)
    : Value(std::string_view(val), std::move(allocator)) {}
Value::Value(std::string_view val, allocator_type allocator)
    : Value(make_node_<String>(
          ValueCategory::String,
          allocator,
          String{string_type(val, Allocator<char>(allocator))})) {}
Value::Value(const char* val, allocator_type allocator)
    : Value(std::string_view(val), std::move(allocator)) {}

void Value::assign_string_(std::string_view val) {
    if (value_cat == ValueCategory::String) {
        static_cast<String*>(ptr.get())->content.assign(val);
    } else {
        *this = Value{val, allocator_};
    }
}

Value& Value::operator=(bool val) {
    if (value_cat == ValueCategory::Boolean) {
        static_cast<Boolean*>(ptr.get())->content = val;
    } else {
        *this = Value{val, allocator_};
    }
    return *this;
}

Value::operator double() const {
    expected_cat(ValueCategory::NumberFloat, ValueCategory::NumberInt);
    if (value_cat == ValueCategory::NumberFloat) {
        return static_cast<NumberFloat*>(ptr.get())->content;
    } else {
        return static_cast<NumberInt*>(ptr.get())->content;
    }
}

bool Value::as_boolean() const {
    expected_cat(ValueCategory::Boolean);
    return static_cast<Boolean*>(ptr.get())->content;
};

Value::operator bool() const {
    return value_cat != ValueCategory::Null;
}

bool Value::operator==(const Value& other) const {
    // NumberInt and NumberFloat compare as double, so 1 == 1.0
    if (is_number() && other.is_number()) {
        return as_number<double>() == other.as_number<double>();
    }
    if (value_cat != other.value_cat) { return false; }
    switch (value_cat) {
        case ValueCategory::Null:
            return true;
        case ValueCategory::Boolean:
            return as_boolean() == other.as_boolean();
        case ValueCategory::String:
            return as_string() == other.as_string();
        case ValueCategory::Array: {
            const auto& lhs = as_array();
            const auto& rhs = other.as_array();
            if (lhs.size() != rhs.size()) { return false; }
            for (std::size_t i = 0; i < lhs.size(); ++i) {
                if (!(lhs[i] == rhs[i])) { return false; }
            }
            return true;
        }
        case ValueCategory::Object: {
            const auto& lhs = as_object();
            const auto& rhs = other.as_object();
            if (lhs.size() != rhs.size()) { return false; }
            for (const auto& [key, val] : lhs) {
                const auto it = rhs.find(key);
                if (it == rhs.end() || !(val == it->second)) { return false; }
            }
            return true;
        }
        case ValueCategory::TypedArrayComplexFloat:
            return as_typed_array<std::complex<float>>() ==
                   other.as_typed_array<std::complex<float>>();
        case ValueCategory::TypedArrayComplexDouble:
            return as_typed_array<std::complex<double>>() ==
                   other.as_typed_array<std::complex<double>>();
        case ValueCategory::NumberInt:
        case ValueCategory::NumberFloat:
            return false;  // unreachable, handled by the number check above
    }
    return false;  // unreachable
}

Value::string_type& Value::as_string() {
    expected_cat(ValueCategory::String);
    return static_cast<String*>(ptr.get())->content;
}
const Value::string_type& Value::as_string() const {
    expected_cat(ValueCategory::String);
    return static_cast<String*>(ptr.get())->content;
}

Value::operator std::string() const {
    const auto& str = as_string();
    return std::string(str.data(), str.size());
}

Value& Value::operator[](std::string_view key) {
    auto& object = as_object();
    if (auto found = object.find(key); found != object.end()) {
        return found->second;
    }
    return object.try_emplace(string_type(key, object.get_allocator()))
        .first->second;
}
Value& Value::operator[](const char* key) {
    return operator[](std::string_view(key));
}
const Value& Value::operator[](std::string_view key) const {
    return at(key);
}
const Value& Value::operator[](const char* key) const {
    return at(key);
}

const Value& Value::at(std::string_view key) const {
    const auto& object = as_object();
    if (auto found = object.find(key); found != object.end()) {
        return found->second;
    }
    throw std::runtime_error("Failed to access key: " + std::string(key));
}
const Value& Value::at(std::size_t idx) const {
    try {
        return as_array().at(idx);
    } catch (const std::out_of_range&) {
        throw std::runtime_error("Failed to access index: " +
                                 std::to_string(idx));
    }
}

Value& Value::at(std::string_view key) {
    return const_cast<Value&>(std::as_const(*this).at(key));
}
Value& Value::at(std::size_t idx) {
    try {
        return as_array().at(idx);
    } catch (const std::out_of_range&) {
        throw std::runtime_error("Failed to access index: " +
                                 std::to_string(idx));
    }
}

const Value::object_container_type& Value::as_object() const {
    expected_cat(ValueCategory::Object);
    return static_cast<Object*>(ptr.get())->content;
}
const Value::array_container_type& Value::as_array() const {
    expected_cat(ValueCategory::Array);
    return static_cast<Array*>(ptr.get())->content;
}

Value::object_container_type& Value::as_object() {
    expected_cat(ValueCategory::Object);
    return static_cast<Object*>(ptr.get())->content;
}
Value::array_container_type& Value::as_array() {
    expected_cat(ValueCategory::Array);
    return static_cast<Array*>(ptr.get())->content;
}

bool Value::is_object() const {
    return value_cat == ValueCategory::Object;
}
bool Value::is_array() const {
    return value_cat == ValueCategory::Array;
}
bool Value::is_number() const {
    return value_cat == ValueCategory::NumberInt ||
           value_cat == ValueCategory::NumberFloat;
};
bool Value::is_string() const {
    return value_cat == ValueCategory::String;
}
bool Value::is_boolean() const {
    return value_cat == ValueCategory::Boolean;
}
bool Value::is_null() const {
    return value_cat == ValueCategory::Null;
}
bool Value::is_typed_array() const {
    return value_cat == ValueCategory::TypedArrayComplexFloat ||
           value_cat == ValueCategory::TypedArrayComplexDouble;
}
bool Value::contains(std::string_view key) const {
    return is_object() && as_object().contains(key);
}

std::size_t Value::size() const {
    expected_cat(ValueCategory::Object, ValueCategory::Array,
                 ValueCategory::TypedArrayComplexFloat,
                 ValueCategory::TypedArrayComplexDouble);
    switch (value_cat) {
        case ValueCategory::Object:
            return static_cast<Object*>(ptr.get())->content.size();
        case ValueCategory::Array:
            return static_cast<Array*>(ptr.get())->content.size();
        case ValueCategory::TypedArrayComplexFloat:
            return static_cast<TypedArray<std::complex<float>>*>(ptr.get())
                ->content.size();
        case ValueCategory::TypedArrayComplexDouble:
            return static_cast<TypedArray<std::complex<double>>*>(ptr.get())
                ->content.size();
        default:
            return 0;  // unreachable
    }
}
bool Value::empty() const {
    // same category set as size()
    expected_cat(ValueCategory::Object, ValueCategory::Array,
                 ValueCategory::TypedArrayComplexFloat,
                 ValueCategory::TypedArrayComplexDouble);
    switch (value_cat) {
        case ValueCategory::Object:
            return static_cast<Object*>(ptr.get())->content.empty();
        case ValueCategory::Array:
            return static_cast<Array*>(ptr.get())->content.empty();
        case ValueCategory::TypedArrayComplexFloat:
            return as_typed_array<std::complex<float>>().empty();
        case ValueCategory::TypedArrayComplexDouble:
            return as_typed_array<std::complex<double>>().empty();
        default:
            return true;  // unreachable
    }
}

ValueCategory Value::value_category() const {
    return value_cat;
}

std::string Value::dump() const {
    std::ostringstream oss;
    // keeps the element type of the typed array: float parts would
    // otherwise be printed with all the digits of their double promotion
    auto dump_complex_part = [&oss]<typename U>(U part) {
        char buf[32];
        const auto result = std::to_chars(buf, buf + sizeof buf, part);
        oss.write(buf, result.ptr - buf);
    };
    auto dump_typed_array = [&oss, &dump_complex_part]<typename T, typename A>(
                                const std::vector<T, A>& typed_array) {
        oss << '[';
        for (std::size_t i = 0; i < typed_array.size(); ++i) {
            const auto& val = typed_array[i];
            if (i) { oss << ','; }
            oss << '[';
            dump_complex_part(val.real());
            oss << ',';
            dump_complex_part(val.imag());
            oss << ']';
        }
        oss << ']';
    };

    switch (value_cat) {
        case ValueCategory::Null:
            oss << "null";
            break;
        case ValueCategory::Boolean:
            oss << (as_boolean() ? "true" : "false");
            break;
        case ValueCategory::NumberInt:
            oss << static_cast<NumberInt*>(ptr.get())->content;
            break;
        case ValueCategory::NumberFloat:
            dump_double(oss, static_cast<NumberFloat*>(ptr.get())->content);
            break;
        case ValueCategory::String:
            dump_string(oss, static_cast<String*>(ptr.get())->content);
            break;
        case ValueCategory::Object:
            oss << '{';
            for (const auto& [key, val] : as_object()) {
                dump_string(oss, key);
                oss << ':' << val.dump() << ',';
            }
            if (!empty()) { oss.seekp(-1, std::ios_base::cur); }
            oss << '}';
            break;
        case ValueCategory::Array:
            oss << '[';
            for (std::size_t i = 0; i < size(); ++i) {
                if (i) { oss << ','; }
                oss << operator[](i).dump();
            }
            oss << ']';
            break;
        case ValueCategory::TypedArrayComplexFloat:
            dump_typed_array(as_typed_array<std::complex<float>>());
            break;
        case ValueCategory::TypedArrayComplexDouble:
            dump_typed_array(as_typed_array<std::complex<double>>());
            break;
    }
    return oss.str();
}

std::string Value::pretty_print(std::size_t indent) const {
    std::ostringstream oss;
    auto dump_complex_part = [&oss]<typename U>(U part) {
        char buf[32];
        const auto result = std::to_chars(buf, buf + sizeof buf, part);
        oss.write(buf, result.ptr - buf);
    };
    auto print_typed_array = [&oss, indent,
                              &dump_complex_part]<typename T, typename A>(
                                 const std::vector<T, A>& typed_array) {
        oss << "[\n";
        for (std::size_t i = 0; i < typed_array.size(); ++i) {
            const auto& val = typed_array[i];
            if (i) { oss << ",\n"; }
            print_space(oss, indent + 4);
            oss << '[';
            dump_complex_part(val.real());
            oss << ", ";
            dump_complex_part(val.imag());
            oss << ']';
        }
        if (typed_array.empty()) {
            oss.seekp(-1, std::ios_base::cur);
            oss << ' ';
        } else {
            oss << '\n';
            print_space(oss, indent);
        }
        oss << ']';
    };

    switch (value_cat) {
        case ValueCategory::Null:
        case ValueCategory::NumberInt:
        case ValueCategory::NumberFloat:
        case ValueCategory::Boolean:
        case ValueCategory::String:
            oss << dump();
            break;
        case ValueCategory::Object:
            oss << "{\n";
            for (const auto& [key, val] : as_object()) {
                print_space(oss, indent + 4);
                dump_string(oss, key);
                oss << ": " << val.pretty_print(indent + 4) << ",\n";
            }
            if (empty()) {
                oss.seekp(-1, std::ios_base::cur);
                oss << ' ';
            } else {
                oss.seekp(-2, std::ios_base::cur);
                oss << '\n';
                print_space(oss, indent);
            }
            oss << '}';
            break;
        case ValueCategory::Array:
            oss << "[\n";
            for (std::size_t i = 0; i < size(); ++i) {
                if (i) { oss << ",\n"; }
                print_space(oss, indent + 4);
                oss << operator[](i).pretty_print(indent + 4);
            }
            if (empty()) {
                oss.seekp(-1, std::ios_base::cur);
                oss << ' ';
            } else {
                oss << '\n';
                print_space(oss, indent);
            }
            oss << ']';
            break;
        case ValueCategory::TypedArrayComplexFloat:
            print_typed_array(as_typed_array<std::complex<float>>());
            break;
        case ValueCategory::TypedArrayComplexDouble:
            print_typed_array(as_typed_array<std::complex<double>>());
            break;
    }
    return oss.str();
}

Value Value::clone() const {
    return clone(allocator_);
}

Value Value::clone(allocator_type allocator) const {
    switch (value_cat) {
        case ValueCategory::NumberInt:
            return Value(as_number<int64_t>(), allocator);
        case ValueCategory::NumberFloat:
            return Value(as_number<double>(), allocator);
        case ValueCategory::Boolean:
            return Value(as_boolean(), allocator);
        case ValueCategory::String:
            return Value(std::string_view(as_string()), allocator);
        case ValueCategory::Array: {
            auto result = create_array(0, allocator);
            auto& array = result.as_array();
            array.reserve(size());
            for (const auto& value : as_array()) { array.emplace_back(value); }
            return result;
        }
        case ValueCategory::Object: {
            auto result = create_object(allocator);
            auto& object = result.as_object();
            object.reserve(size());
            for (const auto& [key, value] : as_object()) {
                object.emplace(key, value);
            }
            return result;
        }
        case ValueCategory::TypedArrayComplexFloat: {
            auto result = create_typed_array<std::complex<float>>(allocator);
            const auto& source = as_typed_array<std::complex<float>>();
            result.as_typed_array<std::complex<float>>().assign(source.begin(),
                                                                source.end());
            return result;
        }
        case ValueCategory::TypedArrayComplexDouble: {
            auto result = create_typed_array<std::complex<double>>(allocator);
            const auto& source = as_typed_array<std::complex<double>>();
            result.as_typed_array<std::complex<double>>().assign(source.begin(),
                                                                 source.end());
            return result;
        }
        case ValueCategory::Null:
            return Value(allocator);
    }
    return Value(allocator);
}

void Value::print_space(std::ostream& os, std::size_t indent) {
    for (std::size_t i = 0; i < indent; ++i) { os << ' '; }
}

void Value::dump_double(std::ostream& os, double val) {
    char buf[32];  // max_digits10 digits + sign + exponent always fits
    const auto result = std::to_chars(buf, buf + sizeof buf, val);
    os.write(buf, result.ptr - buf);
}

void Value::dump_string(std::ostream& os, std::string_view str) {
    os << '"';
    for (const char ch : str) {
        if (ch == '"' || ch == '\\') {
            os << '\\' << ch;
            continue;
        }
        if (static_cast<unsigned char>(ch) >= 0x20) {
            os << ch;
            continue;
        }
        // a control character: the shared table or \u00XX as a fallback
        char letter = '\0';
        for (const auto& escape : string_escape_table) {
            if (escape.decoded == ch) { letter = escape.letter; break; }
        }
        if (letter != '\0') {
            os << '\\' << letter;
        } else {
            constexpr char hex[] = "0123456789abcdef";
            os << "\\u00" << hex[static_cast<unsigned char>(ch) >> 4]
               << hex[static_cast<unsigned char>(ch) & 0xF];
        }
    }
    os << '"';
}

Value Value::create_object(allocator_type allocator) {
    return make_node_<Object>(ValueCategory::Object, allocator,
                              Object{object_container_type(allocator)});
}
Value Value::create_array(std::size_t n, allocator_type allocator) {
    return make_node_<Array>(
        ValueCategory::Array, allocator,
        Array{array_container_type(n, Allocator<Value>(allocator))});
}

JsonLexer::JsonLexer(std::istream& is, std::string file_name)
    : is_(is), filename(file_name), row(1), col(1) {}

JsonLexer::Token JsonLexer::get_token() {
    if (!is_buffer_full || is_buffer_output) { read_token_to_buffer(); }
    is_buffer_output = true;
    return buffer;
}

JsonLexer::Token JsonLexer::peek_token() {
    if (!is_buffer_full || is_buffer_output) { read_token_to_buffer(); }
    is_buffer_output = false;
    return buffer;
}

const std::string& JsonLexer::get_filename() const {
    return filename;
}

void JsonLexer::read_token_to_buffer() {
    char c;
    // skip all whitespaces
    while (is_.get(c) && is_whitespace(c)) {
        ++col;
        if (c == '\n') {
            ++row;
            col = 1;
        }
    }
    buffer.content.clear();
    if (!is_) {
        buffer.name = TokenName::END_OF_FILE;
        buffer.col = 0;
    } else if (c == static_cast<char>(TokenName::BRACE_LEFT) ||
               c == static_cast<char>(TokenName::BRACE_RIGHT) ||
               c == static_cast<char>(TokenName::BRACKET_LEFT) ||
               c == static_cast<char>(TokenName::BRACKET_RIGHT) ||
               c == static_cast<char>(TokenName::COLON) ||
               c == static_cast<char>(TokenName::COMMA)) {
        buffer.name = static_cast<TokenName>(c);
        buffer.content.push_back(c);
        buffer.col = col;
        ++col;
    } else if (c == '"') {
        read_string_token();
    } else if (is_digit_start(c)) {
        // a number; strict grammar is validated when the value is parsed
        bool is_float{};
        buffer.col = col;
        do {
            buffer.content.push_back(c);
            is_float |= c == '.' || c == 'e' || c == 'E';
            ++col;
        } while (is_.get(c) && is_digit(c));
        buffer.name = is_float ? TokenName::FLOAT : TokenName::INTEGER;
        is_.unget();
    } else if (c == 't' || c == 'f' || c == 'n') {
        // a primitive: true, false or null; read char by char instead of
        // readsome, which returns 0 on some streams (e.g. synced stdin)
        buffer.name = TokenName::PRIMITIVE;
        buffer.col = col;
        const char* expected =
            c == 't' ? "true" : (c == 'f' ? "false" : "null");
        buffer.content.push_back(c);
        ++col;
        for (std::size_t i = 1; expected[i] != '\0'; ++i) {
            if (!is_.get(c) || c != expected[i]) { report_lexical_error(); }
            buffer.content.push_back(c);
            ++col;
        }
    } else {
        report_lexical_error();
    }
    buffer.row = row;
    is_buffer_full = true;
}

void JsonLexer::read_string_token() {
    char c;
    // reads 4 hex digits after a '\u'; returns UINT32_MAX on malformed input
    auto read_hex4 = [this, &c]() -> uint32_t {
        uint32_t value = 0;
        for (int i = 0; i < 4; ++i) {
            if (!is_.get(c)) { return UINT32_MAX; }
            ++col;
            value <<= 4;
            if (c >= '0' && c <= '9') {
                value |= static_cast<uint32_t>(c - '0');
            } else if (c >= 'a' && c <= 'f') {
                value |= static_cast<uint32_t>(c - 'a' + 10);
            } else if (c >= 'A' && c <= 'F') {
                value |= static_cast<uint32_t>(c - 'A' + 10);
            } else {
                return UINT32_MAX;
            }
        }
        return value;
    };

    buffer.name = TokenName::STRING;
    buffer.col = col;
    ++col;  // opening quote
    while (is_.get(c) && c != '"') {
        ++col;
        if (c == '\\') {
            if (!is_.get(c)) { report_lexical_error("unterminated string"); }
            ++col;
            switch (c) {
                case '"':
                case '\\':
                case '/':
                    buffer.content.push_back(c);
                    break;
                case 'u': {
                    ++col;  // the 'u'
                    uint32_t code_point = read_hex4();
                    if (code_point == UINT32_MAX) {
                        report_lexical_error("invalid \\u escape in string");
                    }
                    if (code_point >= 0xD800 && code_point <= 0xDBFF) {
                        // high surrogate, must be followed by a low one
                        if (!is_.get(c) || c != '\\' || !is_.get(c) ||
                            c != 'u') {
                            report_lexical_error(
                                "unpaired surrogate in string");
                        }
                        ++col;  // the backslash
                        ++col;  // the 'u'
                        const uint32_t low = read_hex4();
                        if (low == UINT32_MAX || low < 0xDC00 ||
                            low > 0xDFFF) {
                            report_lexical_error(
                                "unpaired surrogate in string");
                        }
                        code_point =
                            0x10000 + ((code_point - 0xD800) << 10) +
                            (low - 0xDC00);
                    } else if (code_point >= 0xDC00 &&
                               code_point <= 0xDFFF) {
                        report_lexical_error("unpaired surrogate in string");
                    }
                    append_utf8(buffer.content, code_point);
                    break;
                }
                default: {
                    // the common escapes: \b \f \n \r \t (shared with
                    // Value::dump_string); '\0' never decodes to a value
                    char decoded = '\0';
                    for (const auto& escape : string_escape_table) {
                        if (escape.letter == c) {
                            decoded = escape.decoded;
                            break;
                        }
                    }
                    if (decoded == '\0') {
                        report_lexical_error(
                            "invalid escape sequence in string");
                    }
                    buffer.content.push_back(decoded);
                }
            }
        } else if (static_cast<unsigned char>(c) < 0x20) {
            // JSON strings may not contain raw control characters
            report_lexical_error("control character in string");
        } else {
            buffer.content.push_back(c);
        }
    }
    if (!is_) { report_lexical_error("unterminated string"); }
    ++col;  // closing quote
}

JsonLexer::operator bool() const {
    return !is_.fail();
}
bool JsonLexer::is_digit(char c) {
    return c == '.' || c == 'E' || c == 'e' || is_digit_start(c);
}
bool JsonLexer::is_digit_start(char c) {
    return (c >= '0' && c <= '9') || c == '-' || c == '+';
}
bool JsonLexer::is_whitespace(char c) {
    return c == '\t' || c == '\n' || c == '\r' || c == ' ';
}

void JsonLexer::report_lexical_error() const {
    report_lexical_error("unrecognized token");
}
void JsonLexer::report_lexical_error(const std::string& message) const {
    std::ostringstream oss;
    oss << filename << ':' << row << ':' << col << ": error: " << message;
    throw std::runtime_error(oss.str());
}

void JsonLexer::append_utf8(std::string& out, uint32_t code_point) {
    if (code_point <= 0x7F) {
        out.push_back(static_cast<char>(code_point));
    } else if (code_point <= 0x7FF) {
        out.push_back(static_cast<char>(0xC0 | (code_point >> 6)));
        out.push_back(static_cast<char>(0x80 | (code_point & 0x3F)));
    } else if (code_point <= 0xFFFF) {
        out.push_back(static_cast<char>(0xE0 | (code_point >> 12)));
        out.push_back(static_cast<char>(0x80 | ((code_point >> 6) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | (code_point & 0x3F)));
    } else {
        out.push_back(static_cast<char>(0xF0 | (code_point >> 18)));
        out.push_back(static_cast<char>(0x80 | ((code_point >> 12) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | ((code_point >> 6) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | (code_point & 0x3F)));
    }
}

JsonParser::JsonParser(JsonLexer&& json_lexer, allocator_type allocator)
    : lexer(std::move(json_lexer)), allocator_(std::move(allocator)) {}

Value JsonParser::parse() {
    if (!lexer) { report_syntax_error(); }  // token list is empty
    auto value = parse_value();
    try_get_from_lexer(true);  // Check if lexer ends
    return value;
}

Value JsonParser::parse_value() {
    auto token = try_get_from_lexer();
    if (depth >= MAX_NESTING_DEPTH) {
        std::ostringstream oss;
        oss << lexer.get_filename() << ':' << token.row << ':' << token.col
            << ": error: JSON nesting too deep (exceeds "
            << MAX_NESTING_DEPTH << ')';
        throw std::runtime_error(oss.str());
    }
    ++depth;
    Value result(allocator_);
    switch (token.name) {
        case JsonLexer::TokenName::STRING:
            result = parse_string(token);
            break;
        case JsonLexer::TokenName::INTEGER:
            result = parse_int(token);
            break;
        case JsonLexer::TokenName::FLOAT:
            result = parse_float(token);
            break;
        case JsonLexer::TokenName::BRACE_LEFT:
            result = parse_object();
            break;
        case JsonLexer::TokenName::BRACKET_LEFT:
            result = parse_array();
            break;
        case JsonLexer::TokenName::PRIMITIVE:
            result = parse_primitive(token);
            break;
        default:
            report_syntax_error(token);  // unreachable
    }
    --depth;
    return result;
}

Value JsonParser::parse_string(const JsonLexer::Token& token) {
    if (token.name != JsonLexer::TokenName::STRING) {
        report_syntax_error(token);
    }
    return Value(token.content, allocator_);
}

Value JsonParser::parse_int(const JsonLexer::Token& token) {
    if (token.name != JsonLexer::TokenName::INTEGER) {
        report_syntax_error(token);
    }
    // strict JSON integer: '-'? digit+ (no leading zero, no trailing junk).
    // atoi/strtol silently stop at the first invalid character, so the whole
    // token is validated first: "1e5" or "1.2" must not parse as the int 1.
    const char* str = token.content.c_str();
    const char* p = str;
    if (*p == '-') { ++p; }
    if (!std::isdigit(static_cast<unsigned char>(*p))) {
        report_syntax_error(token);
    }
    if (*p == '0') {
        ++p;
        if (std::isdigit(static_cast<unsigned char>(*p))) {
            report_syntax_error(token);
        }
    } else {
        while (std::isdigit(static_cast<unsigned char>(*p))) { ++p; }
    }
    if (*p != '\0') { report_syntax_error(token); }
    errno = 0;
    const long long content = std::strtoll(str, nullptr, 10);
    if (errno == ERANGE) { report_syntax_error(token); }
    return Value(content, allocator_);
}
Value JsonParser::parse_float(const JsonLexer::Token& token) {
    if (token.name != JsonLexer::TokenName::FLOAT) {
        report_syntax_error(token);
    }
    // strict JSON number: '-'? digit+ ('.' digit+)? (('e'|'E') ('+'|'-')?
    // digit+)? with no leading zero and no trailing junk, so that "1.2.3"
    // or "1e" is rejected instead of silently truncated by strtod.
    const char* str = token.content.c_str();
    const char* p = str;
    if (*p == '-') { ++p; }
    if (!std::isdigit(static_cast<unsigned char>(*p))) {
        report_syntax_error(token);
    }
    if (*p == '0') {
        ++p;
        if (std::isdigit(static_cast<unsigned char>(*p))) {
            report_syntax_error(token);
        }
    } else {
        while (std::isdigit(static_cast<unsigned char>(*p))) { ++p; }
    }
    if (*p == '.') {
        ++p;
        if (!std::isdigit(static_cast<unsigned char>(*p))) {
            report_syntax_error(token);
        }
        while (std::isdigit(static_cast<unsigned char>(*p))) { ++p; }
    }
    if (*p == 'e' || *p == 'E') {
        ++p;
        if (*p == '+' || *p == '-') { ++p; }
        if (!std::isdigit(static_cast<unsigned char>(*p))) {
            report_syntax_error(token);
        }
        while (std::isdigit(static_cast<unsigned char>(*p))) { ++p; }
    }
    if (*p != '\0') { report_syntax_error(token); }
    errno = 0;
    const double content = std::strtod(str, nullptr);
    if (errno == ERANGE || !std::isfinite(content)) {
        report_syntax_error(token);
    }
    return Value(content, allocator_);
}
Value JsonParser::parse_primitive(const JsonLexer::Token& token) {
    if (token.name != JsonLexer::TokenName::PRIMITIVE) {
        report_syntax_error(token);
    }
    if (token.content.front() == 'n') { return Value(allocator_); }
    return Value(token.content.front() == 't', allocator_);
}

Value JsonParser::parse_object() {
    auto obj = Value::create_object(allocator_);
    auto token = try_peek_from_lexer();
    // empty object
    if (token.name == JsonLexer::TokenName::BRACE_RIGHT) {
        try_get_from_lexer();
        return obj;
    }
    while (true) {
        auto key = try_get_and_check(JsonLexer::TokenName::STRING);
        try_get_and_check(JsonLexer::TokenName::COLON);
        obj.as_object().emplace(key.content, parse_value());
        token = try_get_from_lexer();
        if (token.name == JsonLexer::TokenName::BRACE_RIGHT) { break; }
        if (token.name != JsonLexer::TokenName::COMMA) {
            report_syntax_error(token);
        }
    }
    return obj;
}

Value JsonParser::parse_array() {
    auto arr = Value::create_array(0, allocator_);
    auto token = try_peek_from_lexer();
    // empty array
    if (token.name == JsonLexer::TokenName::BRACKET_RIGHT) {
        try_get_from_lexer();
        return arr;
    }
    while (true) {
        arr.as_array().emplace_back(parse_value());
        token = try_get_from_lexer();
        if (token.name == JsonLexer::TokenName::BRACKET_RIGHT) { break; }
        if (token.name != JsonLexer::TokenName::COMMA) {
            report_syntax_error(token);
        }
    }
    return arr;
}

JsonLexer::Token JsonParser::try_get_and_check(
    JsonLexer::TokenName expected_token_name) {
    auto token = lexer.get_token();
    if (token.name != expected_token_name) { report_syntax_error(token); }
    return token;
}

JsonLexer::Token JsonParser::try_get_from_lexer(bool end_expected) {
    auto token = lexer.get_token();
    if (end_expected ^ (token.name == JsonLexer::TokenName::END_OF_FILE)) {
        report_syntax_error(token);
    }
    return token;
}

JsonLexer::Token JsonParser::try_peek_from_lexer() {
    auto token = lexer.peek_token();
    if (token.name == JsonLexer::TokenName::END_OF_FILE) {
        report_syntax_error(token);
    }
    return token;
}

void JsonParser::report_syntax_error(const JsonLexer::Token& token) {
    std::ostringstream oss;
    oss << lexer.get_filename() << ':' << token.row << ':' << token.col
        << ": error: ";
    if (token.name == JsonLexer::TokenName::END_OF_FILE) {
        // the EOF token has no content; "''" would read as a parse bug
        // instead of a truncated document
        oss << "unexpected end of input";
    } else {
        oss << "unexpected content '" << token.content << '\'';
    }
    throw std::runtime_error(oss.str());
}

std::ostream& operator<<(std::ostream& os, const JsonLexer::Token& token) {
    return os << "{ Name: " << ([&token] {
#define PROCESS_TOKEN_NAME(p) \
    case (p):                 \
        return #p;            \
        break;
               switch (token.name) {
                   PROCESS_TOKEN_NAME(JsonLexer::TokenName::PRIMITIVE)
                   PROCESS_TOKEN_NAME(JsonLexer::TokenName::STRING)
                   PROCESS_TOKEN_NAME(JsonLexer::TokenName::INTEGER)
                   PROCESS_TOKEN_NAME(JsonLexer::TokenName::FLOAT)
                   PROCESS_TOKEN_NAME(JsonLexer::TokenName::BRACE_LEFT)
                   PROCESS_TOKEN_NAME(JsonLexer::TokenName::BRACE_RIGHT)
                   PROCESS_TOKEN_NAME(JsonLexer::TokenName::BRACKET_LEFT)
                   PROCESS_TOKEN_NAME(JsonLexer::TokenName::BRACKET_RIGHT)
                   PROCESS_TOKEN_NAME(JsonLexer::TokenName::COLON)
                   PROCESS_TOKEN_NAME(JsonLexer::TokenName::COMMA)
                   PROCESS_TOKEN_NAME(JsonLexer::TokenName::END_OF_FILE)
               }
#undef PROCESS_TOKEN_NAME
               return "(No such name)";  // unreachable
           })()
              << ", Content: '" << token.content << "', position: ("
              << token.row << ", " << token.col << ')' << " }";
}

Value parse(std::istream& is, Value::allocator_type allocator) {
    return JsonParser{is, std::move(allocator)}.parse();
}

Value parse(std::string str, Value::allocator_type allocator) {
    std::stringstream ss;
    ss.str(str);
    return parse(ss, std::move(allocator));
}

Value parse_file(std::string filename, Value::allocator_type allocator) {
    std::ifstream ifs(filename);  // Its destructor will close the file
    if (!ifs) { throw std::runtime_error("File " + filename + " not found"); }
    return JsonParser{JsonLexer{ifs, filename}, std::move(allocator)}.parse();
}

}  // namespace json

#endif  // ZQ_JSON_PARSER_IMPLEMENTATION
#endif  // ZQ_JSON_PARSER
