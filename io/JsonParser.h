#ifndef ZQ_JSON_PARSER
#define ZQ_JSON_PARSER

#include <complex>
#include <cstdint>
#include <functional>
#include <memory>  // unique_ptr
#include <ostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <vector>

namespace json {

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
struct String {
    std::string content;
};
template <typename T>
struct TypedArray {
    using value_type = T;
    std::vector<T> content;
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
    using object_container_type = std::unordered_map<std::string, Value>;
    using array_container_type = std::vector<Value>;
    Value();

    template <std::integral T>
    Value(T val) : Value(ValueCategory::NumberInt, new NumberInt{val}) {}

    template <std::floating_point T>
    Value(T val) : Value(ValueCategory::NumberFloat, new NumberFloat{val}) {}

    // Non-template overloads are needed for bool and strings: bool is an
    // integral type, so the template above would otherwise store `true` as
    // the number 1, and there was no way to build a String Value at all.
    Value(bool);
    Value(std::string);
    Value(const char*);

    Value clone() const;

    Value(const Value&);
    Value& operator=(const Value&);

    Value(Value&&) = default;
    Value& operator=(Value&&) = default;

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

    std::string& as_string();
    const std::string& as_string() const;

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
                *this = Value{                                                \
                    ValueCategory::NumberFloat,                               \
                    new NumberFloat{                                          \
                        static_cast<NumberInt*>(ptr.get())->content op val}}; \
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
                *this = Value{ValueCategory::NumberFloat, new NumberFloat{val}};
            }
        } else if (value_cat == ValueCategory::NumberFloat) {
            static_cast<NumberFloat*>(ptr.get())->content = val;
        } else {
            if constexpr (std::is_integral_v<std::remove_reference_t<T>>) {
                *this = Value{ValueCategory::NumberInt, new NumberInt{val}};
            } else {
                *this = Value{ValueCategory::NumberFloat, new NumberFloat{val}};
            }
        }
        return *this;
    }

    template <typename T>
    decltype(auto) operator=(
        T val) requires std::convertible_to<T, std::string> {
        if (value_cat == ValueCategory::String) {
            static_cast<String*>(ptr.get())->content = val;
        } else {
            *this = Value{ValueCategory::String, new String{val}};
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

    Value& operator[](const std::string&);
    Value& operator[](const char*);
    // on a const Value an object key cannot be auto-created; a missing key
    // throws instead (same as at())
    const Value& operator[](const std::string&) const;

    const Value& at(const std::string&) const;
    const Value& at(std::size_t) const;

    Value& at(const std::string&);
    Value& at(std::size_t);

    const object_container_type& as_object() const;
    const array_container_type& as_array() const;

    object_container_type& as_object();
    array_container_type& as_array();

    template <typename T>
    const std::vector<T>& as_typed_array() const {
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
    std::vector<T>& as_typed_array() {
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
    bool contains(const std::string& key) const;

    std::size_t size() const;
    bool empty() const;

    ValueCategory value_category() const;

    // unformatted output
    std::string dump() const;

    // formatted output
    std::string pretty_print(std::size_t = 0) const;

    static Value create_object();
    static Value create_array(std::size_t n = 0);

    template <typename T>
    static Value create_typed_array() {
        static_assert(
            std::is_same_v<T, std::complex<float>> ||
                std::is_same_v<T, std::complex<double>>,
            "create_typed_array only supports std::complex<float> and "
            "std::complex<double>");
        return {std::is_same_v<T, std::complex<float>>
                    ? ValueCategory::TypedArrayComplexFloat
                    : ValueCategory::TypedArrayComplexDouble,
                new TypedArray<T>};
    }

    template <typename T>
    static Value create_typed_array(std::vector<T> vec) {
        static_assert(
            std::is_same_v<T, std::complex<float>> ||
                std::is_same_v<T, std::complex<double>>,
            "create_typed_array only supports std::complex<float> and "
            "std::complex<double>");
        return {std::is_same_v<T, std::complex<float>>
                    ? ValueCategory::TypedArrayComplexFloat
                    : ValueCategory::TypedArrayComplexDouble,
                new TypedArray<T>{std::move(vec)}};
    }

   private:
    // internal constructor: takes ownership of the heap-allocated content
    // object; the deleter is type-erased, so passing a category that does
    // not match the dynamic type of T* would be UB
    template <typename T>
    Value(ValueCategory cat, T* raw_ptr)
        : ptr(raw_ptr,
              [](const void* data) { delete static_cast<const T*>(data); }),
          value_cat(cat) {}

    friend class JsonParser;

    std::unique_ptr<void, std::function<void(void*)>> ptr;
    ValueCategory value_cat;

    static void print_space(std::ostream&, std::size_t);
    static void dump_string(std::ostream&, const std::string&);
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
    JsonParser(JsonLexer&&);
    Value parse();

   private:
    JsonLexer lexer;
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

Value parse(std::string);
Value parse(std::istream&);
Value parse_file(std::string);

}  // namespace json

#ifdef ZQ_JSON_PARSER_IMPLEMENTATION

#include <cctype>
#include <cerrno>
#include <charconv>
#include <cmath>
#include <cstdlib>  // strtod, strtoll
#include <fstream>  // ifstream

namespace json {

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

Value::Value() : value_cat{} {}

Value::Value(const Value& other) : Value() { *this = other; }

// Deep copy of every category (the old version only accepted "plain" values
// and threw for Object/Array, which made `Value b = obj;` fail at runtime
// for no visible reason; use clone() only if you prefer to spell it out)
Value& Value::operator=(const Value& other) {
    if (this != &other) { *this = other.clone(); }
    return *this;
}

Value::Value(bool val) : Value(ValueCategory::Boolean, new Boolean{val}) {}
Value::Value(std::string val)
    : Value(ValueCategory::String, new String{std::move(val)}) {}
Value::Value(const char* val) : Value(std::string(val)) {}

Value& Value::operator=(bool val) {
    if (value_cat == ValueCategory::Boolean) {
        static_cast<Boolean*>(ptr.get())->content = val;
    } else {
        *this = Value{val};
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

std::string& Value::as_string() {
    expected_cat(ValueCategory::String);
    return static_cast<String*>(ptr.get())->content;
}
const std::string& Value::as_string() const {
    expected_cat(ValueCategory::String);
    return static_cast<String*>(ptr.get())->content;
}

Value::operator std::string() const {
    expected_cat(ValueCategory::String);
    return static_cast<String*>(ptr.get())->content;
}

Value& Value::operator[](const std::string& key) {
    return as_object()[key];
}
Value& Value::operator[](const char* key) {
    return as_object()[key];
}
const Value& Value::operator[](const std::string& key) const {
    return at(key);
}

const Value& Value::at(const std::string& key) const {
    try {
        return as_object().at(key);
    } catch (const std::out_of_range&) {
        // only wrap a missing key; type errors keep their real message
        throw std::runtime_error("Failed to access key: " + key);
    }
}
const Value& Value::at(std::size_t idx) const {
    try {
        return as_array().at(idx);
    } catch (const std::out_of_range&) {
        throw std::runtime_error("Failed to access index: " +
                                 std::to_string(idx));
    }
}

Value& Value::at(const std::string& key) {
    try {
        return as_object().at(key);
    } catch (const std::out_of_range&) {
        throw std::runtime_error("Failed to access key: " + key);
    }
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
bool Value::contains(const std::string& key) const {
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
    auto dump_typed_array =
        [&oss, &dump_complex_part]<typename T>(
            const std::vector<T>& typed_array) {
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
    auto print_typed_array =
        [&oss, indent, &dump_complex_part]<typename T>(
            const std::vector<T>& typed_array) {
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
    switch (value_cat) {
        case ValueCategory::NumberInt:
            return {value_cat, new NumberInt{
                                   static_cast<NumberInt*>(ptr.get())
                                       ->content}};
            break;
        case ValueCategory::NumberFloat:
            return {value_cat, new NumberFloat{as_number<double>()}};
            break;
        case ValueCategory::Boolean:
            return {value_cat, new Boolean{as_boolean()}};
            break;
        case ValueCategory::String:
            return {value_cat, new String{as_string()}};
            break;
        case ValueCategory::Array: {
            auto arr = new Array;
            for (const auto& value : as_array()) {
                arr->content.push_back(value.clone());
            }
            return {value_cat, arr};
        }
        case ValueCategory::Object: {
            auto obj = new Object;
            for (const auto& [key, val] : as_object()) {
                obj->content.emplace(key, val.clone());
            }
            return {value_cat, obj};
        }
        case ValueCategory::TypedArrayComplexFloat:
            return {value_cat, new TypedArray<std::complex<float>>{
                                   as_typed_array<std::complex<float>>()}};
        case ValueCategory::TypedArrayComplexDouble:
            return {value_cat, new TypedArray<std::complex<double>>{
                                   as_typed_array<std::complex<double>>()}};
        case ValueCategory::Null:
            break;
    }
    return {value_cat, new Null{}};
}

void Value::print_space(std::ostream& os, std::size_t indent) {
    for (std::size_t i = 0; i < indent; ++i) { os << ' '; }
}

void Value::dump_double(std::ostream& os, double val) {
    char buf[32];  // max_digits10 digits + sign + exponent always fits
    const auto result = std::to_chars(buf, buf + sizeof buf, val);
    os.write(buf, result.ptr - buf);
}

void Value::dump_string(std::ostream& os, const std::string& str) {
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

Value Value::create_object() {
    return {ValueCategory::Object, new Object{}};
}
Value Value::create_array(std::size_t n) {
    return {ValueCategory::Array, new Array{array_container_type(n)}};
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

JsonParser::JsonParser(JsonLexer&& json_lexer) : lexer(std::move(json_lexer)) {}

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
    Value result;
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
    return {ValueCategory::String, new String{token.content}};
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
    return {ValueCategory::NumberInt, new NumberInt{content}};
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
    return {ValueCategory::NumberFloat, new NumberFloat{content}};
}
Value JsonParser::parse_primitive(const JsonLexer::Token& token) {
    if (token.name != JsonLexer::TokenName::PRIMITIVE) {
        report_syntax_error(token);
    }
    if (token.content.front() == 'n') {
        return {ValueCategory::Null, new Null{}};
    }
    return {ValueCategory::Boolean, new Boolean{token.content.front() == 't'}};
}

Value JsonParser::parse_object() {
    auto obj = new Object;
    auto token = try_peek_from_lexer();
    // empty object
    if (token.name == JsonLexer::TokenName::BRACE_RIGHT) {
        try_get_from_lexer();
        return {ValueCategory::Object, obj};
    }
    while (true) {
        auto key = try_get_and_check(JsonLexer::TokenName::STRING);
        try_get_and_check(JsonLexer::TokenName::COLON);
        obj->content.emplace(key.content, parse_value());  // value
        token = try_get_from_lexer();
        if (token.name == JsonLexer::TokenName::BRACE_RIGHT) { break; }
        if (token.name != JsonLexer::TokenName::COMMA) {
            report_syntax_error(token);
        }
    }
    return {ValueCategory::Object, obj};
}

Value JsonParser::parse_array() {
    auto arr = new Array;
    auto token = try_peek_from_lexer();
    // empty array
    if (token.name == JsonLexer::TokenName::BRACKET_RIGHT) {
        try_get_from_lexer();
        return {ValueCategory::Array, arr};
    }
    while (true) {
        arr->content.emplace_back(parse_value());
        token = try_get_from_lexer();
        if (token.name == JsonLexer::TokenName::BRACKET_RIGHT) { break; }
        if (token.name != JsonLexer::TokenName::COMMA) {
            report_syntax_error(token);
        }
    }
    return {ValueCategory::Array, arr};
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

Value parse(std::istream& is) {
    return JsonParser{is}.parse();
}

Value parse(std::string str) {
    std::stringstream ss;
    ss.str(str);
    return parse(ss);
}

Value parse_file(std::string filename) {
    std::ifstream ifs(filename);  // Its destructor will close the file
    if (!ifs) { throw std::runtime_error("File " + filename + " not found"); }
    return JsonParser{JsonLexer{ifs, filename}}.parse();
}

}  // namespace json

#endif  // ZQ_JSON_PARSER_IMPLEMENTATION
#endif  // ZQ_JSON_PARSER
