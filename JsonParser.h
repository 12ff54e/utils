#ifndef ZQ_JSON_PARSER
#define ZQ_JSON_PARSER

#include <complex>
#include <functional>
#include <memory>  // unique_ptr
#include <ostream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <variant>
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
    int content;
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
            throw std::runtime_error("Undefined Property");
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

    // Type information in deleter is stored during contruction
    template <typename T>
    Value(ValueCategory cat, T* raw_ptr)
        : ptr(raw_ptr,
              [](const void* data) { delete static_cast<const T*>(data); }),
          value_cat(cat) {}

    template <std::integral T>
    Value(T val) : Value(ValueCategory::NumberInt, new NumberInt{val}) {}

    template <std::floating_point T>
    Value(T val) : Value(ValueCategory::NumberFloat, new NumberFloat{val}) {}

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

#define define_compond_assignment_operator(op, a_op)                          \
    do {                                                                      \
        expected_cat(ValueCategory::NumberFloat, ValueCategory::NumberInt);   \
        if (value_cat == ValueCategory::NumberFloat) {                        \
            static_cast<NumberFloat*>(ptr.get())->content a_op val;           \
        } else {                                                              \
            if constexpr (std::is_integral_v<std::remove_reference_t<T>>) {   \
                static_cast<NumberInt*>(ptr.get())                            \
                    ->content a_op static_cast<int>(val);                     \
            } else {                                                          \
                *this = Value{                                                \
                    ValueCategory::NumberFloat,                               \
                    new NumberFloat{                                          \
                        static_cast<NumberInt*>(ptr.get())->content op val}}; \
            }                                                                 \
        }                                                                     \
    } while (0)

    template <typename T>
    void operator+=(const T& val) requires std::convertible_to<T, double> {
        define_compond_assignment_operator(+, +=);
    }
    template <typename T>
    void operator-=(const T& val) requires std::convertible_to<T, double> {
        define_compond_assignment_operator(-, -=);
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

    decltype(auto) operator[](std::integral auto idx) const {
        return as_array()[idx];
    }
    decltype(auto) operator[](std::integral auto idx) {
        return as_array()[idx];
    }

    Value& operator[](const std::string&);
    Value& operator[](const char*);

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
        return static_cast<TypedArray<T>*>(ptr.get())->content;
    }

    template <typename T>
    std::vector<T>& as_typed_array() {
        return static_cast<TypedArray<T>*>(ptr.get())->content;
    }

    // queries

    bool is_object() const;
    bool is_array() const;
    bool is_number() const;
    bool is_string() const;
    bool is_boolean() const;

    std::size_t size() const;
    std::size_t empty() const;

    ValueCategory value_category() const;

    // unformatted output
    std::string dump() const;

    // formatted output
    std::string pretty_print(std::size_t = 0) const;

    static Value create_object();
    static Value create_array(std::size_t n = 0);

    template <typename T>
    static Value create_typed_array() {
        if constexpr (std::is_same_v<T, std::complex<double>>) {
            return {ValueCategory::TypedArrayComplexDouble, new TypedArray<T>};
        } else if constexpr (std::is_same_v<T, std::complex<float>>) {
            return {ValueCategory::TypedArrayComplexFloat, new TypedArray<T>};
        }
    }

    template <typename T>
    static Value create_typed_array(std::vector<T> vec) {
        if constexpr (std::is_same_v<T, std::complex<double>>) {
            return {ValueCategory::TypedArrayComplexDouble,
                    new TypedArray<T>{vec}};
        } else if constexpr (std::is_same_v<T, std::complex<float>>) {
            return {ValueCategory::TypedArrayComplexFloat,
                    new TypedArray<T>{vec}};
        }
    }

   private:
    std::unique_ptr<void, std::function<void(void*)>> ptr;
    ValueCategory value_cat;

    static void print_space(std::ostream&, std::size_t);
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
    // any char that can be in a float number
    static bool is_digit(char c);
    static bool is_digit_start(char c);
    // tab, lf, cr or space
    static bool is_whitespace(char c);

    void report_lexical_error() const;
};

std::ostream& operator<<(std::ostream& os, const JsonLexer::Token& token);

struct JsonParser {
    JsonParser(JsonLexer&&);
    Value parse();

   private:
    JsonLexer lexer;

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

#include <cstdlib>  // atof, atoi
#include <cstring>  // strcmp, strlen
#include <fstream>  // ifstream

namespace json {

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

Value::Value(const Value& other) : value_cat{} {
    // value_cat is initialized as Null to prevent accessing non allocated
    // address
    *this = other;
}

Value& Value::operator=(const Value& other) {
    other.expected_cat(ValueCategory::Boolean, ValueCategory::NumberFloat,
                       ValueCategory::NumberInt, ValueCategory::String);
    // modify content directly with "plain value"
    if (value_cat == other.value_cat) {
#define PROCESS_TYPE(t)                                      \
    case (ValueCategory::t):                                 \
        static_cast<t*>(ptr.get())->content =                \
            static_cast<const t*>(other.ptr.get())->content; \
        break;
        switch (value_cat) {
            PROCESS_TYPE(Boolean)
            PROCESS_TYPE(NumberFloat)
            PROCESS_TYPE(NumberInt)
            PROCESS_TYPE(String)
            default:
                break;
        }
#undef PROCESS_TYPE
    } else {
        *this = other.clone();
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
    return static_cast<Boolean*>(ptr.get())->content;
};

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

const Value& Value::at(const std::string& key) const {
    try {
        return as_object().at(key);
    } catch (std::exception& e) {
        std::ostringstream oss;
        oss << "Failed to accessing key: " << key;
        throw std::runtime_error(oss.str());
    }
}
const Value& Value::at(std::size_t idx) const {
    try {
        return as_array().at(idx);
    } catch (std::exception& e) {
        std::ostringstream oss;
        oss << "Failed to accessing index: " << idx;
        throw std::runtime_error(oss.str());
    }
}

Value& Value::at(const std::string& key) {
    try {
        return as_object().at(key);
    } catch (std::exception& e) {
        std::ostringstream oss;
        oss << "Failed to accessing key: " << key;
        throw std::runtime_error(oss.str());
    }
}
Value& Value::at(std::size_t idx) {
    try {
        return as_array().at(idx);
    } catch (std::exception& e) {
        std::ostringstream oss;
        oss << "Failed to accessing index: " << idx;
        throw std::runtime_error(oss.str());
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
std::size_t Value::empty() const {
    expected_cat(ValueCategory::Object, ValueCategory::Array);
    if (value_cat == ValueCategory::Object) {
        return static_cast<Object*>(ptr.get())->content.empty();
    } else {
        return static_cast<Array*>(ptr.get())->content.empty();
    }
}

ValueCategory Value::value_category() const {
    return value_cat;
}

std::string Value::dump() const {
    std::ostringstream oss;
    auto dump_typed_array =
        [&oss]<typename T>(const std::vector<T>& typed_array) {
            oss << '[';
            for (std::size_t i = 0; i < typed_array.size(); ++i) {
                const auto& val = typed_array[i];
                if (i) { oss << ','; }
                oss << '[' << val.real() << ',' << val.imag() << ']';
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
            oss << as_number<int>();
            break;
        case ValueCategory::NumberFloat:
            oss << as_number<double>();
            break;
        case ValueCategory::String:
            oss << '"' << static_cast<std::string>(*this) << '"';
            break;
        case ValueCategory::Object:
            oss << '{';
            for (const auto& [key, val] : as_object()) {
                oss << '"' << key << '"' << ':' << val.dump() << ',';
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
    auto print_typed_array =
        [&oss, indent]<typename T>(const std::vector<T>& typed_array) {
            oss << "[\n";
            for (std::size_t i = 0; i < typed_array.size(); ++i) {
                const auto& val = typed_array[i];
                if (i) { oss << ",\n"; }
                print_space(oss, indent + 4);
                oss << '[' << val.real() << ", " << val.imag() << ']';
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
                oss << '"' << key << '"' << ": " << val.pretty_print(indent + 4)
                    << ",\n";
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
            return {value_cat, new NumberInt{as_number<int>()}};
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
        // a string
        buffer.name = TokenName::STRING;
        buffer.col = col;
        ++col;
        while (is_.get(c) && c != '"') {
            buffer.content.push_back(c);
            ++col;
        }
        ++col;
    } else if (is_digit_start(c)) {
        // a number
        bool is_float{};
        buffer.col = col;
        do {
            buffer.content.push_back(c);
            is_float |= c == '.';
            ++col;
        } while (is_.get(c) && is_digit(c));
        buffer.name = is_float ? TokenName::FLOAT : TokenName::INTEGER;
        is_.unget();
    } else if (c == 't' || c == 'f') {
        // a boolean
        buffer.name = TokenName::PRIMITIVE;
        char expected[6] = "true";
        if (c == 'f') { std::strcpy(expected, "false"); }
        char tmp[6] = {c};
        is_.readsome(tmp + 1, std::strlen(expected) - 1);
        if (std::strcmp(tmp, expected) == 0) {
            buffer.content = expected;
        } else {
            report_lexical_error();
        }
    } else if (c == 'n') {
        // null
        buffer.name = TokenName::PRIMITIVE;
        char tmp[5] = {c};
        is_.readsome(tmp + 1, 3);
        if (std::strcmp(tmp, "null") == 0) {
            buffer.content = "null";
        } else {
            report_lexical_error();
        }
    } else {
        report_lexical_error();
    }
    buffer.row = row;
    is_buffer_full = true;
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
    std::ostringstream oss;
    oss << filename << ':' << row << ':' << col
        << ": error: unrecognized token";
    throw std::runtime_error(oss.str());
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
    switch (token.name) {
        case JsonLexer::TokenName::STRING:
            return parse_string(token);
        case JsonLexer::TokenName::INTEGER:
            return parse_int(token);
        case JsonLexer::TokenName::FLOAT:
            return parse_float(token);
        case JsonLexer::TokenName::BRACE_LEFT:
            return parse_object();
        case JsonLexer::TokenName::BRACKET_LEFT:
            return parse_array();
        case JsonLexer::TokenName::PRIMITIVE:
            return parse_primitive(token);
        default:
            report_syntax_error(token);
            return Value{};  // unreachable
    }
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
    return {ValueCategory::NumberInt,
            new NumberInt{std::atoi(token.content.c_str())}};
}
Value JsonParser::parse_float(const JsonLexer::Token& token) {
    if (token.name != JsonLexer::TokenName::FLOAT) {
        report_syntax_error(token);
    }
    return {ValueCategory::NumberFloat,
            new NumberFloat{std::atof(token.content.c_str())}};
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
        << ": error: unexpected content '" << token.content << '\'';
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
