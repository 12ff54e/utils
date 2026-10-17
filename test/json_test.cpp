#define ZQ_JSON_PARSER_IMPLEMENTATION
#include "io/JsonParser.h"

#include "test_support.h"

#include <limits>
#include <utility>

namespace {

using json::parse;
using json::Value;
using json::ValueCategory;

void test_values() {
    const Value empty;
    CHECK(empty.is_null() && !empty);
    CHECK(empty.dump() == "null");
    CHECK(parse("true").is_boolean() && parse("true").as_boolean());
    CHECK(!parse("false").as_boolean());
    CHECK(bool(parse("0")) && bool(parse("false")) && bool(parse("{}")));
    CHECK(Value{true}.value_category() == ValueCategory::Boolean);
    CHECK(Value{"text"}.as_string() == "text");
    CHECK(Value{std::string("owned")}.as_string() == "owned");

    CHECK(parse("-9223372036854775808").as_number<int64_t>() ==
          std::numeric_limits<int64_t>::min());
    CHECK(parse("9223372036854775807").as_number<int64_t>() ==
          std::numeric_limits<int64_t>::max());
    const auto numbers = parse("[0, -0, 1e5, 2E+3, -1.5e-2, 0.1]");
    CHECK(numbers[0].value_category() == ValueCategory::NumberInt);
    CHECK(numbers[2].value_category() == ValueCategory::NumberFloat);
    CHECK(numbers[2] == 100000 && numbers[3] == 2000);
    CHECK(numbers[4] == -0.015 && numbers[5].dump() == "0.1");
    CHECK(parse("1") == parse("1.0"));
    CHECK(parse("false") != parse("0"));

    Value number{int64_t{0}};
    number += 5'000'000'000LL;
    number -= 2LL;
    CHECK(number.as_number<int64_t>() == 4'999'999'998LL);
    number += 0.5;
    CHECK(number.value_category() == ValueCategory::NumberFloat);
    CHECK(number == 4'999'999'998.5);
    number = true;
    CHECK(number.is_boolean() && number.as_boolean());
    number = "changed";
    CHECK(number.is_string() && number.as_string() == "changed");
    number = 42;
    CHECK(number.is_number() && number < 43 && number == 42);
}

void test_containers() {
    auto object = parse(R"({"array":[1,{"x":2}],"name":"a","null":null})");
    CHECK(object.is_object() && object.size() == 3 && !object.empty());
    CHECK(object.contains("array") && !object.contains("missing"));
    CHECK(object["array"].is_array() && object["array"].size() == 2);
    CHECK(object["array"][1]["x"] == 2);
    CHECK(!object["array"].contains("x"));
    object["new"] = false;
    CHECK(object.contains("new") && !object["new"].as_boolean());
    CHECK(object["inserted"].is_null());

    const auto& view = object;
    CHECK(view[std::string("name")].as_string() == "a");
    CHECK(view["name"].as_string() == "a");
    CHECK(view.at("array").at(std::size_t{0}) == 1);
    const auto size = object.size();
    test::throws<std::runtime_error>([&] { view[std::string("absent")]; },
                                     "absent");
    test::throws<std::runtime_error>([&] { object.at("absent"); }, "absent");
    test::throws<std::runtime_error>([&] { view["absent"]; }, "absent");
    CHECK(object.size() == size);
    test::throws<std::runtime_error>([&] { object["array"][-1]; }, "-1");
    test::throws<std::runtime_error>([&] { view.at("array")[9]; }, "9");
    test::throws<std::runtime_error>([&] { object["array"].at(9); }, "9");
    test::throws<std::runtime_error>([&] { object.as_array(); });
    test::throws<std::runtime_error>([&] { object["name"].as_number<int>(); });
    test::throws<std::runtime_error>([&] { object["null"].as_string(); });
    test::throws<std::runtime_error>([&] { object["array"][0].as_boolean(); });

    auto array = Value::create_array(2);
    CHECK(array.size() == 2 && array[0].is_null());
    array[1] = Value::create_object();
    array[1]["child"] = "value";
    CHECK(array[1]["child"].as_string() == "value");
    CHECK(Value::create_array().empty() && Value::create_object().empty());
    CHECK(Value{""}.as_string().empty() &&
          Value{"abc"}.as_string().size() == 3);
}

void test_copy_and_round_trip() {
    const auto original = parse(R"({"a":[1,{"x":"old"}],"b":true})");
    auto copy = original;
    copy["a"][1]["x"] = "new";
    CHECK(original["a"][1]["x"].as_string() == "old");
    CHECK(copy != original);
    Value assigned;
    assigned = original;
    assigned["a"][0] = 9;
    CHECK(original["a"][0] == 1);
    auto clone = original.clone();
    CHECK(clone == original);
    auto& same = clone;
    clone = same;
    CHECK(clone == original);
    auto moved = std::move(clone);
    CHECK(moved == original);
    CHECK(parse(original.dump()) == original);
    CHECK(parse(original.pretty_print()) == original);
    CHECK(original.pretty_print().find('\n') != std::string::npos);
    CHECK(parse(R"({"b":true,"a":[1,{"x":"old"}]})") == original);
    CHECK(parse("[1,2]") != parse("[1,2,3]"));
}

void test_strings() {
    const auto escaped = parse(R"("\"\\\/\b\f\n\r\t\u0000\u001f")");
    const std::string expected = std::string("\"\\/\b\f\n\r\t") + '\0' + '\x1f';
    CHECK(escaped.as_string() == expected);
    CHECK(parse(escaped.dump()) == escaped);
    CHECK(parse(escaped.pretty_print()) == escaped);
    CHECK(escaped.dump().find("\\u0000") != std::string::npos);
    CHECK(parse(R"("\u00e9\u4e2d\ud83d\ude00")").as_string() ==
          "\xc3\xa9\xe4\xb8\xad\xf0\x9f\x98\x80");
    CHECK(parse(R"("raw UTF-8: é中")").as_string() == "raw UTF-8: é中");
    for (int code = 0; code < 32; ++code) {
        const Value control{std::string(1, static_cast<char>(code))};
        CHECK(parse(control.dump()) == control);
    }
}

template <typename T>
void test_typed_arrays() {
    using Complex = std::complex<T>;
    Value array = Value::create_typed_array<Complex>();
    CHECK(array.is_typed_array() && array.empty());
    array.as_typed_array<Complex>().emplace_back(1, -2);
    array.as_typed_array<Complex>().emplace_back(3, 4);
    const auto& view = array;
    CHECK(view.size() == 2 &&
          view.as_typed_array<Complex>()[1] == Complex(3, 4));
    auto copy = array.clone();
    copy.as_typed_array<Complex>()[0] = Complex(9, 0);
    CHECK(array.as_typed_array<Complex>()[0] == Complex(1, -2));
    CHECK(copy != array);
    const auto expected = parse("[[1,-2],[3,4]]");
    CHECK(parse(array.dump()) == expected);
    CHECK(parse(array.pretty_print()) == expected);
    const auto from_vector =
        Value::create_typed_array(std::vector<Complex>{{1, -2}, {3, 4}});
    CHECK(from_vector == array);
    test::throws<std::runtime_error>([&] { array.as_array(); });
}

void test_invalid_documents() {
    for (const char* source : {"",
                               " ",
                               "[1,]",
                               "{\"a\":1,}",
                               "{\"a\":1,,\"b\":2}",
                               "{a:1}",
                               "{\"a\" 1}",
                               "[1 2]",
                               "[1",
                               "{",
                               "true false",
                               "01",
                               "+1",
                               "1.",
                               ".1",
                               "1e",
                               "1e+",
                               "1.2.3",
                               "--1",
                               "-",
                               "NaN",
                               "Infinity",
                               "1e999",
                               "9223372036854775808",
                               "-9223372036854775809",
                               "True",
                               "nul",
                               "\"unterminated",
                               "\"bad\\q\"",
                               "\"\\u12xx\"",
                               "\"\\ud800\"",
                               "\"\\udc00\"",
                               "\"\\ud800\\u0041\"",
                               "\"raw\nline\""}) {
        test::throws<std::runtime_error>([&] { parse(source); });
    }
    test::throws<std::runtime_error>(
        [] { parse(R"({"ok":[1,{"x":"owned"}],"bad":[2,]})"); });
    const std::string too_deep =
        std::string(520, '[') + '0' + std::string(520, ']');
    test::throws<std::runtime_error>([&] { parse(too_deep); },
                                     "nesting too deep");
    CHECK(parse(std::string(32, '[') + '0' + std::string(32, ']')).is_array());
    std::istringstream broken("{\n  \"x\": ]}");
    test::throws<std::runtime_error>(
        [&] {
            json::JsonParser{json::JsonLexer{broken, "broken.json"}}.parse();
        },
        "broken.json:2:");
    test::throws<std::runtime_error>([&] { parse("{\"x\":[1"); },
                                     "unexpected end of input");
}

void test_input_sources() {
    const auto fixture = json::parse_file(TEST_SOURCE_DIR "/test.json");
    CHECK(fixture["a"] == 42.42 && fixture["bs"][0] == 1);
    CHECK(fixture["obj"]["empty_array"].empty());
    CHECK(fixture["primitives"][0].as_boolean());
    test::throws<std::runtime_error>(
        [] { json::parse_file(TEST_SOURCE_DIR "/test.json.missing"); },
        "not found");
    std::istringstream input("[true, 3, \"stream\"]");
    CHECK(json::parse(input) == parse("[true,3,\"stream\"]"));

    std::istringstream tokens("{\n  \"key\": -2}");
    json::JsonLexer lexer(tokens, "tokens.json");
    using Token = json::JsonLexer::TokenName;
    CHECK(lexer.get_filename() == "tokens.json");
    CHECK(lexer.peek_token().name == Token::BRACE_LEFT);
    CHECK(lexer.peek_token().name == Token::BRACE_LEFT);
    CHECK(lexer.get_token().name == Token::BRACE_LEFT);
    const auto key = lexer.get_token();
    CHECK(key.name == Token::STRING && key.content == "key");
    CHECK(key.row == 2 && key.col == 3);
    CHECK(lexer.get_token().name == Token::COLON);
    const auto number = lexer.get_token();
    CHECK(number.name == Token::INTEGER && number.content == "-2");
    CHECK(lexer.get_token().name == Token::BRACE_RIGHT);
    CHECK(lexer.get_token().name == Token::END_OF_FILE);
}

}  // namespace

int main() {
    return test::run("json_test", [] {
        test_values();
        test_containers();
        test_copy_and_round_trip();
        test_strings();
        test_typed_arrays<float>();
        test_typed_arrays<double>();
        test_invalid_documents();
        test_input_sources();
    });
}
