#include <fstream>
#include <iostream>
#include <numbers>
#include <sstream>

#define ZQ_JSON_PARSER_IMPLEMENTATION
#include "../JsonParser.h"

using namespace json;

int main() {
    const char* file_name = "./test.json";
    {
        std::ifstream ifs(file_name);
        JsonLexer json_lexer(ifs);
        std::cout << "Tokens:\n";
        while (json_lexer) { std::cout << json_lexer.get_token() << '\n'; }
        std::cout << '\n';
    }
    {
        auto obj = parse_file(file_name);
        double a = obj["a"];
        int b0 = obj["bs"][0];
        bool bb = obj["primitives"][0].as_boolean();
        std::cout << "a = " << a << "\nbs[0] = " << b0
                  << "\nprimitives[0] = " << std::boolalpha << bb << '\n';

        static_cast<void>(obj["a"] += obj["bs"][1]);
        static_cast<void>(obj["a"] -= obj["bs"][1]);
        static_assert(
            std::is_same_v<decltype(obj["a"] - obj["bs"][1]), double>);

        try {
            double d = obj.at("abc");
            static_cast<void>(d);
        } catch (std::exception& e) {
            std::cout << "Try to get an undefined property as number\n";
            std::cout << e.what() << '\n';
        }
        try {
            double d = obj["obj"];
            static_cast<void>(d);
        } catch (std::exception& e) {
            std::cout << "Try to get number from object\n";
            std::cout << e.what() << '\n';
        }
        try {
            std::string str = obj["bs"];
        } catch (std::exception& e) {
            std::cout << "Try to get string from array\n";
            std::cout << e.what() << '\n';
        }
        std::cout << '\n';

        std::cout << std::boolalpha;
        std::cout << "Is the whole file an object? " << obj.is_object() << '\n';
        std::cout << "Is the 'bs' property an array? " << obj["bs"].is_array()
                  << "\n\n";

        std::cout << "Print type of every property of the outermost object:\n";
        for (auto& [key, val] : obj.as_object()) {
            std::cout << "    " << key << ": ["
                      << get_value_category_name(val.value_category()) << "]\n";
        }
        std::cout << '\n';

        auto compact_form = obj.dump();
        std::cout << "Unformatted output: " << compact_form << "\n\n";
        std::cout << "Formatted output:\n" << obj.pretty_print() << "\n\n";

        {
            auto obj_copy = obj.clone();
            std::cout << "Clone the whole object and print it:\n"
                      << obj_copy.pretty_print() << '\n'
                      << "The destruct sequence of the cloned object:\n";
        }

        std::cout << "\nProperty a < 42 ? " << (obj["a"] < 42) << '\n';
        obj["a"] += 1;
        std::cout << "Property a after adding 1: " << obj["a"].dump() << '\n';
        obj["a"] = 69;
        std::cout << "Property a after being assigned 69: " << obj["a"].dump()
                  << '\n';
        obj["obj"] = 69.69;
        std::cout << "Property obj after being assigned 69.69: "
                  << obj["obj"].dump() << '\n';
        std::cout << '\n';

        std::cout << "The destruct sequence:\n";
    }
    {  // test syntax error detection
        char json_failure_1[] = "{\"a\":1,,\"b\":2}";
        try {
            parse(json_failure_1);
        } catch (std::exception& e) {
            std::cout << "\nTry to parse a problemetic json:\n"
                      << json_failure_1 << '\n';
            std::cout << e.what() << '\n';
        }
        char json_failure_2[] = "{\"a\":1,\"b\":2,c:3}";
        try {
            parse(json_failure_2);
        } catch (std::exception& e) {
            std::cout << "Try to parse a problemetic json:\n"
                      << json_failure_2 << '\n';
            std::cout << e.what() << '\n';
        }
    }
    {
        // test typed array
        auto arr = Value::create_typed_array<std::complex<double>>();
        constexpr std::size_t n = 6;
        constexpr double dt = std::numbers::pi * 2 / n;
        for (std::size_t i = 0; i < n; ++i) {
            arr.as_typed_array<std::complex<double>>().emplace_back(
                std::cos(i * dt), std::sin(i * dt));
        }
        std::cout << "\nPrint root of z^6 - 1 = 0 as an array:\n(dump)\n"
                  << arr.dump() << "\n(Pretty Print of a clone)\n"
                  << arr.clone().pretty_print() << '\n';
    }
    {
        // test object and array creation
        auto obj = Value::create_object();
        auto arr = Value::create_array();

        arr.as_array().push_back(Value::create_object());
        obj["arr"] = std::move(arr);
        obj["obj"] = Value::create_object();

        std::cout << '\n' << obj.pretty_print() << '\n';
    }
    {  // test bool values and string creation
        auto obj = Value::create_object();
        obj["flag"] = true;
        obj["off"] = false;
        std::cout << "Boolean assignment: " << obj["flag"].dump() << ", "
                  << obj["off"].dump() << " ("
                  << get_value_category_name(obj["flag"].value_category())
                  << ")\n";
        Value b{true};
        std::cout << "Value{true} is "
                  << get_value_category_name(b.value_category()) << '\n';
        Value s{"direct string"};
        std::cout << "Value{\"direct string\"} dumps as " << s.dump() << '\n';
    }
    {  // test copying an object value (used to throw)
        auto obj = parse("{\"a\": [1, 2, 3], \"o\": {\"x\": 9}}");
        Value copy = obj;
        copy["a"].as_array()[0] = 100;
        std::cout << "Copy of object is independent: " << obj.dump()
                  << " vs " << copy.dump() << '\n';
    }
    {  // test strict number parsing and scientific notation
        auto v = parse("[1e5, 2E3, 1.5e-2, 123456789012345]");
        std::cout << "Scientific notation: " << v.dump() << '\n';
        for (const char* bad : {"[1.2.3]", "[1e]", "[-,5]", "[007]", "[1.]",
                                "[1e999]"}) {
            try {
                parse(bad);
                std::cout << "FAIL: " << bad << " should be rejected\n";
            } catch (std::exception& e) {
                std::cout << "Rejected " << bad << ": " << e.what() << '\n';
            }
        }
    }
    {  // test string escapes
        auto v = parse("{\"n\": \"x\\ny\", \"q\": \"he said \\\"hi\\\"\", "
                       "\"u\": \"\\u00e9\"}");
        std::cout << "Escapes decoded: " << v.dump() << '\n';
        try {
            parse("\"unterminated");
        } catch (std::exception& e) {
            std::cout << "Unterminated string: " << e.what() << '\n';
        }
    }
    {  // test int64 arithmetic, out-of-range indexing and truthiness
        auto v = parse("[0, 1, 2]");
        v[0] += 5'000'000'000LL;
        std::cout << "int64 += 5000000000: " << v[0].dump() << '\n';
        try {
            static_cast<void>(v[10]);
        } catch (std::exception& e) {
            std::cout << "Out-of-range index: " << e.what() << '\n';
        }
        std::cout << "Truthiness: object=" << !!parse("{}")
                  << ", null=" << !!parse("null")
                  << ", zero=" << !!parse("0") << '\n';
    }
    {  // test Value == Value
        auto doc = parse("[1, {\"a\": \"x\"}, [true, null], 2.0]");
        std::cout << "Round trip equals: "
                  << (parse(doc.dump()) == doc) << '\n';
        std::cout << "Different docs: "
                  << (parse("[1, 2]") != parse("[1, 2, 3]")) << '\n';
    }
    {  // test error message for truncated input
        try {
            parse("{\"a\": [1, 2");
        } catch (std::exception& e) {
            std::cout << "Truncated input: " << e.what() << '\n';
        }
    }
    return 0;
}
