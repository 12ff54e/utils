#define CLAP_DEBUG
#include "io/clap.h"

#include "test_support.h"

#include <cerrno>
#include <fstream>
#include <iterator>

namespace {

struct Options {
    bool verbose = false;
    int count = 2;
    unsigned limit = 10;
    double tolerance = 0.01;
    float scale = 1;
    std::string name = "default";
    std::string input_path;
    int steps = 0;
};

int register_options() {
    CLAP_BEGIN(Options)
    CLAP_REGISTER_OPTION(verbose)
    CLAP_REGISTER_OPTION(count, "--count")
    CLAP_REGISTER_OPTION(limit, "limit", "l")
    CLAP_REGISTER_OPTION_WITH_DESCRIPTION(tolerance, "error tolerance")
    CLAP_REGISTER_OPTION_WITH_DESCRIPTION(scale, "scale", "scale factor")
    CLAP_REGISTER_OPTION_WITH_DESCRIPTION(name, "--name", "-n", "output name")
    CLAP_REGISTER_ARG(input_path)
    CLAP_REGISTER_ARG(steps)
    CLAP_ADD_USAGE("[options] INPUT STEPS")
    CLAP_ADD_DESCRIPTION("Test command line parser")
    CLAP_END(Options)
    return 0;
}

Options parse(std::initializer_list<const char*> arguments) {
    std::vector<std::string> storage{"clap-test"};
    for (const auto* argument : arguments) storage.emplace_back(argument);
    std::vector<char*> argv;
    for (auto& argument : storage) argv.push_back(argument.data());
    Options options;
    CLAP<Options>::parse_input(options, static_cast<int>(argv.size()),
                               argv.data());
    return options;
}

void test_parsing() {
    const auto defaults = parse({"input.dat", "3"});
    CHECK(!defaults.verbose && defaults.count == 2 && defaults.limit == 10);
    CHECK(defaults.tolerance == 0.01 && defaults.scale == 1);
    CHECK(defaults.name == "default" && defaults.input_path == "input.dat");
    CHECK(defaults.steps == 3);

    const auto values =
        parse({"--verbose", "--count", "-4", "-l", "12", "--tolerance=0.125",
               "--scale", "2.5", "-n", "result", "input.dat", "9"});
    CHECK(values.verbose && values.count == -4 && values.limit == 12);
    CHECK(values.tolerance == 0.125 && values.scale == 2.5F);
    CHECK(values.name == "result" && values.steps == 9);
    CHECK(parse({"--verbose=0", "input", "1"}).verbose == false);
    CHECK(parse({"--name=", "input", "1"}).name.empty());
    CHECK(parse({"--name", "two words", "input", "1"}).name == "two words");
    CHECK(parse({"two words.dat", "1"}).input_path == "two words.dat");
    CHECK(parse({"input", "--count=5", "1", "--count", "7"}).count == 7);
    const auto positional = parse({"--", "-input.dat", "-2"});
    CHECK(positional.input_path == "-input.dat" && positional.steps == -2);
}

void test_errors() {
    test::throws<std::invalid_argument>(
        [] { parse({"--cout", "1", "input", "2"}); }, "Do you mean '--count'");
    test::throws<std::invalid_argument>(
        [] { parse({"--unknown-option", "input", "2"}); },
        "unrecognized option");
    test::throws<std::invalid_argument>([] { parse({"--count"}); },
                                        "missing value");
    test::throws<std::invalid_argument>([] { parse({}); }, "too few arguments");
    test::throws<std::invalid_argument>([] { parse({"input"}); },
                                        "too few arguments");
    test::throws<std::invalid_argument>([] { parse({"input", "1", "extra"}); },
                                        "too many arguments");
    CHECK(CLAP<Options>::process_option("count") == "--count");
    CHECK(CLAP<Options>::process_option("--count") == "--count");
    CHECK(CLAP<Options>::process_option("c", true) == "-c");
    CHECK(CLAP<Options>::process_option("-c", true) == "-c");
    test::throws<std::invalid_argument>(
        [] { CLAP<Options>::process_option(""); }, "Invalid long option");
    test::throws<std::invalid_argument>(
        [] { CLAP<Options>::process_option("-count"); }, "Invalid long option");
    test::throws<std::invalid_argument>(
        [] { CLAP<Options>::get_type_code<long long>(); }, "not supported");
    test::throws<std::invalid_argument>(
        [] { CLAP<Options>::report_repeat_definition("--count"); }, "twice");
}

void check_help(const std::string& help) {
    CHECK(help.find("Usage: clap-test [options] INPUT STEPS") !=
          std::string::npos);
    CHECK(help.find("Test command line parser") != std::string::npos);
    CHECK(help.find("-n, --name") != std::string::npos);
    CHECK(help.find("-l, --limit") != std::string::npos);
    CHECK(help.find("-h, --help") != std::string::npos);
    CHECK(help.find("error tolerance") != std::string::npos);
    CHECK(help.find("output name") != std::string::npos);
    CHECK(help.find("--count") < help.find("--help"));
    CHECK(help.find("--help") < help.find("--limit"));
    CHECK(help.find("--limit") < help.find("--name"));
}

void test_help() {
    test::CaptureStream capture(std::cout);
    CLAP<Options>::print_help("clap-test");
    check_help(capture.str());
}

}  // namespace

int main(int argc, char** argv) {
    return test::run("clap_test", [&] {
        CHECK(register_options() == 0);
        if (argc == 2 && std::string_view(argv[1]) == "--help") {
            // Exercise the real exit path in a separate invocation.
            char program[] = "clap-test";
            char* help_argv[] = {program, argv[1]};
            Options options;
            CLAP<Options>::parse_input(options, 2, help_argv);
            throw std::runtime_error("--help did not exit");
        }
        if (argc == 3 && std::string_view(argv[1]) == "--check-help") {
            std::ifstream input(argv[2]);
            CHECK(input.good());
            check_help(std::string(std::istreambuf_iterator<char>(input), {}));
            return;
        }
        CHECK(argc == 1);
        test_parsing();
        test_errors();
        test_help();
    });
}
