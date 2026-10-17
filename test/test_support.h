#ifndef UTILS_TEST_SUPPORT_H
#define UTILS_TEST_SUPPORT_H

#include <cstdlib>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <string_view>

namespace test {

inline void check(bool condition,
                  const char* expression,
                  const char* file,
                  int line) {
    if (!condition)
        throw std::runtime_error(std::string(file) + ':' +
                                 std::to_string(line) + ": " + expression);
}

template <typename Exception, typename Function>
void throws(Function function, std::string_view message = {}) {
    try {
        function();
    } catch (const Exception& error) {
        if (std::string_view(error.what()).find(message) !=
            std::string_view::npos)
            return;
        throw std::runtime_error("Unexpected diagnostic: " +
                                 std::string(error.what()));
    }
    throw std::runtime_error("Expected an exception containing: " +
                             std::string(message));
}

class CaptureStream {
   public:
    explicit CaptureStream(std::ostream& stream)
        : stream_(stream), original_(stream.rdbuf(output_.rdbuf())) {
        format_.copyfmt(stream_);
    }
    ~CaptureStream() {
        stream_.rdbuf(original_);
        stream_.copyfmt(format_);
    }
    CaptureStream(const CaptureStream&) = delete;
    CaptureStream& operator=(const CaptureStream&) = delete;

    std::string str() const { return output_.str(); }

   private:
    std::ostringstream output_;
    std::ostream& stream_;
    std::streambuf* original_;
    std::ios format_{nullptr};
};

template <typename Function>
int run(const char* name, Function function) {
    try {
        function();
        std::cout << name << ": passed\n";
        return EXIT_SUCCESS;
    } catch (const std::exception& error) {
        std::cerr << name << ": " << error.what() << '\n';
        return EXIT_FAILURE;
    }
}

}  // namespace test

// Keep checks active in release builds that define NDEBUG.
#define CHECK(...) \
    ::test::check(bool(__VA_ARGS__), #__VA_ARGS__, __FILE__, __LINE__)

#endif
