#define ZQ_JSON_PARSER_IMPLEMENTATION
#include "io/JsonParser.h"

#include "shared.h"

json::Value value_from_other_file(json::Value::allocator_type allocator) {
    return json::parse(R"({"count":7,"ok":true})", std::move(allocator));
}
