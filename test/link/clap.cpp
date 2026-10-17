#include "io/clap.h"

#include "shared.h"

int count_from_other_file() {
    CLAP_BEGIN(LinkOptions)
    CLAP_REGISTER_OPTION(count)
    CLAP_END(LinkOptions)
    char program[] = "link-test";
    char option[] = "--count=7";
    char* argv[] = {program, option};
    LinkOptions options;
    CLAP<LinkOptions>::parse_input(options, 2, argv);
    return options.count;
}
