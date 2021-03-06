## not a build system

nabs is a single-header library for writing build recipes in C++. It is directly inspired by
[nobuild](https://github.com/tsoding/nobuild), but with more feature (bloat) and built-in support
for makefile-like dependency resolution.

The primary objective of this (not a) build system is to allow writing build recipes/scripts that:

1. are written in one language (the best one) -- C++
2. work for all major platforms -- Linux, macOS, BSDs, Windows
3. easily support, with very little boilerplate, standard makefile project patterns
4. provide platform- and compiler-independent abstractions for common flags/options


### example

While the documentation is not quite there yet, we already have a lot of features. For example, to
create a build recipe that *automatically*:

1. recompiles itself when its source file changes
2. generates header-include dependency information for source files
3. generates and uses a precompiled header (and updates the pch if anything changes)
4. supports incremental builds (one source to one object, only rebuilding when necessary)
5. runs compilation jobs in parallel
6. works in most environments including Windows+MSVC
7. finds `cl.exe` on Windows without needing to be in a `vcvars` command prompt

we just need something like this:

```cpp
#include "nabs.h"

namespace n = nabs;
int main(int argc, char** argv)
{
    auto OUTPUT_EXE = n::auto_set_extension_exe("build/lc");

    n::self_update(argc, argv, __FILE__);

    size_t threads = std::max(std::thread::hardware_concurrency(), 1u);
    n::log("using {} thread{}", threads, threads == 1 ? "" : "s");

    auto tool = n::find_toolchain().expect("failed to find toolchain");
    tool.add_cpp_includes("source/include")
        .use_cpp_precompiled_header("source/include/precompile.h")
        .use_exceptions(false);

    auto src_files = n::fs::find_files_with_extension_recursively("source", ".cpp");

    n::dep::Graph graph;
    n::auto_add_precompiled_header(graph, tool, "source/include/precompile.h", n::LANGUAGE_CPP);
    auto exe_file = n::auto_executable_from_sources(graph, tool, OUTPUT_EXE, src_files)
        .expect("error");

    auto pool = zmt::ThreadPool(threads);
    n::auto_build_target(pool, tool, graph, exe_file)
        .expect("compilation failed");
}
```

### missing features

Some things are still not implemented:

1. Any form of cross compilation support
    - looking for toolchains in another folder (ie. anywhere other than the default)
    - specifying prefixes, sysroots, etc.

2. Searching for libraries using something other than `pkg-config`
    - eg. looking in `/usr/lib` and `/usr/local/lib`
    - any form of library searching on windows

3. Misc features
    - creating shared/static libraries
    - more "automatic" argument parsing (eg. automatically handle stuff like `-j`, `--help`)




### license

This header is licensed under the Apache License 2.0.
