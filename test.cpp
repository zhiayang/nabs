// test.cpp
// Copyright (c) 2021, zhiayang
// Licensed under the Apache License Version 2.0.

#include "source/nabs.h"

int main()
{
	using namespace nabs;

	// auto x = cmd("cat", "test.cpp") | split(file("asdf.txt"), file("bsdf.txt")) | cmd("hexdump", "-C");
	// zpr::println("status = {}", x.run());

	compile_files({ }, "owo", "owo.c", "uwu.c");
}
