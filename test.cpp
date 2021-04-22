// test.cpp
// Copyright (c) 2021, zhiayang
// Licensed under the Apache License Version 2.0.

#include "nabs.h"

int main()
{
	using namespace nabs;

	auto x
		= cmd("cat", "README.md")
		| split(
			cmd("reverse") | file("asdf.txt"),
			file("bsdf.txt"))
		| cmd("hexdump", "-C")
		| file("foozle.txt");

	// auto x = file("test.cpp") | cmd("reverse") | cmd("hexdump", "-C") | file("foozle.txt");
	// auto x = cmd("cat", "test.cpp") | cmd("hexdump", "-C") | cmd("reverse");
	zpr::println("status = {}", x.run());

	auto c = find_c_compiler().path.string();
	zpr::println("path = {}", c);
	// compile_files({ }, "owo", "owo.c", "uwu.c");
}
