// test.cpp
// Copyright (c) 2021, zhiayang
// Licensed under the Apache License Version 2.0.

#include "nabs.h"

int main()
{
	using namespace nabs;

	// auto x = cmd("type", "test.cpp") | split(file("asdf.txt"), file("bsdf.txt")) | cmd("hexdump", "-C");

	auto x = file("test.cpp") | cmd("reverse") | cmd("hexdump", "-C") | file("foozle.txt");
	zpr::println("status = {}", x.run());

	// compile_files({ }, "owo", "owo.c", "uwu.c");
}
