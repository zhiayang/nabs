// test.cpp
// Copyright (c) 2021, zhiayang
// Licensed under the Apache License Version 2.0.

#include "nabs.h"

int main(int argc, char** argv)
{
	nabs::self_update(argc, argv, __FILE__);

	nabs::log("got {} argument{}:", argc, argc == 1 ? "" : "s");
	for(int i = 0; i < argc; i++)
		zpr::println("  {}", argv[i]);

	auto tc = nabs::find_toolchain();

	using namespace nabs;
	map(fs::find_files_ext(".", ".txt"), [](const auto& f) {
		stdfs::remove(f);
	});

	// auto x
	// 	= cmd("cat", "README.md")
	// 	| split(
	// 		cmd("rev") | file("asdf.txt"),
	// 		file("bsdf.txt"))
	// 	| cmd("hexdump", "-C")
	// 	| file("foozle.txt");
	// zpr::println("status = {}", x.run());


#if 1
	namespace nd = nabs::dep;
	{
		nd::Graph graph;
		auto a = graph.add(nd::KIND_PHONY, "A");
		auto b = graph.add(nd::KIND_PHONY, "B");
		auto c = graph.add(nd::KIND_PHONY, "C");
		auto d = graph.add(nd::KIND_PHONY, "D");
		auto e = graph.add(nd::KIND_PHONY, "E");
		auto f = graph.add(nd::KIND_PHONY, "F");

		// a->depend(b);
		// d->depend(b);
		// c->depend(d);
		// e->depend(b);
		// b->depend(f);

		a->depend(b);
		b->depend(c);
		c->depend(d);
		// d->depend(b);

		// c->depend(d);
		// d->depend(b);

		auto sorted = graph.topological_sort({ a, d });
		zpr::println("sorted = {}", sorted);
	}
#endif
}
