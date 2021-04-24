// test.cpp
// Copyright (c) 2021, zhiayang
// Licensed under the Apache License Version 2.0.

#include "nabs.h"

namespace zpr
{
	template <>
	struct print_formatter<nabs::dep::Item*>
	{
		template <typename Cb>
		void print(nabs::dep::Item* item, Cb&& cb, format_args args)
		{
			(void) args;
			cb(item->str());
		}
	};
}

int main(int argc, char** argv)
{
	nabs::self_update(argc, argv, __FILE__);

	zpr::println("got {} argument{}:", argc, argc == 1 ? "" : "s");
	for(int i = 0; i < argc; i++)
		zpr::println("  {}", argv[i]);

	using namespace nabs;
	auto x
		= cmd("cat", "README.md")
		| split(
			cmd("rev") | file("asdf.txt"),
			file("bsdf.txt"))
		| cmd("hexdump", "-C")
		| file("foozle.txt");

	zpr::println("status = {}", x.run());


#if 0
	namespace nd = nabs::dep;
	{
		nd::Graph graph;
		auto a = graph.add_phony("A");
		auto b = graph.add_phony("B");
		auto c = graph.add_phony("C");
		auto d = graph.add_phony("D");
		auto e = graph.add_phony("E");
		auto f = graph.add_phony("F");

		// a->depend(b);
		// d->depend(b);
		// c->depend(d);
		// e->depend(b);
		// b->depend(f);

		a->depend(b);
		// c->depend(d);
		b->depend(a);

		// c->depend(d);
		// d->depend(b);

		auto sorted = graph.topological_sort();
		zpr::println("sorted = {}", sorted);
	}
#endif
}
