## not a build system

nabs is a single-header library for writing build recipes in C++. It is directly inspired by
[nobuild](https://github.com/tsoding/nobuild), but with more feature (bloat) and built-in support
for makefile-like dependency resolution.

The primary objective of this (not a) build system is to allow writing build recipes/scripts that:

1. are written in one language (the best one) -- C++
2. work for all major platforms -- Linux, macOS, BSDs, Windows
3. easily support, with very little boilerplate, standard makefile project patterns

More documentation will be coming soon.
