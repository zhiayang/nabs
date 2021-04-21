// reverse.cpp
// Copyright (c) 2021, zhiayang
// Licensed under the Apache License Version 2.0.

#include <stdio.h>

#include <string>
#include <vector>
#include <iostream>

int main(int argc, char** argv)
{
	(void) argc;
	(void) argv;

	std::vector<std::string> lines;
	for(std::string line; std::getline(std::cin, line); )
		lines.push_back(std::move(line));

	for(auto it = lines.rbegin(); it != lines.rend(); ++it)
		printf("%s\n", it->c_str());
}
