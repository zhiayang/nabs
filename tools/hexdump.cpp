// hexdump.cpp
// Copyright (c) 2021, zhiayang
// Licensed under the Apache License Version 2.0.

#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <ctype.h>
#include <string.h>

// god damned windows
#if defined(_MSC_VER)
#include <io.h>
#endif



int main(int argc, char** argv)
{
	bool canonical = false;
	char* filename = nullptr;

	// look for files
	for(int i = 1; i < argc; i++)
	{
		if(strcmp(argv[i], "-C") == 0)
			canonical = true;

		else
			filename = argv[i];
	}

	FILE* input = (filename ? fopen(filename, "rb") : stdin);
	if(input == nullptr)
		fprintf(stderr, "fopen(): %s\n", strerror(errno));

	size_t offset = 0;
	int fd = fileno(input);
	if(canonical)
		printf("%08zx ", offset);

	else
		printf("%07zx", offset);


	char linebuf[16] { };
	size_t linelen = 0;
	char* line = &linebuf[0];

	while(true)
	{
		char c = 0;
		auto ret = read(fd, &c, 1);
		if(ret == 0)
			break;

		if(ret < 0)
		{
			fprintf(stderr, "read(): %s\n", strerror(errno));
			break;
		}

		*line++ = c;
		linelen++;

		if(canonical)
		{
			if(linelen == 9)
				printf(" ");

			printf(" %02x", c);
			if(linelen == 16)
			{
				printf("  |");
				for(int k = 0; k < 16; k++)
					printf("%c", isprint(linebuf[k]) ? linebuf[k] : '.');

				printf("|\n%08zx ", offset += linelen);

				line = &linebuf[0];
				linelen = 0;
			}
		}
		else
		{
			printf(" %02x", c);
			if(linelen == 16)
			{
				printf("\n%07zx", offset += linelen);
				line = &linebuf[0];
				linelen = 0;
			}
		}
	}


	// flush the rest.
	if(canonical)
	{
		if(linelen > 0)
		{
			for(size_t i = linelen; i < 16; i++)
				printf("   ");

			// compensate for the missing extra space in the middle if we need to
			if(linelen <= 8)
				printf(" ");

			printf("  |");
			for(int k = 0; k < linelen; k++)
				printf("%c", isprint(linebuf[k]) ? linebuf[k] : '.');

			printf("|\n%08zx\n", offset += linelen);
		}
		else
		{
			printf("\n");
		}
	}
	else
	{
		if(linelen > 0)
			printf("\n%07zx\n", offset += linelen);
		else
			printf("\n");
	}

	fclose(input);
}
