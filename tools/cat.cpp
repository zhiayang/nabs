/*
	Copyright 2021 Alexey Kutepov <reximkut@gmail.com>

	Permission is hereby granted, free of charge, to any person obtaining
	a copy of this software and associated documentation files (the
	"Software"), to deal in the Software without restriction, including
	without limitation the rights to use, copy, modify, merge, publish,
	distribute, sublicense, and/or sell copies of the Software, and to
	permit persons to whom the Software is furnished to do so, subject to
	the following conditions:

	The above copyright notice and this permission notice shall be
	included in all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
	EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
	MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
	NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
	LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
	OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
	WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

void cat_file(const char *file_path)
{
    FILE *f = fopen(file_path, "r");
    if (f == NULL) {
        fprintf(stderr, "ERROR: could not open file %s: %s\n",
                file_path, strerror(errno));
        exit(1);
    }

    static char cat_buffer[4098];

    while (!feof(f)) {
        size_t n = fread(cat_buffer,
                         sizeof(cat_buffer[0]),
                         sizeof(cat_buffer) / sizeof(cat_buffer[0]),
                         f);
        fwrite(cat_buffer,
               sizeof(cat_buffer[0]),
               n,
               stdout);
    }

    fclose(f);
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        fprintf(stderr, "USAGE: cat <files...>\n");
        exit(1);
    }

    for (int i = 1; i < argc; ++i) {
        cat_file(argv[i]);
    }

    return 0;
}
