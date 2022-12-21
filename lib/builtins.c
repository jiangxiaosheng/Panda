#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

void println(const char *format, ...) {
	va_list valist;
	va_start(valist, format);
	printf(format, valist);
	va_end(valist);
	printf("\n");
}

void assert(int cond, const char *msg) {
	if (!cond) {
		fprintf(stderr, "[ERROR] (%s:%d: assert fail: %s)\n", __FILE__, __LINE__, msg);
		exit(-1);
	}
}

#ifdef BUILD_TEST
int main() {
	println("hello world");
}
#endif