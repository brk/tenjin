#include <stdio.h>
#include <assert.h>

int main(int argc, char** argv)
{
	printf("Hello, Tenjin!\n");
	assert(1);
	
	if (argc > 2) {
          printf("  (more than two args provided)\n");
	}
	return 0;
}
