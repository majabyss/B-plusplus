#include <stdio.h>
#include <string.h>

long filesize(const char* file) {
	FILE *psi = fopen(file, "rb");
	if (psi == NULL) {
		perror("Error opening file.\n");
		return -1;
	}
	fseek(psi, 0, SEEK_END);
	long size = ftell(psi);
	fclose(psi);
	return size;
}

int main(int argc, char *argv[]) {
	char file[100];
	strcpy(file, argv[2]);

	long length = filesize(file);
	if (length < 0) return 1;

	FILE *pre = fopen(argv[2], "r");
	if (!pre) {
		perror("Error opening file.\n");
		printf("%d", argc);
		return 1;
	}
	FILE *pri = fopen(argv[1], "w");

	char _strings[length + 1];
	while (fgets(_strings, sizeof(_strings), pre) != NULL) {
		fprintf(pri, "%s", _strings);
		}

	fclose(pre);
	fclose(pri);
	return 0;
}
