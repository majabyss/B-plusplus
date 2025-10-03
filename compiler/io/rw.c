/*
 * rw.c - Simple file copy utility for B++ project
 * Copies the contents of one file to another.
 * Usage: ./rw <output_file> <input_file>
 */
#include <stdio.h>
#include <string.h>

// Returns the size of the file in bytes, or -1 on error
long filesize(const char* file) {
    FILE *psi = fopen(file, "rb");
    if (psi == NULL) {
        perror("Error opening file");
        return -1;
    }
    fseek(psi, 0, SEEK_END);
    long size = ftell(psi);
    fclose(psi);
    return size;
}

int main(int argc, char *argv[]) {
    // Check for correct number of arguments
    if (argc != 3) {
        fprintf(stderr, "Usage: %s <output_file> <input_file>\n", argv[0]);
        return 1;
    }

    const char *inputFile = argv[2];
    const char *outputFile = argv[1];

    // Get the size of the input file
    long length = filesize(inputFile);
    if (length < 0) return 1;

    // Open input file for reading
    FILE *inFile = fopen(inputFile, "r");
    if (!inFile) {
        perror("Error opening input file");
        return 1;
    }
    // Open output file for writing
    FILE *outFile = fopen(outputFile, "w");
    if (!outFile) {
        perror("Error opening output file");
        fclose(inFile);
        return 1;
    }

    // Buffer to hold file contents (line by line)
    char buffer[length + 1];
    // Copy contents from input to output
    while (fgets(buffer, sizeof(buffer), inFile) != NULL) {
        fprintf(outFile, "%s", buffer);
    }

    // Close files
    fclose(inFile);
    fclose(outFile);
    return 0;
}
