#include "compiler.h"

#include <stdio.h>
#include <stdlib.h>

#include "dirent.h"
#include "string.h"
#include "symbols.h"

int is_codegen = FALSE;

int is_codegen_phase() {
    return is_codegen;
}

int InitCompiler() {
    return init_symbol();  // return 1;
}

char file_path[512];
char lib_file_path[512];

char file_to_be_compiled[512];
char output_file_path[512];

FILE *get_output_file() {
    FILE *output_file = fopen(output_file_path, "w");
    if (output_file == NULL) {
        printf("error when trying to create or open the compiled file path\n");
        exit(1);
    }
    return output_file;
}

char *replace_file_suffix(const char *filename, const char *suffix) {
    const char *dot = strrchr(filename, '.');
    if (dot && strcmp(dot, ".jack") == 0) {
        size_t length = dot - filename;
        char *new = (char *)malloc(length + strlen(suffix) + 1);
        if (new) {
            strncpy(new, filename, length);
            strcpy(new + length, suffix);
            return new;
        }
    }
    return NULL;
}

ParserInfo compile(char *dir_name) {
    ParserInfo parser_info;

    // parse jack built-in libraries
    struct dirent *lib_file;
    DIR *curr_dir = opendir(".");
    if (curr_dir == NULL) {
        printf("Failed to open current dir\n");
        exit(1);
    }
    while ((lib_file = readdir(curr_dir)) != NULL) {
        if (strstr(lib_file->d_name, ".jack") == NULL) {
            continue;
        };

        // string concatenation for file path
        strcpy(lib_file_path, "./");
        strcat(lib_file_path, lib_file->d_name);

        // parse individual file, return error in error exists
        int init_parser = InitParser(lib_file_path);
        if (init_parser == 0) {
            parser_info.er = lexerErr;
            return parser_info;
        }
        parser_info = Parse();
        StopParser();
        if (parser_info.er != none) {
            return parser_info;
        }
    }
    closedir(curr_dir);

    // parse given program
    struct dirent *program_file;
    DIR *dir = opendir(dir_name);
    if (dir == NULL) {
        printf("Directory %s does not exist.\n", dir_name);
        exit(1);
    }

    while ((program_file = readdir(dir)) != NULL) {
        if (strstr(program_file->d_name, ".jack") == NULL) {
            continue;
        }

        // string concatenation for file path
        strcpy(file_path, dir_name);
        strcat(file_path, "/");
        strcat(file_path, program_file->d_name);

        // parse individual file, return error in error exists
        int init_parser = InitParser(file_path);
        if (init_parser == 0) {
            parser_info.er = lexerErr;
            return parser_info;
        }
        parser_info = Parse();
        StopParser();
        if (parser_info.er != none) {
            return parser_info;
        }
    }

    // find undeclared identifier
    parser_info = find_undeclared_identifier();
    if (parser_info.er != none) {
        return parser_info;
    }

    // code generation
    rewinddir(dir);
    is_codegen = TRUE;

    while ((program_file = readdir(dir)) != NULL) {
        if (strstr(program_file->d_name, ".jack") == NULL) {
            continue;
        }

        // replace suffix .jack with .vm
        const char *jack_suffix = ".jack";
        const char *vm_suffix = ".vm";
        int len = strlen(program_file->d_name);

        char *replace_filename = replace_file_suffix(program_file->d_name, ".vm");

        // string concatenation for output file path
        strcpy(output_file_path, dir_name);
        strcat(output_file_path, "/");
        strcat(output_file_path, replace_filename);

        // memset(replace_filename, 0, 100);
        free(replace_filename);

        // string concatenation for original file path
        strcpy(file_to_be_compiled, dir_name);
        strcat(file_to_be_compiled, "/");
        strcat(file_to_be_compiled, program_file->d_name);

        // parse individual file, return error in error exists
        int init_parser = InitParser(file_to_be_compiled);
        if (init_parser == 0) {
            parser_info.er = lexerErr;
            return parser_info;
        }
        parser_info = Parse();
        StopParser();
        if (parser_info.er != none) {
            return parser_info;
        }
    }

    is_codegen = FALSE;
    closedir(dir);

    parser_info.er = none;
    return parser_info;
}

int StopCompiler() {
    return stop_symbol();  // return 1
}