#include "lexer.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ERROR 0
#define NO_ERROR 1
#define LEXEME_LEN 128
#define FILE_NAME_LEN 32
#define TRUE 1
#define FALSE 0

FILE *input_file;
long *input_file_size;
char *input_file_name;
char *buffer;
char *lexeme;
Token *tokens;
int *total_tokens;
int *line_of_token;
int *parsing_idx;

char *reserved_words[] = {
    "class", "constructor", "method", "function", "int", "boolean", "char",
    "void", "var", "static", "field", "let", "do", "if",
    "else", "while", "return", "true", "false", "null", "this"};

int is_reserved_word(char *word) {
    for (int i = 0; i < 21; i++) {
        if (strcmp(word, reserved_words[i]) == 0)
            return TRUE;
    }
    return FALSE;
}

Token *init_token() {
    Token *token = (Token *)malloc(sizeof(Token));
    token->tp = ERR;
    token->ec = IllSym;
    token->ln = *line_of_token;
    strncpy(token->lx, "Error: illegal symbol in source file", LEXEME_LEN);
    strncpy(token->fl, input_file_name, FILE_NAME_LEN);
    return token;
}

void parse_tokens(char *buffer) {
    int i = 0;
    int token_idx = 0;
    Token *token = init_token();
    tokens = (Token *)malloc((token_idx + 1) * sizeof(Token));
    tokens[token_idx] = *token;
    int flag = FALSE;

    while (i < *input_file_size + 1) {
        if (flag == TRUE) {
            tokens = (Token *)realloc(tokens, (token_idx + 1) * sizeof(Token));
            Token *new_token = init_token();
            tokens[token_idx] = *new_token;
            flag = FALSE;
        }

        // skip existed white spaces
        if (isspace(buffer[i])) {
            if (buffer[i] == '\n') {
                *line_of_token += 1;
                i++;
                continue;
            }
            i++;
            continue;
        }

        // skip existed comments
        if (i + 1 < *input_file_size + 1 && buffer[i] == '/' && buffer[i + 1] == '/') {
            while (buffer[i] != '\n' && buffer[i] != '\0') {
                i++;
            }
            if (buffer[i] == '\n') {
                *line_of_token += 1;
            } else if (buffer[i] == '\0') {
                strncpy(tokens[token_idx].lx, "End of File", LEXEME_LEN);
                tokens[token_idx].ln = *line_of_token;
                strncpy(tokens[token_idx].fl, input_file_name, FILE_NAME_LEN);
                tokens[token_idx].tp = EOFile;
                token_idx++;
                flag = TRUE;
            }
            i++;
            continue;
        }

        if (i < *input_file_size + 1 && buffer[i] == '/' && buffer[i + 1] == '*') {
            i += 2;
            while (buffer[i] != '\0' && !(buffer[i] == '*' && buffer[i + 1] == '/')) {
                if (buffer[i] == '\n') {
                    *line_of_token += 1;
                }
                i++;
            }
            if (buffer[i] == '\0') {
                strncpy(tokens[token_idx].lx, "Error: unexpected eof in comment", LEXEME_LEN);
                tokens[token_idx].ln = *line_of_token;
                strncpy(tokens[token_idx].fl, input_file_name, FILE_NAME_LEN);
                tokens[token_idx].ec = EofInCom;
                tokens[token_idx].tp = ERR;
                token_idx++;
                i++;
                flag = TRUE;
                continue;
            }
            i += 2;
            continue;
        }

        // parse string constants
        if (buffer[i] == '"') {
            int idx = 0;
            tokens[token_idx].lx[0] = '\0';
            i += 1;
            while (buffer[i] != '"') {
                if (buffer[i] == '\0') {
                    tokens[token_idx].tp = ERR;
                    tokens[token_idx].ec = EofInStr;
                    strncpy(tokens[token_idx].lx, "Error: unexpected eof in string constant", LEXEME_LEN);
                    tokens[token_idx].ln = *line_of_token;
                    strncpy(tokens[token_idx].fl, input_file_name, FILE_NAME_LEN);
                    token_idx += 1;
                    i += 1;
                    flag = TRUE;
                    break;
                } else if (buffer[i] == '\n') {
                    tokens[token_idx].tp = ERR;
                    tokens[token_idx].ec = NewLnInStr;
                    strncpy(tokens[token_idx].lx, "Error: new line in string constant", LEXEME_LEN);
                    tokens[token_idx].ln = *line_of_token;
                    strncpy(tokens[token_idx].fl, input_file_name, FILE_NAME_LEN);
                    token_idx += 1;
                    i += 1;
                    flag = TRUE;
                    break;
                }

                if (idx < LEXEME_LEN) {
                    tokens[token_idx].lx[idx] = buffer[i];
                    tokens[token_idx].lx[idx + 1] = '\0';
                }

                idx += 1;
                i += 1;
            }

            tokens[token_idx].tp = STRING;
            tokens[token_idx].ln = *line_of_token;
            strncpy(tokens[token_idx].fl, input_file_name, FILE_NAME_LEN);
            token_idx += 1;
            i += 1;
            flag = TRUE;
            continue;
        }

        // parse integer constants
        if (isdigit(buffer[i])) {
            int idx = 0;
            while (isdigit(buffer[i])) {
                tokens[token_idx].lx[idx] = buffer[i];
                idx += 1;
                i += 1;
            }
            tokens[token_idx].lx[idx] = '\0';
            tokens[token_idx].tp = INT;
            tokens[token_idx].ln = *line_of_token;
            strncpy(tokens[token_idx].fl, input_file_name, FILE_NAME_LEN);
            token_idx += 1;
            flag = TRUE;
            continue;
        }

        // parse identifiers and keywords
        int pos = 0;
        if ((isalpha(buffer[i]) && ((buffer[i] >= 'a' && buffer[i] <= 'z') || (buffer[i] >= 'A' && buffer[i] <= 'Z'))) || buffer[i] == '_' || (pos != 0 && isdigit(buffer[i]))) {
            tokens[token_idx].lx[pos] = buffer[i];
            tokens[token_idx].lx[pos + 1] = '\0';
            pos += 1;
            i += 1;
            while ((isalpha(buffer[i]) && ((buffer[i] >= 'a' && buffer[i] <= 'z') || (buffer[i] >= 'A' && buffer[i] <= 'Z'))) || buffer[i] == '_' || (pos != 0 && isdigit(buffer[i]))) {
                if (pos < LEXEME_LEN) {
                    tokens[token_idx].lx[pos] = buffer[i];
                    tokens[token_idx].lx[pos + 1] = '\0';
                    pos += 1;
                    i += 1;
                } else {
                    break;
                }
            }
            if (is_reserved_word(tokens[token_idx].lx)) {
                tokens[token_idx].tp = RESWORD;
            } else {
                tokens[token_idx].tp = ID;
            }
            tokens[token_idx].ln = *line_of_token;
            strncpy(tokens[token_idx].fl, input_file_name, FILE_NAME_LEN);
            token_idx += 1;
            flag = TRUE;
            continue;
        }

        // parse symbols
        if (buffer[i] == '(' || buffer[i] == ')' || buffer[i] == '[' ||
            buffer[i] == ']' || buffer[i] == '{' || buffer[i] == '}' ||
            buffer[i] == ',' || buffer[i] == ';' || buffer[i] == '.' ||
            buffer[i] == '=' || buffer[i] == '+' || buffer[i] == '-' ||
            buffer[i] == '*' || buffer[i] == '/' || buffer[i] == '&' ||
            buffer[i] == '|' || buffer[i] == '~' || buffer[i] == '<' ||
            buffer[i] == '>') {
            tokens[token_idx].tp = SYMBOL;
            tokens[token_idx].lx[0] = buffer[i];
            tokens[token_idx].lx[1] = '\0';
            tokens[token_idx].ln = *line_of_token;
            strncpy(tokens[token_idx].fl, input_file_name, FILE_NAME_LEN);
            token_idx += 1;
            i += 1;
            flag = TRUE;
            continue;
        } else if (buffer[i] == '\0') {
            strncpy(tokens[token_idx].lx, "End of File", LEXEME_LEN);
            tokens[token_idx].ln = *line_of_token;
            strncpy(tokens[token_idx].fl, input_file_name, FILE_NAME_LEN);
            tokens[token_idx].tp = EOFile;
            token_idx++;
            i++;
            flag = TRUE;
            break;
        } else {
            tokens[token_idx].tp = ERR;
            tokens[token_idx].ec = IllSym;
            tokens[token_idx].ln = *line_of_token;
            strncpy(tokens[token_idx].lx, "Error: illegal symbol in source file", LEXEME_LEN);
            strncpy(tokens[token_idx].fl, input_file_name, FILE_NAME_LEN);
            token_idx++;
            i++;
            flag = TRUE;
            continue;
        }
    }

    *total_tokens = token_idx;
}

// init lexer
int InitLexer(char *file_name) {
    input_file = fopen(file_name, "r");
    if (input_file == NULL) {
        printf("Error when reading the file\n");
        return ERROR;
    }

    input_file_name = (char *)malloc(FILE_NAME_LEN * sizeof(char));
    strncpy(input_file_name, file_name, FILE_NAME_LEN);

    input_file_size = (long *)malloc(sizeof(long));
    fseek(input_file, 0, SEEK_END);
    *input_file_size = ftell(input_file);
    fseek(input_file, 0, SEEK_SET);

    // +1 for '\0'
    buffer = (char *)malloc((*input_file_size + 1) * sizeof(char));

    if (buffer == NULL) {
        printf("Error when allocating memory\n");
        fclose(input_file);
        return ERROR;
    }

    fread(buffer, sizeof(char), *input_file_size, input_file);
    buffer[*input_file_size] = '\0';

    line_of_token = (int *)malloc(sizeof(int));
    *line_of_token = 1;
    parsing_idx = (int *)malloc(sizeof(int));
    *parsing_idx = 0;
    total_tokens = (int *)malloc(sizeof(int));
    *total_tokens = 1;

    parse_tokens(buffer);

    return NO_ERROR;
}

// Get the next token from the source file
Token GetNextToken() {
    Token token = tokens[*parsing_idx];
    *parsing_idx += 1;
    return token;
}

// peek (look) at the next token in the source file without removing it from the stream
Token PeekNextToken() {
    Token token = tokens[*parsing_idx];
    return token;
}

// stop lexer
int StopLexer() {
    if (input_file != NULL) {
        fclose(input_file);
    }
    if (input_file_size != NULL) {
        free(input_file_size);
    }
    if (line_of_token != NULL) {
        free(line_of_token);
    }
    if (parsing_idx != NULL) {
        free(parsing_idx);
    }
    if (buffer != NULL) {
        free(buffer);
    }
    if (input_file_name != NULL) {
        free(input_file_name);
    }
    if (total_tokens != NULL) {
        free(total_tokens);
    }
    if (tokens != NULL) {
        free(tokens);
    }
    return 0;
}