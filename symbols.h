#ifndef SYMBOLS_H
#define SYMBOLS_H

#include "lexer.h"
#include "parser.h"

#define FALSE 0
#define TRUE 1
#define LEXEME_LEN 128

typedef enum {
    CLASS,
    FIELD,
    STATIC,
    VAR,
    CONSTRUCTOR,
    FUNCTION,
    METHOD,
    ARGS,
} SymbolKind;

typedef enum {
    PROGRAM_SCOPE,
    CLASS_SCOPE,
    METHOD_SCOPE
} TableScope;

// forward declaration
struct SymbolTable;

typedef struct TableRow {
    Token token;
    SymbolKind kind;
    char type[128];
    struct SymbolTable *child_table;
    int idx;
    int stack_idx;
} TableRow;

typedef struct SymbolTable {
    TableScope scope;
    char name[128];
    TableRow **rows;
    int size;
    int capacity;
    int symbol_kind_cnt[8];
} SymbolTable;

typedef struct {
    Token token;
    char this[128];
} UncheckedSymbol;

int init_symbol();
int stop_symbol();
SymbolTable *create_table(TableScope scope, char *name);
SymbolTable *get_program_table();
int insert_symbol_into_table(SymbolTable *parent_table, SymbolTable *child_table, SymbolKind kind, Token token, char *type);
TableRow *find_symbol_in_table(SymbolTable *table, char *name);
void add_undeclare(Token token, char *class_name);
ParserInfo find_undeclared_identifier();

#endif