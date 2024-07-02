#include "symbols.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

SymbolTable *program_table = NULL;
UncheckedSymbol *symbol_list = NULL;
int symbol_list_idx = 0;

SymbolTable *get_program_table() {
    return program_table;
}

SymbolTable *create_table(TableScope table_scope, char *name) {
    SymbolTable *table = (SymbolTable *)malloc(sizeof(SymbolTable));
    table->scope = table_scope;
    table->size = 0;
    strncpy(table->name, name, LEXEME_LEN);
    table->capacity = 1000;  // fixed capacity
    table->rows = malloc(sizeof(TableRow *) * table->capacity);
    for (int i = 0; i < table->capacity; i++) {
        table->rows[i] = NULL;
    }
    // init kind count
    for (int i = 0; i < 8; i++) {
        table->symbol_kind_cnt[i] = 0;
    }
    return table;
}

int insert_symbol_into_table(SymbolTable *parent_table, SymbolTable *child_table, SymbolKind kind, Token token, char *type) {
    int index = parent_table->size;  // return the curr size of parent table

    // check for duplicates
    for (int i = 0; i < parent_table->size; i++) {
        if (strncmp(parent_table->rows[i]->token.lx, token.lx, LEXEME_LEN) == 0) {
            return TRUE;
        }
    }

    TableRow *row = (TableRow *)malloc(sizeof(TableRow));
    row->token = token;
    row->idx = index;
    row->kind = kind;
    strncpy(row->type, type, LEXEME_LEN);
    row->child_table = child_table;
    row->stack_idx = parent_table->symbol_kind_cnt[kind];
    parent_table->rows[index] = row;
    parent_table->size = index + 1;  // update actual size of the table
    parent_table->symbol_kind_cnt[kind] += 1;
    return FALSE;
}

TableRow *find_symbol_in_table(SymbolTable *table, char *name) {
    for (int i = 0; i < table->size; i++) {
        if (strncmp(table->rows[i]->token.lx, name, LEXEME_LEN) == 0) {
            return table->rows[i];
        }
    }

    return NULL;
}

void add_undeclare(Token token, char *class_name) {
    for (int i = 0; i < symbol_list_idx; i++) {
        if ((strncmp(symbol_list[i].token.lx, token.lx, LEXEME_LEN) == 0) &&
            ((strncmp(class_name, "", LEXEME_LEN) != 0) ||
             (strncmp(symbol_list[i].this, class_name, LEXEME_LEN) == 0))) {
            return;
        }
    }

    strncpy(symbol_list[symbol_list_idx].this, class_name, LEXEME_LEN);
    symbol_list[symbol_list_idx].token = token;

    symbol_list_idx += 1;
}

ParserInfo find_undeclared_identifier() {
    ParserInfo parser_info;
   

    // symbol_list_idx can possibly be 0
    for (int i = 0; i < symbol_list_idx; i++) {
        UncheckedSymbol s = symbol_list[i];
        if (strncmp(s.this, "", LEXEME_LEN) == 0) {
            // find class in program level table
            if (find_symbol_in_table(program_table, s.token.lx) == NULL) {
                parser_info.tk = s.token;
                parser_info.er = undecIdentifier;
                return parser_info;
            }
        } else {
            // can't find class or can't find subroutine
            TableRow *r = find_symbol_in_table(program_table, s.this);
            if (r == NULL || find_symbol_in_table(r->child_table, s.token.lx) == NULL) {
                parser_info.tk = s.token;
                parser_info.er = undecIdentifier;
                return parser_info;
            }
        }
    }
    parser_info.er = none;
    return parser_info;
}

void free_table(SymbolTable *table) {
    for (int i = 0; i < table->capacity; i++) {
        free(table->rows[i]);
    }
    free(table->rows);
    free(table);
}

int init_symbol() {

    return 1;
}