#include "parser.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "compiler.h"
#include "lexer.h"

#define TRUE 1
#define FALSE 0
#define LEXEME_LEN 128

FILE *output_file;

SymbolTable *class_table;
SymbolTable *method_table;

ParserInfo error_info;
int is_lexer_error = FALSE;

char *class_var_declaration_keywords[] = {"static", "field", NULL};
char *subroutine_declaration_keywords[] = {"function", "method", "constructor", NULL};
char *type_declaration_keywords[] = {"int", "char", "boolean", NULL};
char *statement_keywords[] = {"if", "while", "do", "var", "let", "return", NULL};
char *factor_keywords[] = {"-", "~", "(", "true", "false", "null", "this", NULL};
char *operand_keywords[] = {"true", "false", "null", "this", NULL};

char *vm_commands[] = {"add", "sub", "neg", "eq", "gt", "lt", "and", "or", "not", "pop", "push", "label", "goto", "if-goto", "function", "call", "return"};
char *memory_segments[] = {"static", "argument", "local", "this", "that", "pointer", "temp", "constant"};

int validate_expression();

int is_method = FALSE;
int is_array = FALSE;
int loop_label_idx = 0;
int condition_label_idx = 0;

void new_fprintf(char *cmd, char *seg, int idx) {
    if (output_file == NULL) {
        printf("output file not exists");
        exit(1);
    }
    fprintf(output_file, "%s %s %d\n", cmd, seg, idx);
}

void print_cmd(FILE *output_file, VmCommand cmd, Token token) {
    TableRow *r = find_symbol_in_table(class_table, token.lx);
    if (r != NULL || (method_table != NULL && (r = find_symbol_in_table(method_table, token.lx)) != NULL)) {
        if (r->kind == FIELD) {
            new_fprintf(vm_commands[cmd], memory_segments[THIS_SEG], r->stack_idx);
        } else if (r->kind == STATIC) {
            new_fprintf(vm_commands[cmd], memory_segments[STATIC_SEG], r->stack_idx);
        } else if (r->kind == ARGS) {
            new_fprintf(vm_commands[cmd], memory_segments[ARGUMENT_SEG], r->stack_idx + is_method - 1);
        } else if (r->kind == VAR) {
            new_fprintf(vm_commands[cmd], memory_segments[LOCAL_SEG], r->stack_idx);
        } else {
            printf("error when printing to output file\n");
            exit(1);
        }
    }
}

void print_method_invoke(char *cmd, char *class, char *method, int idx) {
    if (output_file == NULL) {
        printf("output file not exists");
        exit(1);
    }
    fprintf(output_file, "%s %s.%s %d\n", cmd, class, method, idx);
}

void reset() {
    class_table = NULL;
    method_table = NULL;

    if (output_file != NULL) {
        fclose(output_file);
    }

    output_file = NULL;

    loop_label_idx = 0;
    condition_label_idx = 0;
}

int is_lexeme_acceptable(char *lexeme, char **acceptable) {
    int i = 0;
    while (acceptable[i] != NULL) {
        if (strncmp(lexeme, acceptable[i], LEXEME_LEN) == 0) {
            return TRUE;
        }
        i += 1;
    }
    return FALSE;
}

int consume_terminal(TokenType token_type, char **acceptable) {
    Token token0 = PeekNextToken();
    if (token0.tp == ERR) {
        error_info.er = lexerErr;
        error_info.tk = token0;

        is_lexer_error = TRUE;
        return FALSE;
    }

    if (token_type == RESWORD || token_type == SYMBOL) {
        if (token0.tp == token_type && is_lexeme_acceptable(token0.lx, acceptable)) {
            GetNextToken();
            return TRUE;
        }
    } else if (token_type == STRING || token_type == ID || token_type == INT) {
        if (token0.tp == token_type) {
            GetNextToken();
            return TRUE;
        }
    }

    error_info.tk = token0;
    return FALSE;
}

int validate_operand(int in_codegen_phase) {
    Token token1 = PeekNextToken();
    if (token1.tp == ERR) {
        error_info.er = lexerErr;
        error_info.tk = token1;

        is_lexer_error = TRUE;
        return TRUE;
    }

    if (token1.tp == INT || token1.tp == STRING) {
        GetNextToken();

        if (in_codegen_phase == TRUE) {
            if (token1.tp == INT) {
                fprintf(output_file, "%s %s %s\n", vm_commands[PUSH_CM], memory_segments[CONST_SEG], token1.lx);
            } else if (token1.tp == STRING) {
                new_fprintf(vm_commands[PUSH_CM], memory_segments[CONST_SEG], (int)strlen(token1.lx));
                print_method_invoke("call", "String", "new", 1);
                for (int i = 0; i < strlen(token1.lx); i++) {
                    new_fprintf(vm_commands[PUSH_CM], memory_segments[CONST_SEG], token1.lx[i]);
                    print_method_invoke("call", "String", "appendChar", 2);
                }
            }
        }

        return FALSE;
    }

    if (token1.tp == ID) {
        TableRow *class;
        int flag = FALSE;
        Token temp_class_id0 = PeekNextToken();
        Token temp_id_exists1;

        int id_exists0 = consume_terminal(ID, (char *[]){NULL});
        if (is_lexer_error) {
            return TRUE;
        } else if (id_exists0 == FALSE) {
            error_info.er = idExpected;
            return TRUE;
        }

        Token token2 = PeekNextToken();
        if (token2.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token2;
            is_lexer_error = TRUE;
            return TRUE;
        }

        if (is_lexeme_acceptable(token2.lx, (char *[]){".", NULL}) == TRUE) {
            // .
            int dot_exists0 = consume_terminal(SYMBOL, (char *[]){".", NULL});
            if (is_lexer_error == TRUE) {
                return TRUE;
            } else if (dot_exists0 == FALSE) {
                error_info.er = syntaxError;
                return TRUE;
            }
            // identifier
            temp_id_exists1 = PeekNextToken();
            int id_exists1 = consume_terminal(ID, (char *[]){NULL});
            if (is_lexer_error == TRUE) {
                return TRUE;
            } else if (id_exists1 == FALSE) {
                // error_info.er = idExpected;
                error_info.er = idExpected;
                return TRUE;
            }

            class = find_symbol_in_table(get_program_table(), temp_class_id0.lx);
            TableRow *row;
            if ((row = find_symbol_in_table(class_table, temp_class_id0.lx)) != NULL ||
                (method_table != NULL && (row = (find_symbol_in_table(method_table, temp_class_id0.lx))) != NULL)) {
                if (in_codegen_phase == FALSE) {
                    add_undeclare(temp_id_exists1, row->type);
                } else {
                    class = find_symbol_in_table(get_program_table(), row->type);
                }
            } else if (class == NULL) {
                if (in_codegen_phase == FALSE) {
                    add_undeclare(temp_id_exists1, temp_class_id0.lx);
                    add_undeclare(temp_class_id0, "");
                }
            } else {
                if (in_codegen_phase == FALSE) {
                    add_undeclare(temp_id_exists1, temp_class_id0.lx);
                }
            }

            if (in_codegen_phase == TRUE) {
                flag = TRUE;
                // printf("temp_class_id0 :%s\n", temp_class_id0.lx);
                print_cmd(output_file, PUSH_CM, temp_class_id0);
            }

        } else {
            SymbolTable *tr;
            if (find_symbol_in_table(tr = class_table, temp_class_id0.lx) == NULL && (method_table == NULL || find_symbol_in_table(tr = method_table, temp_class_id0.lx) == NULL)) {
                if (in_codegen_phase == FALSE) {
                    add_undeclare(temp_class_id0, tr->name);
                } else {
                    flag = FALSE;
                }
            }
            if (in_codegen_phase == TRUE) {
                flag = FALSE;
            }
        }

        Token token3 = PeekNextToken();
        if (token3.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token3;
            is_lexer_error = TRUE;
            return TRUE;
        }

        // [expression]
        if (is_lexeme_acceptable(token3.lx, (char *[]){"[", NULL}) == TRUE) {
            int open_bracket_exists0 = consume_terminal(SYMBOL, (char *[]){"[", NULL});
            if (is_lexer_error == TRUE) {
                return TRUE;
            } else if (open_bracket_exists0 == FALSE) {
                error_info.er = syntaxError;
                return TRUE;
            }

            Token token4 = PeekNextToken();
            if (token4.tp == ERR) {
                error_info.er = lexerErr;
                error_info.tk = token4;
                is_lexer_error = TRUE;
                return TRUE;
            }

            int is_expression0 = token4.tp == ID || token4.tp == STRING || token4.tp == INT || (token4.tp == RESWORD && is_lexeme_acceptable(token4.lx, factor_keywords)) || (token4.tp == SYMBOL && is_lexeme_acceptable(token4.lx, factor_keywords));
            if (is_expression0 == TRUE) {
                int error_in_expression0 = validate_expression(in_codegen_phase);
                if (error_in_expression0) {
                    return TRUE;
                }
            } else {
                error_info.er = syntaxError;
                error_info.tk = token4;
                return TRUE;
            }

            int close_bracket_exists0 = consume_terminal(SYMBOL, (char *[]){"]", NULL});
            if (is_lexer_error == TRUE) {
                return TRUE;
            } else if (close_bracket_exists0 == FALSE) {
                // error_info.er = closeBracketExpected;
                error_info.er = closeBracketExpected;
                return TRUE;
            }

            if (in_codegen_phase == TRUE) {
                if (flag == TRUE) {
                    TableRow *rr;
                    class = find_symbol_in_table(get_program_table(), temp_class_id0.lx);
                    if ((rr = find_symbol_in_table(class_table, temp_class_id0.lx)) != NULL || (method_table != NULL && (rr = find_symbol_in_table(method_table, temp_class_id0.lx)) != NULL)) {
                        class = find_symbol_in_table(get_program_table(), rr->type);
                    }

                    int stack_idx = find_symbol_in_table(class->child_table, temp_id_exists1.lx)->child_table->symbol_kind_cnt[ARGS] - 1 + (find_symbol_in_table(class->child_table, temp_id_exists1.lx)->kind == METHOD);
                    print_method_invoke(vm_commands[CALL_CM], class->token.lx, temp_id_exists1.lx, stack_idx);
                } else {
                    print_cmd(output_file, PUSH_CM, temp_class_id0);
                }
                fprintf(output_file, "%s\n", vm_commands[ADD_CM]);
                new_fprintf(vm_commands[POP_CM], memory_segments[POINTER_SEG], 1);
                new_fprintf(vm_commands[PUSH_CM], memory_segments[THAT_SEG], 0);

                return FALSE;
            }
        }

        Token token50 = PeekNextToken();
        if (token50.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token50;
            is_lexer_error = TRUE;
            return TRUE;
        }

        // (expressions)
        if (is_lexeme_acceptable(token50.lx, (char *[]){"(", NULL}) == TRUE) {
            // (
            int open_paren_exists0 = consume_terminal(SYMBOL, (char *[]){"(", NULL});
            if (is_lexer_error == TRUE) {
                return TRUE;
            } else if (open_paren_exists0 == FALSE) {
                error_info.er = openParenExpected;
                return TRUE;
            }

            Token token5 = PeekNextToken();
            if (token5.tp == ERR) {
                error_info.er = lexerErr;
                error_info.tk = token5;
                is_lexer_error = TRUE;
                return TRUE;
            }

            if (is_lexeme_acceptable(token5.lx, (char *[]){")", NULL}) == FALSE) {
                int is_expression1 = token5.tp == ID || token5.tp == STRING || token5.tp == INT || (token5.tp == RESWORD && is_lexeme_acceptable(token5.lx, factor_keywords)) || (token5.tp == SYMBOL && is_lexeme_acceptable(token5.lx, factor_keywords));
                if (is_expression1 == TRUE) {
                    int e_in_exp1 = validate_expression(in_codegen_phase);
                    if (e_in_exp1 == TRUE) {
                        return TRUE;
                    }
                } else {
                    error_info.er = syntaxError;
                    error_info.tk = token5;
                    return TRUE;
                }

                while (TRUE) {
                    Token token6 = PeekNextToken();
                    if (token6.tp == ERR) {
                        error_info.er = lexerErr;
                        error_info.tk = token6;
                        is_lexer_error = TRUE;
                        return TRUE;
                    }

                    if (is_lexeme_acceptable(token6.lx, (char *[]){",", NULL}) == FALSE) {
                        break;
                    }

                    // ,
                    GetNextToken();

                    Token token7 = PeekNextToken();
                    if (token7.tp == ERR) {
                        error_info.er = lexerErr;
                        error_info.tk = token7;
                        is_lexer_error = TRUE;
                        return TRUE;
                    }

                    int is_expression2 = token7.tp == ID || token7.tp == STRING || token7.tp == INT || (token7.tp == RESWORD && is_lexeme_acceptable(token7.lx, factor_keywords)) || (token7.tp == SYMBOL && is_lexeme_acceptable(token7.lx, factor_keywords));
                    if (is_expression2 == TRUE) {
                        // GetNextToken();
                        int error2 = validate_expression(in_codegen_phase);
                        if (error2 == TRUE) {
                            return TRUE;
                        }
                    } else {
                        error_info.er = syntaxError;
                        error_info.tk = token7;
                        return TRUE;
                    }
                }
            }

            // )
            int close_paren_exists0 = consume_terminal(SYMBOL, (char *[]){")", NULL});
            if (is_lexer_error == TRUE) {
                return TRUE;
            } else if (close_paren_exists0 == FALSE) {
                error_info.er = closeParenExpected;
                return TRUE;
            }

            if (in_codegen_phase == TRUE) {
                if (flag == TRUE) {
                    TableRow *rz = find_symbol_in_table(class_table, temp_class_id0.lx);

                    class = find_symbol_in_table(get_program_table(), temp_class_id0.lx);

                    if (rz != NULL || (method_table != NULL && (rz = find_symbol_in_table(method_table, temp_class_id0.lx)) != NULL)) {
                        class = find_symbol_in_table(get_program_table(), rz->type);
                    }

                    int stack_idx = find_symbol_in_table(class->child_table, temp_id_exists1.lx)->child_table->symbol_kind_cnt[ARGS] - 1 + (find_symbol_in_table(class->child_table, temp_id_exists1.lx)->kind == METHOD);

                    print_method_invoke(vm_commands[CALL_CM], class->token.lx, temp_id_exists1.lx, stack_idx);

                } else {
                    print_cmd(output_file, PUSH_CM, temp_class_id0);
                }
            }

            return FALSE;
        }

        if (in_codegen_phase == TRUE) {
            if (flag == TRUE) {
                TableRow *r = find_symbol_in_table(class_table, temp_class_id0.lx);
                class = find_symbol_in_table(get_program_table(), temp_class_id0.lx);

                if (r != NULL || (method_table != NULL && (r = find_symbol_in_table(method_table, temp_class_id0.lx)) != NULL)) {
                    class = find_symbol_in_table(get_program_table(), r->type);
                }

                int stack_idx = find_symbol_in_table(class->child_table, temp_id_exists1.lx)->child_table->symbol_kind_cnt[ARGS] - 1 + (find_symbol_in_table(class->child_table, temp_id_exists1.lx)->kind == METHOD);
                fprintf(output_file, "%s %s.%s %d\n", vm_commands[CALL_CM], class->token.lx, temp_id_exists1.lx, stack_idx);
            } else {
                print_cmd(output_file, PUSH_CM, temp_class_id0);
            }
        }

        return FALSE;
    }

    // (expression)
    if (token1.tp == SYMBOL && is_lexeme_acceptable(token1.lx, (char *[]){"(", NULL}) == TRUE) {
        // (
        int open_paren_exists1 = consume_terminal(SYMBOL, (char *[]){"(", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (open_paren_exists1 == FALSE) {
            error_info.er = openParenExpected;
            return TRUE;
        }

        Token token8 = PeekNextToken();
        if (token8.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token8;
            is_lexer_error = TRUE;
            return TRUE;
        }

        int is_expression3 = token8.tp == ID || token8.tp == STRING || token8.tp == INT || (token8.tp == RESWORD && is_lexeme_acceptable(token8.lx, factor_keywords)) || (token8.tp == SYMBOL && is_lexeme_acceptable(token8.lx, factor_keywords));
        if (is_expression3 == TRUE) {
            int error_in_expression3 = validate_expression(in_codegen_phase);
            if (error_in_expression3) {
                return TRUE;
            }
        } else {
            error_info.er = syntaxError;
            error_info.tk = token8;
            return TRUE;
        }

        // )
        int close_paren_exists1 = consume_terminal(SYMBOL, (char *[]){")", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (close_paren_exists1 == FALSE) {
            error_info.er = closeParenExpected;
            return TRUE;
        }

        return FALSE;
    }

    Token operand_keyword = PeekNextToken();
    // true | false | null | this
    int is_valid0 = consume_terminal(RESWORD, operand_keywords);
    if (is_lexer_error) {
        return TRUE;
    } else if (is_valid0 == FALSE) {
        error_info.er = syntaxError;
        return TRUE;
    }

    if (in_codegen_phase == TRUE) {
        if (strncmp(operand_keyword.lx, "true", 5) == 0) {
            new_fprintf(vm_commands[PUSH_CM], memory_segments[CONST_SEG], 0);
            fprintf(output_file, "%s\n", vm_commands[NOT_CM]);
        } else if (!strncmp(operand_keyword.lx, "this", 5)) {
            new_fprintf(vm_commands[PUSH_CM], memory_segments[POINTER_SEG], 0);
        } else {
            new_fprintf(vm_commands[PUSH_CM], memory_segments[CONST_SEG], 0);
        }
    }

    return FALSE;
}

int validate_factor(int in_codegen_phase) {
    Token token9 = PeekNextToken();
    if (token9.tp == ERR) {
        error_info.er = lexerErr;
        error_info.tk = token9;
        is_lexer_error = TRUE;
        return TRUE;
    }

    if (token9.tp == SYMBOL && is_lexeme_acceptable(token9.lx, (char *[]){"-", "~", NULL})) {
        int is_valid1 = consume_terminal(SYMBOL, (char *[]){"-", "~", NULL});
        if (is_lexer_error) {
            return TRUE;
        } else if (is_valid1 == FALSE) {
            error_info.er = syntaxError;
            return TRUE;
        }

        Token token10 = PeekNextToken();
        if (token10.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token10;
            is_lexer_error = TRUE;
            return TRUE;
        }

        if ((token10.tp == INT || token10.tp == ID || token10.tp == STRING) ||
            (token10.tp == SYMBOL && is_lexeme_acceptable(token10.lx, (char *[]){"(", NULL})) ||
            (token10.tp == RESWORD && is_lexeme_acceptable(token10.lx, operand_keywords))) {
            int error_in_operand = validate_operand(in_codegen_phase);
            if (error_in_operand == TRUE) {
                return TRUE;
            }
        }

        if (in_codegen_phase == TRUE) {
            if (strncmp(token9.lx, "-", 2) == 0) {
                fprintf(output_file, "%s\n", vm_commands[NEG_CM]);
            } else {
                fprintf(output_file, "%s\n", vm_commands[NOT_CM]);
            }
        }
    } else if ((token9.tp == INT || token9.tp == ID || token9.tp == STRING) ||
               (token9.tp == SYMBOL && is_lexeme_acceptable(token9.lx, (char *[]){"(", NULL})) ||
               (token9.tp == RESWORD && is_lexeme_acceptable(token9.lx, operand_keywords))) {
        int error_in_operand = validate_operand(in_codegen_phase);
        if (error_in_operand == TRUE) {
            return TRUE;
        }
    }

    return FALSE;
}

int validate_term(int in_codegen_phase) {
    Token token11 = PeekNextToken();
    if (token11.tp == ERR) {
        error_info.er = lexerErr;
        error_info.tk = token11;
        is_lexer_error = TRUE;
        return TRUE;
    }

    int is_expression4 = token11.tp == ID || token11.tp == STRING || token11.tp == INT || (token11.tp == RESWORD && is_lexeme_acceptable(token11.lx, factor_keywords)) || (token11.tp == SYMBOL && is_lexeme_acceptable(token11.lx, factor_keywords));
    if (is_expression4 == FALSE) {
        error_info.er = syntaxError;
        error_info.tk = token11;
        return TRUE;
    }

    int error_exists_in_factor4 = validate_factor(in_codegen_phase);
    if (error_exists_in_factor4 == TRUE) {
        return TRUE;
    }

    while (TRUE) {
        Token token12 = PeekNextToken();
        if (token12.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token12;
            is_lexer_error = TRUE;
            return TRUE;
        }

        if (is_lexeme_acceptable(token12.lx, (char *[]){"*", "/", NULL}) == FALSE) {
            break;
        }

        int is_valid_terminal2 = consume_terminal(SYMBOL, (char *[]){"*", "/", NULL});
        if (is_lexer_error) {
            return TRUE;
        } else if (is_valid_terminal2 == FALSE) {
            error_info.er = syntaxError;
            return TRUE;
        }

        Token token13 = PeekNextToken();
        if (token13.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token13;
            is_lexer_error = TRUE;
            return TRUE;
        }

        int is_expression5 = token13.tp == ID || token13.tp == STRING || token13.tp == INT || (token13.tp == RESWORD && is_lexeme_acceptable(token13.lx, factor_keywords)) || (token13.tp == SYMBOL && is_lexeme_acceptable(token13.lx, factor_keywords));
        if (is_expression5 == FALSE) {
            error_info.er = syntaxError;
            error_info.tk = token13;
            return TRUE;
        }

        int error_in_factor5 = validate_factor(in_codegen_phase);
        if (error_in_factor5 == TRUE) {
            return TRUE;
        }

        if (in_codegen_phase == TRUE) {
            if (strncmp(token12.lx, "*", 2) == 0) {
                print_method_invoke(vm_commands[CALL_CM], "Math", "multiply", 2);
            } else {
                print_method_invoke(vm_commands[CALL_CM], "Math", "divide", 2);
            }
        }
    }
    return FALSE;
}

int validate_arithmetic_expression(int in_codegen_phase) {
    Token token14 = PeekNextToken();
    if (token14.tp == ERR) {
        error_info.er = lexerErr;
        error_info.tk = token14;
        is_lexer_error = TRUE;
        return TRUE;
    }

    int is_expression6 = token14.tp == ID || token14.tp == STRING || token14.tp == INT || (token14.tp == RESWORD && is_lexeme_acceptable(token14.lx, factor_keywords)) || (token14.tp == SYMBOL && is_lexeme_acceptable(token14.lx, factor_keywords));
    if (is_expression6 == FALSE) {
        error_info.er = syntaxError;
        error_info.tk = token14;
        return TRUE;
    }

    int error_exists_in_term6 = validate_term(in_codegen_phase);
    if (error_exists_in_term6 == TRUE) {
        return TRUE;
    }

    while (TRUE) {
        Token token15 = PeekNextToken();
        if (token15.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token15;
            is_lexer_error = TRUE;
            return TRUE;
        }

        if (is_lexeme_acceptable(token15.lx, (char *[]){"+", "-", NULL}) == FALSE) {
            break;
        }

        int is_valid_terminal3 = consume_terminal(SYMBOL, (char *[]){"+", "-", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (is_valid_terminal3 == FALSE) {
            error_info.er = syntaxError;
            return TRUE;
        }

        Token token16 = PeekNextToken();
        if (token16.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token16;
            is_lexer_error = TRUE;
            return TRUE;
        }

        int is_expression7 = token16.tp == ID || token16.tp == STRING || token16.tp == INT || (token16.tp == RESWORD && is_lexeme_acceptable(token16.lx, factor_keywords)) || (token16.tp == SYMBOL && is_lexeme_acceptable(token16.lx, factor_keywords));
        if (is_expression7 == FALSE) {
            error_info.er = syntaxError;
            error_info.tk = token16;
            return TRUE;
        }

        int error_in_term7 = validate_term(in_codegen_phase);
        if (error_in_term7) {
            return TRUE;
        }

        if (in_codegen_phase == TRUE) {
            if (strncmp(token15.lx, "+", 2) == 0) {
                fprintf(output_file, "%s\n", vm_commands[ADD_CM]);
            } else {
                fprintf(output_file, "%s\n", vm_commands[SUB_CM]);
            }
        }
    }
    return FALSE;
}

int validate_relational_expression(int in_codegen_phase) {
    Token token17 = PeekNextToken();
    if (token17.tp == ERR) {
        error_info.er = lexerErr;
        error_info.tk = token17;
        is_lexer_error = TRUE;
        return TRUE;
    }

    int is_expression8 = token17.tp == ID || token17.tp == STRING || token17.tp == INT || (token17.tp == RESWORD && is_lexeme_acceptable(token17.lx, factor_keywords)) || (token17.tp == SYMBOL && is_lexeme_acceptable(token17.lx, factor_keywords));
    if (is_expression8 == FALSE) {
        error_info.er = syntaxError;
        error_info.tk = token17;
        return TRUE;
    }

    int error_exists_in_arithmetic_expression8 = validate_arithmetic_expression(in_codegen_phase);
    if (error_exists_in_arithmetic_expression8 == TRUE) {
        return TRUE;
    }

    while (TRUE) {
        Token token18 = PeekNextToken();
        if (token18.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token18;
            is_lexer_error = TRUE;
            return TRUE;
        }

        if (is_lexeme_acceptable(token18.lx, (char *[]){"=", ">", "<", NULL}) == FALSE) {
            break;
        }

        int is_valid_terminal4 = consume_terminal(SYMBOL, (char *[]){"=", "<", ">", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (is_valid_terminal4 == FALSE) {
            error_info.er = equalExpected;
            return TRUE;
        }

        Token token19 = PeekNextToken();
        if (token19.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token19;
            is_lexer_error = TRUE;
            return TRUE;
        }

        int is_expression9 = token19.tp == ID || token19.tp == STRING || token19.tp == INT || (token19.tp == RESWORD && is_lexeme_acceptable(token19.lx, factor_keywords)) || (token19.tp == SYMBOL && is_lexeme_acceptable(token19.lx, factor_keywords));
        if (is_expression9 == FALSE) {
            error_info.er = syntaxError;
            error_info.tk = token19;
            return TRUE;
        }

        int error_in_arithmetic_expression9 = validate_arithmetic_expression(in_codegen_phase);
        if (error_in_arithmetic_expression9) {
            return TRUE;
        }

        if (in_codegen_phase == TRUE) {
            if (strncmp(token18.lx, "=", 2) == 0) {
                fprintf(output_file, "%s\n", vm_commands[EQ_CM]);
            } else if (strncmp(token18.lx, ">", 2) == 0) {
                fprintf(output_file, "%s\n", vm_commands[GT_CM]);
            } else {
                fprintf(output_file, "%s\n", vm_commands[LT_CM]);
            }
        }
    }
    return FALSE;
}

int validate_expression(int in_codegen_phase) {
    Token token20 = PeekNextToken();
    if (token20.tp == ERR) {
        error_info.er = lexerErr;
        error_info.tk = token20;
        is_lexer_error = TRUE;
        return TRUE;
    }

    int is_expression10 = token20.tp == ID || token20.tp == STRING || token20.tp == INT || (token20.tp == RESWORD && is_lexeme_acceptable(token20.lx, factor_keywords)) || (token20.tp == SYMBOL && is_lexeme_acceptable(token20.lx, factor_keywords));
    if (is_expression10 == FALSE) {
        error_info.er = syntaxError;
        error_info.tk = token20;
        return TRUE;
    }

    int error_exists_in_relational_expression10 = validate_relational_expression(in_codegen_phase);
    if (error_exists_in_relational_expression10) {
        return TRUE;
    }

    while (TRUE) {
        Token token21 = PeekNextToken();
        if (token21.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token21;
            is_lexer_error = TRUE;
            return TRUE;
        }

        if (is_lexeme_acceptable(token21.lx, (char *[]){"&", "|", NULL}) == FALSE) {
            break;
        }

        int is_valid_terminal5 = consume_terminal(SYMBOL, (char *[]){"&", "|", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (is_valid_terminal5 == FALSE) {
            error_info.er = syntaxError;
            return TRUE;
        }

        Token token22 = PeekNextToken();
        if (token22.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token22;
            is_lexer_error = TRUE;
            return TRUE;
        }

        int is_expression11 = token22.tp == ID || token22.tp == STRING || token22.tp == INT || (token22.tp == RESWORD && is_lexeme_acceptable(token22.lx, factor_keywords)) || (token22.tp == SYMBOL && is_lexeme_acceptable(token22.lx, factor_keywords));
        if (is_expression11 == FALSE) {
            error_info.er = syntaxError;
            error_info.tk = token22;
            return TRUE;
        }

        int error_in_relational_expression11 = validate_relational_expression(in_codegen_phase);
        if (error_in_relational_expression11 == TRUE) {
            return TRUE;
        }

        if (in_codegen_phase == TRUE) {
            if (strncmp(token21.lx, "&", 2) == 0) {
                fprintf(output_file, "%s\n", vm_commands[AND_CM]);
            } else {
                fprintf(output_file, "%s\n", vm_commands[OR_CM]);
            }
        }
    }
    return FALSE;
}

int validate_class_var_declaration(int in_codegen_phase) {
    Token temp_static_or_field = PeekNextToken();
    // check "static" or "field" keyword
    int static_or_field_keyword_exists = consume_terminal(RESWORD, class_var_declaration_keywords);
    if (is_lexer_error) {
        return TRUE;
    } else if (static_or_field_keyword_exists == FALSE) {
        error_info.er = classVarErr;
        return TRUE;
    }

    // check type
    Token token23 = PeekNextToken();
    if (token23.tp == ERR) {
        error_info.er = lexerErr;
        error_info.tk = token23;
        is_lexer_error = TRUE;
        return TRUE;
    }
    int is_type_declaration0 = token23.tp == ID || (token23.tp == RESWORD && is_lexeme_acceptable(token23.lx, type_declaration_keywords));
    if (is_type_declaration0 == TRUE) {
        if (in_codegen_phase == FALSE && token23.tp == ID) {
            if (find_symbol_in_table(get_program_table(), token23.lx) == NULL) {
                add_undeclare(token23, "");
            }
        }
        GetNextToken();
    } else {
        error_info.er = illegalType;
        error_info.tk = token23;
        return TRUE;
    }

    // check identifier
    Token temp_class_var = PeekNextToken();
    int identifier_exists0 = consume_terminal(ID, (char *[]){NULL});
    if (is_lexer_error == TRUE) {
        return TRUE;
    } else if (identifier_exists0 == FALSE) {
        error_info.er = idExpected;
        return TRUE;
    }
    SymbolKind class_var_kind;
    if (strncmp("field", temp_static_or_field.lx, LEXEME_LEN) == 0) {
        class_var_kind = FIELD;
    } else {
        class_var_kind = STATIC;
    }
    if (in_codegen_phase == FALSE) {
        int duplicate_class_var_exists = insert_symbol_into_table(class_table, NULL, class_var_kind, temp_class_var, token23.lx);
        if (duplicate_class_var_exists == TRUE) {
            error_info.er = redecIdentifier;
            error_info.tk = temp_class_var;
            return TRUE;
        }
    }

    //  {, identifier}
    while (TRUE) {
        Token token24 = PeekNextToken();
        if (token24.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token24;
            is_lexer_error = TRUE;
        }

        if (is_lexeme_acceptable(token24.lx, (char *[]){",", NULL}) == FALSE) {
            break;
        }

        int comma_exists0 = consume_terminal(SYMBOL, (char *[]){",", NULL});
        if (is_lexer_error) {
            return TRUE;
        } else if (comma_exists0 == FALSE) {
            error_info.er = syntaxError;
            return TRUE;
        }

        Token temp_id_exists3 = PeekNextToken();
        int id_exists3 = consume_terminal(ID, (char *[]){NULL});
        if (is_lexer_error) {
            return TRUE;
        } else if (id_exists3 == FALSE) {
            error_info.er = idExpected;
            return TRUE;
        }
        if (in_codegen_phase == FALSE) {
            int duplicate_temp_id3_exists = insert_symbol_into_table(class_table, NULL, class_var_kind, temp_id_exists3, token23.lx);
            if (duplicate_temp_id3_exists == TRUE) {
                error_info.er = redecIdentifier;
                error_info.tk = temp_id_exists3;
            }
        }
    }

    // ;
    int semicolon_exists0 = consume_terminal(SYMBOL, (char *[]){";", NULL});
    if (is_lexer_error) {
        return TRUE;
    } else if (semicolon_exists0 == FALSE) {
        error_info.er = semicolonExpected;
        return TRUE;
    }
    return FALSE;
}

int validate_subroutine_params(int in_codegen_phase) {
    Token token25 = PeekNextToken();
    if (token25.tp == ERR) {
        error_info.er = lexerErr;
        error_info.tk = token25;
        is_lexer_error = TRUE;
        return TRUE;
    }

    int is_type_declaration1 = token25.tp == ID || (token25.tp == RESWORD && is_lexeme_acceptable(token25.lx, type_declaration_keywords));
    if (is_type_declaration1) {
        if (in_codegen_phase == FALSE && token25.tp == ID) {
            if (find_symbol_in_table(get_program_table(), token25.lx) == NULL) {
                add_undeclare(token25, "");
            }
        }
        GetNextToken();
    } else {
        error_info.er = illegalType;
        error_info.tk = token25;
        return TRUE;
    }

    Token temp_method_param1 = PeekNextToken();
    int id_exists4 = consume_terminal(ID, (char *[]){NULL});
    if (is_lexer_error == TRUE) {
        return TRUE;
    } else if (id_exists4 == FALSE) {
        error_info.er = idExpected;
        return TRUE;
    }
    if (in_codegen_phase == FALSE) {
        int duplicate_temp_method_param1_exists = insert_symbol_into_table(method_table, NULL, ARGS, temp_method_param1, token25.lx);
        if (duplicate_temp_method_param1_exists == TRUE) {
            error_info.er = redecIdentifier;
            error_info.tk = temp_method_param1;
            return TRUE;
        }
    }

    while (TRUE) {
        Token token26 = PeekNextToken();
        if (token26.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token26;
            is_lexer_error = TRUE;
            return TRUE;
        }

        if (is_lexeme_acceptable(token26.lx, (char *[]){",", NULL}) == FALSE) {
            break;
        }

        int comma_exists1 = consume_terminal(SYMBOL, (char *[]){",", NULL});
        if (is_lexer_error) {
            return TRUE;
        } else if (comma_exists1 == FALSE) {
            error_info.er = syntaxError;
            return TRUE;
        }

        Token token27 = PeekNextToken();
        if (token27.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token27;
            is_lexer_error = TRUE;
            return TRUE;
        }

        int is_type2 = token27.tp == ID || (token27.tp == RESWORD && is_lexeme_acceptable(token27.lx, type_declaration_keywords));
        if (is_type2) {
            if (in_codegen_phase == FALSE && token27.tp == ID) {
                if (find_symbol_in_table(get_program_table(), token27.lx) == NULL) {
                    add_undeclare(token27, "");
                }
            }
            GetNextToken();
        } else {
            error_info.er = illegalType;
            error_info.tk = token27;
            return TRUE;
        }

        Token temp_method_param2 = PeekNextToken();
        int identifier_exists1 = consume_terminal(ID, (char *[]){NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (identifier_exists1 == FALSE) {
            error_info.er = idExpected;
            return TRUE;
        }
        if (in_codegen_phase == FALSE) {
            int duplicate_temp_method_param2_exists = insert_symbol_into_table(method_table, NULL, ARGS, temp_method_param2, token27.lx);
            if (duplicate_temp_method_param2_exists == TRUE) {
                error_info.er = redecIdentifier;
                error_info.tk = temp_method_param2;
                return TRUE;
            }
        }
    }

    return FALSE;
}

int validate_subroutine_statement(int in_codegen_phase) {
    Token token28 = PeekNextToken();
    if (token28.tp == ERR) {
        error_info.er = lexerErr;
        error_info.tk = token28;
        is_lexer_error = TRUE;
        return TRUE;
    }

    if (strncmp(token28.lx, "if", LEXEME_LEN) == 0) {
        int curr_condition_label_idx = loop_label_idx;
        loop_label_idx += 1;

        // if
        int if_keyword_exists = consume_terminal(RESWORD, (char *[]){"if", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (if_keyword_exists == FALSE) {
            error_info.er = syntaxError;
            return TRUE;
        }

        // (
        int open_paren_exists2 = consume_terminal(SYMBOL, (char *[]){"(", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (open_paren_exists2 == FALSE) {
            error_info.er = openParenExpected;
            return TRUE;
        }

        // check if expression is valid
        Token token29 = PeekNextToken();
        if (token29.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token29;
            is_lexer_error = TRUE;
            return TRUE;
        }
        int is_expression12 = token29.tp == ID || token29.tp == STRING || token29.tp == INT || (token29.tp == RESWORD && is_lexeme_acceptable(token29.lx, factor_keywords)) || (token29.tp == SYMBOL && is_lexeme_acceptable(token29.lx, factor_keywords));
        if (is_expression12 == TRUE) {
            int error_in_expression12 = validate_expression(in_codegen_phase);
            if (error_in_expression12 == TRUE) {
                return TRUE;
            }
        } else {
            error_info.er = syntaxError;
            error_info.tk = token29;
            return TRUE;
        }

        // )
        int close_paren_exists2 = consume_terminal(SYMBOL, (char *[]){")", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (close_paren_exists2 == FALSE) {
            error_info.er = closeParenExpected;
            return TRUE;
        }

        if (in_codegen_phase == TRUE) {
            new_fprintf(vm_commands[IF_GOTO_CM], "", curr_condition_label_idx);
            new_fprintf(vm_commands[GOTO_CM], "", curr_condition_label_idx);
            new_fprintf(vm_commands[LABEL_CM], "", curr_condition_label_idx);
        }

        // {
        int open_brace_exists0 = consume_terminal(SYMBOL, (char *[]){"{", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (open_brace_exists0 == FALSE) {
            error_info.er = openBraceExpected;
            return TRUE;
        }

        // statement
        while (TRUE) {
            Token token30 = PeekNextToken();
            if (token30.tp == ERR) {
                error_info.er = lexerErr;
                error_info.tk = token30;
                is_lexer_error = TRUE;
                return TRUE;
            }

            if (is_lexeme_acceptable(token30.lx, statement_keywords) == FALSE) {
                break;
            }

            int error_exists0 = validate_subroutine_statement(in_codegen_phase);
            if (error_exists0 == TRUE) {
                return TRUE;
            }
        }

        // }
        int close_brace_exists0 = consume_terminal(SYMBOL, (char *[]){"}", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (close_brace_exists0 == FALSE) {
            error_info.er = closeBraceExpected;
            return TRUE;
        }

        // 'else'
        Token token31 = PeekNextToken();
        if (token31.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token31;
            is_lexer_error = TRUE;
            return TRUE;
        }
        if (token31.tp == RESWORD && is_lexeme_acceptable(token31.lx, (char *[]){"else", NULL})) {
            if (in_codegen_phase == TRUE) {
                fprintf(output_file, "%s %d\n", vm_commands[GOTO_CM], curr_condition_label_idx);
                fprintf(output_file, "%s %d\n", vm_commands[LABEL_CM], curr_condition_label_idx);
            }

            int else_keyword_exists = consume_terminal(RESWORD, (char *[]){"else", NULL});
            if (is_lexer_error == TRUE) {
                return TRUE;
            } else if (else_keyword_exists == FALSE) {
                error_info.er = syntaxError;
                return TRUE;
            }

            // {
            int ob_exists0 = consume_terminal(SYMBOL, (char *[]){"{", NULL});
            if (is_lexer_error == TRUE) {
                return TRUE;
            } else if (ob_exists0 == FALSE) {
                error_info.er = openBraceExpected;
                return TRUE;
            }

            while (TRUE) {
                Token token32 = PeekNextToken();
                if (token32.tp == ERR) {
                    error_info.er = lexerErr;
                    error_info.tk = token32;
                    is_lexer_error = TRUE;
                    return TRUE;
                }
                if (is_lexeme_acceptable(token32.lx, statement_keywords) == FALSE) {
                    break;
                }
                int e0 = validate_subroutine_statement(in_codegen_phase);
                if (e0 == TRUE) {
                    return TRUE;
                }
            }

            // }
            int cb_exists0 = consume_terminal(SYMBOL, (char *[]){"}", NULL});
            if (is_lexer_error == TRUE) {
                return TRUE;
            } else if (cb_exists0 == FALSE) {
                error_info.er = closeBraceExpected;
                return TRUE;
            }

            if (in_codegen_phase == TRUE) {
                fprintf(output_file, "%s %d\n", vm_commands[LABEL_CM], curr_condition_label_idx);
            }
        } else {
            if (in_codegen_phase == TRUE) {
                fprintf(output_file, "%s %d\n", vm_commands[LABEL_CM], curr_condition_label_idx);
            }
        }
        return FALSE;
    } else if (strncmp(token28.lx, "while", LEXEME_LEN) == 0) {
        int curr_loop_label_idx = loop_label_idx;
        loop_label_idx += 1;

        // "while"
        int while_keyword_exists = consume_terminal(RESWORD, (char *[]){"while", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (while_keyword_exists == FALSE) {
            error_info.er = syntaxError;
            return TRUE;
        }

        if (in_codegen_phase == TRUE) {
            fprintf(output_file, "%s %d\n", vm_commands[LABEL_CM], curr_loop_label_idx);
        }

        // "("
        int open_paren_exists3 = consume_terminal(SYMBOL, (char *[]){"(", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (open_paren_exists3 == FALSE) {
            error_info.er = openParenExpected;
            return TRUE;
        }

        // expression
        Token token33 = PeekNextToken();
        if (token33.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token33;
            is_lexer_error = TRUE;
            return TRUE;
        }

        int is_expression13 = token33.tp == ID || token33.tp == STRING || token33.tp == INT || (token33.tp == RESWORD && is_lexeme_acceptable(token33.lx, factor_keywords)) || (token33.tp == SYMBOL && is_lexeme_acceptable(token33.lx, factor_keywords));
        if (is_expression13 == TRUE) {
            int error_in_expression13 = validate_expression(in_codegen_phase);
            if (error_in_expression13 == TRUE) {
                return TRUE;
            }
        } else {
            error_info.er = syntaxError;
            error_info.tk = token33;
            return TRUE;
        }

        // ")"
        int close_paren_exists3 = consume_terminal(SYMBOL, (char *[]){")", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (close_paren_exists3 == FALSE) {
            error_info.er = closeParenExpected;
            return TRUE;
        }

        if (in_codegen_phase == TRUE) {
            fprintf(output_file, "%s\n", vm_commands[NOT_CM]);
            fprintf(output_file, "%s %d\n", vm_commands[IF_GOTO_CM], curr_loop_label_idx);
        }

        // "{"
        int ob_exists1 = consume_terminal(SYMBOL, (char *[]){"{", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (ob_exists1 == FALSE) {
            error_info.er = openBraceExpected;
            return TRUE;
        }

        // stmt
        while (TRUE) {
            Token token34 = PeekNextToken();
            if (token34.tp == ERR) {
                error_info.er = lexerErr;
                error_info.tk = token34;
                is_lexer_error = TRUE;
                return TRUE;
            }
            if (is_lexeme_acceptable(token34.lx, statement_keywords) == FALSE) {
                break;
            }
            int e1 = validate_subroutine_statement(in_codegen_phase);
            if (e1 == TRUE) {
                return TRUE;
            }
        }

        if (in_codegen_phase == TRUE) {
            fprintf(output_file, "%s %d\n", vm_commands[GOTO_CM], curr_loop_label_idx);
            fprintf(output_file, "%s %d\n", vm_commands[LABEL_CM], curr_loop_label_idx);
        }

        // "}"
        int cb_exists1 = consume_terminal(SYMBOL, (char *[]){"}", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (cb_exists1 == FALSE) {
            error_info.er = closeBraceExpected;
            return TRUE;
        }
        return FALSE;
    } else if (strncmp(token28.lx, "do", LEXEME_LEN) == 0) {
        int flag = FALSE;
        TableRow *class = NULL;
        Token temp_id_exists5;

        // "do"
        int do_keyword_exists = consume_terminal(RESWORD, (char *[]){"do", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (do_keyword_exists == FALSE) {
            error_info.er = syntaxError;
            return TRUE;
        }

        // Caller class ID
        Token token35 = PeekNextToken();
        if (token35.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token35;
            is_lexer_error = TRUE;
            return TRUE;
        }
        if (token35.tp != ID) {
            error_info.er = idExpected;
            error_info.tk = token35;
            is_lexer_error = TRUE;
            return TRUE;
        } else {
            GetNextToken();
        }

        Token token36 = PeekNextToken();
        if (token36.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token36;
            is_lexer_error = TRUE;
            return TRUE;
        }

        // .
        if (is_lexeme_acceptable(token36.lx, (char *[]){".", NULL})) {
            int dot_exists1 = consume_terminal(SYMBOL, (char *[]){".", NULL});
            if (is_lexer_error == TRUE) {
                return TRUE;
            } else if (dot_exists1 == FALSE) {
                error_info.er = syntaxError;
                return TRUE;
            }

            temp_id_exists5 = PeekNextToken();

            // identifier
            int id_exists5 = consume_terminal(ID, (char *[]){NULL});
            if (is_lexer_error == TRUE) {
                return TRUE;
            } else if (id_exists5 == FALSE) {
                error_info.er = idExpected;
                return TRUE;
            }

            // caller class
            class = find_symbol_in_table(get_program_table(), token35.lx);
            TableRow *r;
            if ((r = find_symbol_in_table(class_table, token35.lx)) != NULL || (method_table != NULL && (r = find_symbol_in_table(method_table, token35.lx)) != NULL)) {
                if (in_codegen_phase == FALSE) {
                    add_undeclare(temp_id_exists5, r->type);
                } else {
                    class = find_symbol_in_table(get_program_table(), r->type);
                }
            } else if (class == NULL) {
                // class unknown
                if (in_codegen_phase == FALSE) {
                    // the final function checks class first them the method
                    add_undeclare(token35, "");
                    add_undeclare(temp_id_exists5, token35.lx);
                }
            } else {
                // class known
                if (in_codegen_phase == FALSE) {
                    add_undeclare(temp_id_exists5, token35.lx);
                }
            }

            if (in_codegen_phase == TRUE) {
                flag = TRUE;
                print_cmd(output_file, PUSH_CM, token35);
            }

        } else {
            if (in_codegen_phase == FALSE && find_symbol_in_table(class_table, token35.lx) == NULL) {
                add_undeclare(token35, class_table->name);
            }
            if (in_codegen_phase == TRUE) {
                flag = FALSE;
                new_fprintf(vm_commands[PUSH_CM], memory_segments[POINTER_SEG], 0);
            }
        }

        // "("
        int open_paren_exists4 = consume_terminal(SYMBOL, (char *[]){"("});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (open_paren_exists4 == FALSE) {
            error_info.er = openParenExpected;
            return TRUE;
        }

        // expressions
        Token token37 = PeekNextToken();
        if (token37.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token37;
            is_lexer_error = TRUE;
            return TRUE;
        }

        // non empty (...)
        if (is_lexeme_acceptable(token37.lx, (char *[]){")", NULL}) == FALSE) {
            int is_expression14 = token37.tp == ID || token37.tp == STRING || token37.tp == INT || (token37.tp == RESWORD && is_lexeme_acceptable(token37.lx, factor_keywords)) || (token37.tp == SYMBOL && is_lexeme_acceptable(token37.lx, factor_keywords));

            if (is_expression14 == TRUE) {
                int err_in_expression14 = validate_expression(in_codegen_phase);
                if (err_in_expression14 == TRUE) {
                    return TRUE;
                }

                // GetNextToken();
                while (TRUE) {
                    Token token38 = PeekNextToken();
                    if (token38.tp == ERR) {
                        error_info.er = lexerErr;
                        error_info.tk = token38;
                        is_lexer_error = TRUE;
                        return TRUE;
                    }

                    if (is_lexeme_acceptable(token38.lx, (char *[]){",", NULL}) == FALSE) {
                        break;
                    }

                    int comma_exists2 = consume_terminal(SYMBOL, (char *[]){",", NULL});
                    if (is_lexer_error) {
                        return TRUE;
                    } else if (comma_exists2 == FALSE) {
                        error_info.er = syntaxError;
                        return TRUE;
                    }

                    Token token39 = PeekNextToken();
                    if (token39.tp == ERR) {
                        error_info.er = lexerErr;
                        error_info.tk = token39;
                        is_lexer_error = TRUE;
                        return TRUE;
                    }
                    int is_another_expression = token39.tp == ID || token39.tp == STRING || token39.tp == INT || (token39.tp == RESWORD && is_lexeme_acceptable(token39.lx, factor_keywords)) || (token39.tp == SYMBOL && is_lexeme_acceptable(token39.lx, factor_keywords));
                    if (is_another_expression == TRUE) {
                        int err_in_another_expression = validate_expression(in_codegen_phase);
                        if (err_in_another_expression == TRUE) {
                            return TRUE;
                        }
                    } else {
                        error_info.er = syntaxError;
                        error_info.tk = token39;
                        return TRUE;
                    }
                }
            }
        }

        // ')'
        int close_paren_exists4 = consume_terminal(SYMBOL, (char *[]){")"});
        if (is_lexer_error) {
            return TRUE;
        } else if (close_paren_exists4 == FALSE) {
            error_info.er = closeParenExpected;
            return TRUE;
        }

        if (in_codegen_phase == TRUE) {
            if (flag == TRUE) {
                if (class != NULL) {
                    int stack_idx = find_symbol_in_table(class->child_table, temp_id_exists5.lx)->child_table->symbol_kind_cnt[ARGS] - 1 + (find_symbol_in_table(class->child_table, temp_id_exists5.lx)->kind == METHOD);
                    print_method_invoke(vm_commands[CALL_CM], class->token.lx, temp_id_exists5.lx, stack_idx);
                }
            } else {
                int stack_idx = find_symbol_in_table(class_table, token35.lx)->child_table->symbol_kind_cnt[ARGS] - 1 + (find_symbol_in_table(class_table, token35.lx)->kind == METHOD);
                print_method_invoke(vm_commands[CALL_CM], class_table->name, token35.lx, stack_idx);
            }
            new_fprintf(vm_commands[POP_CM], memory_segments[TEMP_SEG], 0);
        }

        // ;
        int semicolon_exists1 = consume_terminal(SYMBOL, (char *[]){";", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (semicolon_exists1 == FALSE) {
            error_info.er = semicolonExpected;
            return TRUE;
        }
        return FALSE;
    } else if (strncmp(token28.lx, "var", LEXEME_LEN) == 0) {
        // "var"
        int var_keyword_exists = consume_terminal(RESWORD, (char *[]){"var", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (var_keyword_exists == FALSE) {
            error_info.er = syntaxError;
            return TRUE;
        }

        Token token40 = PeekNextToken();
        if (token40.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token40;
            is_lexer_error = TRUE;
            return TRUE;
        }

        // type
        int is_type_declaration3 = token40.tp == ID || (token40.tp == RESWORD && is_lexeme_acceptable(token40.lx, type_declaration_keywords));
        if (is_type_declaration3) {
            if (in_codegen_phase == FALSE && token40.tp == ID) {
                if (find_symbol_in_table(get_program_table(), token40.lx) == NULL) {
                    add_undeclare(token40, "");
                }
            }
            GetNextToken();
        } else {
            error_info.er = illegalType;
            error_info.tk = token40;
            return TRUE;
        }

        // id
        Token temp_var_statement1 = PeekNextToken();
        int id_exists6 = consume_terminal(ID, (char *[]){NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (id_exists6 == FALSE) {
            error_info.er = idExpected;
            return TRUE;
        }
        if (in_codegen_phase == FALSE) {
            int duplicate_temp_var_statement1_exists = insert_symbol_into_table(method_table, NULL, VAR, temp_var_statement1, token40.lx);
            if (duplicate_temp_var_statement1_exists == TRUE) {
                error_info.er = redecIdentifier;
                error_info.tk = temp_var_statement1;
                return TRUE;
            }
        }

        while (TRUE) {
            Token token41 = PeekNextToken();
            if (token41.tp == ERR) {
                error_info.er = lexerErr;
                error_info.tk = token41;
                is_lexer_error = TRUE;
                return TRUE;
            }
            if (is_lexeme_acceptable(token41.lx, (char *[]){",", NULL}) == FALSE) {
                break;
            }
            GetNextToken();  // consume ,
            // consume_terminal(SYMBOL, (char *[]){",", NULL}, FALSE); // ,
            Token new_id = PeekNextToken();
            int new_id_exists = consume_terminal(ID, (char *[]){NULL});
            if (is_lexer_error == TRUE) {
                return TRUE;
            } else if (new_id_exists == FALSE) {
                error_info.er = idExpected;
                return TRUE;
            }
            if (in_codegen_phase == FALSE) {
                if (find_symbol_in_table(class_table, new_id.lx) == NULL && insert_symbol_into_table(method_table, NULL, VAR, new_id, token40.lx) == TRUE) {
                    error_info.er = redecIdentifier;
                    error_info.tk = new_id;
                    return TRUE;
                }
            }
        }

        // ;
        int semicolon_exists2 = consume_terminal(SYMBOL, (char *[]){";", NULL});
        if (is_lexer_error) {
            return TRUE;
        } else if (semicolon_exists2 == FALSE) {
            error_info.er = semicolonExpected;
            return TRUE;
        }
        return FALSE;
    } else if (strncmp(token28.lx, "let", LEXEME_LEN) == 0) {
        if (in_codegen_phase == TRUE) {
        }
        // "let"
        int let_keyword_exists = consume_terminal(RESWORD, (char *[]){"let", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (let_keyword_exists == FALSE) {
            error_info.er = syntaxError;
            return TRUE;
        }

        // id
        Token temp_id_7 = PeekNextToken();
        int id_exists7 = consume_terminal(ID, (char *[]){NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (id_exists7 == FALSE) {
            error_info.er = idExpected;
            return TRUE;
        }

        // printf("method table name :%s\n", method_table->name);
        // for (int i = 0; i < method_table->size; i++) {
        //     printf("ln: %d, lx: %s\n", method_table->rows[i]->token.ln, method_table->rows[i]->token.lx);
        // }
        if (in_codegen_phase == FALSE && find_symbol_in_table(class_table, temp_id_7.lx) == NULL && find_symbol_in_table(method_table, temp_id_7.lx) == NULL) {
            // printf("here!\n");
            add_undeclare(temp_id_7, class_table->name);
        }

        Token token42 = PeekNextToken();
        if (token42.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token42;
            is_lexer_error = TRUE;
            return TRUE;
        }

        if (is_lexeme_acceptable(token42.lx, (char *[]){"[", NULL}) == TRUE) {
            // [
            int open_bracket_exists1 = consume_terminal(SYMBOL, (char *[]){"[", NULL});
            if (is_lexer_error == TRUE) {
                return TRUE;
            } else if (open_bracket_exists1 == FALSE) {
                error_info.er = syntaxError;
                return TRUE;
            }

            Token token43 = PeekNextToken();
            if (token43.tp == ERR) {
                error_info.er = lexerErr;
                error_info.tk = token43;
                is_lexer_error = TRUE;
                return TRUE;
            }

            int is_expression15 = token43.tp == ID || token43.tp == STRING || token43.tp == INT || (token43.tp == RESWORD && is_lexeme_acceptable(token43.lx, factor_keywords)) || (token43.tp == SYMBOL && is_lexeme_acceptable(token43.lx, factor_keywords));
            if (is_expression15 == TRUE) {
                int error_in_expression15 = validate_expression(in_codegen_phase);
                if (error_in_expression15 == TRUE) {
                    return TRUE;
                }
            } else {
                error_info.er = syntaxError;
                error_info.tk = token43;
                return TRUE;
            }

            // ]
            int close_bracket_exists1 = consume_terminal(SYMBOL, (char *[]){"]", NULL});
            if (is_lexer_error == TRUE) {
                return TRUE;
            } else if (close_bracket_exists1 == FALSE) {
                error_info.er = syntaxError;
                return TRUE;
            }

            if (in_codegen_phase == TRUE) {
                print_cmd(output_file, PUSH_CM, temp_id_7);
                fprintf(output_file, "%s\n", vm_commands[ADD_CM]);
                is_array = TRUE;
            }

            // return FALSE;
        }

        // =
        int equal_exists = consume_terminal(SYMBOL, (char *[]){"=", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (equal_exists == FALSE) {
            error_info.er = equalExpected;
            return TRUE;
        }

        Token token44 = PeekNextToken();
        if (token44.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token44;
            is_lexer_error = TRUE;
            return TRUE;
        }

        int is_expression16 = token44.tp == ID || token44.tp == STRING || token44.tp == INT || (token44.tp == RESWORD && is_lexeme_acceptable(token44.lx, factor_keywords)) || (token44.tp == SYMBOL && is_lexeme_acceptable(token44.lx, factor_keywords));
        if (is_expression16 == TRUE) {
            int e_in_expression16 = validate_expression(in_codegen_phase);
            if (e_in_expression16 == TRUE) {
                return TRUE;
            }
        } else {
            error_info.er = syntaxError;
            error_info.tk = token44;
            return TRUE;
        }

        if (in_codegen_phase == TRUE) {
            if (is_array == FALSE) {
                print_cmd(output_file, POP_CM, temp_id_7);
            } else {
                new_fprintf(vm_commands[POP_CM], memory_segments[TEMP_SEG], 0);
                new_fprintf(vm_commands[POP_CM], memory_segments[POINTER_SEG], 1);
                new_fprintf(vm_commands[PUSH_CM], memory_segments[TEMP_SEG], 0);
                new_fprintf(vm_commands[POP_CM], memory_segments[THAT_SEG], 0);
            }
            is_array = FALSE;
        }

        // ;
        int semicolon_exists3 = consume_terminal(SYMBOL, (char *[]){";", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (semicolon_exists3 == FALSE) {
            error_info.er = semicolonExpected;
            return TRUE;
        }

        return FALSE;
    } else if (strncmp(token28.lx, "return", LEXEME_LEN) == 0) {
        // "return"
        int return_keyword_exists = consume_terminal(RESWORD, (char *[]){"return", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (return_keyword_exists == FALSE) {
            error_info.er = syntaxError;
            return TRUE;
        }

        Token token45 = PeekNextToken();
        if (token45.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token45;
            is_lexer_error = TRUE;
            return TRUE;
        }

        int is_expression17 = token45.tp == ID || token45.tp == STRING || token45.tp == INT || (token45.tp == RESWORD && is_lexeme_acceptable(token45.lx, factor_keywords)) || (token45.tp == SYMBOL && is_lexeme_acceptable(token45.lx, factor_keywords));
        // can be a empty return
        // thus no else cases checked
        if (is_expression17 == TRUE) {
            int error_exists_in_expression17 = validate_expression(in_codegen_phase);
            if (error_exists_in_expression17 == TRUE) {
                return TRUE;
            }
        } else if (in_codegen_phase == TRUE) {
            new_fprintf(vm_commands[PUSH_CM], memory_segments[CONST_SEG], 0);
        }

        if (in_codegen_phase == TRUE) {
            fprintf(output_file, "%s\n", vm_commands[RETURN_CM]);
        }

        // ;
        int semicolon_exists4 = consume_terminal(SYMBOL, (char *[]){";", NULL});
        if (is_lexer_error == TRUE) {
            return TRUE;
        } else if (semicolon_exists4 == FALSE) {
            error_info.er = semicolonExpected;
            return TRUE;
        }
        return FALSE;

    } else {
        error_info.er = syntaxError;
        error_info.tk = token28;
        return TRUE;
    }
}

int validate_subroutine_body(int in_codegen_phase) {
    // "{"
    int open_brace_exists1 = consume_terminal(SYMBOL, (char *[]){"{", NULL});
    if (is_lexer_error) {
        return TRUE;
    } else if (open_brace_exists1 == FALSE) {
        error_info.er = openBraceExpected;
        return TRUE;
    }

    // statement
    while (TRUE) {
        Token token46 = PeekNextToken();
        if (token46.tp == ERR) {
            error_info.er = lexerErr;
            error_info.tk = token46;
            is_lexer_error = TRUE;
            return TRUE;
        }

        if (is_lexeme_acceptable(token46.lx, statement_keywords) == FALSE) {
            break;
        }

        int error_exists_in_statement = validate_subroutine_statement(in_codegen_phase);
        if (error_exists_in_statement == TRUE) {
            return TRUE;
        }
    }

    // "}"
    int close_brace_exists1 = consume_terminal(SYMBOL, (char *[]){"}", NULL});
    if (is_lexer_error) {
        return TRUE;
    } else if (close_brace_exists1 == FALSE) {
        error_info.er = closeBraceExpected;
        return TRUE;
    }

    return FALSE;
}

int validate_subroutine_declaration(int in_codegen_phase) {
    Token temp_subroutine_declare = PeekNextToken();
    // check "function" or "method" or "constructor"
    int subroutine_keyword_exists = consume_terminal(RESWORD, subroutine_declaration_keywords);
    if (is_lexer_error) {
        return TRUE;
    } else if (subroutine_keyword_exists == FALSE) {
        error_info.er = subroutineDeclarErr;
        return TRUE;
    }

    SymbolKind subroutine_declaration_kind;
    if (strncmp(temp_subroutine_declare.lx, "function", LEXEME_LEN) == 0) {
        subroutine_declaration_kind = FUNCTION;
    } else if (strncmp(temp_subroutine_declare.lx, "method", LEXEME_LEN) == 0) {
        subroutine_declaration_kind = METHOD;
        // TODO is method
        is_method = TRUE;
    } else {
        subroutine_declaration_kind = CONSTRUCTOR;
    }

    Token token47 = PeekNextToken();
    if (token47.tp == ERR) {
        error_info.er = lexerErr;
        error_info.tk = token47;
        is_lexer_error = TRUE;
        return TRUE;
    }

    // type
    int is_type_declaration4 = token47.tp == ID || (token47.tp == RESWORD && is_lexeme_acceptable(token47.lx, type_declaration_keywords));
    int is_void_declaration4 = token47.tp == RESWORD && is_lexeme_acceptable(token47.lx, (char *[]){"void", NULL});
    if (is_type_declaration4 == TRUE) {
        if (in_codegen_phase == FALSE && token47.tp == ID) {
            if (find_symbol_in_table(get_program_table(), token47.lx) == NULL) {
                add_undeclare(token47, "");
            }
        }
        GetNextToken();
    } else if (is_void_declaration4 == TRUE) {
        GetNextToken();
    } else {
        error_info.er = illegalType;
        error_info.tk = token47;
        return TRUE;
    }

    Token temp_id_exists8 = PeekNextToken();
    // identifier
    int id_exists8 = consume_terminal(ID, (char *[]){NULL});
    if (is_lexer_error == TRUE) {
        return TRUE;
    } else if (id_exists8 == FALSE) {
        error_info.er = idExpected;
        return TRUE;
    }
    if (in_codegen_phase == FALSE) {
        method_table = NULL;
        method_table = create_table(METHOD_SCOPE, temp_id_exists8.lx);
        int duplicate_temp_id_exists8 = insert_symbol_into_table(class_table, method_table, subroutine_declaration_kind, temp_id_exists8, token47.lx);
        if (duplicate_temp_id_exists8 == TRUE) {
            error_info.er = redecIdentifier;
            error_info.tk = temp_id_exists8;
        }

        // manually add "this" arg
        Token this_arg;
        this_arg.tp = ID;
        this_arg.ln = temp_id_exists8.ln;
        this_arg.ec = NoLexErr;
        strncpy(this_arg.lx, "this", LEXEME_LEN);
        insert_symbol_into_table(method_table, NULL, ARGS, this_arg, class_table->name);
    } else {
        TableRow *r = find_symbol_in_table(class_table, temp_id_exists8.lx);
        method_table = r->child_table;

        print_method_invoke(vm_commands[FUNCTION_CM], class_table->name, method_table->name, method_table->symbol_kind_cnt[VAR]);

        if (subroutine_declaration_kind == CONSTRUCTOR) {
            new_fprintf(vm_commands[PUSH_CM], memory_segments[CONST_SEG], class_table->symbol_kind_cnt[FIELD]);
            print_method_invoke(vm_commands[CALL_CM], "Memory", "alloc", 1);
            new_fprintf(vm_commands[POP_CM], memory_segments[POINTER_SEG], 0);
        } else if (subroutine_declaration_kind == METHOD) {
            new_fprintf(vm_commands[PUSH_CM], memory_segments[ARGUMENT_SEG], 0);
            new_fprintf(vm_commands[POP_CM], memory_segments[POINTER_SEG], 0);
        }
    }

    // "("
    int open_paren_exists5 = consume_terminal(SYMBOL, (char *[]){"("});
    if (is_lexer_error == TRUE) {
        return TRUE;
    } else if (open_paren_exists5 == FALSE) {
        error_info.er = openParenExpected;
        return TRUE;
    }

    // parameters
    Token token48 = PeekNextToken();
    if (token48.tp == ERR) {
        error_info.er = lexerErr;
        error_info.tk = token48;
        is_lexer_error = TRUE;
        return TRUE;
    }

    if (is_lexeme_acceptable(token48.lx, (char *[]){")", NULL}) == FALSE) {
        int is_type5 = token48.tp == ID || (token48.tp == RESWORD && is_lexeme_acceptable(token48.lx, type_declaration_keywords));
        if (is_type5 == TRUE) {
            int error_exists_in_params = validate_subroutine_params(in_codegen_phase);
            if (error_exists_in_params == TRUE) {
                return TRUE;
            }
        } else {
            error_info.er = closeParenExpected;
            error_info.tk = token48;
            return TRUE;
        }
    }

    // ')'
    int close_paren_exists5 = consume_terminal(SYMBOL, (char *[]){")"});
    if (is_lexer_error) {
        return TRUE;
    } else if (close_paren_exists5 == FALSE) {
        error_info.er = closeParenExpected;
        return TRUE;
    }

    // subroutine body
    int error_exists_subroutine_body = validate_subroutine_body(in_codegen_phase);
    if (error_exists_subroutine_body == TRUE) {
        return TRUE;
    }

    // reset
    method_table = NULL;
    is_method = 0;

    return FALSE;
}

int validate_class_declaration(int in_codegen_phase) {
    // check "class"
    int class_keyword_exists = consume_terminal(RESWORD, (char *[]){"class", NULL});
    if (is_lexer_error) {
        return TRUE;
    } else if (class_keyword_exists == FALSE) {
        error_info.er = classExpected;
        return TRUE;
    }

    SymbolTable *program_table = get_program_table();
    Token temp_token = PeekNextToken();

    // check identifier after "class" keyword
    int identifier_exists2 = consume_terminal(ID, (char *[]){NULL});
    if (is_lexer_error) {
        return TRUE;
    } else if (identifier_exists2 == FALSE) {
        error_info.er = idExpected;
        return TRUE;
    }

    // create class level table if not in codegen phase
    if (in_codegen_phase == FALSE) {
        class_table = create_table(CLASS_SCOPE, temp_token.lx);
        int duplicate_exists = insert_symbol_into_table(program_table, class_table, CLASS, temp_token, "class");
        if (duplicate_exists == TRUE) {
            error_info.er = redecIdentifier;
            error_info.tk = temp_token;
            return TRUE;
        }
    } else {
        TableRow *r = find_symbol_in_table(program_table, temp_token.lx);
        if (r == NULL) {
            exit(1);
        }
        class_table = r->child_table;
    }

    // check "{"
    int open_brace_exists2 = consume_terminal(SYMBOL, (char *[]){"{", NULL});
    if (is_lexer_error) {
        return TRUE;
    } else if (open_brace_exists2 == FALSE) {
        error_info.er = openBraceExpected;
        return TRUE;
    }

    // class var and subroutine
    while (TRUE) {
        Token token49 = PeekNextToken();
        if (token49.tp == ERR) {
            error_info.tk = token49;
            error_info.er = lexerErr;
            is_lexer_error = TRUE;
            return TRUE;
        }

        int is_token_class_var_declaration_keywords = is_lexeme_acceptable(token49.lx, class_var_declaration_keywords);
        int is_token_subroutine_declaration_keywords = is_lexeme_acceptable(token49.lx, subroutine_declaration_keywords);

        if (is_token_class_var_declaration_keywords == FALSE && is_token_subroutine_declaration_keywords == FALSE) {
            break;
        }

        if (is_token_class_var_declaration_keywords == TRUE) {
            int error_class_var_declaration_exists = validate_class_var_declaration(in_codegen_phase);
            if (error_class_var_declaration_exists == TRUE) {
                return TRUE;
            }
        }

        if (is_token_subroutine_declaration_keywords == TRUE) {
            int error_subroutine_declaration_exists = validate_subroutine_declaration(in_codegen_phase);
            if (error_subroutine_declaration_exists == TRUE) {
                return TRUE;
            }
        }
    }

    // check "}"
    int close_brace_exists2 = consume_terminal(SYMBOL, (char *[]){"}", NULL});
    if (is_lexer_error) {
        return TRUE;
    } else if (close_brace_exists2 == FALSE) {
        error_info.er = closeBraceExpected;
        return TRUE;
    }

    return FALSE;
}

int InitParser(char *file_name) {
    int in_codegen_phase = is_codegen_phase();
    if (in_codegen_phase == TRUE) {
        output_file = get_output_file();
    }

    condition_label_idx = 0;
    loop_label_idx = 0;
    return InitLexer(file_name);
}

ParserInfo Parse() {
    is_lexer_error = FALSE;
    ParserInfo parser_info;

    parser_info.er = none;
    parser_info.tk.tp = ERR;
    parser_info.tk.ec = IllSym;
    parser_info.tk.ln = 0;
    strcpy(parser_info.tk.lx, "");
    strcpy(error_info.tk.fl, "");

    error_info.er = none;
    error_info.tk.tp = ERR;
    error_info.tk.ec = IllSym;
    error_info.tk.ln = 0;
    strcpy(error_info.tk.lx, "");
    strcpy(error_info.tk.fl, "");

    // flag indicating whether currently in codegen phase
    int in_codegen_phase = is_codegen_phase();
    int error_exists_class_declaration = validate_class_declaration(in_codegen_phase);
    if (error_exists_class_declaration == TRUE) {
        return error_info;
    }

    parser_info.er = none;
    return parser_info;
}

int StopParser() {
    reset();

    return StopLexer();
}