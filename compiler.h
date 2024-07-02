#ifndef COMPILER_H
#define COMPILER_H

#include <stdio.h>

#include "parser.h"
#include "symbols.h"

typedef enum {
    ADD_CM,
    SUB_CM,
    NEG_CM,
    EQ_CM,
    GT_CM,
    LT_CM,
    AND_CM,
    OR_CM,
    NOT_CM,
    POP_CM,
    PUSH_CM,
    LABEL_CM,
    GOTO_CM,
    IF_GOTO_CM,
    FUNCTION_CM,
    CALL_CM,
    RETURN_CM
} VmCommand;

typedef enum {
    STATIC_SEG,
    ARGUMENT_SEG,
    LOCAL_SEG,
    THIS_SEG,
    THAT_SEG,
    POINTER_SEG,  // contains two locations, pointer[0], pointer[1]
    TEMP_SEG,
    CONST_SEG,
} MemorySegment;

int InitCompiler();
ParserInfo compile(char* dir_name);
int StopCompiler();
int is_codegen_phase();
FILE* get_output_file();

#endif