// compiler.h
#ifndef leetlang_compiler_h
#define leetlang_compiler_h

#include "chunk.h"
#include "object.h"

bool compile(const char* source, Chunk* chunk);

#endif
