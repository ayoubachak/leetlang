## chunk.c :
```
// chunk.c

#include <stdlib.h>
#include "chunk.h"
#include "memory.h"
#include "vm.h"

void initChunk(Chunk* chunk) {
  chunk->count = 0;
  chunk->capacity = 0;
  chunk->code = NULL;
  chunk->lines = NULL;
  initValueArray(&chunk->constants);
}

void freeChunk(Chunk* chunk) {
  FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
  FREE_ARRAY(int, chunk->lines, chunk->capacity);
  freeValueArray(&chunk->constants);
  initChunk(chunk);
}
/* Chunks of Bytecode write-chunk < Chunks of Bytecode write-chunk-with-line
void writeChunk(Chunk* chunk, uint8_t byte) {
*/
void writeChunk(Chunk* chunk, uint8_t byte, int line) {
  if (chunk->capacity < chunk->count + 1) {
    int oldCapacity = chunk->capacity;
    chunk->capacity = GROW_CAPACITY(oldCapacity);
    chunk->code = GROW_ARRAY(uint8_t, chunk->code,
        oldCapacity, chunk->capacity);
    chunk->lines = GROW_ARRAY(int, chunk->lines,
        oldCapacity, chunk->capacity);
  }
  chunk->code[chunk->count] = byte;
  chunk->lines[chunk->count] = line;
  chunk->count++;
}


int addConstant(Chunk* chunk, Value value) {
  push(value);
  writeValueArray(&chunk->constants, value);
  pop();
  return chunk->constants.count - 1;
}

```

## chunk.h :
```
// chunk.h
#ifndef leetlang_chunk_h
#define leetlang_chunk_h
#include "common.h"
#include "value.h"


typedef enum {
  OP_CONSTANT,
  OP_CONSTANT_LONG,
  OP_NIL,
  OP_TRUE,
  OP_FALSE,
  OP_POP,
  OP_GET_LOCAL,
  OP_SET_LOCAL,
  OP_GET_GLOBAL,
  OP_DEFINE_GLOBAL,
  OP_SET_GLOBAL,
  OP_GET_UPVALUE,
  OP_SET_UPVALUE,
  OP_GET_PROPERTY,
  OP_SET_PROPERTY,
  OP_GET_SUPER,
  OP_EQUAL,
  OP_GREATER,
  OP_LESS,
  OP_ADD,
  OP_SUBTRACT,
  OP_MULTIPLY,
  OP_DIVIDE,
  OP_NOT,
  OP_NEGATE,
  OP_PRINT,
  OP_JUMP,
  OP_JUMP_IF_FALSE,
  OP_LOOP,
  OP_CALL,
  OP_INVOKE,
  OP_SUPER_INVOKE,
  OP_CLOSURE,
  OP_CLOSE_UPVALUE,
  OP_RETURN,
  OP_CLASS,
  OP_INHERIT,
  OP_METHOD,
  // Exception handling
  OP_TRY_START, 
  OP_TRY_END, 
  OP_THROW, 
  OP_CATCH_START, 
  OP_CATCH_END, 
  OP_FINALLY_START, 
  OP_FINALLY_END,
  OP_END_TRY_CATCH_FINALLY
} OpCode;



typedef struct {
  int count;
  int capacity;
  uint8_t* code;
  int* lines;
  ValueArray constants;
} Chunk;


void initChunk(Chunk* chunk);

void freeChunk(Chunk* chunk);

/* Chunks of Bytecode write-chunk-h < Chunks of Bytecode write-chunk-with-line-h
void writeChunk(Chunk* chunk, uint8_t byte);
*/
void writeChunk(Chunk* chunk, uint8_t byte, int line);

int addConstant(Chunk* chunk, Value value);

#endif
```

## compiler.c :
```
// compiler.c

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "scanner.h"


#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif


typedef struct {
  Token current;
  Token previous;
  bool hadError;
  bool panicMode;
} Parser;


typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT,  // =
  PREC_OR,          // or
  PREC_AND,         // and
  PREC_EQUALITY,    // == !=
  PREC_COMPARISON,  // < > <= >=
  PREC_TERM,        // + -
  PREC_FACTOR,      // * /
  PREC_UNARY,       // ! -
  PREC_CALL,        // . ()
  PREC_PRIMARY
} Precedence;


/* Compiling Expressions parse-fn-type < Global Variables parse-fn-type
typedef void (*ParseFn)();
*/

typedef void (*ParseFn)(bool canAssign);

typedef struct {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule;



typedef struct {
  Token name;
  int depth;
  bool isCaptured;
} Local;


typedef struct {
  uint8_t index;
  bool isLocal;
} Upvalue;


typedef enum {
  TYPE_FUNCTION,
  TYPE_INITIALIZER,
  TYPE_METHOD,
  TYPE_SCRIPT
} FunctionType;


/* Local Variables compiler-struct < Calls and Functions enclosing-field
typedef struct {
*/

typedef struct Compiler {
  struct Compiler* enclosing;
  ObjFunction* function;
  FunctionType type;
  Local locals[UINT8_COUNT];
  int localCount;
  Upvalue upvalues[UINT8_COUNT];
  int scopeDepth;
} Compiler;



typedef struct ClassCompiler {
  struct ClassCompiler* enclosing;
  bool hasSuperclass;
} ClassCompiler;


Parser parser;

Compiler* current = NULL;

ClassCompiler* currentClass = NULL;

/* Compiling Expressions compiling-chunk < Calls and Functions current-chunk
Chunk* compilingChunk;

static Chunk* currentChunk() {
  return compilingChunk;
}
*/

static Chunk* currentChunk() {
  return &current->function->chunk;
}

static void errorAt(Token* token, const char* message) {
  if (parser.panicMode) return;
  parser.panicMode = true;
  fprintf(stderr, "[line %d] Error", token->line);
  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Nothing.
  } else {
    fprintf(stderr, " at '%.*s'", token->length, token->start);
  }
  fprintf(stderr, ": %s\n", message);
  parser.hadError = true;
}

static void error(const char* message) {
  errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
  errorAt(&parser.current, message);
}

static void advance() {
  parser.previous = parser.current;
  for (;;) {
    parser.current = scanToken();
    if (parser.current.type != TOKEN_ERROR) break;
    errorAtCurrent(parser.current.start);
  }
}

static void consume(TokenType type, const char* message) {
  if (parser.current.type == type) {
    advance();
    return;
  }
  errorAtCurrent(message);
}

static bool check(TokenType type) {
  return parser.current.type == type;
}

static bool match(TokenType type) {
  if (!check(type)) return false;
  advance();
  return true;
}

static void emitByte(uint8_t byte) {
  writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
  emitByte(byte1);
  emitByte(byte2);
}

static void emitLoop(int loopStart) {
  emitByte(OP_LOOP);
  int offset = currentChunk()->count - loopStart + 2;
  if (offset > UINT16_MAX) error("Loop body too large.");
  emitByte((offset >> 8) & 0xff);
  emitByte(offset & 0xff);
}

static int emitJump(uint8_t instruction) {
  emitByte(instruction);
  emitByte(0xff);
  emitByte(0xff);
  return currentChunk()->count - 2;
}

static void emitReturn() {
/* Calls and Functions return-nil < Methods and Initializers return-this
  emitByte(OP_NIL);
*/
  if (current->type == TYPE_INITIALIZER) {
    emitBytes(OP_GET_LOCAL, 0);
  } else {
    emitByte(OP_NIL);
  }
  emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value) {
  int constant = addConstant(currentChunk(), value);
  if (constant > UINT8_MAX) {
    error("Too many constants in one chunk.");
    return 0;
    // printf("Warning : Too many constants in one chunk.");
    // return constant;
  }
  return (uint8_t)constant;
}

void writeConstant(Chunk* chunk, Value value, int line) {
  int constant = addConstant(chunk, value);
  if (constant <= 255) {
    writeChunk(chunk, OP_CONSTANT, line);
    writeChunk(chunk, constant, line);
  } else {
    writeChunk(chunk, OP_CONSTANT_LONG, line);
    writeChunk(chunk, (constant >> 16) & 0xff, line); // Higher byte
    writeChunk(chunk, (constant >> 8) & 0xff, line);  // Middle byte
    writeChunk(chunk, constant & 0xff, line);         // Lower byte
  }
}

static void emitConstant(Value value) {
    emitBytes(OP_CONSTANT, makeConstant(value));
    // this implementation slowed down the process too much 
    /*
    int index = makeConstant(value);
    if (index <= UINT8_MAX) {
        emitBytes(OP_CONSTANT, (uint8_t)index);
    } else {
        writeConstant(currentChunk(), value, parser.previous.line);
    }
    */
}

static void patchJump(int offset) {
  // -2 to adjust for the bytecode for the jump offset itself.
  int jump = currentChunk()->count - offset - 2;
  if (jump > UINT16_MAX) {
    error("Too much code to jump over.");
  }
  currentChunk()->code[offset] = (jump >> 8) & 0xff;
  currentChunk()->code[offset + 1] = jump & 0xff;
}

/* Local Variables init-compiler < Calls and Functions init-compiler
static void initCompiler(Compiler* compiler) {
*/

static void initCompiler(Compiler* compiler, FunctionType type) {
  compiler->enclosing = current;
  compiler->function = NULL;
  compiler->type = type;
  compiler->localCount = 0;
  compiler->scopeDepth = 0;
  compiler->function = newFunction();
  current = compiler;
  if (type != TYPE_SCRIPT) {
    current->function->name = copyString(parser.previous.start,
                                         parser.previous.length);
  }
  Local* local = &current->locals[current->localCount++];
  local->depth = 0;
  local->isCaptured = false;
/* Calls and Functions init-function-slot < Methods and Initializers slot-zero
  local->name.start = "";
  local->name.length = 0;
*/
  if (type != TYPE_FUNCTION) {
    local->name.start = "this";
    local->name.length = 4;
  } else {
    local->name.start = "";
    local->name.length = 0;
  }
}


/* Compiling Expressions end-compiler < Calls and Functions end-compiler
static void endCompiler() {
*/

static ObjFunction* endCompiler() {
  emitReturn();
  ObjFunction* function = current->function;
#ifdef DEBUG_PRINT_CODE
  if (!parser.hadError) {
/* Compiling Expressions dump-chunk < Calls and Functions disassemble-end
    disassembleChunk(currentChunk(), "code");
*/

    disassembleChunk(currentChunk(), function->name != NULL
        ? function->name->chars : "<script>");
  }
#endif
  current = current->enclosing;
  return function;

}

static void beginScope() {
  current->scopeDepth++;
}

static void endScope() {
  current->scopeDepth--;
  while (current->localCount > 0 &&
         current->locals[current->localCount - 1].depth >
            current->scopeDepth) {
/* Local Variables pop-locals < Closures end-scope
    emitByte(OP_POP);
*/
    if (current->locals[current->localCount - 1].isCaptured) {
      emitByte(OP_CLOSE_UPVALUE);
    } else {
      emitByte(OP_POP);
    }
    current->localCount--;
  }
}



static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);


static uint8_t identifierConstant(Token* name) {
  return makeConstant(OBJ_VAL(copyString(name->start,
                                         name->length)));
}

static bool identifiersEqual(Token* a, Token* b) {
  if (a->length != b->length) return false;
  return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler* compiler, Token* name) {
  for (int i = compiler->localCount - 1; i >= 0; i--) {
    Local* local = &compiler->locals[i];
    if (identifiersEqual(name, &local->name)) {
      if (local->depth == -1) {
        error("Can't read local variable in its own initializer.");
      }
      return i;
    }
  }
  return -1;
}


static int addUpvalue(Compiler* compiler, uint8_t index,
                      bool isLocal) {
  int upvalueCount = compiler->function->upvalueCount;
  for (int i = 0; i < upvalueCount; i++) {
    Upvalue* upvalue = &compiler->upvalues[i];
    if (upvalue->index == index && upvalue->isLocal == isLocal) {
      return i;
    }
  }

  if (upvalueCount == UINT8_COUNT) {
    error("Too many closure variables in function.");
    return 0;
  }

  compiler->upvalues[upvalueCount].isLocal = isLocal;
  compiler->upvalues[upvalueCount].index = index;
  return compiler->function->upvalueCount++;
}


static int resolveUpvalue(Compiler* compiler, Token* name) {
  if (compiler->enclosing == NULL) return -1;
  int local = resolveLocal(compiler->enclosing, name);
  if (local != -1) {
    compiler->enclosing->locals[local].isCaptured = true;
    return addUpvalue(compiler, (uint8_t)local, true);
  }

  int upvalue = resolveUpvalue(compiler->enclosing, name);
  if (upvalue != -1) {
    return addUpvalue(compiler, (uint8_t)upvalue, false);
  }
  
  return -1;
}


static void addLocal(Token name) {
  if (current->localCount == UINT8_COUNT) {
    error("Too many local variables in function.");
    return;
  }

  Local* local = &current->locals[current->localCount++];
  local->name = name;
/* Local Variables add-local < Local Variables declare-undefined
  local->depth = current->scopeDepth;
*/
  local->depth = -1;
  local->isCaptured = false;

}


static void declareVariable() {
  if (current->scopeDepth == 0) return;
  Token* name = &parser.previous;
  for (int i = current->localCount - 1; i >= 0; i--) {
    Local* local = &current->locals[i];
    if (local->depth != -1 && local->depth < current->scopeDepth) {
      break; // [negative]
    }
    if (identifiersEqual(name, &local->name)) {
      error("Already a variable with this name in this scope.");
    }
  }
  addLocal(*name);
}


static uint8_t parseVariable(const char* errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);
  declareVariable();
  if (current->scopeDepth > 0) return 0;
  return identifierConstant(&parser.previous);
}


static void markInitialized() {
  if (current->scopeDepth == 0) return;
  current->locals[current->localCount - 1].depth =
      current->scopeDepth;
}


static void defineVariable(uint8_t global) {
  if (current->scopeDepth > 0) {
    markInitialized();
    return;
  }
  emitBytes(OP_DEFINE_GLOBAL, global);
}


static uint8_t argumentList() {
  uint8_t argCount = 0;
  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      expression();
      if (argCount == 255) {
        error("Can't have more than 255 arguments.");
      }
      argCount++;
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
  return argCount;
}


static void and_(bool canAssign) {
  int endJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  parsePrecedence(PREC_AND);
  patchJump(endJump);
}


/* Compiling Expressions binary < Global Variables binary
static void binary() {
*/

static void binary(bool canAssign) {
  TokenType operatorType = parser.previous.type;
  ParseRule* rule = getRule(operatorType);
  parsePrecedence((Precedence)(rule->precedence + 1));
  switch (operatorType) {
    case TOKEN_BANG_EQUAL:    emitBytes(OP_EQUAL, OP_NOT); break;
    case TOKEN_EQUAL_EQUAL:   emitByte(OP_EQUAL); break;
    case TOKEN_GREATER:       emitByte(OP_GREATER); break;
    case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
    case TOKEN_LESS:          emitByte(OP_LESS); break;
    case TOKEN_LESS_EQUAL:    emitBytes(OP_GREATER, OP_NOT); break;
    case TOKEN_PLUS:          emitByte(OP_ADD); break;
    case TOKEN_MINUS:         emitByte(OP_SUBTRACT); break;
    case TOKEN_STAR:          emitByte(OP_MULTIPLY); break;
    case TOKEN_SLASH:         emitByte(OP_DIVIDE); break;
    default: return; // Unreachable.
  }
}


static void call(bool canAssign) {
  uint8_t argCount = argumentList();
  emitBytes(OP_CALL, argCount);
}


static void dot(bool canAssign) {
  consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
  uint8_t name = identifierConstant(&parser.previous);
  if (canAssign && match(TOKEN_EQUAL)) {
    expression();
    emitBytes(OP_SET_PROPERTY, name);
  } else if (match(TOKEN_LEFT_PAREN)) {
    uint8_t argCount = argumentList();
    emitBytes(OP_INVOKE, name);
    emitByte(argCount);
  } else {
    emitBytes(OP_GET_PROPERTY, name);
  }
}


/* Types of Values parse-literal < Global Variables parse-literal
static void literal() {
*/

static void literal(bool canAssign) {
  switch (parser.previous.type) {
    case TOKEN_FALSE: emitByte(OP_FALSE); break;
    case TOKEN_NIL: emitByte(OP_NIL); break;
    case TOKEN_TRUE: emitByte(OP_TRUE); break;
    default: return; // Unreachable.
  }
}


/* Compiling Expressions grouping < Global Variables grouping
static void grouping() {
*/

static void grouping(bool canAssign) {
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

/* Compiling Expressions number < Global Variables number
static void number() {
*/


static void number(bool canAssign) {
  double value = strtod(parser.previous.start, NULL);
/* Compiling Expressions number < Types of Values const-number-val
  emitConstant(value);
*/
  emitConstant(NUMBER_VAL(value));
}

static void or_(bool canAssign) {
  int elseJump = emitJump(OP_JUMP_IF_FALSE);
  int endJump = emitJump(OP_JUMP);
  patchJump(elseJump);
  emitByte(OP_POP);
  parsePrecedence(PREC_OR);
  patchJump(endJump);
}

/* Strings parse-string < Global Variables string
static void string() {
*/
static void string(bool canAssign) {
  emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,
                                  parser.previous.length - 2)));
}

/* Global Variables read-named-variable < Global Variables named-variable-signature
static void namedVariable(Token name) {
*/
static void namedVariable(Token name, bool canAssign) {
/* Global Variables read-named-variable < Local Variables named-local
  uint8_t arg = identifierConstant(&name);
*/
  uint8_t getOp, setOp;
  int arg = resolveLocal(current, &name);
  if (arg != -1) {
    getOp = OP_GET_LOCAL;
    setOp = OP_SET_LOCAL;
  } else if ((arg = resolveUpvalue(current, &name)) != -1) {
    getOp = OP_GET_UPVALUE;
    setOp = OP_SET_UPVALUE;
  } else {
    arg = identifierConstant(&name);
    getOp = OP_GET_GLOBAL;
    setOp = OP_SET_GLOBAL;
  }
/* Global Variables read-named-variable < Global Variables named-variable
  emitBytes(OP_GET_GLOBAL, arg);
*/
/* Global Variables named-variable < Global Variables named-variable-can-assign
  if (match(TOKEN_EQUAL)) {
*/
  if (canAssign && match(TOKEN_EQUAL)) {
    expression();
/* Global Variables named-variable < Local Variables emit-set
    emitBytes(OP_SET_GLOBAL, arg);
*/
    emitBytes(setOp, (uint8_t)arg);
  } else {
/* Global Variables named-variable < Local Variables emit-get
    emitBytes(OP_GET_GLOBAL, arg);
*/
    emitBytes(getOp, (uint8_t)arg);
  }
}

/* Global Variables variable-without-assign < Global Variables variable
static void variable() {
  namedVariable(parser.previous);
}
*/
static void variable(bool canAssign) {
  namedVariable(parser.previous, canAssign);
}

static Token syntheticToken(const char* text) {
  Token token;
  token.start = text;
  token.length = (int)strlen(text);
  return token;
}

static void super_(bool canAssign) {
  if (currentClass == NULL) {
    error("Can't use 'super' outside of a class.");
  } else if (!currentClass->hasSuperclass) {
    error("Can't use 'super' in a class with no superclass.");
  }

  consume(TOKEN_DOT, "Expect '.' after 'super'.");
  consume(TOKEN_IDENTIFIER, "Expect superclass method name.");
  uint8_t name = identifierConstant(&parser.previous);

  
  namedVariable(syntheticToken("this"), false);
/* Superclasses super-get < Superclasses super-invoke
  namedVariable(syntheticToken("super"), false);
  emitBytes(OP_GET_SUPER, name);
*/

  if (match(TOKEN_LEFT_PAREN)) {
    uint8_t argCount = argumentList();
    // namedVariable(syntheticToken("super"), false);
    emitBytes(OP_SUPER_INVOKE, name);
    emitByte(argCount);
  } else {
    // namedVariable(syntheticToken("super"), false);
    emitBytes(OP_GET_SUPER, name);
  }
}


static void this_(bool canAssign) {
  if (currentClass == NULL) {
    error("Can't use 'this' outside of a class.");
    return;
  }
  variable(false);
} // [this]




/* Compiling Expressions unary < Global Variables unary
static void unary() {
*/
static void unary(bool canAssign) {
  TokenType operatorType = parser.previous.type;
  // Compile the operand.
/* Compiling Expressions unary < Compiling Expressions unary-operand
  expression();
*/
  parsePrecedence(PREC_UNARY);
  // Emit the operator instruction.
  switch (operatorType) {
    case TOKEN_BANG: emitByte(OP_NOT); break;
    case TOKEN_NOT: emitByte(OP_NOT); break;
    case TOKEN_MINUS: emitByte(OP_NEGATE); break;
    default: return; // Unreachable.
  }
}

ParseRule rules[] = {
/* Compiling Expressions rules < Calls and Functions infix-left-paren
  [TOKEN_LEFT_PAREN]    = {grouping, NULL,   PREC_NONE},
*/
  [TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE}, // [big]
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
/* Compiling Expressions rules < Classes and Instances table-dot
  [TOKEN_DOT]           = {NULL,     NULL,   PREC_NONE},
*/
  [TOKEN_DOT]           = {NULL,     dot,    PREC_CALL},
  [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
  [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
  [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
  [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
/* Compiling Expressions rules < Types of Values table-not
  [TOKEN_BANG]          = {NULL,     NULL,   PREC_NONE},
*/
  [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
  [TOKEN_NOT]          = {unary,    NULL,   PREC_NONE},
/* Compiling Expressions rules < Types of Values table-equal
  [TOKEN_BANG_EQUAL]    = {NULL,     NULL,   PREC_NONE},
*/
  [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
/* Compiling Expressions rules < Types of Values table-comparisons
  [TOKEN_EQUAL_EQUAL]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_GREATER]       = {NULL,     NULL,   PREC_NONE},
  [TOKEN_GREATER_EQUAL] = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LESS]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LESS_EQUAL]    = {NULL,     NULL,   PREC_NONE},
*/
  [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
/* Compiling Expressions rules < Global Variables table-identifier
  [TOKEN_IDENTIFIER]    = {NULL,     NULL,   PREC_NONE},
*/
  [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
/* Compiling Expressions rules < Strings table-string
  [TOKEN_STRING]        = {NULL,     NULL,   PREC_NONE},
*/
  [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
  [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
/* Compiling Expressions rules < Jumping Back and Forth table-and
  [TOKEN_AND]           = {NULL,     NULL,   PREC_NONE},
*/
  [TOKEN_AND]           = {NULL,     and_,   PREC_AND},
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
/* Compiling Expressions rules < Types of Values table-false
  [TOKEN_FALSE]         = {NULL,     NULL,   PREC_NONE},
*/
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
/* Compiling Expressions rules < Types of Values table-nil
  [TOKEN_NIL]           = {NULL,     NULL,   PREC_NONE},
*/
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
/* Compiling Expressions rules < Jumping Back and Forth table-or
  [TOKEN_OR]            = {NULL,     NULL,   PREC_NONE},
*/
  [TOKEN_OR]            = {NULL,     or_,    PREC_OR},
  [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
/* Compiling Expressions rules < Superclasses table-super
  [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
*/
  [TOKEN_SUPER]         = {super_,   NULL,   PREC_NONE},
/* Compiling Expressions rules < Methods and Initializers table-this
  [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE},
*/
  [TOKEN_THIS]          = {this_,    NULL,   PREC_NONE},
/* Compiling Expressions rules < Types of Values table-true
  [TOKEN_TRUE]          = {NULL,     NULL,   PREC_NONE},
*/
  [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
  [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

static void parsePrecedence(Precedence precedence) {
/* Compiling Expressions parse-precedence < Compiling Expressions precedence-body
  // What goes here?
*/
  advance();
  ParseFn prefixRule = getRule(parser.previous.type)->prefix;
  if (prefixRule == NULL) {
    error("Expect expression.");
    return;
  }
/* Compiling Expressions precedence-body < Global Variables prefix-rule
  prefixRule();
*/
  bool canAssign = precedence <= PREC_ASSIGNMENT;
  prefixRule(canAssign);

  while (precedence <= getRule(parser.current.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.previous.type)->infix;
/* Compiling Expressions infix < Global Variables infix-rule
    infixRule();
*/
    infixRule(canAssign);
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    error("Invalid assignment target.");
  }

}

static ParseRule* getRule(TokenType type) {
  return &rules[type];
}

static void expression() {
/* Compiling Expressions expression < Compiling Expressions expression-body
  // What goes here?
*/
  parsePrecedence(PREC_ASSIGNMENT);
}


static void block() {
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    declaration();
  }
  consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}


static void tryStatement() {
    consume(TOKEN_TRY, "Expect 'try' before try block.");
    consume(TOKEN_LEFT_BRACE, "Expect '{' before try block body.");
    beginScope();
    block();
    endScope();
    if (match(TOKEN_CATCH)) {
        consume(TOKEN_LEFT_PAREN, "Expect '(' after 'catch'.");
        consume(TOKEN_IDENTIFIER, "Expect identifier for exception.");
        defineVariable(parseVariable("Expect variable name."));
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after catch parameters.");
        beginScope();
        block();
        endScope();
    }
    if (match(TOKEN_FINALLY)) {
        consume(TOKEN_LEFT_BRACE, "Expect '{' before finally block.");
        beginScope();
        block();
        endScope();
    }
}

static void function(FunctionType type) {
  Compiler compiler;
  initCompiler(&compiler, type);
  beginScope(); // [no-end-scope]
  consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      current->function->arity++;
      if (current->function->arity > 255) {
        errorAtCurrent("Can't have more than 255 parameters.");
      }
      uint8_t constant = parseVariable("Expect parameter name.");
      defineVariable(constant);
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
  consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
  block();
  ObjFunction* function = endCompiler();
/* Calls and Functions compile-function < Closures emit-closure
  emitBytes(OP_CONSTANT, makeConstant(OBJ_VAL(function)));
*/
  emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));
  for (int i = 0; i < function->upvalueCount; i++) {
    emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
    emitByte(compiler.upvalues[i].index);
  }
}

static void method() {
  consume(TOKEN_IDENTIFIER, "Expect method name.");
  uint8_t constant = identifierConstant(&parser.previous);
/* Methods and Initializers method-body < Methods and Initializers method-type
  FunctionType type = TYPE_FUNCTION;
*/
  FunctionType type = TYPE_METHOD;
  if (parser.previous.length == 4 &&
      memcmp(parser.previous.start, "init", 4) == 0) {
    type = TYPE_INITIALIZER;
  }
  function(type);
  emitBytes(OP_METHOD, constant);
}


static void classDeclaration() {
    consume(TOKEN_IDENTIFIER, "Expect class name.");
    Token className = parser.previous;
    uint8_t nameConstant = identifierConstant(&parser.previous);
    declareVariable();
    emitBytes(OP_CLASS, nameConstant);
    defineVariable(nameConstant);

    ClassCompiler classCompiler;
    classCompiler.hasSuperclass = false;
    classCompiler.enclosing = currentClass;
    currentClass = &classCompiler;

    // Check for a list of superclasses
    if (match(TOKEN_LESS) || match(TOKEN_LEFT_PAREN)) {
        do {
            consume(TOKEN_IDENTIFIER, "Expect superclass name.");
            variable(false); // Load the superclass
            if (identifiersEqual(&className, &parser.previous)) {
                error("A class can't inherit from itself.");
            }

            if (!classCompiler.hasSuperclass) {
                beginScope();
                addLocal(syntheticToken("super"));
                defineVariable(0);
                classCompiler.hasSuperclass = true;
            }

            namedVariable(className, false); // Load the current class
            emitByte(OP_INHERIT); // Inherit from the current superclass
        } while (match(TOKEN_COMMA));

        if (check(TOKEN_RIGHT_PAREN)) {
            consume(TOKEN_RIGHT_PAREN, "Expect ')' after superclasses.");
        }
    }

    namedVariable(className, false);
    consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        method();
    }
    consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
    emitByte(OP_POP);

    if (classCompiler.hasSuperclass) {
        endScope();
    }

    currentClass = currentClass->enclosing;
}


static void funDeclaration() {
  uint8_t global = parseVariable("Expect function name.");
  markInitialized();
  function(TYPE_FUNCTION);
  defineVariable(global);
}

static void varDeclaration() {
  uint8_t global = parseVariable("Expect variable name.");
  if (match(TOKEN_EQUAL)) {
    expression();
  } else {
    emitByte(OP_NIL);
  }
  consume(TOKEN_SEMICOLON,
          "Expect ';' after variable declaration.");

  defineVariable(global);
}

static void expressionStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
  emitByte(OP_POP);
}

static void forStatement() {
  beginScope();
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
/* Jumping Back and Forth for-statement < Jumping Back and Forth for-initializer
  consume(TOKEN_SEMICOLON, "Expect ';'.");
*/
  if (match(TOKEN_SEMICOLON)) {
    // No initializer.
  } else if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    expressionStatement();
  }
  int loopStart = currentChunk()->count;
/* Jumping Back and Forth for-statement < Jumping Back and Forth for-exit
  consume(TOKEN_SEMICOLON, "Expect ';'.");
*/
  int exitJump = -1;
  if (!match(TOKEN_SEMICOLON)) {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");
    // Jump out of the loop if the condition is false.
    exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP); // Condition.
  }
/* Jumping Back and Forth for-statement < Jumping Back and Forth for-increment
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");
*/
  if (!match(TOKEN_RIGHT_PAREN)) {
    int bodyJump = emitJump(OP_JUMP);
    int incrementStart = currentChunk()->count;
    expression();
    emitByte(OP_POP);
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");
    emitLoop(loopStart);
    loopStart = incrementStart;
    patchJump(bodyJump);
  }
  statement();
  emitLoop(loopStart);
  if (exitJump != -1) {
    patchJump(exitJump);
    emitByte(OP_POP); // Condition.
  }
  endScope();
}


static void ifStatement() {
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition."); // [paren]
  int thenJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  statement();
  int elseJump = emitJump(OP_JUMP);
  patchJump(thenJump);
  emitByte(OP_POP);
  if (match(TOKEN_ELSE)) statement();
  patchJump(elseJump);
}

static void throwStatement() {
    consume(TOKEN_THROW, "Expect 'throw' before an expression.");
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after throw expression.");
    emitByte(OP_THROW);
}

static void tryCatchFinallyStatement() {
    int tryJump = emitJump(OP_TRY_START);
    // Compile try block
    patchJump(tryJump);

    int catchJump = emitJump(OP_CATCH_START);
    // Compile catch block
    patchJump(catchJump);

    emitJump(OP_FINALLY_START);
    // Compile finally block
    emitByte(OP_END_TRY_CATCH_FINALLY);
}

static void printStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after value.");
  emitByte(OP_PRINT);
}

static void returnStatement() {
  if (current->type == TYPE_SCRIPT) {
    error("Can't return from top-level code.");
  }

  if (match(TOKEN_SEMICOLON)) {
    emitReturn();
  } else {
    if (current->type == TYPE_INITIALIZER) {
      error("Can't return a value from an initializer.");
    }
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
    emitByte(OP_RETURN);
  }
}

static void whileStatement() {
  int loopStart = currentChunk()->count;
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");
  int exitJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  statement();
  emitLoop(loopStart);
  patchJump(exitJump);
  emitByte(OP_POP);
}

static void synchronize() {
  parser.panicMode = false;
  while (parser.current.type != TOKEN_EOF) {
    if (parser.previous.type == TOKEN_SEMICOLON) return;
    switch (parser.current.type) {
      case TOKEN_CLASS:
      case TOKEN_FUN:
      case TOKEN_VAR:
      case TOKEN_FOR:
      case TOKEN_IF:
      case TOKEN_WHILE:
      case TOKEN_PRINT:
      case TOKEN_RETURN:
        return;
      default:
        ; // Do nothing.
    }
    advance();
  }
}

static void declaration() {
  if (match(TOKEN_CLASS)) {
    classDeclaration();
/* Calls and Functions match-fun < Classes and Instances match-class
  if (match(TOKEN_FUN)) {
*/
  } else if (match(TOKEN_FUN)) {
    funDeclaration();
/* Global Variables match-var < Calls and Functions match-fun
  if (match(TOKEN_VAR)) {
*/
  } else if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    statement();
  }
/* Global Variables declaration < Global Variables match-var
  statement();
*/
  if (parser.panicMode) synchronize();
}

static void statement() {
  if (match(TOKEN_PRINT)) {
    printStatement();
  } else if (match(TOKEN_FOR)) {
    forStatement();
  } else if (match(TOKEN_IF)) {
    ifStatement();
  } else if (match(TOKEN_RETURN)) {
    returnStatement();
  } else if (match(TOKEN_WHILE)) {
    whileStatement();
  } else if (match(TOKEN_TRY)) {   // Add this line
    tryStatement();              // Call the function to handle try statements
  } else if (match(TOKEN_LEFT_BRACE)) {
    beginScope();
    block();
    endScope();
  } else {
    expressionStatement();
  }
}

/* Scanning on Demand compiler-c < Compiling Expressions compile-signature
void compile(const char* source) {
*/
/* Compiling Expressions compile-signature < Calls and Functions compile-signature
bool compile(const char* source, Chunk* chunk) {
*/
ObjFunction* compile(const char* source) {
  initScanner(source);
/* Scanning on Demand dump-tokens < Compiling Expressions compile-chunk
  int line = -1;
  for (;;) {
    Token token = scanToken();
    if (token.line != line) {
      printf("%4d ", token.line);
      line = token.line;
    } else {
      printf("   | ");
    }
    printf("%2d '%.*s'\n", token.type, token.length, token.start); // [format]

    if (token.type == TOKEN_EOF) break;
  }
*/
  Compiler compiler;
/* Local Variables compiler < Calls and Functions call-init-compiler
  initCompiler(&compiler);
*/
  initCompiler(&compiler, TYPE_SCRIPT);
/* Compiling Expressions init-compile-chunk < Calls and Functions call-init-compiler
  compilingChunk = chunk;
*/
  parser.hadError = false;
  parser.panicMode = false;
  advance();
/* Compiling Expressions compile-chunk < Global Variables compile
  expression();
  consume(TOKEN_EOF, "Expect end of expression.");
*/
  while (!match(TOKEN_EOF)) {
    declaration();
  }
/* Compiling Expressions finish-compile < Calls and Functions call-end-compiler
  endCompiler();
*/
/* Compiling Expressions return-had-error < Calls and Functions call-end-compiler
  return !parser.hadError;
*/
  ObjFunction* function = endCompiler();
  return parser.hadError ? NULL : function;
}

void markCompilerRoots() {
  Compiler* compiler = current;
  while (compiler != NULL) {
    markObject((Obj*)compiler->function);
    compiler = compiler->enclosing;
  }
}

```

## compiler.h :
```
// compiler.h
#ifndef leetlang_compiler_h
#define leetlang_compiler_h

#include "object.h"
#include "vm.h"
/* Scanning on Demand compiler-h < Compiling Expressions compile-h
void compile(const char* source);
*/
/* Compiling Expressions compile-h < Calls and Functions compile-h
bool compile(const char* source, Chunk* chunk);
*/

ObjFunction* compile(const char* source);


void markCompilerRoots();


#endif
```

## memory.c :
```
// memory.c

#include <stdlib.h>
#include "compiler.h"
#include "memory.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>
#include "debug.h"
#endif

#define GC_HEAP_GROW_FACTOR 2

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
  vm.bytesAllocated += newSize - oldSize;
  if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
    collectGarbage();
#endif
    if (vm.bytesAllocated > vm.nextGC) {
      collectGarbage();
    }
  }

  if (newSize == 0) {
    free(pointer);
    return NULL;
  }
  void* result = realloc(pointer, newSize);
  if (result == NULL) exit(1);
  return result;
}

void markObject(Obj* object) {
  if (object == NULL) return;
  if (object->isMarked) return;

#ifdef DEBUG_LOG_GC
  printf("%p mark ", (void*)object);
  printValue(OBJ_VAL(object));
  printf("\n");
#endif

  object->isMarked = true;

  if (vm.grayCapacity < vm.grayCount + 1) {
    vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
    vm.grayStack = (Obj**)realloc(vm.grayStack,
                                  sizeof(Obj*) * vm.grayCapacity);

    if (vm.grayStack == NULL) exit(1);
  }
  vm.grayStack[vm.grayCount++] = object;
}


void markValue(Value value) {
  if (IS_OBJ(value)) markObject(AS_OBJ(value));
}

static void markArray(ValueArray* array) {
  for (int i = 0; i < array->count; i++) {
    markValue(array->values[i]);
  }
}

static void blackenObject(Obj* object) {
#ifdef DEBUG_LOG_GC
  printf("%p blacken ", (void*)object);
  printValue(OBJ_VAL(object));
  printf("\n");
#endif

  switch (object->type) {
    case OBJ_BOUND_METHOD: {
      ObjBoundMethod* bound = (ObjBoundMethod*)object;
      markValue(bound->receiver);
      markObject((Obj*)bound->method);
      break;
    }
    case OBJ_CLASS: {
      ObjClass* klass = (ObjClass*)object;
      markObject((Obj*)klass->name);

      markTable(&klass->methods);

      break;
    }


    case OBJ_CLOSURE: {
      ObjClosure* closure = (ObjClosure*)object;
      markObject((Obj*)closure->function);
      for (int i = 0; i < closure->upvalueCount; i++) {
        markObject((Obj*)closure->upvalues[i]);
      }
      break;
    }


    case OBJ_FUNCTION: {
      ObjFunction* function = (ObjFunction*)object;
      markObject((Obj*)function->name);
      markArray(&function->chunk.constants);
      break;
    }


    case OBJ_INSTANCE: {
      ObjInstance* instance = (ObjInstance*)object;
      markObject((Obj*)instance->klass);
      markTable(&instance->fields);
      break;
    }


    case OBJ_UPVALUE:
      markValue(((ObjUpvalue*)object)->closed);
      break;

    case OBJ_NATIVE:
    case OBJ_STRING:
      break;
  }
}


static void freeObject(Obj* object) {

#ifdef DEBUG_LOG_GC
  printf("%p free type %d\n", (void*)object, object->type);
#endif


  switch (object->type) {

    case OBJ_BOUND_METHOD:
      FREE(ObjBoundMethod, object);
      break;


    case OBJ_CLASS: {

      ObjClass* klass = (ObjClass*)object;
      freeTable(&klass->methods);

      FREE(ObjClass, object);
      break;
    } // [braces]


    case OBJ_CLOSURE: {

      ObjClosure* closure = (ObjClosure*)object;
      FREE_ARRAY(ObjUpvalue*, closure->upvalues,
                 closure->upvalueCount);

      FREE(ObjClosure, object);
      break;
    }


    case OBJ_FUNCTION: {
      ObjFunction* function = (ObjFunction*)object;
      freeChunk(&function->chunk);
      FREE(ObjFunction, object);
      break;
    }


    case OBJ_INSTANCE: {
      ObjInstance* instance = (ObjInstance*)object;
      freeTable(&instance->fields);
      FREE(ObjInstance, object);
      break;
    }


    case OBJ_NATIVE:
      FREE(ObjNative, object);
      break;

    case OBJ_STRING: {
      ObjString* string = (ObjString*)object;
      FREE_ARRAY(char, string->chars, string->length + 1);
      FREE(ObjString, object);
      break;
    }

    case OBJ_UPVALUE:
      FREE(ObjUpvalue, object);
      break;

  }
}


static void markRoots() {
  for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
    markValue(*slot);
  }


  for (int i = 0; i < vm.frameCount; i++) {
    markObject((Obj*)vm.frames[i].closure);
  }



  for (ObjUpvalue* upvalue = vm.openUpvalues;
       upvalue != NULL;
       upvalue = upvalue->next) {
    markObject((Obj*)upvalue);
  }



  markTable(&vm.globals);


  markCompilerRoots();


  markObject((Obj*)vm.initString);

}


static void traceReferences() {
  while (vm.grayCount > 0) {
    Obj* object = vm.grayStack[--vm.grayCount];
    blackenObject(object);
  }
}


static void sweep() {
  Obj* previous = NULL;
  Obj* object = vm.objects;
  while (object != NULL) {
    if (object->isMarked) {

      object->isMarked = false;

      previous = object;
      object = object->next;
    } else {
      Obj* unreached = object;
      object = object->next;
      if (previous != NULL) {
        previous->next = object;
      } else {
        vm.objects = object;
      }

      freeObject(unreached);
    }
  }
}


void collectGarbage() {

#ifdef DEBUG_LOG_GC
  printf("-- gc begin\n");

  size_t before = vm.bytesAllocated;

#endif



  markRoots();


  traceReferences();


  tableRemoveWhite(&vm.strings);


  sweep();



  vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;



#ifdef DEBUG_LOG_GC
  printf("-- gc end\n");

  printf("   collected %zu bytes (from %zu to %zu) next at %zu\n",
         before - vm.bytesAllocated, before, vm.bytesAllocated,
         vm.nextGC);

#endif

}


void freeObjects() {
  Obj* object = vm.objects;
  while (object != NULL) {
    Obj* next = object->next;
    freeObject(object);
    object = next;
  }


  free(vm.grayStack);

}

```

## memory.h :
```
// memory.h
#ifndef leetlang_memory_h
#define leetlang_memory_h
#include "common.h"

#include "object.h"



#define ALLOCATE(type, count) \
    (type*)reallocate(NULL, 0, sizeof(type) * (count))


#define FREE(type, pointer) reallocate(pointer, sizeof(type), 0)



#define GROW_CAPACITY(capacity) \
    ((capacity) < 8 ? 8 : (capacity) * 2)


#define GROW_ARRAY(type, pointer, oldCount, newCount) \
    (type*)reallocate(pointer, sizeof(type) * (oldCount), \
        sizeof(type) * (newCount))


#define FREE_ARRAY(type, pointer, oldCount) \
    reallocate(pointer, sizeof(type) * (oldCount), 0)


void* reallocate(void* pointer, size_t oldSize, size_t newSize);


void markObject(Obj* object);


void markValue(Value value);


void collectGarbage();


void freeObjects();


#endif
```

## object.c :
```
// object.c

#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"

#include "table.h"

#include "value.h"
#include "vm.h"


#define ALLOCATE_OBJ(type, objectType) \
    (type*)allocateObject(sizeof(type), objectType)



static Obj* allocateObject(size_t size, ObjType type) {
  Obj* object = (Obj*)reallocate(NULL, 0, size);
  object->type = type;

  object->isMarked = false;


  
  object->next = vm.objects;
  vm.objects = object;



#ifdef DEBUG_LOG_GC
  printf("%p allocate %zu for %d\n", (void*)object, size, type);
#endif


  return object;
}


ObjBoundMethod* newBoundMethod(Value receiver,
                               ObjClosure* method) {
  ObjBoundMethod* bound = ALLOCATE_OBJ(ObjBoundMethod,
                                       OBJ_BOUND_METHOD);
  bound->receiver = receiver;
  bound->method = method;
  return bound;
}


ObjClass* newClass(ObjString* name) {
    ObjClass* klass = ALLOCATE_OBJ(ObjClass, OBJ_CLASS);
    klass->name = name;
    initTable(&klass->methods);
    klass->superclassCount = 0;
    klass->superclasses = NULL;  // No superclasses initially
    return klass;
}

void classAddSuperclass(ObjClass* klass, ObjClass* superclass) {
    klass->superclassCount++;
    klass->superclasses = GROW_ARRAY(ObjClass*, klass->superclasses, klass->superclassCount - 1, klass->superclassCount);
    klass->superclasses[klass->superclassCount - 1] = superclass;
    tableAddAll(&superclass->methods, &klass->methods);  // Inherit methods
}

ObjClosure* newClosure(ObjFunction* function) {

  ObjUpvalue** upvalues = ALLOCATE(ObjUpvalue*,
                                   function->upvalueCount);
  for (int i = 0; i < function->upvalueCount; i++) {
    upvalues[i] = NULL;
  }


  ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
  closure->function = function;

  closure->upvalues = upvalues;
  closure->upvalueCount = function->upvalueCount;

  return closure;
}


ObjFunction* newFunction() {
  ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
  function->arity = 0;

  function->upvalueCount = 0;

  function->name = NULL;
  initChunk(&function->chunk);
  return function;
}


ObjInstance* newInstance(ObjClass* klass) {
  ObjInstance* instance = ALLOCATE_OBJ(ObjInstance, OBJ_INSTANCE);
  instance->klass = klass;
  initTable(&instance->fields);
  return instance;
}


ObjNative* newNative(NativeFn function) {
  ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
  native->function = function;
  return native;
}

ObjException* newException(ObjString* message) {
    ObjException* exception = ALLOCATE_OBJ(ObjException, OBJ_EXCEPTION);
    exception->message = message;
    return exception;
}
/* Strings allocate-string < Hash Tables allocate-string
static ObjString* allocateString(char* chars, int length) {
*/


static ObjString* allocateString(char* chars, int length,
                                 uint32_t hash) {

  ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
  string->length = length;
  string->chars = chars;
  string->hash = hash;
  push(OBJ_VAL(string));
  tableSet(&vm.strings, string, NIL_VAL);
  pop();
  return string;
}


static uint32_t hashString(const char* key, int length) {
  uint32_t hash = 2166136261u;
  for (int i = 0; i < length; i++) {
    hash ^= (uint8_t)key[i];
    hash *= 16777619;
  }
  return hash;
}


ObjString* takeString(char* chars, int length) {
/* Strings take-string < Hash Tables take-string-hash
  return allocateString(chars, length);
*/

  uint32_t hash = hashString(chars, length);

  ObjString* interned = tableFindString(&vm.strings, chars, length,
                                        hash);
  if (interned != NULL) {
    FREE_ARRAY(char, chars, length + 1);
    return interned;
  }


  return allocateString(chars, length, hash);

}

ObjString* copyString(const char* chars, int length) {

  uint32_t hash = hashString(chars, length);

  ObjString* interned = tableFindString(&vm.strings, chars, length,
                                        hash);
  if (interned != NULL) return interned;



  char* heapChars = ALLOCATE(char, length + 1);
  memcpy(heapChars, chars, length);
  heapChars[length] = '\0';
/* Strings object-c < Hash Tables copy-string-allocate
  return allocateString(heapChars, length);
*/

  return allocateString(heapChars, length, hash);

}

ObjUpvalue* newUpvalue(Value* slot) {
  ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);

  upvalue->closed = NIL_VAL;

  upvalue->location = slot;

  upvalue->next = NULL;

  return upvalue;
}

ObjExceptionHandler* newExceptionHandler(int catchAddress) {
    ObjExceptionHandler* handler = ALLOCATE_OBJ(ObjExceptionHandler, OBJ_EXCEPTION_HANDLER);
    handler->catchAddress = catchAddress;
    return handler;
}

ObjString* asString(Obj* obj) {
    if (obj->type == OBJ_STRING) {
        return (ObjString*)obj;
    } else {
        return NULL; // Return NULL if it is not the correct type
    }
}

static void printFunction(ObjFunction* function) {

  if (function->name == NULL) {
    printf("<script>");
    return;
  }

  printf("<fn %s>", function->name->chars);
}


void printObject(Value value) {
  switch (OBJ_TYPE(value)) {

    case OBJ_BOUND_METHOD:
      printFunction(AS_BOUND_METHOD(value)->method->function);
      break;


    case OBJ_CLASS:
      printf("%s", AS_CLASS(value)->name->chars);
      break;


    case OBJ_CLOSURE:
      printFunction(AS_CLOSURE(value)->function);
      break;


    case OBJ_FUNCTION:
      printFunction(AS_FUNCTION(value));
      break;


    case OBJ_INSTANCE:
      printf("%s instance",
             AS_INSTANCE(value)->klass->name->chars);
      break;


    case OBJ_NATIVE:
      printf("<native fn>");
      break;

    case OBJ_STRING:
      printf("%s", AS_CSTRING(value));
      break;

    case OBJ_UPVALUE:
      printf("upvalue");
      break;

  }
}

```

## object.h :
```
// object.h
#ifndef leetlang_object_h
#define leetlang_object_h
#include "common.h"

#include "chunk.h"


#include "table.h"

#include "value.h"


#define OBJ_TYPE(value)        (AS_OBJ(value)->type)




#define IS_BOUND_METHOD(value) isObjType(value, OBJ_BOUND_METHOD)
#define IS_CLASS(value)        isObjType(value, OBJ_CLASS)
#define IS_CLOSURE(value)      isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value)     isObjType(value, OBJ_FUNCTION)
#define IS_INSTANCE(value)     isObjType(value, OBJ_INSTANCE)
#define IS_NATIVE(value)       isObjType(value, OBJ_NATIVE)
#define IS_STRING(value)       isObjType(value, OBJ_STRING)
#define IS_UPVALUE(value)      isObjType(value, OBJ_UPVALUE)
#define IS_EXCEPTION_HANDLER(value) isObjType(value, OBJ_EXCEPTION_HANDLER)



#define AS_BOUND_METHOD(value) ((ObjBoundMethod*)AS_OBJ(value))
#define AS_CLASS(value)        ((ObjClass*)AS_OBJ(value))
#define AS_CLOSURE(value)      ((ObjClosure*)AS_OBJ(value))
#define AS_FUNCTION(value)     ((ObjFunction*)AS_OBJ(value))
#define AS_INSTANCE(value)     ((ObjInstance*)AS_OBJ(value))
#define AS_NATIVE(value) \
    (((ObjNative*)AS_OBJ(value))->function)
#define AS_STRING(value)       ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value)      (((ObjString*)AS_OBJ(value))->chars)
#define AS_UPVALUE(value)      ((ObjUpvalue*)AS_OBJ(value))
#define AS_EXCEPTION_HANDLER(value) ((ObjExceptionHandler*)AS_OBJ(value))

typedef enum {
    OBJ_BOUND_METHOD,
    OBJ_CLASS,
    OBJ_CLOSURE,
    OBJ_FUNCTION,
    OBJ_INSTANCE,
    OBJ_NATIVE,
    OBJ_STRING,
    OBJ_UPVALUE,
    OBJ_EXCEPTION,
    OBJ_EXCEPTION_HANDLER
} ObjType;


struct Obj {
  ObjType type;
  bool isMarked;
  struct Obj* next;
};


typedef struct {
  Obj obj;
  int arity;
  int upvalueCount;
  Chunk chunk;
  ObjString* name;
} ObjFunction;

typedef struct {
    Obj obj;
    ObjString* message;  // Message for exception details
} ObjException;

typedef Value (*NativeFn)(int argCount, Value* args);

typedef struct {
  Obj obj;
  NativeFn function;
} ObjNative;

struct ObjString {
  Obj obj;
  int length;
  char* chars;
  uint32_t hash;
};


typedef struct ObjUpvalue {
  Obj obj;
  Value* location;

  Value closed;


  struct ObjUpvalue* next;

} ObjUpvalue;


typedef struct {
  Obj obj;
  ObjFunction* function;
  ObjUpvalue** upvalues;
  int upvalueCount;
} ObjClosure;
typedef struct ObjClass ObjClass; 
typedef struct ObjClass {
  Obj obj;
  ObjString* name;
  Table methods;
  int superclassCount;
  ObjClass** superclasses;
} ObjClass;

typedef struct {
  Obj obj;
  ObjClass* klass;
  Table fields; // [fields]
} ObjInstance;



typedef struct {
  Obj obj;
  Value receiver;
  ObjClosure* method;
} ObjBoundMethod;

typedef struct {
    Obj obj;
    int catchAddress;  // Address to jump to for the catch block
} ObjExceptionHandler;

ObjBoundMethod* newBoundMethod(Value receiver,
                               ObjClosure* method);
ObjClass* newClass(ObjString* name);
ObjClosure* newClosure(ObjFunction* function);
ObjFunction* newFunction();
ObjInstance* newInstance(ObjClass* klass);
ObjNative* newNative(NativeFn function);
ObjString* takeString(char* chars, int length);
ObjString* copyString(const char* chars, int length);
void classAddSuperclass(ObjClass* klass, ObjClass* superclass);
ObjUpvalue* newUpvalue(Value* slot);
ObjString* asString(Obj* obj);
ObjException* newException(ObjString* message);
ObjExceptionHandler* newExceptionHandler(int catchAddress);
void printObject(Value value);


static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}


#endif
```

## scanner.c :
```
// scanner.c

#include <stdio.h>
#include <string.h>

#include "common.h"
#include "scanner.h"

typedef struct {
  const char* start;
  const char* current;
  int line;
} Scanner;

Scanner scanner;

void initScanner(const char* source) {
  scanner.start = source;
  scanner.current = source;
  scanner.line = 1;
}


static bool isAlpha(char c) {
  return (c >= 'a' && c <= 'z') ||
         (c >= 'A' && c <= 'Z') ||
          c == '_';
}


static bool isDigit(char c) {
  return c >= '0' && c <= '9';
}


static bool isAtEnd() {
  return *scanner.current == '\0';
}


static char advance() {
  scanner.current++;
  return scanner.current[-1];
}


static char peek() {
  return *scanner.current;
}


static char peekNext() {
  if (isAtEnd()) return '\0';
  return scanner.current[1];
}


static bool match(char expected) {
  if (isAtEnd()) return false;
  if (*scanner.current != expected) return false;
  scanner.current++;
  return true;
}


static Token makeToken(TokenType type) {
  Token token;
  token.type = type;
  token.start = scanner.start;
  token.length = (int)(scanner.current - scanner.start);
  token.line = scanner.line;
  return token;
}


static Token errorToken(const char* message) {
  Token token;
  token.type = TOKEN_ERROR;
  token.start = message;
  token.length = (int)strlen(message);
  token.line = scanner.line;
  return token;
}


static void skipWhitespace() {
  for (;;) {
    char c = peek();
    switch (c) {
      case ' ':
      case '\r':
      case '\t':
        advance();
        break;

      case '\n':
        scanner.line++;
        advance();
        break;


      case '/':
        if (peekNext() == '/') {
          // A comment goes until the end of the line.
          while (peek() != '\n' && !isAtEnd()) advance();
        } else {
          return;
        }
        break;

      default:
        return;
    }
  }
}


static TokenType checkKeyword(int start, int length,
    const char* rest, TokenType type) {
  if (scanner.current - scanner.start == start + length &&
      memcmp(scanner.start + start, rest, length) == 0) {
    return type;
  }

  return TOKEN_IDENTIFIER;
}


static TokenType identifierType() {
    switch (scanner.start[0]) {
        case 'a': return checkKeyword(1, 2, "nd", TOKEN_AND);
        case 'c': 
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'a': return checkKeyword(2, 4, "tch", TOKEN_CATCH);
                    case 'l': return checkKeyword(2, 3, "ass", TOKEN_CLASS);
                }
            }
            break;
        case 'e': return checkKeyword(1, 3, "lse", TOKEN_ELSE);
        case 'f':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'a': return checkKeyword(2, 3, "lse", TOKEN_FALSE);
                    case 'i': return checkKeyword(2, 5, "nally", TOKEN_FINALLY);
                    case 'o': return checkKeyword(2, 1, "r", TOKEN_FOR);
                    case 'u': return checkKeyword(2, 1, "n", TOKEN_FUN);
                }
            }
            break;
        case 'i': return checkKeyword(1, 1, "f", TOKEN_IF);
        case 'n':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'o': return checkKeyword(2, 1, "t", TOKEN_NOT);
                    case 'i': return checkKeyword(2, 1, "l", TOKEN_NIL);
                }
            }
            break;
        case 'o': return checkKeyword(1, 1, "r", TOKEN_OR);
        case 'p': return checkKeyword(1, 4, "rint", TOKEN_PRINT);
        case 'r': return checkKeyword(1, 5, "eturn", TOKEN_RETURN);
        case 's': return checkKeyword(1, 4, "uper", TOKEN_SUPER);
        case 't':
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'h': 
                        if (scanner.current - scanner.start > 2) {
                            switch (scanner.start[2]) {
                                case 'i': return checkKeyword(2, 2, "s", TOKEN_THIS);
                                case 'r': return checkKeyword(2, 3, "ow", TOKEN_THROW);
                            }
                        }
                        break;
                    case 'r': 
                        if (scanner.current - scanner.start > 2) {
                            switch (scanner.start[2]) {
                                case 'u': return checkKeyword(3, 1, "e", TOKEN_TRUE);
                                case 'y': return checkKeyword(3, 0, "", TOKEN_TRY);
                            }
                        }
                        break;
                }
            }
            break;
        case 'v': return checkKeyword(1, 2, "ar", TOKEN_VAR);
        case 'w': return checkKeyword(1, 4, "hile", TOKEN_WHILE);
    }

    return TOKEN_IDENTIFIER;
}


static Token identifier() {
  while (isAlpha(peek()) || isDigit(peek())) advance();
  return makeToken(identifierType());
}


static Token number() {
  while (isDigit(peek())) advance();

  // Look for a fractional part.
  if (peek() == '.' && isDigit(peekNext())) {
    // Consume the ".".
    advance();

    while (isDigit(peek())) advance();
  }

  return makeToken(TOKEN_NUMBER);
}


static Token string() {
  while (peek() != '"' && !isAtEnd()) {
    if (peek() == '\n') scanner.line++;
    advance();
  }

  if (isAtEnd()) return errorToken("Unterminated string.");

  // The closing quote.
  advance();
  return makeToken(TOKEN_STRING);
}


Token scanToken() {

  skipWhitespace();

  scanner.start = scanner.current;

  if (isAtEnd()) return makeToken(TOKEN_EOF);

  
  char c = advance();

  if (isAlpha(c)) return identifier();


  if (isDigit(c)) return number();


  switch (c) {
    case '(': return makeToken(TOKEN_LEFT_PAREN);
    case ')': return makeToken(TOKEN_RIGHT_PAREN);
    case '{': return makeToken(TOKEN_LEFT_BRACE);
    case '}': return makeToken(TOKEN_RIGHT_BRACE);
    case ';': return makeToken(TOKEN_SEMICOLON);
    case ',': return makeToken(TOKEN_COMMA);
    case '.': return makeToken(TOKEN_DOT);
    case '-': return makeToken(TOKEN_MINUS);
    case '+': return makeToken(TOKEN_PLUS);
    case '/': return makeToken(TOKEN_SLASH);
    case '*': return makeToken(TOKEN_STAR);

    case '!':
      return makeToken(
          match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
    case '=':
      return makeToken(
          match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
    case '<':
      return makeToken(
          match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
    case '>':
      return makeToken(
          match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);


    case '"': return string();

  }


  return errorToken("Unexpected character.");
}

```

## scanner.h :
```
// scanner.h
#ifndef leetlang_scanner_h
#define leetlang_scanner_h

typedef enum {
  // Single-character tokens.
  TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN,
  TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,
  TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS,
  TOKEN_SEMICOLON, TOKEN_SLASH, TOKEN_STAR,
  // One or two character tokens.
  TOKEN_BANG, TOKEN_BANG_EQUAL, TOKEN_NOT,
  TOKEN_EQUAL, TOKEN_EQUAL_EQUAL,
  TOKEN_GREATER, TOKEN_GREATER_EQUAL,
  TOKEN_LESS, TOKEN_LESS_EQUAL,
  // Literals.
  TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER,
  // Keywords.
  TOKEN_AND, TOKEN_CLASS, TOKEN_ELSE, TOKEN_FALSE,
  TOKEN_FOR, TOKEN_FUN, TOKEN_IF, TOKEN_NIL, TOKEN_OR,
  TOKEN_PRINT, TOKEN_RETURN, TOKEN_SUPER, TOKEN_THIS,
  TOKEN_TRUE, TOKEN_VAR, TOKEN_WHILE,
  TOKEN_TRY, TOKEN_CATCH, TOKEN_THROW, TOKEN_FINALLY, TOKEN_EXCEPTION,

  TOKEN_ERROR, TOKEN_EOF
} TokenType;



typedef struct {
  TokenType type;
  const char* start;
  int length;
  int line;
} Token;


void initScanner(const char* source);

Token scanToken();


#endif
```

## table.c :
```
// table.c

#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"


#define TABLE_MAX_LOAD 0.75


void initTable(Table* table) {
  table->count = 0;
  table->capacity = 0;
  table->entries = NULL;
}

void freeTable(Table* table) {
  FREE_ARRAY(Entry, table->entries, table->capacity);
  initTable(table);
}


static Entry* findEntry(Entry* entries, int capacity,
                        ObjString* key) {
  uint32_t index = key->hash & (capacity - 1);
  Entry* tombstone = NULL;

  for (;;) {
    Entry* entry = &entries[index];

    if (entry->key == NULL) {
      if (IS_NIL(entry->value)) {
        // Empty entry.
        return tombstone != NULL ? tombstone : entry;
      } else {
        // We found a tombstone.
        if (tombstone == NULL) tombstone = entry;
      }
    } else if (entry->key == key) {
      // We found the key.
      return entry;
    }

    index = (index + 1) & (capacity - 1);
  }
}


static void adjustCapacity(Table* table, int capacity) {
  Entry* entries = ALLOCATE(Entry, capacity);
  for (int i = 0; i < capacity; i++) {
    entries[i].key = NULL;
    entries[i].value = NIL_VAL;
  }

  table->count = 0;
  for (int i = 0; i < table->capacity; i++) {
    Entry* entry = &table->entries[i];
    if (entry->key == NULL) continue;

    Entry* dest = findEntry(entries, capacity, entry->key);
    dest->key = entry->key;
    dest->value = entry->value;
    table->count++;
  }

  FREE_ARRAY(Entry, table->entries, table->capacity);
  table->entries = entries;
  table->capacity = capacity;
}


bool tableGet(Table* table, ObjString* key, Value* value) {
  if (table->entries == NULL) return false;

  Entry* entry = findEntry(table->entries, table->capacity, key);
  if (entry->key == NULL) return false;

  *value = entry->value;
  return true;
}


bool tableDelete(Table* table, ObjString* key) {
  if (table->count == 0) return false;

  // Find the entry.
  Entry* entry = findEntry(table->entries, table->capacity, key);
  if (entry->key == NULL) return false;

  // Place a tombstone in the entry.
  entry->key = NULL;
  entry->value = BOOL_VAL(true);

  return true;
}



bool tableSet(Table* table, ObjString* key, Value value) {
  if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
    int capacity = GROW_CAPACITY(table->capacity);
    adjustCapacity(table, capacity);
  }

  Entry* entry = findEntry(table->entries, table->capacity, key);
  bool isNewKey = entry->key == NULL;
  if (isNewKey && IS_NIL(entry->value)) table->count++;

  entry->key = key;
  entry->value = value;
  return isNewKey;
}

void tableAddAll(Table* from, Table* to) {
  for (int i = 0; i < from->capacity; i++) {
    Entry* entry = &from->entries[i];
    if (entry->key != NULL) {
      tableSet(to, entry->key, entry->value);
    }
  }
}


ObjString* tableFindString(Table* table, const char* chars,
                           int length, uint32_t hash) {
  if (table->entries == NULL) return NULL;

  uint32_t index = hash & (table->capacity - 1);

  for (;;) {
    Entry* entry = &table->entries[index];

    if (entry->key == NULL) {
      // Stop if we find an empty non-tombstone entry.
      if (IS_NIL(entry->value)) return NULL;
    } else if (entry->key->length == length &&
               entry->key->hash == hash &&
               memcmp(entry->key->chars, chars, length) == 0) {
      // We found it.
      return entry->key;
    }

    index = (index + 1) & (table->capacity - 1);
  }
}

void markTable(Table* table) {
  for (int i = 0; i < table->capacity; i++) {
    Entry* entry = &table->entries[i];
    markObject((Obj*)entry->key);
    markValue(entry->value);
  }
}

void tableRemoveWhite(Table* table) {
  for (int i = 0; i < table->capacity; i++) {
    Entry* entry = &table->entries[i];
    if (entry->key != NULL && !entry->key->obj.isMarked) {
      tableDelete(table, entry->key);
    }
  }
}





```

## table.h :
```
// table.h
#ifndef leetlang_table_h
#define leetlang_table_h
#include "common.h"
#include "value.h"

typedef struct {
  ObjString* key;
  Value value;
} Entry;

typedef struct {
  int count;
  int capacity;
  Entry* entries;
} Table;

void initTable(Table* table);
void freeTable(Table* table);
bool tableGet(Table* table, ObjString* key, Value* value);
bool tableSet(Table* table, ObjString* key, Value value);
bool tableDelete(Table* table, ObjString* key);
void tableAddAll(Table* from, Table* to);
ObjString* tableFindString(Table* table, const char* chars,
                           int length, uint32_t hash);
void tableRemoveWhite(Table* table);
void markTable(Table* table);
#endif
```

## value.c :
```
// value.c

#include <stdio.h>

#include <string.h>



#include "object.h"

#include "memory.h"
#include "value.h"

void initValueArray(ValueArray* array) {
  array->values = NULL;
  array->capacity = 0;
  array->count = 0;
}

void writeValueArray(ValueArray* array, Value value) {
  if (array->capacity < array->count + 1) {
    int oldCapacity = array->capacity;
    array->capacity = GROW_CAPACITY(oldCapacity);
    array->values = GROW_ARRAY(Value, array->values,
                               oldCapacity, array->capacity);
  }
  
  array->values[array->count] = value;
  array->count++;
}


void freeValueArray(ValueArray* array) {
  FREE_ARRAY(Value, array->values, array->capacity);
  initValueArray(array);
}


void printValue(Value value) {

#ifdef NAN_BOXING
  if (IS_BOOL(value)) {
    printf(AS_BOOL(value) ? "true" : "false");
  } else if (IS_NIL(value)) {
    printf("nil");
  } else if (IS_NUMBER(value)) {
    printf("%g", AS_NUMBER(value));
  } else if (IS_OBJ(value)) {
    printObject(value);
  }
#else

/* Chunks of Bytecode print-value < Types of Values print-number-value
  printf("%g", value);
*/
/* Types of Values print-number-value < Types of Values print-value
 printf("%g", AS_NUMBER(value));
 */

  switch (value.type) {
    case VAL_BOOL:
      printf(AS_BOOL(value) ? "true" : "false");
      break;
    case VAL_NIL: printf("nil"); break;
    case VAL_NUMBER: printf("%g", AS_NUMBER(value)); break;

    case VAL_OBJ: printObject(value); break;

  }


#endif

}


bool valuesEqual(Value a, Value b) {

#ifdef NAN_BOXING

  if (IS_NUMBER(a) && IS_NUMBER(b)) {
    return AS_NUMBER(a) == AS_NUMBER(b);
  }

  return a == b;
#else

  if (a.type != b.type) return false;
  switch (a.type) {
    case VAL_BOOL:   return AS_BOOL(a) == AS_BOOL(b);
    case VAL_NIL:    return true;
    case VAL_NUMBER: return AS_NUMBER(a) == AS_NUMBER(b);
/* Strings strings-equal < Hash Tables equal
    case VAL_OBJ: {
      ObjString* aString = AS_STRING(a);
      ObjString* bString = AS_STRING(b);
      return aString->length == bString->length &&
          memcmp(aString->chars, bString->chars,
                 aString->length) == 0;
    }
 */

    case VAL_OBJ:    return AS_OBJ(a) == AS_OBJ(b);

    default:         return false; // Unreachable.
  }

#endif

}

```

## value.h :
```
// value.h
#ifndef leetlang_value_h
#define leetlang_value_h

#include <string.h>
#include "common.h"

typedef struct Obj Obj;
typedef struct ObjString ObjString;

#ifdef NAN_BOXING
#define SIGN_BIT ((uint64_t)0x8000000000000000)
#define QNAN     ((uint64_t)0x7ffc000000000000)
#define TAG_NIL   1 // 01.
#define TAG_FALSE 2 // 10.
#define TAG_TRUE  3 // 11.

typedef uint64_t Value;

#define IS_BOOL(value)      (((value) | 1) == TRUE_VAL)
#define IS_NIL(value)       ((value) == NIL_VAL)
#define IS_NUMBER(value)    (((value) & QNAN) != QNAN)
#define IS_OBJ(value) \
    (((value) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))
#define IS_EXCEPTION(value) (IS_OBJ(value) && AS_OBJ(value)->type == OBJ_EXCEPTION)

#define AS_BOOL(value)      ((value) == TRUE_VAL)
#define AS_NUMBER(value)    valueToNum(value)
#define AS_OBJ(value) \
    ((Obj*)(uintptr_t)((value) & ~(SIGN_BIT | QNAN)))
#define AS_EXCEPTION(value) ((ObjException*)AS_OBJ(value))  

#define BOOL_VAL(b)     ((b) ? TRUE_VAL : FALSE_VAL)
#define FALSE_VAL       ((Value)(uint64_t)(QNAN | TAG_FALSE))
#define TRUE_VAL        ((Value)(uint64_t)(QNAN | TAG_TRUE))
#define NIL_VAL         ((Value)(uint64_t)(QNAN | TAG_NIL))
#define NUMBER_VAL(num) numToValue(num)
#define OBJ_VAL(obj) \
    (Value)(SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj))
#define EXCEPTION_VAL(exception) OBJ_VAL(exception)


static inline double valueToNum(Value value) {
  double num;
  memcpy(&num, &value, sizeof(Value));
  return num;
}

static inline Value numToValue(double num) {
  Value value;
  memcpy(&value, &num, sizeof(double));
  return value;
}


#else
typedef enum {
  VAL_BOOL,
  VAL_NIL, // [user-types]
  VAL_NUMBER,
  VAL_OBJ
} ValueType;

typedef struct {
  ValueType type;
  union {
    bool boolean;
    double number;
    Obj* obj;
  } as; // [as]
} Value;

#define IS_BOOL(value)    ((value).type == VAL_BOOL)
#define IS_NIL(value)     ((value).type == VAL_NIL)
#define IS_NUMBER(value)  ((value).type == VAL_NUMBER)
#define IS_OBJ(value)     ((value).type == VAL_OBJ)
#define AS_OBJ(value)     ((value).as.obj)
#define AS_BOOL(value)    ((value).as.boolean)
#define AS_NUMBER(value)  ((value).as.number)
#define BOOL_VAL(value)   ((Value){VAL_BOOL, {.boolean = value}})
#define NIL_VAL           ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object)   ((Value){VAL_OBJ, {.obj = (Obj*)object}})

#endif

typedef struct {
  int capacity;
  int count;
  Value* values;
} ValueArray;

bool valuesEqual(Value a, Value b);
void initValueArray(ValueArray* array);
void writeValueArray(ValueArray* array, Value value);
void freeValueArray(ValueArray* array);
void printValue(Value value);

#endif

```

## vm.c :
```
// vm.c


#include <stdarg.h>


#include <stdio.h>

#include <string.h>


#include <time.h>



#include "common.h"

#include "compiler.h"


#include "debug.h"


#include "object.h"
#include "memory.h"

#include "vm.h"

VM vm; // [one]

static void resetStack() {
  vm.stackTop = vm.stack;

  vm.frameCount = 0;


  vm.openUpvalues = NULL;

}


static void runtimeError(const char* format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

/* Types of Values runtime-error < Calls and Functions runtime-error-temp
  size_t instruction = vm.ip - vm.chunk->code - 1;
  int line = vm.chunk->lines[instruction];
*/
/* Calls and Functions runtime-error-temp < Calls and Functions runtime-error-stack
  CallFrame* frame = &vm.frames[vm.frameCount - 1];
  size_t instruction = frame->ip - frame->function->chunk.code - 1;
  int line = frame->function->chunk.lines[instruction];
*/
/* Types of Values runtime-error < Calls and Functions runtime-error-stack
  fprintf(stderr, "[line %d] in script\n", line);
*/

  for (int i = vm.frameCount - 1; i >= 0; i--) {
    CallFrame* frame = &vm.frames[i];
/* Calls and Functions runtime-error-stack < Closures runtime-error-function
    ObjFunction* function = frame->function;
*/

    ObjFunction* function = frame->closure->function;

    size_t instruction = frame->ip - function->chunk.code - 1;
    fprintf(stderr, "[line %d] in ", // [minus]
            function->chunk.lines[instruction]);
    if (function->name == NULL) {
      fprintf(stderr, "script\n");
    } else {
      fprintf(stderr, "%s()\n", function->name->chars);
    }
  }


  resetStack();
}

// Helper function to check if a class is a subclass of another
bool isSubclass(ObjClass* derived, ObjClass* superclass) {
    if (derived == superclass) {
        return true;
    }
    for (int i = 0; i < derived->superclassCount; i++) {
        if (isSubclass(derived->superclasses[i], superclass)) {
            return true;
        }
    }
    return false;
}

// Check if the object is an instance of the given class or a subclass thereof
bool instanceOf(ObjInstance* instance, ObjClass* klass) {
    ObjClass* instanceClass = instance->klass;
    if (instanceClass == klass) return true;

    for (int i = 0; i < instanceClass->superclassCount; i++) {
        if (isSubclass(instanceClass->superclasses[i], klass)) {
            return true;
        }
    }
    return false;
}

static Value exceptionInit(int argCount, Value* args) {
    // Initialize the exception with a message

    return NIL_VAL;
}

static Value exceptionGetMessage(int argCount, Value* args) {
    // Return the message associated with the exception
    return NIL_VAL;
}

void defineNativeMethod(ObjClass* klass, const char* name, NativeFn function) {
    push(OBJ_VAL(copyString(name, (int)strlen(name) )));
    push(OBJ_VAL(newNative(function)));
    tableSet(&klass->methods, AS_STRING(vm.stack[0]), vm.stack[1]);
    pop();
    pop();
}

void initExceptionClass() {
    ObjString* name = copyString("Exception", 9);
    ObjClass* klass = newClass(name);
    push(OBJ_VAL(klass)); // Protect from GC

    defineNativeMethod(klass, "init", exceptionInit);
    defineNativeMethod(klass, "getMessage", exceptionGetMessage);

    tableSet(&vm.globals, name, OBJ_VAL(klass));
    pop(); // Remove class from stack
}

static Value clockNative(int argCount, Value* args) {
  return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static Value assertNative(int argCount, Value* args) {
    // printf("Assert called\n");
    if (argCount != 2) {
        runtimeError("Assert takes two arguments: the condition and the error message.");
        return NIL_VAL;
    }

    if (!IS_BOOL(args[0])) {
        runtimeError("Assert condition must be a boolean.");
        return NIL_VAL;
    }

    if (!IS_STRING(args[1])) {
        runtimeError("Assert message must be a string.");
        return NIL_VAL;
    }

    bool condition = AS_BOOL(args[0]);
    ObjString* message = asString(AS_OBJ(args[1]));

    if (message == NULL) {
        runtimeError("Assert message is not a string.");
        return NIL_VAL;
    }

    if (!condition) {
        runtimeError("Assertion failed: %s", message->chars);
        return NIL_VAL;
    }

    return BOOL_VAL(true);
}

static Value printNative(int argCount, Value* args) {
    for (int i = 0; i < argCount; i++) {
        printValue(args[i]);
        printf("\n");
    }
    return NIL_VAL;
}

static void defineNative(const char* name, NativeFn function) {
  push(OBJ_VAL(copyString(name, (int)strlen(name))));
  push(OBJ_VAL(newNative(function)));
  tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
  pop();
  pop();
}


void initVM() {

  resetStack();


  vm.objects = NULL;


  vm.bytesAllocated = 0;
  vm.nextGC = 1024 * 1024;



  vm.grayCount = 0;
  vm.grayCapacity = 0;
  vm.grayStack = NULL;



  initTable(&vm.globals);


  initTable(&vm.strings);




  vm.initString = NULL;

  vm.initString = copyString("init", 4);



  defineNative("clock", clockNative);
  defineNative("assert", assertNative);

}

void freeVM() {

  freeTable(&vm.globals);


  freeTable(&vm.strings);


  vm.initString = NULL;


  freeObjects();

}

void push(Value value) {
  *vm.stackTop = value;
  vm.stackTop++;
}


Value pop() {
  vm.stackTop--;
  return *vm.stackTop;
}


static Value peek(int distance) {
  return vm.stackTop[-1 - distance];
}

/* Calls and Functions call < Closures call-signature
static bool call(ObjFunction* function, int argCount) {
*/


static bool call(ObjClosure* closure, int argCount) {

/* Calls and Functions check-arity < Closures check-arity
  if (argCount != function->arity) {
    runtimeError("Expected %d arguments but got %d.",
        function->arity, argCount);
*/

  if (argCount != closure->function->arity) {
    runtimeError("Expected %d arguments but got %d.",
        closure->function->arity, argCount);


    return false;
  }



  if (vm.frameCount == FRAMES_MAX) {
    runtimeError("Stack overflow.");
    return false;
  }


  CallFrame* frame = &vm.frames[vm.frameCount++];
/* Calls and Functions call < Closures call-init-closure
  frame->function = function;
  frame->ip = function->chunk.code;
*/

  frame->closure = closure;
  frame->ip = closure->function->chunk.code;

  frame->slots = vm.stackTop - argCount - 1;
  return true;
}


static bool callValue(Value callee, int argCount) {
  if (IS_OBJ(callee)) {
    switch (OBJ_TYPE(callee)) {

      case OBJ_BOUND_METHOD: {
        ObjBoundMethod* bound = AS_BOUND_METHOD(callee);

        vm.stackTop[-argCount - 1] = bound->receiver;

        return call(bound->method, argCount);
      }


      case OBJ_CLASS: {
        ObjClass* klass = AS_CLASS(callee);
        vm.stackTop[-argCount - 1] = OBJ_VAL(newInstance(klass));

        Value initializer;
        if (tableGet(&klass->methods, vm.initString,
                     &initializer)) {
          return call(AS_CLOSURE(initializer), argCount);

        } else if (argCount != 0) {
          runtimeError("Expected 0 arguments but got %d.",
                       argCount);
          return false;

        }

        return true;
      }


      case OBJ_CLOSURE:
        return call(AS_CLOSURE(callee), argCount);

/* Calls and Functions call-value < Closures call-value-closure
      case OBJ_FUNCTION: // [switch]
        return call(AS_FUNCTION(callee), argCount);
*/

      case OBJ_NATIVE: {
        NativeFn native = AS_NATIVE(callee);
        Value result = native(argCount, vm.stackTop - argCount);
        vm.stackTop -= argCount + 1;
        push(result);
        return true;
      }

      default:
        break; // Non-callable object type.
    }
  }
  runtimeError("Can only call functions and classes.");
  return false;
}


static bool invokeFromClass(ObjClass* klass, ObjString* name,
                            int argCount) {
  Value method;
  if (!tableGet(&klass->methods, name, &method)) {
    runtimeError("Undefined property '%s'.", name->chars);
    return false;
  }
  return call(AS_CLOSURE(method), argCount);
}


static bool invoke(ObjString* name, int argCount) {
  Value receiver = peek(argCount);
  if (!IS_INSTANCE(receiver)) {
    runtimeError("Only instances have methods.");
    return false;
  }
  ObjInstance* instance = AS_INSTANCE(receiver);
  Value value;
  if (tableGet(&instance->fields, name, &value)) {
    vm.stackTop[-argCount - 1] = value;
    return callValue(value, argCount);
  }
  return invokeFromClass(instance->klass, name, argCount);
}


static bool bindMethod(ObjClass* klass, ObjString* name) {
  Value method;
  for (int i = 0; i < klass->superclassCount; i++) {
    if (tableGet(&klass->superclasses[i]->methods, name, &method)) {
      ObjBoundMethod* bound = newBoundMethod(peek(0), AS_CLOSURE(method));
      pop(); // Pop the receiver which is now bound
      push(OBJ_VAL(bound)); // Push the bound method onto the stack
      return true;
    }
  }
  return false; // Method not found in any superclass
}

static ObjUpvalue* captureUpvalue(Value* local) {

  ObjUpvalue* prevUpvalue = NULL;
  ObjUpvalue* upvalue = vm.openUpvalues;
  while (upvalue != NULL && upvalue->location > local) {
    prevUpvalue = upvalue;
    upvalue = upvalue->next;
  }

  if (upvalue != NULL && upvalue->location == local) {
    return upvalue;
  }


  ObjUpvalue* createdUpvalue = newUpvalue(local);

  createdUpvalue->next = upvalue;

  if (prevUpvalue == NULL) {
    vm.openUpvalues = createdUpvalue;
  } else {
    prevUpvalue->next = createdUpvalue;
  }


  return createdUpvalue;
}


static void closeUpvalues(Value* last) {
  while (vm.openUpvalues != NULL &&
         vm.openUpvalues->location >= last) {
    ObjUpvalue* upvalue = vm.openUpvalues;
    upvalue->closed = *upvalue->location;
    upvalue->location = &upvalue->closed;
    vm.openUpvalues = upvalue->next;
  }
}


static void defineMethod(ObjString* name) {
  Value method = peek(0);
  ObjClass* klass = AS_CLASS(peek(1));
  tableSet(&klass->methods, name, method);
  pop();
}


static bool isFalsey(Value value) {
  return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}


static void concatenate() {
/* Strings concatenate < Garbage Collection concatenate-peek
  ObjString* b = AS_STRING(pop());
  ObjString* a = AS_STRING(pop());
*/

  ObjString* b = AS_STRING(peek(0));
  ObjString* a = AS_STRING(peek(1));


  int length = a->length + b->length;
  char* chars = ALLOCATE(char, length + 1);
  memcpy(chars, a->chars, a->length);
  memcpy(chars + a->length, b->chars, b->length);
  chars[length] = '\0';

  ObjString* result = takeString(chars, length);

  pop();
  pop();

  push(OBJ_VAL(result));
}


static InterpretResult run() {
  CallFrame* frame = &vm.frames[vm.frameCount - 1];

#define READ_BYTE() (*frame->ip++)
#define READ_SHORT() \
    (frame->ip += 2, \
    (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))

#define READ_CONSTANT() \
    (frame->closure->function->chunk.constants.values[READ_BYTE()])

#define READ_STRING() AS_STRING(READ_CONSTANT())
#define BINARY_OP(valueType, op) \
    do { \
      if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
        runtimeError("Operands must be numbers."); \
        return INTERPRET_RUNTIME_ERROR; \
      } \
      double b = AS_NUMBER(pop()); \
      double a = AS_NUMBER(pop()); \
      push(valueType(a op b)); \
    } while (false)

  for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
    printf("          ");
    for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
      printf("[ ");
      printValue(*slot);
      printf(" ]");
    }
    printf("\n");
    disassembleInstruction(&frame->closure->function->chunk,
        (int)(frame->ip - frame->closure->function->chunk.code));
#endif
    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
      case OP_CONSTANT: {
        Value constant = READ_CONSTANT();
        push(constant);
        break;
      }
      case OP_CONSTANT_LONG: {
        uint8_t byte1 = READ_BYTE();
        uint8_t byte2 = READ_BYTE();
        uint8_t byte3 = READ_BYTE();
        int constantIndex = (byte1 << 16) | (byte2 << 8) | byte3;
        push(frame->closure->function->chunk.constants.values[constantIndex]);
        break;
      }
      case OP_NIL: push(NIL_VAL); break;
      case OP_TRUE: push(BOOL_VAL(true)); break;
      case OP_FALSE: push(BOOL_VAL(false)); break;
      case OP_POP: pop(); break;
      case OP_GET_LOCAL: {
        uint8_t slot = READ_BYTE();
        push(frame->slots[slot]);
        break;
      }
      case OP_SET_LOCAL: {
        uint8_t slot = READ_BYTE();
        frame->slots[slot] = peek(0);
        break;
      }
      case OP_GET_GLOBAL: {
        ObjString* name = READ_STRING();
        Value value;
        if (!tableGet(&vm.globals, name, &value)) {
          runtimeError("Undefined variable '%s'.", name->chars);
          return INTERPRET_RUNTIME_ERROR;
        }
        push(value);
        break;
      }
      case OP_DEFINE_GLOBAL: {
        ObjString* name = READ_STRING();
        tableSet(&vm.globals, name, peek(0));
        pop();
        break;
      }
      case OP_SET_GLOBAL: {
        ObjString* name = READ_STRING();
        if (tableSet(&vm.globals, name, peek(0))) {
          tableDelete(&vm.globals, name); // [delete]
          runtimeError("Undefined variable '%s'.", name->chars);
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
      case OP_GET_UPVALUE: {
        uint8_t slot = READ_BYTE();
        push(*frame->closure->upvalues[slot]->location);
        break;
      }
      case OP_SET_UPVALUE: {
        uint8_t slot = READ_BYTE();
        *frame->closure->upvalues[slot]->location = peek(0);
        break;
      }
      case OP_GET_PROPERTY: {
        if (!IS_INSTANCE(peek(0))) {
          runtimeError("Only instances have properties.");
          return INTERPRET_RUNTIME_ERROR;
        }
        ObjInstance* instance = AS_INSTANCE(peek(0));
        ObjString* name = READ_STRING();
        Value value;
        if (tableGet(&instance->fields, name, &value)) {
          pop(); // Instance.
          push(value);
          break;
        }
        if (!bindMethod(instance->klass, name)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
      case OP_SET_PROPERTY: {
        if (!IS_INSTANCE(peek(1))) {
          runtimeError("Only instances have fields.");
          return INTERPRET_RUNTIME_ERROR;
        }
        ObjInstance* instance = AS_INSTANCE(peek(1));
        tableSet(&instance->fields, READ_STRING(), peek(0));
        Value value = pop();
        pop();
        push(value);
        break;
      }
      case OP_GET_SUPER: {
        ObjString* name = READ_STRING();
        ObjClass* superclass = AS_CLASS(pop());
        
        if (!bindMethod(superclass, name)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
      case OP_EQUAL: {
        Value b = pop();
        Value a = pop();
        push(BOOL_VAL(valuesEqual(a, b)));
        break;
      }
      case OP_GREATER:  BINARY_OP(BOOL_VAL, >); break;
      case OP_LESS:     BINARY_OP(BOOL_VAL, <); break;
      case OP_ADD: {
        if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
          concatenate();
        } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
          double b = AS_NUMBER(pop());
          double a = AS_NUMBER(pop());
          push(NUMBER_VAL(a + b));
        } else {
          runtimeError(
              "Operands must be two numbers or two strings.");
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
      case OP_SUBTRACT: BINARY_OP(NUMBER_VAL, -); break;
      case OP_MULTIPLY: BINARY_OP(NUMBER_VAL, *); break;
      case OP_DIVIDE:   BINARY_OP(NUMBER_VAL, /); break;
      case OP_NOT:
        push(BOOL_VAL(isFalsey(pop())));
        break;
      case OP_NEGATE:
        if (!IS_NUMBER(peek(0))) {
          runtimeError("Operand must be a number.");
          return INTERPRET_RUNTIME_ERROR;
        }
        push(NUMBER_VAL(-AS_NUMBER(pop())));
        break;
      case OP_PRINT: {
        printValue(pop());
        printf("\n");
        break;
      }
      case OP_JUMP: {
        uint16_t offset = READ_SHORT();
        frame->ip += offset;
        break;
      }
      case OP_JUMP_IF_FALSE: {
        uint16_t offset = READ_SHORT();
        if (isFalsey(peek(0))) frame->ip += offset;
        break;
      }
      case OP_LOOP: {
        uint16_t offset = READ_SHORT();
        frame->ip -= offset;
        break;
      }
      case OP_THROW: {
        ObjException* exception = AS_EXCEPTION(pop());
        // Unwind the stack to find the nearest exception handler
        // If no handler is found, this results in a runtime error
        break;
      }
      case OP_TRY_START: {
        int catchOffset = READ_SHORT();
        push(OBJ_VAL(newExceptionHandler(catchOffset)));
        break;
      }
      // Handle entering a catch block, possibly populating the caught exception
      case OP_CATCH_START: {
        ObjClass* expectedClass = AS_CLASS(peek(0)); // The expected class is on top of the stack
        ObjException* exception = AS_EXCEPTION(peek(1)); // The thrown exception object

        if (!instanceOf((ObjInstance*)exception, expectedClass)) {
            // If it's not an instance of the expected class or a subclass, skip the catch block
            int skipOffset = READ_SHORT();
            frame->ip += skipOffset;
        } else {
            // It's the right type, enter the catch block
            pop(); // Remove the class from the stack
        }
        break;
    }
      // Clean up after catch, run finally if present
      case OP_FINALLY_START: {
          // Prepare for finally execution
          break;
      }
      // End of finally block, restore normal execution flow
      case OP_FINALLY_END: {
          break;
      }
      case OP_CALL: {
        int argCount = READ_BYTE();
        if (!callValue(peek(argCount), argCount)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        frame = &vm.frames[vm.frameCount - 1];
        break;
      }
      case OP_INVOKE: {
        ObjString* method = READ_STRING();
        int argCount = READ_BYTE();
        if (!invoke(method, argCount)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        frame = &vm.frames[vm.frameCount - 1];
        break;
      }
      case OP_SUPER_INVOKE: {
        ObjString* method = READ_STRING();
        int argCount = READ_BYTE();
        ObjClass* superclass = AS_CLASS(pop());
        if (!invokeFromClass(superclass, method, argCount)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        frame = &vm.frames[vm.frameCount - 1];
        break;
      }
      case OP_CLOSURE: {
        ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
        ObjClosure* closure = newClosure(function);
        push(OBJ_VAL(closure));
        for (int i = 0; i < closure->upvalueCount; i++) {
          uint8_t isLocal = READ_BYTE();
          uint8_t index = READ_BYTE();
          if (isLocal) {
            closure->upvalues[i] =
                captureUpvalue(frame->slots + index);
          } else {
            closure->upvalues[i] = frame->closure->upvalues[index];
          }
        }
        break;
      }
      case OP_CLOSE_UPVALUE:
        closeUpvalues(vm.stackTop - 1);
        pop();
        break;
      case OP_RETURN: {
        Value result = pop();
        closeUpvalues(frame->slots);
        vm.frameCount--;
        if (vm.frameCount == 0) {
          pop();
          return INTERPRET_OK;
        }
        vm.stackTop = frame->slots;
        push(result);
        frame = &vm.frames[vm.frameCount - 1];
        break;
      }
      case OP_CLASS:
        push(OBJ_VAL(newClass(READ_STRING())));
        break;
      case OP_INHERIT: {
        Value superclass = peek(1);
        if (!IS_CLASS(superclass)) {
          runtimeError("Superclass must be a class.");
          return INTERPRET_RUNTIME_ERROR;
        }
        ObjClass* subclass = AS_CLASS(peek(0));
        tableAddAll(&AS_CLASS(superclass)->methods,
                    &subclass->methods);
        pop(); // Subclass.
        break;
      }
      case OP_METHOD:
        defineMethod(READ_STRING());
        break;
    }
  }

#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
}

void hack(bool b) {
  // Hack to avoid unused function error. run() is not used in the
  // scanning chapter.
  run();
  if (b) hack(false);
}


InterpretResult interpret(const char* source) {
  ObjFunction* function = compile(source);
  if (function == NULL) return INTERPRET_COMPILE_ERROR;
  push(OBJ_VAL(function));
  ObjClosure* closure = newClosure(function);
  pop();
  push(OBJ_VAL(closure));
  call(closure, 0);
  return run();
}

```

## vm.h :
```
// vm.h
#ifndef leetlang_vm_h
#define leetlang_vm_h
/* A Virtual Machine vm-h < Calls and Functions vm-include-object
#include "chunk.h"
*/

#include "object.h"


#include "table.h"


#include "value.h"




/* A Virtual Machine stack-max < Calls and Functions frame-max
#define STACK_MAX 256
*/

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)



typedef struct {
/* Calls and Functions call-frame < Closures call-frame-closure
  ObjFunction* function;
*/

  ObjClosure* closure;

  uint8_t* ip;
  Value* slots;
} CallFrame;


typedef struct {
/* A Virtual Machine vm-h < Calls and Functions frame-array
  Chunk* chunk;
*/
/* A Virtual Machine ip < Calls and Functions frame-array
  uint8_t* ip;
*/

  CallFrame frames[FRAMES_MAX];
  int frameCount;
  


  Value stack[STACK_MAX];
  Value* stackTop;


  Table globals;


  Table strings;


  ObjString* initString;


  ObjUpvalue* openUpvalues;



  size_t bytesAllocated;
  size_t nextGC;


  Obj* objects;


  int grayCount;
  int grayCapacity;
  Obj** grayStack;

} VM;


typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR
} InterpretResult;



extern VM vm;


void initVM();
void freeVM();
/* A Virtual Machine interpret-h < Scanning on Demand vm-interpret-h
InterpretResult interpret(Chunk* chunk);
*/

InterpretResult interpret(const char* source);


void push(Value value);
Value pop();


#endif
```

