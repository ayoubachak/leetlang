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
  ObjClosure* closure;
  uint8_t* ip;
  Value* slots;
} CallFrame;


typedef struct {
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

InterpretResult interpret(const char* source);


void push(Value value);
Value pop();


#endif