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
#include <stdlib.h>
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

  for (int i = vm.frameCount - 1; i >= 0; i--) {
    CallFrame* frame = &vm.frames[i];

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

static Value typeNative(int argCount, Value* args) {
    if (argCount != 1) {
        runtimeError("typeNative takes one argument: the value to check the type of.");
        return NIL_VAL;
    }

    Value value = args[0];

    if (IS_BOOL(value)) {
        return OBJ_VAL(copyString("boolean", 7));
    } else if (IS_NUMBER(value)) {
        return OBJ_VAL(copyString("number", 6));
    } else if (IS_ARRAY(value)){
        return OBJ_VAL(copyString("array", 5));
    } else if (IS_OBJ(value)) {
        switch (OBJ_TYPE(value)) {
            case OBJ_BOUND_METHOD: return OBJ_VAL(copyString("bound method", 12));
            case OBJ_CLASS: return OBJ_VAL(copyString("class", 5));
            case OBJ_CLOSURE: return OBJ_VAL(copyString("closure", 7));
            case OBJ_FUNCTION: return OBJ_VAL(copyString("function", 8));
            case OBJ_INSTANCE: return OBJ_VAL(copyString("instance", 8));
            case OBJ_NATIVE: return OBJ_VAL(copyString("native", 6));
            case OBJ_STRING: return OBJ_VAL(copyString("string", 6));
            case OBJ_UPVALUE: return OBJ_VAL(copyString("upvalue", 7));
            case OBJ_EXCEPTION_HANDLER: return OBJ_VAL(copyString("exception handler", 17));
            default: return OBJ_VAL(copyString("unknown", 7));
        }
    } else {
        return OBJ_VAL(copyString("unknown", 7));
    }
}

static Value isinstanceNative(int argCount, Value* args) {
    if (argCount != 2) {
        runtimeError("isinstanceNative takes two arguments: the instance and the class.");
        return NIL_VAL;
    }

    if (!IS_INSTANCE(args[0])) {
        runtimeError("First argument must be an instance.");
        return NIL_VAL;
    }

    if (!IS_CLASS(args[1])) {
        runtimeError("Second argument must be a class.");
        return NIL_VAL;
    }

    ObjInstance* instance = AS_INSTANCE(args[0]);
    ObjClass* klass = AS_CLASS(args[1]);

    return BOOL_VAL(instanceOf(instance, klass));
}

static Value asmNative(int argCount, Value* args) {
    if (argCount != 1) {
        runtimeError("asm takes one argument: the assembly code to execute.");
        return NIL_VAL;
    }

    if (!IS_STRING(args[0])) {
        runtimeError("Argument to asm must be a string.");
        return NIL_VAL;
    }
    runtimeError("Not Implemented: asm");
    return NIL_VAL;
}

static Value exitNative(int argCount, Value* args) {
    if (argCount != 1) {
        // runtimeError("exit takes one argument: the exit code.");
        exit(0);
    }

    if (!IS_NUMBER(args[0])) {
        runtimeError("Exit code must be a number.");
        return NIL_VAL;
    }

    exit((int)AS_NUMBER(args[0]));
    return NIL_VAL;
}

static Value sortNative(int argCount, Value* args) {
  if (argCount != 1) {
    runtimeError("sort takes one argument: the array to sort.");
    return NIL_VAL;
  }
  if (IS_ARRAY(args[0])) {
    sortArray(AS_ARRAY(args[0]));
    return args[0];
  }
  runtimeError("Argument to sort must be an array.");
  return NIL_VAL;
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
  defineNative("type", typeNative);
  defineNative("isinstance", isinstanceNative);
  defineNative("asm", asmNative);
  defineNative("exit", exitNative);
  defineNative("sort", sortNative);
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

static Value arrayIndex(Value arrayVal, Value indexVal) {
  ObjArray* array = AS_ARRAY(arrayVal);
  int index = (int)AS_NUMBER(indexVal);
  if (index < 0 || index >= array->length) {
    runtimeError("Array index out of bounds.");
    return NIL_VAL;
  }
  return array->elements[index];
}

static Value stringIndex(Value stringVal, Value indexVal) {
  ObjString* string = AS_STRING(stringVal);
  int index = (int)AS_NUMBER(indexVal);
  if (index < 0 || index >= string->length) {
    runtimeError("String index out of bounds.");
    return NIL_VAL;
  }
  char chars[2] = { string->chars[index], '\0' };
  return OBJ_VAL(copyString(chars, 1));
}

static void setArrayIndex(Value arrayVal, Value indexVal, Value value) {
  ObjArray* array = AS_ARRAY(arrayVal);
  int index = (int)AS_NUMBER(indexVal);
  if (index < 0 || index >= array->length) {
    runtimeError("Array index out of bounds.");
    return;
  }
  array->elements[index] = value;
}

static void setStringIndex(Value stringVal, Value indexVal, Value value) {
  runtimeError("Strings are immutable.");
}

static bool call(ObjClosure* closure, int argCount) {

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
  if (!tableGet(&klass->methods, name, &method)) {
    runtimeError("Undefined property '%s'.", name->chars);
    return false;
  }

  ObjBoundMethod* bound = newBoundMethod(peek(0),
                                         AS_CLOSURE(method));
  pop();
  push(OBJ_VAL(bound));
  return true;
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
      case OP_ARRAY: {
        int elementCount = READ_BYTE();
        ObjArray* array = newArray(elementCount);
        for (int i = elementCount - 1; i >= 0; i--) {
          array->elements[i] = pop();
        }
        push(OBJ_VAL(array));
        break;
      }
      case OP_INDEX: {
        Value index = pop();
        Value collection = peek(0);
        
        if (IS_STRING(collection)) {
          if (!IS_NUMBER(index)) {
            runtimeError("String index must be a number.");
            return INTERPRET_RUNTIME_ERROR;
          }
          
          int i = (int)AS_NUMBER(index);
          ObjString* string = AS_STRING(collection);
          
          if (i < 0 || i >= string->length) {
            runtimeError("String index out of bounds.");
            return INTERPRET_RUNTIME_ERROR;
          }
          
          push(OBJ_VAL(copyString(&string->chars[i], 1)));
        } else if (IS_ARRAY(collection)) {
          if (!IS_NUMBER(index)) {
            runtimeError("Array index must be a number.");
            return INTERPRET_RUNTIME_ERROR;
          }
          
          int i = (int)AS_NUMBER(index);
          ObjArray* array = AS_ARRAY(collection);
          
          if (i < 0 || i >= array->length) {
            runtimeError("Array index out of bounds.");
            return INTERPRET_RUNTIME_ERROR;
          }
          
          push(array->elements[i]);
        } else {
          runtimeError("Cannot index non-array or non-string.");
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
      case OP_SET_INDEX: {
        Value value = pop();
        Value index = pop();
        Value collection = pop();
        if (IS_ARRAY(collection)) {
          setArrayIndex(collection, index, value);
        } else if (IS_STRING(collection)) {
          setStringIndex(collection, index, value);
        } else {
          runtimeError("Can only set elements on arrays and strings.");
          return INTERPRET_RUNTIME_ERROR;
        }
        push(value);  // Push the value to keep the stack balanced
        break;
      }
      case OP_ARRAY_INDEX: {
        Value index = pop();
        Value array = pop();
        if (!IS_ARRAY(array) || !IS_NUMBER(index)) {
          runtimeError("Invalid array indexing.");
          return INTERPRET_RUNTIME_ERROR;
        }
        push(arrayIndex(array, index));
        break;
      }
      case OP_STRING_INDEX: {
        Value index = pop();
        Value string = pop();
        if (!IS_STRING(string) || !IS_NUMBER(index)) {
          runtimeError("Invalid string indexing.");
          return INTERPRET_RUNTIME_ERROR;
        }
        push(stringIndex(string, index));
        break;
      }
      case OP_SORT: {
        Value array = pop();
        if (!IS_ARRAY(array)) {
          runtimeError("Can only sort arrays.");
          return INTERPRET_RUNTIME_ERROR;
        }
        sortArray(AS_ARRAY(array));
        push(array);
        break;
      }
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
