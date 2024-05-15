// object.c

#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"

#include "table.h"

#include "value.h"
#include "vm.h"



ObjClass* arrayClass;
ObjClass* stringClass;

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



ObjArray* newArray(int length) {
  ObjArray* array = ALLOCATE_OBJ(ObjArray, OBJ_ARRAY);
  array->length = length;
  array->elements = ALLOCATE(Value, length);
  for (int i = 0; i < length; i++) {
    array->elements[i] = NIL_VAL;
  }
  return array;
}

void sortArray(ObjArray* array) {
  // Simple bubble sort for demonstration purposes
  for (int i = 0; i < array->length - 1; i++) {
    for (int j = 0; j < array->length - i - 1; j++) {
      if (AS_NUMBER(array->elements[j]) > AS_NUMBER(array->elements[j + 1])) {
        Value temp = array->elements[j];
        array->elements[j] = array->elements[j + 1];
        array->elements[j + 1] = temp;
      }
    }
  }
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
    case OBJ_ARRAY: {
      ObjArray* array = AS_ARRAY(value);
      printf("[");
      for (int i = 0; i < array->length; i++) {
        printValue(array->elements[i]);
        if (i < array->length - 1) printf(", ");
      }
      printf("]");
      break;
    }

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
