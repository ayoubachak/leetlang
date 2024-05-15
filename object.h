// object.h
#ifndef leetlang_object_h
#define leetlang_object_h
#include "common.h"

#include "chunk.h"


#include "table.h"

#include "value.h"


#define OBJ_TYPE(value)        (AS_OBJ(value)->type)

#define IS_ARRAY(value) isObjType(value, OBJ_ARRAY)
#define AS_ARRAY(value) ((ObjArray*)AS_OBJ(value))


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
    OBJ_EXCEPTION_HANDLER,
    OBJ_ARRAY 
} ObjType;


struct Obj {
  ObjType type;
  bool isMarked;
  struct Obj* next;
};

typedef struct {
  Obj obj;
  int length;
  Value* elements;
} ObjArray;

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

ObjArray* newArray(int length);
void sortArray(ObjArray* array);

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