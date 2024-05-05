#ifndef leetlang_memory_h
#define leetlang_memory_h
#include "common.h"

#define GROW_CAPACITY(capacity) \
    ((capacity) < 8 ? 8 : (capacity) * 2)

#define GROW_ARRAY(type, pointer, oldCount, newCount) \
    (type*)reallocate(pointer, sizeof(type) * (oldCount), \
    sizeof(type) * (newCount))

#define FREE_ARRAY(type, pointer, oldCount) \
    reallocate(pointer, sizeof(type) * (oldCount), 0)

void* reallocate(void* pointer, size_t oldSize, size_t newSize);

void initMemory(void);
void* customAllocate(size_t size); // I can't be bothered to use this memory management funciton so I'll be using reallocate
void freeMemory(void);

#endif

