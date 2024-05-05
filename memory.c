#include <stdlib.h>
#include "memory.h"

// Define the total memory pool size
#define MEMORY_POOL_SIZE 1024 * 1024 // 1MB for example

static char* memoryPool;
static size_t memoryUsed;

void initMemory() {
    memoryPool = malloc(MEMORY_POOL_SIZE);
    if (memoryPool == NULL) exit(1);
    memoryUsed = 0;
}

void* customAllocate(size_t size) {
    if (memoryUsed + size > MEMORY_POOL_SIZE) {
        return NULL; // Out of memory
    }
    void* block = memoryPool + memoryUsed;
    memoryUsed += size;
    return block;
}

void freeMemory() {
    free(memoryPool);
    memoryPool = NULL;
    memoryUsed = 0;
}

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
    if (newSize == 0) {
        free(pointer);
        return NULL;
    }
    void* result = realloc(pointer, newSize);
    if (result == NULL) exit(1);
    return result;
}



