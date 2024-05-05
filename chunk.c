#include <stdlib.h>
#include "chunk.h"
#include "memory.h"
#include "stdio.h"
#include <string.h>

void initChunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;
    chunk->lines = NULL;
    initValueArray(&chunk->constants);
}

void writeChunk(Chunk* chunk, uint8_t byte, int line) {
    if (chunk->capacity < chunk->count + 1) {
        int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
        
        LineStart* newLines = GROW_ARRAY(LineStart, NULL, oldCapacity, chunk->capacity);
        // Initialize new part of the array
        for (int i = oldCapacity; i < chunk->capacity; i++) {
            newLines[i].line = -1;  // Default invalid line number
            newLines[i].times = 0;  // No repetitions
        }
        memcpy(newLines, chunk->lines, sizeof(LineStart) * oldCapacity);
        FREE_ARRAY(LineStart, chunk->lines, oldCapacity);
        chunk->lines = newLines;
    }
    chunk->code[chunk->count] = byte;

    // Check if this is a new line or an increment of the previous
    if (chunk->count == 0 || chunk->lines[chunk->count - 1].line != line) {
        chunk->lines[chunk->count].line = line;
        chunk->lines[chunk->count].times = 1;
    } else {
        chunk->lines[chunk->count - 1].times++;
    }

    chunk->count++;
}


int getLine(Chunk* chunk, int instruction) {
    int cumulativeCount = 0;
    for (int i = 0; i < chunk->count; i++) {
        cumulativeCount += chunk->lines[i].times;
        if (instruction < cumulativeCount) {
            return chunk->lines[i].line;
        }
    }
    return -1;  // Default error case, which should be handled gracefully
}

void freeChunk(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    FREE_ARRAY(LineStart, chunk->lines, chunk->capacity);
    freeValueArray(&chunk->constants);
    initChunk(chunk);
}

int addConstant(Chunk* chunk, Value value) {
    writeValueArray(&chunk->constants, value);
    if (chunk->constants.count - 1 >= (1 << 24)) {
        // Handle constant overflow, perhaps by splitting into a new chunk or raising an error
        fprintf(stderr, "Too many constants in chunk.\n");
        exit(EXIT_FAILURE);
    }
    return chunk->constants.count - 1;
}

void writeConstant(Chunk* chunk, Value value, int line) {
    int index = addConstant(chunk, value);
    if (index < 256) {
        writeChunk(chunk, OP_CONSTANT, line);
        writeChunk(chunk, (uint8_t)index, line);
    } else {
        writeChunk(chunk, OP_CONSTANT_LONG, line);
        writeChunk(chunk, (index & 0xFF0000) >> 16, line);
        writeChunk(chunk, (index & 0x00FF00) >> 8, line);
        writeChunk(chunk, (index & 0x0000FF), line);
    }
}
