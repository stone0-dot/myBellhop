#pragma once
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef DEBUG
#define INIT_VECTOR_TYPE(type)                                                 \
    struct Vector##type {                                                      \
        type* arr;                                                             \
        unsigned int size;                                                     \
        unsigned int capacity;                                                 \
    };                                                                         \
    static struct Vector##type vector##type##CreatebyCapacity(                 \
        const unsigned int capacity) {                                         \
        struct Vector##type vector = {NULL, 0, 0};                             \
        vector.arr = (type*)malloc(sizeof(type) * capacity);                   \
        memset(vector.arr, 0, sizeof(type) * capacity);                        \
        vector.capacity = capacity;                                            \
        return vector;                                                         \
    }                                                                          \
    static struct Vector##type vector##type##CreatebyPtr(                      \
        const type* type##Ptr, unsigned int len) {                             \
        struct Vector##type vector = {NULL, 0, 0};                             \
        vector.arr = (type*)malloc(sizeof(type) * len);                        \
        vector.capacity = len;                                                 \
        vector.size = len;                                                     \
        memcpy(vector.arr, type##Ptr, sizeof(type) * len);                     \
        return vector;                                                         \
    }                                                                          \
    static struct Vector##type vector##type##CreatebyCopy(                     \
        const struct Vector##type* vectorCopyPtr) {                            \
        struct Vector##type vector = {NULL, 0, 0};                             \
        vector.size = vectorCopyPtr->size;                                     \
        vector.capacity = vectorCopyPtr->capacity;                             \
        vector.arr = (type*)malloc(sizeof(type) * vectorCopyPtr->capacity);    \
        memcpy(vector.arr, vectorCopyPtr->arr,                                 \
               sizeof(type) * vectorCopyPtr->size);                            \
        return vector;                                                         \
    }                                                                          \
    static void vector##type##Destory(struct Vector##type* vectorPtr) {        \
        free(vectorPtr->arr);                                                  \
    }                                                                          \
    static void vector##type##ExpandCapacity(struct Vector##type* vectorPtr) { \
        unsigned int newCapacity = 1;                                          \
        while (newCapacity <= vectorPtr->capacity) {                           \
            newCapacity *= 2;                                                  \
        }                                                                      \
        struct Vector##type newVector =                                        \
            vector##type##CreatebyCapacity(newCapacity);                       \
        newVector.size = vectorPtr->size;                                      \
        memcpy(newVector.arr, vectorPtr->arr, sizeof(type) * vectorPtr->size); \
        *vectorPtr = newVector;                                                \
    }                                                                          \
    static int vector##type##Size(const struct Vector##type* vectorPtr) {      \
        return vectorPtr->size;                                                \
    }                                                                          \
    static type vector##type##Index(const struct Vector##type* vectorPtr,      \
                                    unsigned int idx) {                        \
        if (idx >= vectorPtr->size || idx < 0) {                               \
            printf(                                                            \
                "\n-------------------------------------\nVector%s "           \
                "IndexError:idx out of "                                       \
                "range\n-------------------------------------",                \
                #type);                                                        \
            exit(-1);                                                          \
        }                                                                      \
        return vectorPtr->arr[idx];                                            \
    }                                                                          \
    static void vector##type##Pushback(struct Vector##type* vectorPtr,         \
                                       type type##ele) {                       \
        if (vectorPtr->size < vectorPtr->capacity) {                           \
            vectorPtr->arr[vectorPtr->size++] = type##ele;                     \
        } else {                                                               \
            vector##type##ExpandCapacity(vectorPtr);                           \
            vectorPtr->arr[vectorPtr->size++] = type##ele;                     \
        }                                                                      \
    }                                                                          \
    static void vector##type##Copy(struct Vector##type* vectorPtr,             \
                                   const struct Vector##type* vectorCopyPtr) { \
        struct Vector##type temp;                                              \
        struct Vector##type newVector =                                        \
            vector##type##CreatebyCopy(vectorCopyPtr);                         \
        temp = *vectorPtr;                                                     \
        *vectorPtr = newVector;                                                \
        vector##type##Destory(&temp);                                          \
    }
#else
#define INIT_VECTOR_TYPE(type)                                                 \
    struct Vector##type {                                                      \
        type* arr;                                                             \
        unsigned int size;                                                     \
        unsigned int capacity;                                                 \
    };                                                                         \
    static struct Vector##type vector##type##CreatebyCapacity(                 \
        const unsigned int capacity) {                                         \
        struct Vector##type vector = {NULL, 0, 0};                             \
        vector.arr = (type*)malloc(sizeof(type) * capacity);                   \
        memset(vector.arr, 0, sizeof(type) * capacity);                        \
        vector.capacity = capacity;                                            \
        return vector;                                                         \
    }                                                                          \
    static struct Vector##type vector##type##CreatebyPtr(                      \
        const type* type##Ptr, const unsigned int len) {                       \
        struct Vector##type vector = {NULL, 0, 0};                             \
        vector.arr = (type*)malloc(sizeof(type) * len);                        \
        vector.capacity = len;                                                 \
        vector.size = len;                                                     \
        memcpy(vector.arr, type##Ptr, sizeof(type) * len);                     \
        return vector;                                                         \
    }                                                                          \
    static struct Vector##type vector##type##CreatebyCopy(                     \
        const struct Vector##type* vectorCopyPtr) {                            \
        struct Vector##type vector = {NULL, 0, 0};                             \
        vector.size = vectorCopyPtr->size;                                     \
        vector.capacity = vectorCopyPtr->capacity;                             \
        vector.arr = (type*)malloc(sizeof(type) * vectorCopyPtr->capacity);    \
        memcpy(vector.arr, vectorCopyPtr->arr,                                 \
               sizeof(type) * vectorCopyPtr->size);                            \
        return vector;                                                         \
    }                                                                          \
    static void vector##type##Destory(struct Vector##type* vectorPtr) {        \
        free(vectorPtr->arr);                                                  \
    }                                                                          \
    static void vector##type##ExpandCapacity(struct Vector##type* vectorPtr) { \
        unsigned int newCapacity = 1;                                          \
        while (newCapacity <= vectorPtr->capacity) {                           \
            newCapacity *= 2;                                                  \
        }                                                                      \
        struct Vector##type newVector =                                        \
            vector##type##CreatebyCapacity(newCapacity);                       \
        newVector.size = vectorPtr->size;                                      \
        memcpy(newVector.arr, vectorPtr->arr, sizeof(type) * vectorPtr->size); \
        *vectorPtr = newVector;                                                \
    }                                                                          \
    static int vector##type##Size(const struct Vector##type* vectorPtr) {      \
        return vectorPtr->size;                                                \
    }                                                                          \
    static type vector##type##Index(const struct Vector##type* vectorPtr,      \
                                    unsigned int idx) {                        \
        return vectorPtr->arr[idx];                                            \
    }                                                                          \
    static void vector##type##Pushback(struct Vector##type* vectorPtr,         \
                                       type type##ele) {                       \
        if (vectorPtr->size < vectorPtr->capacity) {                           \
            vectorPtr->arr[vectorPtr->size++] = type##ele;                     \
        } else {                                                               \
            vector##type##ExpandCapacity(vectorPtr);                           \
            vectorPtr->arr[vectorPtr->size++] = type##ele;                     \
        }                                                                      \
    }                                                                          \
    static void vector##type##Copy(struct Vector##type* vectorPtr,             \
                                   const struct Vector##type* vectorCopyPtr) { \
        struct Vector##type temp;                                              \
        struct Vector##type newVector =                                        \
            vector##type##CreatebyCopy(vectorCopyPtr);                         \
        temp = *vectorPtr;                                                     \
        *vectorPtr = newVector;                                                \
        vector##type##Destory(&temp);                                          \
    }
#endif