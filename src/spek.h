//----------------------------------------------------------------------------------------------------------------------
// ZX-Spectrum emulator API
//----------------------------------------------------------------------------------------------------------------------

#pragma once

//----------------------------------------------------------------------------------------------------------------------
// INDEX
//  Search for {xxx} where xxx is the index name listed below.
//
//  ARRAY       Array implementation
//  EVENT       Event management
//  MEMORY      Memory operations
//  Z80         Z80 Emulation
//
//----------------------------------------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------------------------------------
// Basic macros
//----------------------------------------------------------------------------------------------------------------------

#define YES (1)
#define NO (0)

#define AS_BOOL(b) ((b) ? YES : NO)

#define KB(x) (1024 * (x))
#define MB(x) (1024 * KB(x))
#define GB(x) (1024 * MB(x))

#define internal static

//--------------------------------------------------------------------------------------------------------------{MEMORY}
//----------------------------------------------------------------------------------------------------------------------
// M E M O R Y   M A N A G E M E N T
//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------------------------------------
// Basic allocation
//----------------------------------------------------------------------------------------------------------------------

internal void* memoryOp(void* oldAddress, i64 oldNumBytes, i64 newNumBytes, const char* file, int line)
{
    void* p = 0;

    if (newNumBytes)
    {
        p = realloc(oldAddress, newNumBytes);
    }
    else
    {
        free(oldAddress);
    }

    return p;
}

void* memoryAlloc(i64 numBytes, const char* file, int line)
{
    return memoryOp(0, 0, numBytes, file, line);
}

void* memoryRealloc(void* address, i64 oldNumBytes, i64 newNumBytes, const char* file, int line)
{
    return memoryOp(address, oldNumBytes, newNumBytes, file, line);
}

void memoryFree(void* address, i64 numBytes, const char* file, int line)
{
    memoryOp(address, numBytes, 0, file, line);
}

void memoryCopy(const void* src, void* dst, i64 numBytes)
{
    memcpy(dst, src, (size_t)numBytes);
}

int memoryCompare(const void* mem1, const void* mem2, i64 numBytes)
{
    return memcmp(mem1, mem2, (size_t)numBytes);
}

void memoryClear(void* mem, i64 numBytes)
{
    memset(mem, 0, (size_t)numBytes);
}

#define K_ALLOC(numBytes) memoryAlloc((numBytes), __FILE__, __LINE__)
#define K_REALLOC(address, oldNumBytes, newNumBytes) memoryRealloc((address), (oldNumBytes), (newNumBytes), __FILE__, __LINE__)
#define K_FREE(address, oldNumBytes) memoryFree((address), (oldNumBytes), __FILE__, __LINE__)

//---------------------------------------------------------------------------------------------------------------{ARRAY}
//----------------------------------------------------------------------------------------------------------------------
// A R R A Y S
//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------------------------------------
// Arrays
//----------------------------------------------------------------------------------------------------------------------

#define Array(T) T*

#define K_ARRAY_COUNT(a) (sizeof(a) / sizeof((a)[0]))

// Destroy an array
#define arrayRelease(a) ((a) ? K_FREE((u8 *)a - (sizeof(i64) * 2), (sizeof(*a) * __arrayCapacity(a)) + (sizeof(i64) * 2)), 0 : 0)

// Add an element to the end of an array
#define arrayAdd(a, v) (__arrayMayGrow(a, 1), (a)[__arrayCount(a)++] = (v))

// Return the number of elements in an array
#define arrayCount(a) ((a) ? __arrayCount(a) : 0)

// Add n uninitialised elements to the array
#define arrayExpand(a, n) (__arrayMayGrow(a, n), __arrayCount(a) += (n), &(a)[__arrayCount(a) - (n)])

// Reserve capacity for n extra items to the array
#define arrayReserve(a, n) (__arrayMayGrow(a, n))

// Clear the array
#define arrayClear(a) (arrayCount(a) = 0)

//
// Internal routines
//

#define __arrayRaw(a) ((i64 *)(a) - 2)
#define __arrayCount(a) __arrayRaw(a)[1]
#define __arrayCapacity(a) __arrayRaw(a)[0]

#define __arrayNeedsToGrow(a, n) ((a) == 0 || __arrayCount(a) + (n) >= __arrayCapacity(a))
#define __arrayMayGrow(a, n) (__arrayNeedsToGrow(a, (n)) ? __arrayGrow(a, n) : 0)
#define __arrayGrow(a, n) ((a) = __arrayInternalGrow((a), (n), sizeof(*(a))))

internal void* __arrayInternalGrow(void* a, i64 increment, i64 elemSize)
{
    i64 doubleCurrent = a ? 2 * __arrayCapacity(a) : 0;
    i64 minNeeded = arrayCount(a) + increment;
    i64 capacity = doubleCurrent > minNeeded ? doubleCurrent : minNeeded;
    i64 oldBytes = a ? elemSize * arrayCount(a) + sizeof(i64) * 2 : 0;
    i64 bytes = elemSize * capacity + sizeof(i64) * 2;
    i64* p = (i64 *)K_REALLOC(a ? __arrayRaw(a) : 0, oldBytes, bytes);
    if (p)
    {
        if (!a) p[1] = 0;
        p[0] = capacity;
        return p + 2;
    }
    else
    {
        return 0;
    }
}

//---------------------------------------------------------------------------------------------------------------{EVENT}
//----------------------------------------------------------------------------------------------------------------------
// E V E N T   M A N A G E R
//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------

//-----------------------------------------------------------------------------------------------------------------{Z80}
//----------------------------------------------------------------------------------------------------------------------
// Z 8 0   E M U L A T I O N
//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------


//----------------------------------------------------------------------------------------------------------------------
// Spectrum Emulator interface
//----------------------------------------------------------------------------------------------------------------------

typedef struct
{
    int _;
}
ZXInput;

typedef struct
{
    int _;
}
ZXOutput;

int spek(ZXInput* in, ZXOutput* out)
{
    return 0;
}

//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------
