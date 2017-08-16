//----------------------------------------------------------------------------------------------------------------------
// ZX-Spectrum emulator API
//----------------------------------------------------------------------------------------------------------------------

#pragma once

//----------------------------------------------------------------------------------------------------------------------
// INDEX
//  Search for {xxx} where xxx is the index name listed below.
//
//  ARRAY       Array implementation
//  DATA        Data loading
//  EVENT       Event management
//  MEMORY      Memory operations
//  RAM         ROM & RAM hardware
//  STATE       Data structures describing emulator state
//  VIDEO       Video hardware
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

//---------------------------------------------------------------------------------------------------------------{STATE}
//----------------------------------------------------------------------------------------------------------------------
// E M U L A T O R   S T A T E
//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------

typedef enum
{
    M_ZX48,
}
Model;

typedef struct
{
    Model           model;
    u32             tStates;
    u8*             memory;
    u8              border;
}
Machine;

typedef struct
{
    int _;
}
ZXInput;

typedef struct
{
    u32*            screen;     // 352 x 304
}
ZXOutput;

//-----------------------------------------------------------------------------------------------------------------{RAM}
//----------------------------------------------------------------------------------------------------------------------
// M E M O R Y   H A R D W A R E
//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------

u8 memoryGet8(Machine* M, u16 address)
{
    return M->memory[address];
}

u16 memoryGet16(Machine* M, u16 address)
{
    return ((u16)(M->memory[address]) << 8) + M->memory[(address + 1)];
}

void memorySet8(Machine* M, u16 address, u8 data)
{
    if (address >= 0x4000)
    {
        M->memory[address] = data;
    }
}

void memorySet16(Machine* M, u16 address, u16 data)
{
    memorySet8(M, address, data & 0xff);
    memorySet8(M, address + 1, data >> 8);
}

void memoryLoad(Machine* M, const u8* data, i64 len, u16 address)
{
    memoryCopy(data, &M->memory[address], len);
}

//---------------------------------------------------------------------------------------------------------------{VIDEO}
//----------------------------------------------------------------------------------------------------------------------
// V I D E O   H A R D W A R E
//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------

void videoRender(Machine* M, ZXOutput* out)
{
    static const u32 colours[16] =
    {
        0x000000, 0x0000d7, 0xd70000, 0xd700d7, 0x00d700, 0x00d7d7, 0xd7d700, 0xd7d7d7,
        0x000000, 0x0000ff, 0xff0000, 0xff00ff, 0x00ff00, 0x00ffff, 0xffff00, 0xffffff,
    };

    u32* img = out->screen;

    for (int r = -56; r < (192 + 56); ++r)
    {
        if (r < 0 || r >= 192)
        {
            for (int c = -48; c < (256 + 48); ++c)
            {
                *img++ = colours[M->border];
            }
        }
        else
        {
            for (int c = -48; c < (256 + 48); ++c)
            {
                if (c < 0 || c >= 256)
                {
                    *img++ = colours[M->border];
                }
                else
                {
                    // Video data.
                    //  Pixels address is 010S SRRR CCCX XXXX
                    //  Attrs address is 0101 10YY YYYX XXXX
                    //  S = Section (0-2)
                    //  C = Cell row within section (0-7)
                    //  R = Pixel row within cell (0-7)
                    //  X = X coord (0-31)
                    //  Y = Y coord (0-23)
                    //
                    //  ROW = SSCC CRRR
                    //      = YYYY Y000
                    u16 p = 0x4000 + ((r & 0x0c0) << 5) + ((r & 0x7) << 8) + ((r & 0x38) << 2);
                    u16 a = 0x5800 + ((r & 0xf8) << 2);

                    u8 data = memoryGet8(M, p);
                    u8 attr = memoryGet8(M, a);
                    u32 ink = colours[(attr & 7) + ((attr & 0x40) >> 3)];
                    u32 paper = colours[(attr & 0x7f) >> 3];

                    for (int i = 7; i >= 0; --i)
                    {
                        img[i] = (data & 1) ? ink : paper;
                        data >>= 1;
                    }
                    img += 8;
                    c += 7;  // for loop with increment it once more
                }
            }
        }
    }
}


//---------------------------------------------------------------------------------------------------------------{EVENT}
//----------------------------------------------------------------------------------------------------------------------
// E V E N T   M A N A G E R
//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------

int eventUpdate(Machine* M)
{
    return 0;
}

//-----------------------------------------------------------------------------------------------------------------{Z80}
//----------------------------------------------------------------------------------------------------------------------
// Z 8 0   E M U L A T I O N
//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------



//----------------------------------------------------------------------------------------------------------------------
// Spectrum Emulator interface
//----------------------------------------------------------------------------------------------------------------------

int spek(Machine* M, ZXInput* in, ZXOutput* out)
{
    videoRender(M, out);
    return YES;
}

void initSpek(Machine* M, Model model)
{
    switch (model)
    {
    case M_ZX48:
        M->memory = K_ALLOC(KB(64));
        break;

    default:
        // Invalid model
        K_ASSERT(0);
        abort();
    }

    M->tStates = 0;
    M->border = 7;
    M->model = model;
}

//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------
