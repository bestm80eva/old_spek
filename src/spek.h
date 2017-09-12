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

typedef void* (*DataLoadFunc)(const char* fileName, u8** bytes, i64* len);
typedef void(*DataUnloadFunc)(void* data);

typedef struct
{
    DataLoadFunc    dataLoad;
    DataUnloadFunc  dataUnload;
}
Platform;

typedef union
{
    struct {
        u8  l;
        u8  h;
    };
    u16 w;
}
RegPair;

typedef struct
{
    RegPair     af;
    RegPair     bc;
    RegPair     de;
    RegPair     hl;
    RegPair     af_;
    RegPair     bc_;
    RegPair     de_;
    RegPair     hl_;
    RegPair     ix;
    RegPair     iy;

    u8          i;
    u8          r;

    RegPair     sp;
    RegPair     pc;
    RegPair     p;

    u8          iff1;
    u8          iff2;
    u8          im;

    bool        halted;
    u32         interruptsAt;       // Tstates to allow interrupts
}
Z80;

typedef struct
{
    Platform    platform;
    Model       model;
    Z80         z80;
    u32         tStates;
    u32         nextEvent;
    u8*         memory;
    u8*         memoryEnd;
    u8          border;
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

u32 memoryContend(Machine* M, u16 address, u32 time)
{
    // #todo: Contention
    return time;
}

u32 memoryContendNoMReq(Machine* M, u16 address, u32 time)
{
    // #todo: Contention
    return time;
}

u8 memoryGet8(Machine* M, u16 address)
{
    M->tStates += 3;
    // #todo: Deal with contention
    return M->memory[address];
}

u16 memoryGet16(Machine* M, u16 address)
{
    return ((u16)memoryGet8(M, address) << 8) + memoryGet8(M, address + 1);
}

void memorySet8(Machine* M, u16 address, u8 data)
{
    if (address >= 0x4000)
    {
        M->memory[address] = data;
    }
    M->tStates += 3;
    // #todo: Deal with contention
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

//----------------------------------------------------------------------------------------------------------------{PORT}
//----------------------------------------------------------------------------------------------------------------------
// I O   P O R T S
//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------

u8 portRead(Machine* M, u16 port)
{
    return 0;
}

void portWrite(Machine* M, u16 port)
{

}

//----------------------------------------------------------------------------------------------------------------{DATA}
//----------------------------------------------------------------------------------------------------------------------
// D A T A   L O A D I N G
//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------

void dataLoad(Platform* P, Machine* M, u16 address, const char* fileName)
{
    u8* buffer;
    i64 len;
    void* h = P->dataLoad(fileName, &buffer, &len);
    if (M->memory + address + len <= M->memoryEnd)
    {
        memoryLoad(M, buffer, len, address);
    }
    else
    {
        // #todo: Add error message
    }
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

            for (int c = -48; c < (256 + 48); ++c)
            {
                if (c < 0 || c >= 256)
                {
                    *img++ = colours[M->border];
                }
                else
                {
                    u8 data = memoryGet8(M, p++);
                    u8 attr = memoryGet8(M, a++);
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

#define CONTEND_READ(address, time) \
    M->tStates += memoryContend(M, address, time)

#define CONTEND_READ_NO_MREQ(address, time) \
    M->tStates += memoryContendNoMReq(M, address, time)

#define CONTEND_WRITE_NO_MREQ(address, time) \
    M->tStates += memoryContendNoMReq(M, address, time)

#define A       M->z80.af.h
#define F       M->z80.af.l
#define AF      M->z80.af.w

#define B       M->z80.bc.h
#define C       M->z80.bc.l
#define BC      M->z80.bc.w

#define D       M->z80.de.h
#define E       M->z80.de.l
#define DE      M->z80.de.w

#define H       M->z80.hl.h
#define L       M->z80.hl.l
#define HL      M->z80.hl.w

#define A_      M->z80.af_.h
#define F_      M->z80.af_.l
#define AF_     M->z80.af_.w

#define B_      M->z80.bc_.h
#define C_      M->z80.bc_.l
#define BC_     M->z80.bc_.w

#define D_      M->z80.de_.h
#define E_      M->z80.de_.l
#define DE_     M->z80.de_.w

#define H_      M->z80.hl_.h
#define L_      M->z80.hl_.l
#define HL_     M->z80.hl_.w

#define IXH     M->z80.ix.h
#define IXL     M->z80.ix.l
#define IX      M->z80.ix.w

#define IYH     M->z80.iy.h
#define IYL     M->z80.iy.l
#define IY      M->z80.iy.w

#define SPH     M->z80.sp.h
#define SPL     M->z80.sp.l
#define SP      M->z80.sp.w

#define PCH     M->z80.pc.h
#define PCL     M->z80.pc.l
#define PC      M->z80.pc.w

#define I       M->z80.i
#define R       M->z80.r
#define IFF1    M->z80.iff1
#define IFF2    M->z80.iff2
#define IM      M->z80.im

#define IR      ((M->z80.i) << 8 | (M->z80.r & 0x7f))

#define FLAG_C      0x01
#define FLAG_N      0x02
#define FLAG_P      0x04
#define FLAG_V      FLAG_P
#define FLAG_3      0x08
#define FLAG_H      0x10
#define FLAG_5      0x20
#define FLAG_Z      0x40
#define FLAG_S      0x80

u8 gFlagsSZ53[256];
u8 gFlagsSZ53P[256];
u8 gParity[256];

u8 kHalfCarryAdd[] = { 0, FLAG_H, FLAG_H, FLAG_H, 0, 0, 0, FLAG_H };
u8 kHalfCarrySub[] = { 0, 0, FLAG_H, 0, FLAG_H, 0, FLAG_H, FLAG_H };
u8 kOverflowAdd[] = { 0, 0, 0, FLAG_V, FLAG_V, 0, 0, 0 };
u8 kOverflowSub[] = { 0, FLAG_V, 0, 0, 0, 0, FLAG_V, 0 };

#define AND(value) \
{ \
    A &= (value); \
    F = FLAG_H | gFlagsSZ53P[A]; \
}

#define ADC(value) \
{ \
    u16 t = A + (value) + (F & FLAG_C); \
    u8 x = ((A & 0x88) >> 3) | (((value) & 0x88) >> 2) | ((t & 0x88) >> 1); \
    A = t; \
    F = (t & 0x100 ? FLAG_C : 0) | \
        kHalfCarryAdd[x & 0x07] | kOverflowAdd[x >> 4] | \
        gFlagsSZ53[A]; \
}

#define ADC16(value) \
{ \
    u16 t = HL + (value) + (F & FLAG_C); \
    u8 x = ((HL & 0x8800) >> 11) | (((value) & 0x8800) >> 10) | ((t & 0x8800) >> 9); \
    z80.p.w = HL+1; \
    HL = t; \
    F = (t & 0x10000 ? FLAG_C : 0) | \
        kOverflowAdd[x >> 4] | \
        (H & (FLAG_3 | FLAG_5 | FLAG_S)) | \
        kHalfCarryAdd[x & 7] | \
        (HL ? 0 : FLAG_Z); \
}

#define ADD(value) \
{ \
    u16 t = A + (value); \
    u8 x = ((A & 0x88) >> 3) | (((value) & 0x88) >> 2) | ((t & 0x88) >> 1); \
    A = (u8)t; \
    F = (t & 0x100 ? FLAG_C : 0) | \
        kHalfCarryAdd[x & 0x07] | kOverflowAdd[x >> 4] | \
        gFlagsSZ53[A]; \
}

#define ADD16(value1, value2) \
{ \
    u16 t = (value1) + (value2); \
    u8 x = (((value1) & 0x8800) >> 11) | (((value2) & 0x8800) >> 10) | ((t & 0x8800) >> 9); \
    M->z80.p.w = (value1)+1; \
    (value1) = t; \
    F = (F & (FLAG_V | FLAG_Z | FLAG_S)) | \
        (t & 0x10000 ? FLAG_C : 0) | \
        ((t >> 8) & (FLAG_3 | FLAG_5)) | \
        kHalfCarryAdd[x]; \
}

#define BIT(bit, value) \
{ \
    F = (F & FLAG_C) | FLAG_H | ((value) & (FLAG_3 | FLAG_5)); \
    if (!((value) & (0x01 << (bit)))) F |= FLAG_P | FLAG_Z; \
    if (bit) == 7 && (value) & 0x80) F |= FLAG_S; \
}

#define BIT_MEMPTR(bit, value) \
{ \
    F = (F & FLAG_C) | FLAG_H | (z80.p.h & (FLAG_3 | FLAG_5)); \
    if (!((value) & (0x01 << (bit)))) F |= FLAG_P | FLAG_Z; \
    if (bit) == 7 && (value) & 0x80) F |= FLAG_S; \
}

#define CALL() \
{ \
    CONTEND_READ_NO_MREQ(PC, 1); \
    ++PC; \
    PUSH16(PCL, PCH); \
    PC = z80.p.w; \
}

#define CP(value) \
{ \
    u16 t = A - (value); \
    u8 x = ((A & 0x88) >> 3) | (((value & 0x88) >> 2) | ((t & 0x88) >> 1); \
    F = (t & 0x100 ? FLAG_C : (t ? 0 : FLAG_Z)) | FLAG_N | \
        kHalfCarrySub[x & 7] | \
        kOverflowSub[x >> 4] | \
        ((value) & (FLAG_3 | FLAG_5)) | \
        (t & FLAG_S); \
}

#define DDFDCB_ROTATESHIFT(time, target, instruction) \
M->tStates += (time); \
{ \
    (target) = memoryGet8(tempAddr); \
    instruction((target)); \
    memorySet8(tempAddr, (target)); \
} \
break;

#define DEC(value) \
{ \
    F = (F & FLAG_C) | ((value) & 0x0f ? 0 : FLAG_H) | FLAG_N; \
    --(value); \
    F |= ((value) == 0x7f ? FLAG_V : 0) | gFlagsSZ53[(value)]; \
}

#define Z80_IN(reg, port) \
{ \
    z80.p.w = (port) + 1; \
    (reg) = portRead((port)); \
    F = (F & FLAG_C) | gFlagsSZ53P[(reg)]; \
}

#define INC(value) \
{ \
    ++(value); \
    F = (F & FLAG_C) | ((value) & 0x80 ? FLAG_V : 0) | \
        ((value) & 0x0f ? 0 : FLAG_H) | gFlagsSZ53[(value)]; \
}

#define LD16_NNRR(r) \
{ \
    u16 t = memoryGet16(M, PC); \
    PC += 2; \
    memorySet16(M, t, r); \
    M->z80.p.w = t; \
    break; \
}

#define LD16_RRNN(r) \
{ \
    u16 t = memoryGet16(M, PC); \
    (r) = memoryGet16(M, t); \
    break; \
}

#define JP() \
{ \
    PC = z80.p.w; \
}

#define JR() \
{ \
    i8 t = (i8)memoryGet8(M, PC); \
    CONTEND_READ_NO_MREQ(PC, 1); CONTEND_READ_NO_MREQ(PC, 1); \
    CONTEND_READ_NO_MREQ(PC, 1); CONTEND_READ_NO_MREQ(PC, 1); \
    CONTEND_READ_NO_MREQ(PC, 1); \
    PC += t; \
    ++PC; \
    M->z80.p.w = PC; \
}

#define OR(value) \
{ \
    A |= (value); \
    F = gFlagsSZ53P[A]; \
}

#define POP16(r) \
{ \
    (r) = memoryGet16(M, SP); \
    SP += 2; \
}

#define PUSH16(r) \
{ \
    SP -= 2; \
    memorySet16(M, SP, (r)); \
}

#define RET() \
{ \
    POP16(PC); \
    z80.p.w = PC; \
}

#define RL(value) \
{ \
    u8 t = (value); \
    (value) = ((value) << 1) | (F & FLAG_C); \
    F = (t >> 7) | gFlagsSZ53P[(value)]; \
}

#define RLC(value) \
{ \
    (value) = ((value) << 1 | ((value) >> 7); \
    F = ((value) & FLAG_C) | gFlagsSZ53P[(value)]; \
}

#define RR(value) \
{ \
    u8 t = (value); \
    (value) = ((value) >> 1) | (F << 7); \
    F = (t & FLAG_C ) | gFlagsSZ53P[(value)]; \
}

#define RRC(value) \
{ \
    F = (value) & FLAG_C; \
    (value) = ((value) >> 1) | ((value) << 7); \
    F |= gFlagsSZ53P[(value)]; \
}

#define RST(value) \
{ \
    PUSH16(PC); \
    PC = (value); \
    z80.p.w = PC; \
}

#define SBC(value) \
{ \
    u16 t = A - (value) - (F & FLAG_C); \
    u8 x = ((A & 0x88) >> 3) | (((value) & 0x88) >> 2) | ((t & 0x88) >> 1);  \
    A = t; \
    F = (t & 0x100 ? FLAG_C : 0) | FLAG_N | \
        kHalfCarrySub[x & 0x07] | kOverflowSub[x >> 4] | \
        gFlagsSZ53[A]; \
}

#define SBC16(value) \
{ \
    u16 t = HL - (value) - (F & FLAG_C); \
    u8 x= ((HL & 0x8800) >> 11) | (((value) & 0x8800) >> 10) | ((sub16temp & 0x8800) >> 9); \
    z80.p.w = HL + 1; \
    HL = t; \
    F = (t & 0x10000 ? FLAG_C : 0) | \
        FLAG_N | kOverflowSub[x >> 4] | \
        (H & (FLAG_3 | FLAG_5 | FLAG_S)) | \
        kHalfCarrySub[x & 0x07] | \
        (HL ? 0 : FLAG_Z); \
}

#define SLA(value) \
{ \
    F = (value) >> 7; \
    (value) <<= 1; \
    F |= gFlagsSZ53P[(value)]; \
}

#define SLL(value) \
{ \
    F = (value) >> 7; \
    (value) = ((value) << 1) | 0x01; \
    F |= gFlagsSZ53P[(value)]; \
}

#define SRA(value) \
{ \
    F = (value) & FLAG_C; \
    (value) = ((value) & 0x80) | ((value) >> 1); \
    F |= gFlagsSZ53P[(value)]; \
}

#define SRL(value) \
{ \
    F = (value) & FLAG_C; \
    (value) >>= 1; \
    F |= gFlagsSZ53P[(value)]; \
}

#define SUB(value) \
{ \
    u16 t = A - (value); \
    u8 x = ((A & 0x88) >> 3) | (((value) & 0x88) >> 2) | ((t & 0x88) >> 1);  \
    A = (u8)t; \
    F = (t & 0x100 ? FLAG_C : 0) | FLAG_N | \
        kHalfCarrySub[x & 0x07] | kOverflowSub[x >> 4] | \
        gFlagsSZ53[A]; \
}

#define XOR(value) \
{ \
  A ^= (value); \
  F = gFlagsSZ53P[A]; \
}


void z80Update(Machine* M)
{
    while (M->tStates < M->nextEvent)
    {
        CONTEND_READ(PC, 4);

        u8 opCode = memoryGet8(M, PC);

        // #todo: check IFF2 read check
        ++PC;
        ++R;

        switch (opCode)
        {
        case 0x00:  //  NOP
            break;
        case 0x01:  //  LD BC,nnnn
            BC = memoryGet16(M, PC);
            PC += 2;
            break;
        case 0x02:  //  LD (BC),A
            M->z80.p.l = BC + 1;
            M->z80.p.h = A;
            memorySet8(M, BC, A);
            break;
        case 0x03:  // INC BC
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            ++BC;
            break;
        case 0x04:  //  INC B
            INC(B);
            break;
        case 0x05:  //  DEC B
            DEC(B);
            break;
        case 0x06:  //  LD B,nn
            B = memoryGet8(M, PC++);
            break;
        case 0x07:  //  RLCA
            A = (A << 1) | (A >> 7);
            F = (F & (FLAG_P | FLAG_Z | FLAG_S)) | (A & (FLAG_C | FLAG_3 | FLAG_5));
            break;
        case 0x08:  //  EX AF, AF'
            {
                u16 t = AF;
                AF = AF_;
                AF_ = t;
            }
            break;
        case 0x09:  //  ADD HL,BC
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            ADD16(HL, BC);
            break;
        case 0x0a:  //  LD A,(BC)
            M->z80.pc.w = BC + 1;
            A = memoryGet8(M, BC);
            break;
        case 0x0b:  //  DEC BC
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            --BC;
            break;
        case 0x0c:  //  INC C
            INC(C);
            break;
        case 0x0d:  //  DEC C
            DEC(C)
            break;
        case 0x0e:  //  LD C,nn
            C = memoryGet8(M, PC++);
            break;
        case 0x0f:  //  RRCA
            F = (F & (FLAG_P | FLAG_Z | FLAG_S)) | (A & FLAG_C);
            A = (A >> 1) | (A << 7);
            F |= (A & (FLAG_3 | FLAG_5));
            break;
        case 0x10:  //  DJNZ offset
            CONTEND_READ_NO_MREQ(IR, 1);
            --B;
            if (B) {
                JR();
            }
            else
            {
                CONTEND_READ(PC, 3);
                ++PC;
            }
            break;
        case 0x11:  //  LD DE,nnnn
            DE = memoryGet16(M, PC);
            PC += 2;
            break;
        case 0x12:  //  LD (DE),A
            M->z80.p.l = DE + 1;
            M->z80.p.h = A;
            memorySet8(M, DE, A);
            break;
        case 0x13:  //  INC DE
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            ++DE;
            break;
        case 0x14:  //  INC D
            INC(D);
            break;
        case 0x15:  //  DEC D
            DEC(D);
            break;
        case 0x16:  //  LD D,nn
            D = memoryGet8(M, PC++);
            break;
        case 0x17:  //  RLA
            {
                u8 t = A;
                A = (A << 1) | (F & FLAG_C);
                F = (F & (FLAG_P | FLAG_Z | FLAG_S)) |
                    (A & (FLAG_3 | FLAG_5)) | (t >> 7);
            }
            break;
        case 0x18:  //  JR offset
            JR();
            break;
        case 0x19:  //  ADD HL, DE
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            ADD16(HL, DE);
            break;
        case 0x1a:  //  LD A,(DE)
            M->z80.p.w = DE + 1;
            A = memoryGet8(M, DE);
            break;
        case 0x1b:  //  DEC DE
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            --DE;
            break;
        case 0x1c:  //  INC E
            INC(E);
            break;
        case 0x1d:  //  DEC E
            DEC(E);
            break;
        case 0x1e:  //  LD E,nn
            E = memoryGet8(M, PC++);
            break;
        case 0x1f:  //  RRA
            {
                u8 t = A;
                A = (A >> 1) | (F << 7);
                F = (F & (FLAG_P | FLAG_Z | FLAG_S)) |
                    (A & (FLAG_3 | FLAG_5)) | (t & FLAG_C);
            }
            break;
        case 0x20:  //  JR NZ,offset
            if (!(F & FLAG_Z)) {
                JR();
            }
            else
            {
                CONTEND_READ(PC, 3);
                ++PC;
            }
            break;
        case 0x21:  //  LD HL,nnnn
            HL = memoryGet16(M, PC);
            PC += 2;
            break;
        case 0x22:  //  LD (nnnn),HL
            LD16_NNRR(HL);
            break;
        case 0x23:  //  INC HL
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            ++HL;
            break;
        case 0x24:  //  INC H
            INC(H);
            break;
        case 0x25:  //  DEC H
            DEC(H)
            break;
        case 0x26:  //  LD H,nn
            H = memoryGet8(M, PC++);
            break;
        case 0x27:  //  DAA
            {
                u8 add = 0;
                u8 carry = (F & FLAG_C);
                if ((F & FLAG_H) || ((A & 0x0f) > 9)) add = 6;
                if (carry || (A > 0x99)) add |= 0x60;
                if (A > 0x99) carry = FLAG_C;
                if (F & FLAG_N) {
                    SUB(add);
                }
                else
                {
                    ADD(add);
                }
                F = (F & ~(FLAG_C | FLAG_P)) | carry | gParity[A];
            }
            break;
        case 0x28:  //  JR Z,offset
            if (F & FLAG_Z)
            {
                JR();
            }
            else
            {
                CONTEND_READ(PC, 3);
                ++PC;
            }
            break;
        case 0x29:  //  ADD HL,HL
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            ADD16(HL, HL);
            break;
        case 0x2a:  //  LD HL,(nnnn)
            LD16_RRNN(HL)
            break;
        case 0x2b:  //  DEC HL
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            --HL;
            break;
        case 0x2c:  //  INC L
            INC(L);
            break;
        case 0x2d:  //  DEC L
            DEC(L);
            break;
        case 0x2e:  //  LD L,nn
            L = memoryGet8(M, PC++);
            break;
        case 0x2f:  //  CPL
            A ^= 0xff;
            F = (F & (FLAG_C | FLAG_P | FLAG_Z | FLAG_S)) |
                (A & (FLAG_3 | FLAG_5)) | (FLAG_N | FLAG_H);
            break;
        case 0x30:  //  JR NC,offset
            if (!(F & FLAG_C))
            {
                JR();
            }
            else
            {
                CONTEND_READ(PC, 3);
                ++PC;
            }
            break;
        case 0x31:  // LD SP,nnnn
            SP = memoryGet16(M, PC);
            PC += 2;
            break;
        case 0x32:  //  LD (nnnn),A
            {
                u16 t = memoryGet16(M, PC);
                PC += 2;
                M->z80.p.l = (u8)t + 1;
                M->z80.p.h = A;
                memorySet8(M, t, A);
            }
            break;
        case 0x33:  //  INC SP
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            ++SP;
            break;
        case 0x34:  //  INC (HL)
            {
                u8 t = memoryGet8(M, HL);
                CONTEND_READ_NO_MREQ(HL, 1);
                INC(t);
                memorySet8(M, HL, t);
            }
            break;
        case 0x35:  //  DEC (HL)
            {
                u8 t = memoryGet8(M, HL);
                CONTEND_READ_NO_MREQ(HL, 1);
                DEC(t);
                memorySet8(M, HL, t);
            }
            break;
        case 0x36:  //  LD (HL),nn
            memorySet8(M, HL, memoryGet8(M, PC++));
            break;
        case 0x37:  //  SCF
            F = (F & (FLAG_P | FLAG_Z | FLAG_S)) |
                (A & (FLAG_3 | FLAG_5)) |
                FLAG_C;
            break;
        case 0x38:  //  JR C,offset
            if (F & FLAG_C)
            {
                JR();
            }
            else
            {
                CONTEND_READ(PC, 3);
                ++PC;
            }
            break;
        case 0x39:  //  ADD HL,SP
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            ADD16(HL, SP);
            break;
        case 0x3a:  //  LD A,(nnnn)
            M->z80.p.w = memoryGet16(M, PC);
            PC += 2;
            A = memoryGet8(M, M->z80.p.w++);
            break;
        case 0x3b:  //  DEC SP
            CONTEND_READ_NO_MREQ(IR, 1);
            CONTEND_READ_NO_MREQ(IR, 1);
            --SP;
            break;
        case 0x3c:  //  INC A
            INC(A);
            break;
        case 0x3d:  //  DEC A
            DEC(A);
            break;
        case 0x3e:  //  LD A,nn
            A = memoryGet8(M, PC++);
            break;
        case 0x3f:  //  CCF
            F = (F & (FLAG_P | FLAG_Z | FLAG_S)) |
                ((F & FLAG_C) ? FLAG_H : FLAG_C) | (A & (FLAG_3 | FLAG_5));
            break;
        case 0x40:  //  LD B,B
            break;
        case 0x41:  //  LD B,C
            B = C;
            break;
        case 0x42:  //  LD B,D
            B = D;
            break;
        case 0x43:  //  LD B,E
            B = E;
            break;
        case 0x44:  //  LD B,H
            B = H;
            break;
        case 0x45:  //  LD B,L
            B = L;
            break;
        case 0x46:  //  LD B,(HL)
            B = memoryGet8(M, HL);
            break;
        case 0x47:  //  LD B,A
            B = A;
            break;
        case 0x48:  //  LD C,B
            C = B;
            break;
        case 0x49:  //  LD C,C
            break;
        case 0x4a:  //  LD C,D
            C = D;
            break;
        case 0x4b:  //  LD C,E
            C = E;
            break;
        case 0x4c:  //  LD C,H
            C = H;
            break;
        case 0x4d:  //  LD C,L
            C = L;
            break;
        case 0x4e:  //  LD C,(HL)
            C = memoryGet8(M, HL);
            break;
        case 0x4f:  //  LD C,A
            C = A;
            break;
        case 0x50:  //  LD D,B
            D = B;
            break;
        case 0x51:  //  LD D,C
            D = C;
            break;
        case 0x52:  //  LD D,D
            break;
        case 0x53:  //  LD D,E
            D = E;
            break;
        case 0x54:  //  LD D,H
            D = H;
            break;
        case 0x55:  //  LD D,L
            D = L;
            break;
        case 0x56:  //  LD D,(HL)
            D = memoryGet8(M, HL);
            break;
        case 0x57:  //  LD D,A
            D = A;
            break;
        case 0x58:  //  LD E,B
            E = B;
            break;
        case 0x59:  //  LD E,C
            E = C;
            break;
        case 0x5a:  //  LD E,D
            E = D:
            break;
        case 0x5b:  //  LD E,E
            break;
        case 0x5c:  //  LD E,H
            E = H;
            break;
        case 0x5d:  //  LD E,L
            E = L;
            break;
        case 0x5e:  //  LD E,(HL)
            E = memoryGet8(M, HL);
            break;
        case 0x5f:  //  LD E,A
            E = A;
            break;
        case 0x60:  //  LD H,B
            H = B;
            break;
        case 0x61:  //  LD H,C
            H = C;
            break;
        case 0x62:  //  LD H,D
            H = D;
            break;
        case 0x63:  //  LD H,E
            H = E;
            break;
        case 0x64:  //  LD H,H
            break;
        case 0x65:  //  LD H,L
            H = L;
            break;
        case 0x66:  //  LD H,(HL)
            H = memoryGet8(M, HL);
            break;
        case 0x67:  //  LD H,A
            H = A;
            break;
        case 0x68:  //  LD L,B
            L = B;
            break;
        case 0x69:  //  LD L,C
            L = C;
            break;
        case 0x6a:  //  LD L,D
            L = D;
            break;
        case 0x6b:  //  LD L,E
            L = E;
            break;
        case 0x6c:  //  LD L,H
            L = H;
            break;
        case 0x6d:  //  LD L,L
            break;
        case 0x6e:  //  LD L,(HL)
            L = memoryGet8(M, HL);
            break;
        case 0x6f:  //  LD L,A
            L = A;
            break;
        case 0x70:  //  LD (HL),B
            memorySet8(M, HL, B);
            break;
        case 0x71:  //  LD (HL),C
            memorySet8(M, HL, C);
            break;
        case 0x72:  //  LD (HL),D
            memorySet8(M, HL, D);
            break;
        case 0x73:  //  LD (HL),E
            memorySet8(M, HL, E);
            break;
        case 0x74:  //  LD (HL),H
            memorySet8(M, HL, H);
            break;
        case 0x75:  //  LD (HL),L
            memorySet8(M, HL, L);
            break;
        case 0x76:  // HALT
            M->z80.halted = 1;
            --PC;
            break;
        case 0x77:  //  LD (HL),A
            memorySet8(M, HL, A);
            break;
        case 0x78:  //  LD A,B
            A = B;
            break;
        case 0x79:  //  LD A,C
            A = C;
            break;
        case 0x7a:  //  LD A,D
            A = D;
            break;
        case 0x7b:  //  LD A,E
            A = E;
            break;
        case 0x7c:  //  LD A,H
            A = H;
            break;
        case 0x7d:  //  LD A,L
            A = L;
            break;
        case 0x7e:  //  LD A,(HL)
            A = memoryGet8(M, HL);
            break;
        case 0x7f:  //  LD A,A
            break;
        case 0x80:  //  ADD A,B
            ADD(B);
            break;
        case 0x81:  //  ADD A,C
            ADD(C);
            break;
        case 0x82:  //  ADD A,D
            ADD(D);
            break;
        case 0x83:  //  ADD A,E
            ADD(E);
            break;
        case 0x84:  //  ADD A,H
            ADD(H);
            break;
        case 0x85:  //  ADD A,L
            ADD(L);
            break;
        case 0x86:  //  ADD A,(HL)
            {
                u8 t = memoryGet8(M, HL);
                ADD(t);
            }
            break;
        case 0x87:  //  ADD A,A
            ADD(A);
            break;
        case 0x88:  //  ADC A,B
            ADC(B);
            break;
        case 0x89:  //  ADC A,C
            ADC(C);
            break;
        case 0x8a:  //  ADC A,D
            ADC(D);
            break;
        case 0x8b:  //  ADC A,E
            ADC(E);
            break;
        case 0x8c:  //  ADC A,H
            ADC(H);
            break;
        case 0x8d:  //  ADC A,L
            ADC(L);
            break;
        case 0x8e:  //  ADC A,(HL)
            {
                u8 t = memoryGet8(M, HL);
                ADC(t);
            }
            break;
        case 0x8f:  //  ADC A,A
            ADC(A);
            break;
        case 0x90:  //
            break;
        case 0x91:  //
            break;
        case 0x92:  //
            break;
        case 0x93:  //
            break;
        case 0x94:  //
            break;
        case 0x95:  //
            break;
        case 0x96:  //
            break;
        case 0x97:  //
            break;
        case 0x98:  //
            break;
        case 0x99:  //
            break;
        case 0x9a:  //
            break;
        case 0x9b:  //
            break;
        case 0x9c:  //
            break;
        case 0x9d:  //
            break;
        case 0x9e:  //
            break;
        case 0x9f:  //
            break;
        case 0xa0:  //
            break;
        case 0xa1:  //
            break;
        case 0xa2:  //
            break;
        case 0xa3:  //
            break;
        case 0xa4:  //
            break;
        case 0xa5:  //
            break;
        case 0xa6:  //
            break;
        case 0xa7:  //
            break;
        case 0xa8:  //
            break;
        case 0xa9:  //
            break;
        case 0xaa:  //
            break;
        case 0xab:  //
            break;
        case 0xac:  //
            break;
        case 0xad:  //
            break;
        case 0xae:  //
            break;
        case 0xaf:  //
            break;
        case 0xb0:  //
            break;
        case 0xb1:  //
            break;
        case 0xb2:  //
            break;
        case 0xb3:  //
            break;
        case 0xb4:  //
            break;
        case 0xb5:  //
            break;
        case 0xb6:  //
            break;
        case 0xb7:  //
            break;
        case 0xb8:  //
            break;
        case 0xb9:  //
            break;
        case 0xba:  //
            break;
        case 0xbb:  //
            break;
        case 0xbc:  //
            break;
        case 0xbd:  //
            break;
        case 0xbe:  //
            break;
        case 0xbf:  //
            break;
        case 0xc0:  //
            break;
        case 0xc1:  //
            break;
        case 0xc2:  //
            break;
        case 0xc3:  //
            break;
        case 0xc4:  //
            break;
        case 0xc5:  //
            break;
        case 0xc6:  //
            break;
        case 0xc7:  //
            break;
        case 0xc8:  //
            break;
        case 0xc9:  //
            break;
        case 0xca:  //
            break;
        case 0xcb:  //
            break;
        case 0xcc:  //
            break;
        case 0xcd:  //
            break;
        case 0xce:  //
            break;
        case 0xcf:  //
            break;
        case 0xd0:  //
            break;
        case 0xd1:  //
            break;
        case 0xd2:  //
            break;
        case 0xd3:  //
            break;
        case 0xd4:  //
            break;
        case 0xd5:  //
            break;
        case 0xd6:  //
            break;
        case 0xd7:  //
            break;
        case 0xd8:  //
            break;
        case 0xd9:  //
            break;
        case 0xda:  //
            break;
        case 0xdb:  //
            break;
        case 0xdc:  //
            break;
        case 0xdd:  //
            break;
        case 0xde:  //
            break;
        case 0xdf:  //
            break;
        case 0xe0:  //
            break;
        case 0xe1:  //
            break;
        case 0xe2:  //
            break;
        case 0xe3:  //
            break;
        case 0xe4:  //
            break;
        case 0xe5:  //
            break;
        case 0xe6:  //
            break;
        case 0xe7:  //
            break;
        case 0xe8:  //
            break;
        case 0xe9:  //
            break;
        case 0xea:  //
            break;
        case 0xeb:  //
            break;
        case 0xec:  //
            break;
        case 0xed:  //
            break;
        case 0xee:  //
            break;
        case 0xef:  //
            break;
        case 0xf0:  //
            break;
        case 0xf1:  //
            break;
        case 0xf2:  //
            break;
        case 0xf3:  //
            break;
        case 0xf4:  //
            break;
        case 0xf5:  //
            break;
        case 0xf6:  //
            break;
        case 0xf7:  //
            break;
        case 0xf8:  //
            break;
        case 0xf9:  //
            break;
        case 0xfa:  //
            break;
        case 0xfb:  //
            break;
        case 0xfc:  //
            break;
        case 0xfd:  //
            break;
        case 0xfe:  //
            break;
        case 0xff:  //
            break;
        }
    }
}


//----------------------------------------------------------------------------------------------------------------------
// Spectrum Emulator interface
//----------------------------------------------------------------------------------------------------------------------

int spek(Machine* M, ZXInput* in, ZXOutput* out)
{
    z80Update(M);
    videoRender(M, out);
    return YES;
}

void initSpek(Machine* M, Model model, Platform* P)
{
    //
    // Initialise the machine
    //
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
    M->nextEvent = 0;
    M->border = 7;
    M->model = model;

    M->platform = *P;

    M->z80.af.w = M->z80.af_.w = 0xffff;
    M->z80.bc.w = M->z80.bc_.w = 0x0000;
    M->z80.de.w = M->z80.de_.w = 0x0000;
    M->z80.hl.w = M->z80.hl_.w = 0x0000;
    M->z80.ix.w = 0x0000;
    M->z80.iy.w = 0x0000;
    M->z80.pc.w = 0x0000;
    M->z80.sp.w = 0xffff;
    M->z80.i = 0x00;
    M->z80.r = 0x00;
    M->z80.im = 0x00;
    M->z80.iff1 = 0x00;
    M->z80.iff2 = 0x00;
    M->z80.p.w = 0;
    M->z80.interruptsAt = -1;

    //
    // Initialise the lookup tables
    //
    u8 parity;
    int i, j, k;
    for (i = 0; i < 256; ++i)
    {
        gFlagsSZ53[i] = i & (FLAG_3 | FLAG_5 | FLAG_S);
        j = i;
        parity = 0;
        for (k = 0; k < 8; ++k)
        {
            parity ^= j & 1;
            j >>= 1;
        }
        gParity[i] = (parity ? 0 : FLAG_P);
        gFlagsSZ53P[i] = gFlagsSZ53[i] | gParity[i];
    }
    gFlagsSZ53[0] |= FLAG_Z;
    gFlagsSZ53P[0] |= FLAG_Z;

    //
    // Initialise the memory
    //
    dataLoad(P, M, 0, "48.rom");
    dataLoad(P, M, 0x4000, "Wizball.scr");
}

//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------
