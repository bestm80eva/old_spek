//----------------------------------------------------------------------------------------------------------------------
// Spek - ZX Spectrum emulator
//----------------------------------------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------------------------------------
// INDEX
//
//  MAIN        Main entry point.
//  PLATFORM    Platform determination macros.
//
//----------------------------------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------------------------{PLATFORM}
//----------------------------------------------------------------------------------------------------------------------
// P L A T F O R M   D E T E R M I N A T I O N   M A C R O S
//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------

#define YES (1)
#define NO (0)

//----------------------------------------------------------------------------------------------------------------------
// Compiler defines

#define COMPILER_MSVC   NO

#ifdef _MSC_VER
#   undef COMPILER_MSVC
#   define COMPILER_MSVC YES
#else
#   error Unknown compiler.  Please define COMPILER_XXX macro for your compiler.
#endif

//----------------------------------------------------------------------------------------------------------------------
// OS Determination

#define OS_WIN32        NO

#ifdef _WIN32
#   undef OS_WIN32
#   define OS_WIN32 YES
#else
#   error Unknown OS.  Please define OS_XXX macro for your OS.
#endif

//----------------------------------------------------------------------------------------------------------------------
// CPU determination

#define CPU_X64         NO
#define CPU_X86         NO

#if COMPILER_MSVC
#   if defined(_M_X64)
#       undef CPU_X64
#       define CPU_X64 YES
#   elif defined(_M_IX86)
#       undef CPU_X86
#       define CPU_X86 YES
#   else
#       error Cannot determine processor - something's gone very wrong here!
#   endif
#else
#   error Add CPU determination logic for your compiler.
#endif

//----------------------------------------------------------------------------------------------------------------------
// Standard headers

#if OS_WIN32
#   define WIN32_LEAN_AND_MEAN
#   include <Windows.h>
#endif

#include <stdlib.h>

//----------------------------------------------------------------------------------------------------------------------
// Basic types and definitions

#if OS_WIN32

typedef INT8    i8;
typedef INT16   i16;
typedef INT32   i32;
typedef INT64   i64;

typedef UINT8   u8;
typedef UINT16  u16;
typedef UINT32  u32;
typedef UINT64  u64;

typedef float   f32;
typedef double  f64;

typedef char    bool;

#else
#   error Define basic types for your platform.
#endif

#define AS_BOOL(b) ((b) ? YES : NO)

#define KB(x) (1024 * (x))
#define MB(x) (1024 * KB(x))
#define GB(x) (1024 * MB(x))

#define K_ASSERT(x, ...) assert(x)

//----------------------------------------------------------------------------------------------------------------{MAIN}
//----------------------------------------------------------------------------------------------------------------------
// M A I N   E N T R Y   P O I N T
//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------

int kmain(int, char**);

#if OS_WIN32
int WINAPI WinMain(HINSTANCE inst, HINSTANCE prevInst, LPSTR cmdLine, int cmdShow)
{
    return kmain(__argc, __argv);
}
#endif

int main(int argc, char** argv)
{
    return kmain(argc, argv);
}

//----------------------------------------------------------------------------------------------------------------------
// Main entry point

int kmain(int argc, char** argv)
{
    return 0;
}

//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------

