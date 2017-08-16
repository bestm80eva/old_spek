//----------------------------------------------------------------------------------------------------------------------
// Spek - ZX Spectrum emulator
//----------------------------------------------------------------------------------------------------------------------

// CONSTANTS
#define SPEK_WIDTH      (48*2 + 256)
#define SPEK_HEIGHT     (56*2 + 192)

//----------------------------------------------------------------------------------------------------------------------
// INDEX
//  Search for {xxx} where xxx is the index name listed below.
//
//  API         Interface point between OS-specific code and OS-agnostic code.
//  MAIN        Main entry point.
//  PLATFORM    Platform determination macros.
//  WIN32       Win-32 section.
//
//----------------------------------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------------------------{PLATFORM}
//----------------------------------------------------------------------------------------------------------------------
// P L A T F O R M   D E T E R M I N A T I O N   M A C R O S
//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------------------------------------
// Standard headers

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

//----------------------------------------------------------------------------------------------------------------------
// Required headers

#include <conio.h>
#include <fcntl.h>
#include <io.h>

//----------------------------------------------------------------------------------------------------------------------
// Basic types and definitions

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

#define K_ASSERT(x, ...) assert(x)

//-----------------------------------------------------------------------------------------------------------------{API}|
//----------------------------------------------------------------------------------------------------------------------
// A P I
//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------

#include "spek.h"

//---------------------------------------------------------------------------------------------------------------{WIN32}
//----------------------------------------------------------------------------------------------------------------------
// W I N D O W S   P L A T F O R M
//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------

bool gRunning = YES;

//----------------------------------------------------------------------------------------------------------------------
// Rendering
//----------------------------------------------------------------------------------------------------------------------

typedef int Window;

typedef struct
{
    HWND handle;
    BITMAPINFO info;

    u32* image;
    int imgWidth;
    int imgHeight;
    int wndWidth;
    int wndHeight;
}
WindowInfo;

Array(WindowInfo) gWindows = 0;
int gWindowCount = 0;
ATOM gWindowClassAtom = 0;

internal Window __windowAllocHandle()
{
    for (int i = 0; i < arrayCount(gWindows); ++i)
    {
        if (gWindows[i].handle == 0)
        {
            return i;
        }
    }

    gWindows = arrayExpand(gWindows, 1);
    return (Window)(arrayCount(gWindows) - 1);
}


internal Window __windowFindHandle(HWND wnd)
{
    i64 count = arrayCount(gWindows);
    for (int i = 0; i < count; ++i)
    {
        if (gWindows[i].handle == wnd) return i;
    }

    return -1;
}

typedef struct WindowCreateInfo
{
    Window handle;
}
WindowCreateInfo;

internal LRESULT CALLBACK __windowProc(HWND wnd, UINT msg, WPARAM w, LPARAM l)
{
    if (msg == WM_CREATE)
    {
        CREATESTRUCTA* cs = (CREATESTRUCTA *)l;
        WindowCreateInfo* wci = (WindowCreateInfo *)cs->lpCreateParams;
        gWindows[wci->handle].handle = wnd;
    }
    else
    {
        Window window = __windowFindHandle(wnd);
        WindowInfo* info = (window == -1 ? 0 : &gWindows[window]);

        switch (msg)
        {
        case WM_SIZE:
            if (info)
            {
                int bitmapMemorySize = 0;

                info->wndWidth = LOWORD(l);
                info->wndHeight = HIWORD(l);

                ZeroMemory(&info->info.bmiHeader, sizeof(info->info.bmiHeader));

                info->info.bmiHeader.biSize = sizeof(info->info.bmiHeader);
                info->info.bmiHeader.biWidth = info->imgWidth;
                info->info.bmiHeader.biHeight = -info->imgHeight;
                info->info.bmiHeader.biPlanes = 1;
                info->info.bmiHeader.biBitCount = 32;
                info->info.bmiHeader.biClrImportant = BI_RGB;
            }
            break;

        case WM_PAINT:
            if (info)
            {
                PAINTSTRUCT ps;
                HDC dc = BeginPaint(wnd, &ps);
                StretchDIBits(dc,
                    0, 0, info->wndWidth, info->wndHeight,
                    0, 0, info->imgWidth, info->imgHeight,
                    info->image, &info->info,
                    DIB_RGB_COLORS, SRCCOPY);
                EndPaint(wnd, &ps);
            }
            break;

        case WM_CLOSE:
            DestroyWindow(wnd);
            break;

        case WM_DESTROY:
            if (--gWindowCount == 0)
            {
                PostQuitMessage(0);
            }
            info->handle = 0;
            break;

        default:
            return DefWindowProcA(wnd, msg, w, l);
        }
    }

    return 0;
}

Window windowMake(const char* title, u32* image, int width, int height, int scale)
{
    WindowCreateInfo wci;
    Window w = __windowAllocHandle();

    wci.handle = w;

    gWindows[w].handle = 0;
    gWindows[w].image = image;
    gWindows[w].imgWidth = width;
    gWindows[w].imgHeight = height;
    gWindows[w].wndWidth = width * scale;
    gWindows[w].wndHeight = width * scale;

    RECT r = { 0, 0, width * scale, height * scale };
    int style = WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU | WS_VISIBLE;

    if (!gWindowClassAtom)
    {
        WNDCLASSEXA wc = { 0 };

        wc.cbSize = sizeof(WNDCLASSEXA);
        wc.style = CS_HREDRAW | CS_VREDRAW;
        wc.lpfnWndProc = &__windowProc;
        wc.hInstance = GetModuleHandleA(0);
        wc.hIcon = wc.hIconSm = LoadIconA(0, IDI_APPLICATION);
        wc.hCursor = LoadCursorA(0, IDC_ARROW);
        wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
        wc.lpszClassName = "k_bitmap_window";

        gWindowClassAtom = RegisterClassExA(&wc);
    }

    AdjustWindowRect(&r, style, FALSE);

    gWindows[w].handle = CreateWindowA("k_bitmap_window", title, style,
        CW_USEDEFAULT, CW_USEDEFAULT,
        r.right - r.left, r.bottom - r.top,
        0, 0, GetModuleHandleA(0), &wci);

    ++gWindowCount;

    return w;
}

void windowClose(Window window)
{
    SendMessageA(gWindows[window].handle, WM_CLOSE, 0, 0);
}

void windowRedraw(Window window)
{
    InvalidateRect(gWindows[window].handle, 0, FALSE);
}

int windowWidth(Window window)
{
    return gWindows[window].imgWidth;
}

int windowHeight(Window window)
{
    return gWindows[window].imgHeight;
}

u32* windowImage(Window window)
{
    return gWindows[window].image;
}

void windowConsole()
{
    AllocConsole();
    SetConsoleTitleA("Debug Window");

    HANDLE handle_out = GetStdHandle(STD_OUTPUT_HANDLE);
    int hCrt = _open_osfhandle((intptr_t)handle_out, _O_TEXT);
    FILE* hf_out = _fdopen(hCrt, "w");
    setvbuf(hf_out, NULL, _IONBF, 1);
    freopen("CONOUT$", "w", stdout);

    HANDLE handle_in = GetStdHandle(STD_INPUT_HANDLE);
    hCrt = _open_osfhandle((intptr_t)handle_in, _O_TEXT);
    FILE* hf_in = _fdopen(hCrt, "r");
    setvbuf(hf_in, NULL, _IONBF, 0);
    freopen("CONIN$", "r", stdin);

    DWORD mode;
    GetConsoleMode(handle_out, &mode);
    mode |= 0x4;
    SetConsoleMode(handle_out, mode);
}

void windowConsolePause()
{
    printf("\nPress any key...");
    _getch();
}

//----------------------------------------------------------------------------------------------------------------------
// Main Loop
//----------------------------------------------------------------------------------------------------------------------

internal void win32ProcessPendingMessages()
{
    MSG message;

    while (PeekMessage(&message, 0, 0, 0, PM_REMOVE))
    {
        if (message.message == WM_QUIT)
        {
            gRunning = NO;
        }

        // Process keyboard
        switch (message.message)
        {
        case WM_SYSKEYDOWN:
        case WM_SYSKEYUP:
        case WM_KEYDOWN:
        case WM_KEYUP:
        {
            u32 vkCode = (u32)message.wParam;
            bool wasDown = ((message.lParam & (1 << 30)) != 0);
            bool isDown = ((message.lParam & (1 << 31)) == 0);

            if (wasDown != isDown)
            {
            }

            bool altKeyWasDown = AS_BOOL(message.lParam & (1ull << 29));
            if ((vkCode == VK_F4) && altKeyWasDown)
            {
                Window w = __windowFindHandle(message.hwnd);
                windowClose(w);
            }
        }
        break;

        default:
            TranslateMessage(&message);
            DispatchMessage(&message);
            break;
        }
    }
}

int kmain(int argc, char** argv)
{
    i64 imageSize = sizeof(u32) * SPEK_WIDTH * SPEK_HEIGHT;
    u32* image = K_ALLOC(imageSize);

    memoryClear(image, imageSize);

    Window wnd = windowMake("SPEK (V0.1)", image, SPEK_WIDTH, SPEK_HEIGHT, 2);
    Machine M;

    initSpek(&M, M_ZX48);

    while (gRunning)
    {
        ZXInput in;
        ZXOutput out;

        win32ProcessPendingMessages();

        out.screen = image;
        if (!spek(&M, &in, &out)) gRunning = NO;

        windowRedraw(wnd);
    }

    K_FREE(image, imageSize);

    return 0;
}


//----------------------------------------------------------------------------------------------------------------{MAIN}
//----------------------------------------------------------------------------------------------------------------------
// M A I N   E N T R Y   P O I N T
//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------

int WINAPI WinMain(HINSTANCE inst, HINSTANCE prevInst, LPSTR cmdLine, int cmdShow)
{
    return kmain(__argc, __argv);
}

//----------------------------------------------------------------------------------------------------------------------
// Main entry point

//----------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------

