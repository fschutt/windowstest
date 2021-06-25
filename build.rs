fn main() {
    windows::build! {
        Windows::Win32::{
            UI::WindowsAndMessaging::{
                RegisterClassW, ShowWindow, CreateWindowExW, DefWindowProcW,
                GetMessageW, TranslateMessage, DispatchMessageW, GetClientRect,
                DestroyWindow, PostQuitMessage, GetWindowLongPtrW, SetWindowLongPtrW,
                WNDCLASSW, CW_USEDEFAULT, WINDOW_STYLE,
                HMENU, WINDOW_EX_STYLE, CREATESTRUCTW,
                WM_NCCREATE, WM_CREATE, WM_NCMOUSELEAVE, WM_MOUSEMOVE, WM_PAINT, WM_DESTROY,
            },
            System::LibraryLoader::{LoadLibraryW, FreeLibrary, GetModuleHandleW, GetProcAddress},
            UI::Controls::MARGINS,
            Foundation::{PWSTR, HWND},
            System::SystemInformation::{GetVersionExW, OSVERSIONINFOW},
            Graphics::OpenGL::{
                wglMakeCurrent, wglDeleteContext, wglCreateContext, wglGetProcAddress,
                ChoosePixelFormat, SetPixelFormat, SwapBuffers, DescribePixelFormat,
            },
            Graphics::Gdi::{GetDC, HDC, HRGN, ReleaseDC, UpdateWindow, PFD_DRAW_TO_WINDOW, PFD_SUPPORT_OPENGL, PFD_DOUBLEBUFFER, PFD_TYPE_RGBA, PFD_MAIN_PLANE },
            Graphics::Dwm::{DWM_BLURBEHIND, DWM_BB_ENABLE},
        }
    };
}