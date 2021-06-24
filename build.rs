fn main() {
    windows::build! {
        Windows::Win32::{
            UI::WindowsAndMessaging::{
                RegisterClassW, ShowWindow, CreateWindowExW, DefWindowProcW,
                GetMessageW, TranslateMessage, DispatchMessageW, GetClientRect,
                DestroyWindow,
                WNDCLASSW, CW_USEDEFAULT, WINDOW_STYLE,
                HMENU, WINDOW_EX_STYLE,
            },
            System::LibraryLoader::{LoadLibraryW, GetModuleHandleW, GetProcAddress},
            UI::Controls::MARGINS,
            Foundation::{PWSTR, HWND},
            System::SystemInformation::{GetVersionExW, OSVERSIONINFOW},
            Graphics::OpenGL::{
                wglMakeCurrent, wglDeleteContext, wglCreateContext,
                ChoosePixelFormat, SetPixelFormat, SwapBuffers, DescribePixelFormat,
            },
            Graphics::Gdi::{GetDC, ReleaseDC, UpdateWindow},
            Graphics::Dwm::{DWM_BLURBEHIND, DWM_BB_ENABLE},
        }
    };
}