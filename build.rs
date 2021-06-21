fn main() {
    windows::build! {
        Windows::Win32::{
            UI::WindowsAndMessaging::{
                RegisterClassW, ShowWindow, CreateWindowExW, DefWindowProcW,
                GetMessageW, TranslateMessage, DispatchMessageW,
                WNDCLASSW, CW_USEDEFAULT, WINDOW_STYLE,
                HMENU, WINDOW_EX_STYLE,
            },
            System::LibraryLoader::GetModuleHandleW,
            Foundation::{PWSTR, HWND},
        }
    };
}