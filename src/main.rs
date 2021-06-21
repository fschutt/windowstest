#![windows_subsystem = "windows"]

pub mod bindings {
    windows::include_bindings!();
}

use std::ptr;
use std::ffi::OsStr;
use std::os::windows::ffi::OsStrExt;
use bindings::{
    Windows::Win32::{
        System::LibraryLoader::GetModuleHandleW,
        UI::WindowsAndMessaging::{
            RegisterClassW, ShowWindow, CreateWindowExW, DefWindowProcW,
            GetMessageW, TranslateMessage, DispatchMessageW,
            WNDCLASSW, CS_HREDRAW, CS_VREDRAW, CS_OWNDC,
            WS_OVERLAPPEDWINDOW, WS_VISIBLE, CW_USEDEFAULT, SW_SHOW,
            HMENU, WS_EX_LAYERED, MSG,
        },
        Foundation::{HWND, PWSTR, LPARAM, WPARAM, LRESULT},
    }
};

const CLASS_NAME: &str = "Window Class";

fn main() -> windows::Result<()> {

    let app_instance = unsafe { GetModuleHandleW(PWSTR::NULL) };
    if app_instance.is_null() {
        return Err(windows::Error::new(windows::HRESULT::from_thread(), "Failed to create HINSTANCE (GetModuleHandleW failed)"));
    }

    let mut class_name = OsStr::new(CLASS_NAME)
        .encode_wide()
        .chain(Some(0).into_iter())
        .collect::<Vec<_>>();

    // Register the application class
    {
        let wc = WNDCLASSW {
            style: CS_HREDRAW | CS_VREDRAW| CS_OWNDC,
            hInstance: app_instance,
            lpszClassName: PWSTR(class_name.as_mut_ptr()),
            lpfnWndProc:  Some(WindowProc), // DefWindowProcW
            .. Default::default()
        };

        if unsafe { RegisterClassW(&wc) } == 0 {
            return Err(windows::Error::new(windows::HRESULT::from_thread(), "Failed to create HINSTANCE (GetModuleHandleW failed)"));
            // TODO: delete hinstance / WNDCLASS
        }
    }

    // Create the window.
    let hwnd = unsafe {CreateWindowExW(
        WS_EX_LAYERED, // Optional window styles
        CLASS_NAME,      // Window class
        "Learn to program Windows",                  // Window text
        WS_OVERLAPPEDWINDOW | WS_VISIBLE,            // Window style

        // Size and position
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,

        HWND::NULL,         // Parent window
        HMENU::NULL,        // Menu
        app_instance,       // Instance handle
        ptr::null_mut(),        // Additional application data
    ) };


    if hwnd.is_null() {
        return Err(windows::Error::new(windows::HRESULT::from_thread(), "Failed to create HWND (CreateWindowExW failed)"));
    }

    unsafe { ShowWindow(hwnd, SW_SHOW) }; // | SW_MAXIMIZE

    let mut msg = MSG::default();

    unsafe {
        while GetMessageW(&mut msg, hwnd, 0, 0).as_bool() {
            TranslateMessage(&msg);
            DispatchMessageW(&msg);
        }
    }

    Ok(())
}

unsafe extern "system" fn WindowProc(hwnd: HWND, msg: u32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
    DefWindowProcW(hwnd, msg, wparam, lparam)
}