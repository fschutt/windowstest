#![windows_subsystem = "windows"]

pub mod bindings {
    windows::include_bindings!();
}

use std::{ptr, mem};
use std::ffi::OsStr;
use std::os::windows::ffi::OsStrExt;
use bindings::{
    Windows::Win32::{
        System::LibraryLoader::{LoadLibraryW, GetModuleHandleW, GetProcAddress},
        UI::WindowsAndMessaging::{
            RegisterClassW, ShowWindow, CreateWindowExW, DefWindowProcW,
            GetMessageW, TranslateMessage, DispatchMessageW, GetClientRect,
            DestroyWindow,
            WNDCLASSW, CS_HREDRAW, CS_VREDRAW, CS_OWNDC,
            WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, SW_SHOW, SW_MAXIMIZE,
            HMENU, MSG, WS_EX_APPWINDOW,
        },
        UI::Controls::MARGINS,
        Foundation::{HWND, PWSTR, LPARAM, WPARAM, LRESULT, BOOL, RECT},
        System::SystemInformation::{GetVersionExW, OSVERSIONINFOW},
        Graphics::OpenGL::{
            HGLRC, PIXELFORMATDESCRIPTOR,
            wglMakeCurrent, wglDeleteContext, wglCreateContext,
            ChoosePixelFormat, SetPixelFormat, SwapBuffers, DescribePixelFormat,
        },
        Graphics::Gdi::{GetDC, ReleaseDC, UpdateWindow},
        Graphics::Dwm::{DWM_BLURBEHIND, DWM_BB_ENABLE},
    },
};
use windows::HRESULT;

const CLASS_NAME: &str = "Window Class";

// Optional function pointer for "DwmEnableBlurBehindWindow" function
static mut DwmEnableBlurBehindWindow: Option<unsafe extern "C" fn(HWND, &DWM_BLURBEHIND) -> HRESULT> = None;
// Optional function pointer for "DwmExtendFrameIntoClientArea" function
static mut DwmExtendFrameIntoClientArea: Option<unsafe extern "C" fn(HWND, &MARGINS) -> HRESULT> = None;
// Optional function pointer for getting the opaque system color of the window
static mut DwmGetColorizationColor: Option<unsafe extern "C" fn(&mut u32, &mut BOOL) -> HRESULT> = None;

unsafe fn initialize_function_pointers() -> windows::Result<()> {

    if DwmEnableBlurBehindWindow.is_some() ||
       DwmExtendFrameIntoClientArea.is_some() ||
       DwmExtendFrameIntoClientArea.is_some() {
        return Ok(());
    }

    let mut os_vinfo = OSVERSIONINFOW::default();
    GetVersionExW(&mut os_vinfo);

    if os_vinfo.dwMajorVersion >= 6 {
        let hDwmAPI_DLL = LoadLibraryW("dwmapi.dll");
        if !hDwmAPI_DLL.is_null() {
            DwmEnableBlurBehindWindow = Some(mem::transmute(GetProcAddress(hDwmAPI_DLL, "DwmEnableBlurBehindWindow")));
            DwmExtendFrameIntoClientArea = Some(mem::transmute(GetProcAddress(hDwmAPI_DLL, "DwmExtendFrameIntoClientArea")));
            DwmGetColorizationColor = Some(mem::transmute(GetProcAddress(hDwmAPI_DLL, "DwmGetColorizationColor")));
        }
    }

    Ok(())
}

enum WindowsWindowCreateError {
    FailedToCreateHInstance(HRESULT),
    FailedToCreateHWND(HRESULT),
    FailedToGetDC(HRESULT),
}

enum WindowsOpenGlError {
    FailedToGetPixelFormat(HRESULT),
    NoMatchingPixelFormat(HRESULT),
    OpenGLNotAvailable(HRESULT),

}

enum WindowsStartupError {
    Create(WindowsWindowCreateError),
    Gl(WindowsOpenGlError),
}

struct WindowsWindowData {
    // OpenGL 3.1 context pointer
    hRC: HGLRC,
}

fn create_opengl_window() -> windows::Result<HWND> {

    let app_instance = unsafe { GetModuleHandleW(PWSTR::NULL) };
    if app_instance.is_null() {
        return Err(windows::Error::new(windows::HRESULT::from_thread(), "Failed to create HINSTANCE (GetModuleHandleW failed)"));
    }

    let mut class_name = OsStr::new(CLASS_NAME)
        .encode_wide()
        .chain(Some(0).into_iter())
        .collect::<Vec<_>>();

    // Register the application class
    let wc = WNDCLASSW {
        style: CS_HREDRAW | CS_VREDRAW | CS_OWNDC,
        hInstance: app_instance,
        lpszClassName: PWSTR(class_name.as_mut_ptr()),
        lpfnWndProc:  Some(WindowProc), // DefWindowProcW
        .. Default::default()
    };

    // RegisterClass can fail if the same class is registered twice,
    // error can be ignored
    unsafe { RegisterClassW(&wc) };

    // Create the window.
    let hwnd = unsafe { CreateWindowExW(
        WS_EX_APPWINDOW,                // Optional window styles
        CLASS_NAME,                     // Window class
        "Learn to program Windows",     // Window text
        WS_OVERLAPPEDWINDOW,            // Window style

        // Size and position
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,

        HWND::NULL,         // Parent window
        HMENU::NULL,        // Menu
        app_instance,       // Instance handle
        ptr::null_mut(),    // Additional application data
    ) };

    if hwnd.is_null() {
        return Err(windows::Error::new(windows::HRESULT::from_thread(), "Failed to create HWND (CreateWindowExW failed)"));
    }

    // -- window created, now create OpenGL context

    // Get DC
    let hDC = unsafe { GetDC(hwnd) };
    if hDC.is_null()  {
        unsafe { DestroyWindow(hwnd) };
        return Err(windows::Error::new(windows::HRESULT::from_thread(), "Failed to get drawing context (GetDC(hwnd) failed)"));
    }

    /*
        PIXELFORMATDESCRIPTOR pfd = {
            sizeof(PIXELFORMATDESCRIPTOR),  //  size of this pfd
            1,                     // version number
            PFD_DRAW_TO_WINDOW |   // support window
            PFD_SUPPORT_OPENGL |   // support OpenGL
            PFD_DOUBLEBUFFER,      // double buffered
            PFD_TYPE_RGBA,         // RGBA type
            24,                    // 24-bit color depth
            0, 0, 0, 0, 0, 0,      // color bits ignored
            0,                     // no alpha buffer
            0,                     // shift bit ignored
            0,                     // no accumulation buffer
            0, 0, 0, 0,            // accum bits ignored
            32,                    // 32-bit z-buffer
            0,                     // no stencil buffer
            0,                     // no auxiliary buffer
            PFD_MAIN_PLANE,        // main layer
            0,                     // reserved
            0, 0, 0                // layer masks ignored
            };
            HDC  hdc;
            int  iPixelFormat;

        iPixelFormat = ChoosePixelFormat(hdc, &pfd);
    */

    /*
    let attribs = [
        WGL_DRAW_TO_WINDOW_ARB, TRUE,
        WGL_DOUBLE_BUFFER_ARB, TRUE,
        WGL_SUPPORT_OPENGL_ARB, TRUE,
        WGL_PIXEL_TYPE_ARB, WGL_TYPE_RGBA_ARB,
        WGL_TRANSPARENT_ARB, TRUE,
        WGL_COLOR_BITS_ARB, 32,
        WGL_RED_BITS_ARB, 8,
        WGL_GREEN_BITS_ARB, 8,
        WGL_BLUE_BITS_ARB, 8,
        WGL_ALPHA_BITS_ARB, 8,
        WGL_DEPTH_BITS_ARB, 24,
        WGL_STENCIL_BITS_ARB, 8,
        0, 0
    ];
    */

    let mut pixel_format = 0;
    let mut num_pixel_formats = 0;

    if unsafe { !ChoosePixelFormat(hDC, attribs, NULL, 1, &mut pixel_format, &mut num_pixel_formats).as_bool() } {
        unsafe {
            ReleaseDC(hwnd, hDC);
            DestroyWindow(hwnd);
        }
        return Err(windows::Error::new(windows::HRESULT::from_thread(), "Failed to get pixel format (ChoosePixelFormat failed)"));
    }

    if num_pixel_formats == 0 {
        unsafe {
            ReleaseDC(hwnd, hDC);
            DestroyWindow(hwnd);
        }
        return Err(windows::Error::new(windows::HRESULT::from_thread(), "OS has no matching pixel format (num_pixel_formats = 0)"));
    }

    // now this is a kludge; we need to pass something in the PIXELFORMATDESCRIPTOR
    // to SetPixelFormat; it will be ignored, mostly. OTOH we want to send something
    // sane, we're nice people after all - it doesn't hurt if this fails.
    let mut pfd = PIXELFORMATDESCRIPTOR::NULL;
    unsafe {
        DescribePixelFormat(hDC, pixel_format, &mut pfd);
        if !SetPixelFormat(hDC, pixel_format, &pfd).as_bool() {
            ReleaseDC(hwnd, hDC);
            DestroyWindow(hwnd);
            return Err(windows::Error::new(windows::HRESULT::from_thread(), "OS has no matching pixel format (num_pixel_formats = 0)"));
        }
    }

    // Créate OpenGL 3.1 context
    let context_attribs = [
        WGL_CONTEXT_MAJOR_VERSION_ARB, 3,
        WGL_CONTEXT_MINOR_VERSION_ARB, 1,
        0, 0
    ];

    let hRC = unsafe { wglCreateContext(hDC, NULL, context_attribs) };
    if hRC.is_null() {
        unsafe {
            ReleaseDC(hwnd, hDC);
            DestroyWindow(hwnd);
        }
        return Err(windows::Error::new(windows::HRESULT::from_thread(), "Could not create OpenGL 3.1 context (wglCreateContext failed)"));
    }

    unsafe {
        ReleaseDC(hwnd, hDC);
        // SetWindowLongPtr(Box::new(WindowsWindowData { hRC }));
        initialize_function_pointers();

        // optional: create transparency, extend window frame
        if let Some(impl_DwmEnableBlurBehindWindow) = DwmEnableBlurBehindWindow {
            // no error checking here...
            impl_DwmEnableBlurBehindWindow(hwnd, &DWM_BLURBEHIND {
                dwFlags: DWM_BB_ENABLE,
                fEnable: BOOL::TRUE,
                hRgnBlur: NULL,
                fTransitionOnMaximized: BOOL::TRUE,
            });
        }

        if let Some(impl_DwmExtendFrameIntoClientArea) = DwmExtendFrameIntoClientArea {
            // no error checking here...
            impl_DwmExtendFrameIntoClientArea(hwnd, &MARGINS {
                cxLeftWidth: -1,
                cxRightWidth: -1,
                cyTopHeight: -1,
                cyBottomHeight: -1,
            });
        }
    }


    return Ok(hwnd);
}

unsafe extern "system" fn WindowProc(hwnd: HWND, msg: u32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {

    let mut hRC = GetWindowLongPtr(hwnd, ); // HGLRC::NULL;

    match msg {
        WM_CREATE => { },
        WM_MOUSELEAVE => {
            // cursor_needs_setting = TRUE;
        },
        WM_MOUSEMOVE => {
            // if( cursor_needs_setting ) {
            //     SetClassLongPtr(hwnd, GCLP_HCURSOR, (LONG_PTR)LoadCursor(NULL, IDC_ARROW));
            //     cursor_needs_setting = FALSE;
            // }
        },
        WM_DESTROY => unsafe {
            wglMakeCurrent(NULL,NULL);
            wglDeleteContext(hRC);
            PostQuitMessage(0);
        },
        WM_PAINT => unsafe {
            let hDC = GetDC(hwnd);
            let mut rect = RECT::NULL;
            GetClientRect(hwnd, &mut rect);

            wglMakeCurrent(hDC, hRC);

            glViewport(0, 0, rect.right, rect.bottom);
            glClearColor(0.0, 0.0, 0.0, 0.0);
            glClearDepth(1.0);
            glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

            // OpenGL calls go here ...

            /*
                glMatrixMode(GL_PROJECTION);
                glLoadIdentity();
                glOrtho(-ratio, ratio, -1., 1., -1, 1);

                glMatrixMode(GL_MODELVIEW);
                glLoadIdentity();

                float const cos60 = cosf(M_PI*60.0/180.0);
                float const sin60 = sinf(M_PI*60.0/180.0);

                GLfloat const triangle[] = {
                    -1.0, -sin60, 1.0, 0.0, 0.0,
                     1.0, -sin60, 0.0, 1.0, 0.0,
                     0.0,  sin60, 0.0, 0.0, 1.0
                };

                glEnable(GL_BLEND);
                glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

                glEnableClientState(GL_VERTEX_ARRAY);
                glEnableClientState(GL_COLOR_ARRAY);

                glVertexPointer(2, GL_FLOAT, sizeof(GLfloat)*5, &triangle[0]);
                glColorPointer( 3, GL_FLOAT, sizeof(GLfloat)*5, &triangle[0]);

                glDrawArrays(GL_TRIANGLES, 0, 3);
            */

            SwapBuffers(hDC);
            glFinish(); // OpenGL 3?

            wglMakeCurrent(NULL, NULL);
            ReleaseDC(hwnd, hDC);
        },
        _ => { }
    }

    DefWindowProcW(hwnd, msg, wparam, lparam)
}

fn main() -> windows::Result<()> {

    let hwnd = create_opengl_window()?;
    let mut msg = MSG::default();

    unsafe {
        UpdateWindow(hwnd);
        ShowWindow(hwnd, SW_SHOW | SW_MAXIMIZE);
        while GetMessageW(&mut msg, hwnd, 0, 0) > 0 {
            TranslateMessage(&msg);
            DispatchMessageW(&msg);
        }
    }

    // return msg.wParam;

    Ok(())
}