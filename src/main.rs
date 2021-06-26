#![allow(non_snake_case)]
#![windows_subsystem = "windows"]
#![no_main]
#![no_std]

extern crate core;
extern crate alloc;

use core::{ptr, mem};
use core::ffi::c_void;
use core::panic::PanicInfo;

use alloc::rc::Rc;
use alloc::cell::RefCell;
use libc_alloc::LibcAlloc;

use gleam::gl::GlFns;
use gleam::gl::Gl;

#[global_allocator]
static ALLOCATOR: LibcAlloc = LibcAlloc;

#[panic_handler]
fn panic(_panic: &PanicInfo<'_>) -> ! { loop {} }

/*
use bindings::{
    Windows::Win32::{
        System::LibraryLoader::{LoadLibraryW, FreeLibrary, GetModuleHandleW, GetProcAddress},
        UI::WindowsAndMessaging::{
            RegisterClassW, ShowWindow, CreateWindowExW, DefWindowProcW, AnimateWindow, AW_BLEND, AW_HOR_POSITIVE, AW_VER_POSITIVE, AW_ACTIVATE,
            GetMessageW, TranslateMessage, DispatchMessageW, GetClientRect,
            DestroyWindow, PostQuitMessage, GetWindowLongPtrW, SetWindowLongPtrW,
            WNDCLASSW, CS_HREDRAW, CS_VREDRAW, CS_OWNDC,
            WS_OVERLAPPEDWINDOW, WS_POPUP, CW_USEDEFAULT, SW_SHOW, SW_MAXIMIZE, SW_SHOWNORMAL,
            HMENU, MSG, WS_EX_APPWINDOW, CREATESTRUCTW, GWLP_USERDATA,
            WM_NCCREATE, WM_CREATE, WM_NCMOUSELEAVE, WM_MOUSEMOVE, WM_PAINT, WM_DESTROY
        },
        UI::Controls::MARGINS,
        Foundation::{HWND, PWSTR, LPARAM, WPARAM, LRESULT, BOOL, RECT, HINSTANCE},
        System::SystemInformation::{GetVersionExW, OSVERSIONINFOW},
        Graphics::OpenGL::{
            HGLRC, PIXELFORMATDESCRIPTOR,
            wglMakeCurrent, wglDeleteContext, wglCreateContext, wglGetProcAddress,
            ChoosePixelFormat, SetPixelFormat, SwapBuffers, DescribePixelFormat,
        },
        Graphics::Gdi::{GetDC, HDC, HRGN, ReleaseDC, UpdateWindow, PFD_DRAW_TO_WINDOW, PFD_SUPPORT_OPENGL, PFD_DOUBLEBUFFER, PFD_TYPE_RGBA, PFD_MAIN_PLANE },
        Graphics::Dwm::{DWM_BLURBEHIND, DWM_BB_ENABLE},
    },
};
use windows::HRESULT;
*/


// windef.h
const TRUE: i32 = 1;

const CLASS_NAME: &str = "Window Class";

enum WindowsWindowCreateError {
    FailedToCreateHInstance(HRESULT),
    FailedToCreateHWND(HRESULT),
}

enum WindowsOpenGlError {
    OpenGL32DllNotFound(HRESULT),
    FailedToGetDC(HRESULT),
    FailedToGetPixelFormat(HRESULT),
    NoMatchingPixelFormat(HRESULT),
    OpenGLNotAvailable(HRESULT),
    FailedToStoreContext(HRESULT),
}

enum WindowsStartupError {
    Create(WindowsWindowCreateError),
    Gl(WindowsOpenGlError),
}

struct WindowsWindowData {
    // OpenGL 3.1 context pointer
    gl_context: Rc<RefCell<Option<WindowsGlContext>>>,
}

struct WindowsGlContext {
    hrc: HGLRC,
    /// OpenGL 1.1 functions are loaded separately (for example glViewport)
    /// In order to fill out the Rc<Gl> properly, we need to open opengl32.dll
    /// and use GetProcAddress on the HINSTANCE if GetProcAddress returns null
    opengl32_dll: HINSTANCE,
    gl: Rc<dyn Gl>,
}

fn create_window() -> Result<(HWND, Rc<RefCell<Option<WindowsGlContext>>>), WindowsWindowCreateError> {
    use self::WindowsWindowCreateError::*;

    let app_instance = unsafe { GetModuleHandleW(PWSTR::NULL) };
    if app_instance.is_null() {
        return Err(FailedToCreateHInstance(HRESULT::from_thread()));
    }

    let mut class_name = CLASS_NAME
        .encode_utf16()
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

    let context_arc = Rc::new(RefCell::new(None));
    let window_data = Box::new(WindowsWindowData {
        gl_context: context_arc.clone()
    });

    // Create the window.
    let hwnd = unsafe { CreateWindowExW(
        WS_EX_APPWINDOW,                // Optional window styles
        CLASS_NAME,                     // Window class
        "Learn to program Windows",     // Window text
        WS_OVERLAPPEDWINDOW | WS_POPUP,            // Window style

        // Size and position
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,

        HWND::NULL,         // Parent window
        HMENU::NULL,        // Menu
        app_instance,       // Instance handle
        Box::leak(window_data) as *mut WindowsWindowData as *mut c_void,    // Additional application data
    ) };

    if hwnd.is_null() {
        Err(FailedToCreateHWND(windows::HRESULT::from_thread()))
    } else {
        Ok((hwnd, context_arc))
    }
}

// function can fail: creates an OpenGL context on the HWND, stores the context on the window-associated data
fn create_opengl_context(hwnd: HWND, context_arc: Rc<RefCell<Option<WindowsGlContext>>>) -> Result<(), WindowsOpenGlError> {

    use self::WindowsOpenGlError::*;

    // -- window created, now create OpenGL context

    let opengl32_dll = unsafe { LoadLibraryW("opengl32.dll") };
    if opengl32_dll.is_null() { return Err(OpenGL32DllNotFound(HRESULT::from_thread())); }

    // Get DC
    let hDC = unsafe { GetDC(hwnd) };
    if hDC.is_null()  {
        // unsafe { DestroyWindow(hwnd) };
        return Err(FailedToGetDC(HRESULT::from_thread()));
    }

    // now this is a kludge; we need to pass something in the PIXELFORMATDESCRIPTOR
    // to SetPixelFormat; it will be ignored, mostly. OTOH we want to send something
    // sane, we're nice people after all - it doesn't hurt if this fails.
    let mut pfd = PIXELFORMATDESCRIPTOR {
        nSize: mem::size_of::<PIXELFORMATDESCRIPTOR> as u16,
        nVersion: 1,
        dwFlags: {
            PFD_DRAW_TO_WINDOW |   // support window
            PFD_SUPPORT_OPENGL |   // support OpenGL
            PFD_DOUBLEBUFFER       // double buffered
        },
        iPixelType: PFD_TYPE_RGBA as u8,
        cColorBits: 24,
        cRedBits: 0,
        cRedShift: 0,
        cGreenBits: 0,
        cGreenShift: 0,
        cBlueBits: 0,
        cBlueShift: 0,
        cAlphaBits: 0,
        cAlphaShift: 0,
        cAccumBits: 0,
        cAccumRedBits: 0,
        cAccumGreenBits: 0,
        cAccumBlueBits: 0,
        cAccumAlphaBits: 0,
        cDepthBits: 32, // 32-bit z-buffer
        cStencilBits: 0, // no stencil buffer
        cAuxBuffers: 0, // no auxiliary buffer
        iLayerType: PFD_MAIN_PLANE as u8, // main layer
        bReserved: 0,
        dwLayerMask: 0,
        dwVisibleMask: 0,
        dwDamageMask: 0,
    };

    let default_pixel_format = unsafe { ChoosePixelFormat(hDC, &pfd) };
    unsafe {
        DescribePixelFormat(hDC, default_pixel_format, mem::size_of::<PIXELFORMATDESCRIPTOR>() as u32, &mut pfd);
        if !SetPixelFormat(hDC, default_pixel_format, &pfd).as_bool() {
            // can't even set the default fallback pixel format: no OpenGL possible
            ReleaseDC(hwnd, hDC);
            // DestroyWindow(hwnd);
            return Err(NoMatchingPixelFormat(HRESULT::from_thread()));
        }
    }

    // wglGetProcAddress will fail if there is no context being current,
    // create a dummy context and activate it
    let dummy_context = unsafe {
        let dc = wglCreateContext(hDC);
        wglMakeCurrent(hDC, dc);
        dc
    };

    let mut b_transparent_succeeded = false;
    let transparent_opengl_pixelformat_index = match get_transparent_pixel_format_index(hDC) {
        Some(i) => { b_transparent_succeeded = true; i },
        None => default_pixel_format,
    };

    // if the transparent pixel format is available, try getting the wglCreateContextAttribsARB function
    // while the context is active
    let mut CreateContextAttribsARB: Option<unsafe extern "system" fn(HDC, HGLRC, *const [i32]) -> HGLRC> = None;
    let CreateContextAttribsARB = if b_transparent_succeeded {
        unsafe {
            wglGetProcAddress("wglCreateContextAttribsARB").map(|s| {
                let w: unsafe extern "system" fn(HDC, HGLRC, *const [i32]) -> HGLRC = mem::transmute(s);
                w
            })
        }
    } else {
        None
    };

    // destroy the dummy context
    unsafe {
        wglMakeCurrent(HDC::NULL, HGLRC::NULL);
        wglDeleteContext(dummy_context);
    }

    // set the new pixel format. if transparency is not available, this will fallback to the default PFD
    unsafe {
        DescribePixelFormat(hDC, transparent_opengl_pixelformat_index, mem::size_of::<PIXELFORMATDESCRIPTOR>() as u32, &mut pfd);
        if !SetPixelFormat(hDC, transparent_opengl_pixelformat_index, &pfd).as_bool() {
            ReleaseDC(hwnd, hDC);
            // DestroyWindow(hwnd);
            return Err(NoMatchingPixelFormat(HRESULT::from_thread()));
        }
    }

    // https://www.khronos.org/registry/OpenGL/extensions/ARB/WGL_ARB_create_context.txt
    const WGL_CONTEXT_MAJOR_VERSION_ARB: i32 = 0x2091;
    const WGL_CONTEXT_MINOR_VERSION_ARB: i32 = 0x2092;

    // Create OpenGL 3.1 context
    let context_attribs = [
        WGL_CONTEXT_MAJOR_VERSION_ARB, 3,
        WGL_CONTEXT_MINOR_VERSION_ARB, 1,
        0, 0
    ];

    let hRC = match CreateContextAttribsARB {
        Some(wglarb_CreateContextAttribsARB) => unsafe {
            wglarb_CreateContextAttribsARB(hDC, HGLRC::NULL, &context_attribs[..])
        },
        None => unsafe { wglCreateContext(hDC) },
    };

    if hRC.is_null() {
        unsafe {
            ReleaseDC(hwnd, hDC);
            // DestroyWindow(hwnd);
        }
        return Err(OpenGLNotAvailable(HRESULT::from_thread()));
    }

    // store hRC in the windows application data
    if let Ok(mut lock) = context_arc.try_borrow() {
        let lock = &mut *lock;
        if let Some(context) = lock.as_mut() {
            unsafe { wglDeleteContext(context.hrc); }
        }
        unsafe { wglMakeCurrent(hDC, hRC) };
        let loaded_function_pointers = unsafe { GlFns::load_with(|func_name| {
            let address = wglGetProcAddress(func_name);
            match address {
                Some(s) => mem::transmute(s),
                None => {

                    // OpenGL 1.1 functions (such as glViewport) are missing, load
                    // them from opengl32.dll

                    // NOTE: regular GetProcAddress, not wglGetProcAddress!
                    match GetProcAddress(opengl32_dll, func_name) {
                        Some(s) => mem::transmute(s),
                        None => ptr::null(),
                    }
                },
            }
        }) };

        unsafe { wglMakeCurrent(HDC::NULL, HGLRC::NULL) };

        *lock = Some(WindowsGlContext {
            hrc: hRC,
            opengl32_dll,
            gl: loaded_function_pointers,
        });
    } else {
        unsafe {
            ReleaseDC(hwnd, hDC);
            // DestroyWindow(hwnd);
        }
        return Err(FailedToStoreContext(HRESULT::from_thread()));
    }

    unsafe {
        ReleaseDC(hwnd, hDC);
    }

    return Ok(());
}

fn get_transparent_pixel_format_index(hDC: HDC) -> Option<i32> {

    // https://www.khronos.org/registry/OpenGL/api/GL/wglext.h
    const WGL_DRAW_TO_WINDOW_ARB: i32 = 0x2001;
    const WGL_DOUBLE_BUFFER_ARB: i32 = 0x2011;
    const WGL_SUPPORT_OPENGL_ARB: i32 = 0x2010;
    const WGL_PIXEL_TYPE_ARB: i32 = 0x2013;
    const WGL_TYPE_RGBA_ARB: i32 = 0x202B;
    const WGL_TRANSPARENT_ARB: i32 = 0x200A;
    const WGL_COLOR_BITS_ARB: i32 = 0x2014;
    const WGL_RED_BITS_ARB: i32 = 0x2015;
    const WGL_GREEN_BITS_ARB: i32 = 0x2017;
    const WGL_BLUE_BITS_ARB: i32 = 0x2019;
    const WGL_ALPHA_BITS_ARB: i32 = 0x201B;
    const WGL_DEPTH_BITS_ARB: i32 = 0x2022;
    const WGL_STENCIL_BITS_ARB: i32 = 0x2023;

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

    let mut pixel_format = 0;
    let mut num_pixel_formats = 0;

    let wglarb_ChoosePixelFormatARB = unsafe { wglGetProcAddress("wglChoosePixelFormatARB").or(wglGetProcAddress("wglChoosePixelFormatEXT")) }?;
    let wglarb_ChoosePixelFormatARB: unsafe extern "system" fn(HDC, *const [i32], *const f32, u32, &mut i32, &mut u32) -> BOOL = unsafe { mem::transmute(wglarb_ChoosePixelFormatARB) };
    let choose_pixel_format_result = unsafe { wglarb_ChoosePixelFormatARB(
        hDC, &attribs[..], ptr::null(), 1, &mut pixel_format, &mut num_pixel_formats
    ) };

    if !choose_pixel_format_result.as_bool() {
        return None; // wglarb_ChoosePixelFormatARB failed
    }

    // pixel format is now the index of the PIXELFORMATDESCRIPTOR
    // that can handle a transparent OpenGL context
    if num_pixel_formats == 0 {
        None
    } else {
        Some(pixel_format)
    }
}

unsafe fn set_window_transparency_and_region(hwnd: HWND, transparent: bool, no_decorations: bool) -> windows::Result<()> {

    if !transparent && !no_decorations {
        return Ok(()); // nothing to do
    }

    let mut os_vinfo = OSVERSIONINFOW::default();
    GetVersionExW(&mut os_vinfo);
    if os_vinfo.dwMajorVersion < 6 {
        return Ok(()); // compositing not supported
    }

    let hDwmAPI_DLL = LoadLibraryW("dwmapi.dll");
    if hDwmAPI_DLL.is_null() {
        return Ok(()); // dwnapi.dll not found
    }

    if transparent {
        if let Some(DwmEnableBlurBehindWindow) = GetProcAddress(hDwmAPI_DLL, "DwmEnableBlurBehindWindow") {
            let DwmEnableBlurBehindWindow: unsafe extern "system" fn(HWND, &DWM_BLURBEHIND) -> HRESULT = mem::transmute(DwmEnableBlurBehindWindow);
            DwmEnableBlurBehindWindow(hwnd, &DWM_BLURBEHIND {
                dwFlags: DWM_BB_ENABLE,
                fEnable: BOOL(TRUE),
                hRgnBlur: HRGN::NULL,
                fTransitionOnMaximized: BOOL(TRUE),
            });
        }
    }

    if no_decorations {
        if let Some(DwmExtendFrameIntoClientArea) = GetProcAddress(hDwmAPI_DLL, "DwmExtendFrameIntoClientArea") {
            let DwmExtendFrameIntoClientArea: unsafe extern "system" fn(HWND, &MARGINS) -> HRESULT = mem::transmute(DwmExtendFrameIntoClientArea);

            DwmExtendFrameIntoClientArea(hwnd, &MARGINS {
                cxLeftWidth: -1,
                cxRightWidth: -1,
                cyTopHeight: -1,
                cyBottomHeight: -1,
            });
        }
    }

    FreeLibrary(hDwmAPI_DLL);

    Ok(())
}

unsafe extern "system" fn WindowProc(hwnd: HWND, msg: u32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {

    if msg == WM_NCCREATE {
        let createstruct: *mut CREATESTRUCTW = mem::transmute(lparam);
        SetWindowLongPtrW(hwnd, GWLP_USERDATA, mem::transmute((*createstruct).lpCreateParams));
    } else {
        let window_data: *mut WindowsWindowData = mem::transmute(GetWindowLongPtrW(hwnd, GWLP_USERDATA));
        let window_data: &mut WindowsWindowData = &mut *window_data;

        match msg {
            WM_CREATE => { },
            WM_NCMOUSELEAVE => {
                // cursor_needs_setting = TRUE;
            },
            WM_MOUSEMOVE => {
                // if( cursor_needs_setting ) {
                //     SetClassLongPtr(hwnd, GCLP_HCURSOR, (LONG_PTR)LoadCursor(NULL, IDC_ARROW));
                //     cursor_needs_setting = FALSE;
                // }
            },
            WM_DESTROY => {
                // destruct the window data
                let window_data = Box::from_raw(GetWindowLongPtrW(hwnd, GWLP_USERDATA) as *mut WindowsWindowData);
                wglMakeCurrent(HDC::NULL, HGLRC::NULL);
                if let Ok(mut lock) = window_data.gl_context.try_borrow() {
                    let lock = &mut *lock;
                    if let Some(context) = lock.as_mut() {
                        wglDeleteContext(context.hrc);
                        FreeLibrary(context.opengl32_dll);
                    }
                }
                PostQuitMessage(0);
            },
            WM_PAINT => {
                let hDC = GetDC(hwnd);
                // if hDC.is_mull();

                let mut rect = RECT::default();
                GetClientRect(hwnd, &mut rect);

                if let Ok(mut lock) = window_data.gl_context.try_borrow() {
                    let lock = &mut *lock;
                    if let Some(context) = lock.as_mut() {
                        wglMakeCurrent(hDC, context.hrc);

                        context.gl.viewport(0, 0, rect.right, rect.bottom);
                        context.gl.clear_color(0.0, 0.0, 0.0, 0.0);
                        context.gl.clear_depth(1.0);
                        context.gl.clear(gleam::gl::COLOR_BUFFER_BIT | gleam::gl::DEPTH_BUFFER_BIT);

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
                        context.gl.finish(); // TODO: OpenGL 3?

                        wglMakeCurrent(HDC::NULL, HGLRC::NULL);
                    } else {
                        // TODO: software rendering
                    }
                } else {
                    // TODO: software rendering
                }

                ReleaseDC(hwnd, hDC);
            },
            _ => { }
        }
    }

    DefWindowProcW(hwnd, msg, wparam, lparam)
}

#[no_mangle] // don't mangle the name of this function
pub extern "C" fn mainCRTStartup() -> ! {
    unsafe { ExitProcess(128) }
}

/*

fn main() -> windows::Result<()> {

    let (hwnd, context_arc) = create_window()?;

    if let Err(_) = create_opengl_context(hwnd, context_arc) {
        // log error but do not panic: opengl context is not required for running
        // println!("ERROR initializing OpenGL, continuing in software rendering mode: {}", e.message());
    }

    if let Err(_) = unsafe { set_window_transparency_and_region(hwnd, /* transparent window */ true, /* no decorations */ true) } {
        // println!("ERROR setting transparency and region: {}", e.message());
    }

    let mut msg = MSG::default();

    unsafe {
        ShowWindow(hwnd, SW_SHOWNORMAL | SW_MAXIMIZE);
        // AnimateWindow(hwnd, 200, AW_ACTIVATE | AW_VER_POSITIVE);

        while GetMessageW(&mut msg, hwnd, 0, 0).0 > 0 {
            TranslateMessage(&msg);
            DispatchMessageW(&msg);
        }
    }

    if msg.wParam.0 == 0 {
        Ok(())
    } else {
        Err(windows::Error::new(windows::HRESULT::from_thread(), "Error while running application"))
    }
}

*/