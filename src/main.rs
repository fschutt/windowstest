#![allow(non_snake_case)]
#![windows_subsystem = "windows"]
// #![no_std]
// #![no_main]

extern crate core;
#[macro_use]
extern crate alloc;

use core::{ptr, mem};
use core::panic::PanicInfo;
use core::cell::RefCell;

use alloc::rc::Rc;
use alloc::vec::Vec;
use alloc::boxed::Box;

use libc_alloc::LibcAlloc;

use gleam::gl::GlFns;
use gleam::gl::Gl;

#[global_allocator]
static ALLOCATOR: LibcAlloc = LibcAlloc;

/*
#[panic_handler]
fn panic(_panic: &PanicInfo<'_>) -> ! { loop {} }
*/

use winapi::{
    ctypes::c_void,
    shared::{
        windef::{HWND, RECT, HGLRC, HDC, },
        ntdef::{PWSTR, HRESULT},
        minwindef::{LPARAM, WPARAM, LRESULT, BOOL, HINSTANCE, HRGN, TRUE},
    },
    um::{
        errhandlingapi::GetLastError,
        libloaderapi::{LoadLibraryW, FreeLibrary, GetModuleHandleW, GetProcAddress},
        winuser::{
            RegisterClassW, ShowWindow, CreateWindowExW, DefWindowProcW,
            GetMessageW, TranslateMessage, DispatchMessageW, GetClientRect,
            PostQuitMessage, GetWindowLongPtrW, SetWindowLongPtrW,
            WNDCLASSW, CS_HREDRAW, CS_VREDRAW, CS_OWNDC,
            WS_OVERLAPPEDWINDOW, WS_POPUP, CW_USEDEFAULT, SW_MAXIMIZE, SW_SHOWNORMAL,
            MSG, WS_EX_APPWINDOW, CREATESTRUCTW, GWLP_USERDATA,
            WM_NCCREATE, WM_CREATE, WM_NCMOUSELEAVE, WM_MOUSEMOVE, WM_PAINT, WM_DESTROY,

            GetDC, ReleaseDC,
        },
        uxtheme::MARGINS,
        dwmapi::{DWM_BLURBEHIND, DWM_BB_ENABLE},
        winnt::OSVERSIONINFOW,
        sysinfoapi::GetVersionExW,
        wingdi::{
            PIXELFORMATDESCRIPTOR,
            wglMakeCurrent, wglDeleteContext, wglCreateContext, wglGetProcAddress,
            ChoosePixelFormat, SetPixelFormat, SwapBuffers, DescribePixelFormat,

            PFD_DRAW_TO_WINDOW, PFD_SUPPORT_OPENGL, PFD_DOUBLEBUFFER,
            PFD_TYPE_RGBA, PFD_MAIN_PLANE
        },
    }
};

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

    let app_instance = unsafe { GetModuleHandleW(ptr::null_mut()) };
    if app_instance.is_null() {
        return Err(FailedToCreateHInstance(get_last_error()));
    }

    let mut class_name = encode_wide(CLASS_NAME);
    let mut window_title = encode_wide("Learn to program Windows");

    // Register the application class
    let mut wc: WNDCLASSW = unsafe { mem::zeroed() };
    wc.style = CS_HREDRAW | CS_VREDRAW | CS_OWNDC;
    wc.hInstance = app_instance;
    wc.lpszClassName = class_name.as_mut_ptr();
    wc.lpfnWndProc = Some(WindowProc);

    // RegisterClass can fail if the same class is registered twice,
    // error can be ignored
    unsafe { RegisterClassW(&wc) };

    let context_arc = Rc::new(RefCell::new(None));
    let window_data = Box::new(WindowsWindowData {
        gl_context: context_arc.clone()
    });

    // Create the window.
    let hwnd = unsafe { CreateWindowExW(
        WS_EX_APPWINDOW,                    // Optional window styles
        class_name.as_mut_ptr(),                // Window class
        window_title.as_mut_ptr(),              // Window text
        WS_OVERLAPPEDWINDOW | WS_POPUP,     // Window style

        // Size and position
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,

        ptr::null_mut(),         // Parent window
        ptr::null_mut(),        // Menu
        app_instance,       // Instance handle
        Box::leak(window_data) as *mut WindowsWindowData as *mut c_void,    // Additional application data
    ) };

    if hwnd.is_null() {
        Err(FailedToCreateHWND(get_last_error()))
    } else {
        Ok((hwnd, context_arc))
    }
}

fn encode_wide(input: &str) -> Vec<u16> {
    input
    .encode_utf16()
    .chain(Some(0).into_iter())
    .collect::<Vec<_>>()
}

fn encode_ascii(input: &str) -> Vec<i8> {
    input
    .chars()
    .filter(|c| c.is_ascii())
    .map(|c| c as i8)
    .chain(Some(0).into_iter())
    .collect::<Vec<_>>()
}

fn get_last_error() -> HRESULT {
   (unsafe { GetLastError() }) as HRESULT
}

fn print(s: &str) {
    println!("{}", s);
}

// function can fail: creates an OpenGL context on the HWND, stores the context on the window-associated data
fn create_opengl_context(hwnd: HWND, context_arc: Rc<RefCell<Option<WindowsGlContext>>>) -> Result<(), WindowsOpenGlError> {

    use self::WindowsOpenGlError::*;

    // -- window created, now create OpenGL context

    let mut dll_name = encode_wide("opengl32.dll");
    let opengl32_dll = unsafe { LoadLibraryW(dll_name.as_mut_ptr()) };
    if opengl32_dll.is_null() { return Err(OpenGL32DllNotFound(get_last_error())); }

    // Get DC
    let hDC = unsafe { GetDC(hwnd) };
    if hDC.is_null()  {
        // unsafe { DestroyWindow(hwnd) };
        return Err(FailedToGetDC(get_last_error()));
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
        if !SetPixelFormat(hDC, default_pixel_format, &pfd) == TRUE {
            // can't even set the default fallback pixel format: no OpenGL possible
            ReleaseDC(hwnd, hDC);
            // DestroyWindow(hwnd);
            return Err(NoMatchingPixelFormat(get_last_error()));
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
        let mut func_name = encode_ascii("wglCreateContextAttribsARB");
        unsafe {
            let proc_address = wglGetProcAddress(func_name.as_mut_ptr());
            if proc_address == ptr::null_mut() {
                None
            } else {
                let w: unsafe extern "system" fn(HDC, HGLRC, *const [i32]) -> HGLRC = mem::transmute(proc_address);
                Some(w)
            }
        }
    } else {
        None
    };

    // destroy the dummy context
    unsafe {
        wglMakeCurrent(ptr::null_mut(), ptr::null_mut());
        wglDeleteContext(dummy_context);
    }

    // set the new pixel format. if transparency is not available, this will fallback to the default PFD
    unsafe {
        DescribePixelFormat(hDC, transparent_opengl_pixelformat_index, mem::size_of::<PIXELFORMATDESCRIPTOR>() as u32, &mut pfd);
        if SetPixelFormat(hDC, transparent_opengl_pixelformat_index, &pfd) != TRUE {
            ReleaseDC(hwnd, hDC);
            // DestroyWindow(hwnd);
            return Err(NoMatchingPixelFormat(get_last_error()));
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
            wglarb_CreateContextAttribsARB(hDC, ptr::null_mut(), &context_attribs[..])
        },
        None => unsafe { wglCreateContext(hDC) },
    };

    if hRC.is_null() {
        unsafe {
            ReleaseDC(hwnd, hDC);
            // DestroyWindow(hwnd);
        }
        return Err(OpenGLNotAvailable(get_last_error()));
    }

    // store hRC in the windows application data
    if let Ok(mut lock) = context_arc.try_borrow_mut() {

        let lock = &mut *lock;
        if let Some(context) = lock.as_mut() {
            unsafe { wglDeleteContext(context.hrc); }
        }

        unsafe { wglMakeCurrent(hDC, hRC) };

        let loaded = crate::gl::WglContext::initialize(opengl32_dll);

        println!("{:#?}", loaded);

        let loaded_function_pointers = unsafe { GlFns::load_with(|func_name| {
            let mut func_name = encode_ascii(func_name);
            let address = wglGetProcAddress(func_name.as_mut_ptr());
            match address == ptr::null_mut() {
                false => mem::transmute(address),
                true => {

                    // OpenGL 1.1 functions (such as glViewport) are missing, load
                    // them from opengl32.dll

                    // NOTE: regular GetProcAddress, not wglGetProcAddress!
                    let address = GetProcAddress(opengl32_dll, func_name.as_mut_ptr());
                    match address == ptr::null_mut() {
                        false => mem::transmute(address),
                        true => ptr::null_mut(),
                    }
                },
            }
        }) };

        unsafe { wglMakeCurrent(ptr::null_mut(), ptr::null_mut()) };

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
        return Err(FailedToStoreContext(get_last_error()));
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

    let mut func_name_1 = encode_ascii("wglChoosePixelFormatARB");
    let mut func_name_2 = encode_ascii("wglChoosePixelFormatEXT");

    let wgl1_result = unsafe { wglGetProcAddress(func_name_1.as_mut_ptr()) };
    let wgl2_result = unsafe { wglGetProcAddress(func_name_2.as_mut_ptr()) };

    let wglarb_ChoosePixelFormatARB = if wgl1_result != ptr::null_mut() {
        wgl1_result
    } else if wgl2_result != ptr::null_mut() {
        wgl2_result
    } else {
        return None;
    };
    let wglarb_ChoosePixelFormatARB: unsafe extern "system" fn(HDC, *const [i32], *const f32, u32, &mut i32, &mut u32) -> BOOL = unsafe { mem::transmute(wglarb_ChoosePixelFormatARB) };
    let choose_pixel_format_result = unsafe { wglarb_ChoosePixelFormatARB(
        hDC, &attribs[..], ptr::null(), 1, &mut pixel_format, &mut num_pixel_formats
    ) };

    if choose_pixel_format_result != TRUE {
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

enum SetTransparencyError {
    DwmNotSupported,
    DwmApiDllNotFound,
}

unsafe fn set_window_transparency_and_region(hwnd: HWND, transparent: bool, no_decorations: bool) -> Result<(), SetTransparencyError> {

    use self::SetTransparencyError::*;

    if !transparent && !no_decorations {
        return Ok(()); // nothing to do
    }

    let mut os_vinfo: OSVERSIONINFOW = unsafe { mem::zeroed() };
    GetVersionExW(&mut os_vinfo);
    if os_vinfo.dwMajorVersion < 6 {
        return Err(DwmNotSupported); // compositing not supported
    }

    let mut dll_name = encode_wide("dwmapi.dll");
    let hDwmAPI_DLL = LoadLibraryW(dll_name.as_mut_ptr());
    if hDwmAPI_DLL.is_null() {
        return Err(DwmApiDllNotFound); // dwnapi.dll not found
    }

    if transparent {
        let mut func_name = encode_ascii("DwmEnableBlurBehindWindow");
        let DwmEnableBlurBehindWindow = GetProcAddress(hDwmAPI_DLL, func_name.as_mut_ptr());
        if DwmEnableBlurBehindWindow != ptr::null_mut() {
            let DwmEnableBlurBehindWindow: unsafe extern "system" fn(HWND, &DWM_BLURBEHIND) -> HRESULT = mem::transmute(DwmEnableBlurBehindWindow);
            DwmEnableBlurBehindWindow(hwnd, &DWM_BLURBEHIND {
                dwFlags: DWM_BB_ENABLE,
                fEnable: TRUE,
                hRgnBlur: ptr::null_mut(),
                fTransitionOnMaximized: TRUE,
            });
            // TODO: check error
        }
    }

    if no_decorations {
        let mut func_name = encode_ascii("DwmExtendFrameIntoClientArea");
        let DwmExtendFrameIntoClientArea = GetProcAddress(hDwmAPI_DLL, func_name.as_mut_ptr());
        if DwmExtendFrameIntoClientArea != ptr::null_mut() {
            let DwmExtendFrameIntoClientArea: unsafe extern "system" fn(HWND, &MARGINS) -> HRESULT = mem::transmute(DwmExtendFrameIntoClientArea);

            DwmExtendFrameIntoClientArea(hwnd, &MARGINS {
                cxLeftWidth: -1,
                cxRightWidth: -1,
                cyTopHeight: -1,
                cyBottomHeight: -1,
            });

            // TODO: check error
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
                wglMakeCurrent(ptr::null_mut(), ptr::null_mut());
                if let Ok(mut lock) = window_data.gl_context.try_borrow_mut() {
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

                let mut rect: RECT = mem::zeroed();
                GetClientRect(hwnd, &mut rect);

                if let Ok(mut lock) = window_data.gl_context.try_borrow_mut() {
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

                        wglMakeCurrent(ptr::null_mut(), ptr::null_mut());
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

mod gl {

    use super::{wglGetProcAddress, GetProcAddress,HINSTANCE};
    use core::{ptr, mem};
    use crate::encode_ascii;

    #[allow(non_camel_case_types)]
    pub mod ctypes {
        pub enum c_void {}
        pub type c_char = i8;
        pub type c_schar = i8;
        pub type c_uchar = u8;
        pub type c_short = i16;
        pub type c_ushort = u16;
        pub type c_int = i32;
        pub type c_uint = u32;
        pub type c_long = i32;
        pub type c_ulong = u32;
        pub type c_longlong = i64;
        pub type c_ulonglong = u64;
        pub type c_float = f32;
        pub type c_double = f64;
        pub type __int8 = i8;
        pub type __uint8 = u8;
        pub type __int16 = i16;
        pub type __uint16 = u16;
        pub type __int32 = i32;
        pub type __uint32 = u32;
        pub type __int64 = i64;
        pub type __uint64 = u64;
        pub type wchar_t = u16;
    }

    pub use self::ctypes::*;

    /// Typedef for an OpenGL handle
    pub type GLuint = u32;
    pub type GLint = i32;
    pub type GLint64 = i64;
    pub type GLuint64 = u64;
    pub type GLenum = u32;
    pub type GLintptr = isize;
    pub type GLboolean = u8;
    pub type GLsizeiptr = isize;
    pub type GLvoid = core::ffi::c_void;
    pub type GLbitfield = u32;
    pub type GLsizei = i32;
    pub type GLclampf = f32;
    pub type GLfloat = f32;
    pub type GLeglImageOES = *const c_void;

    #[derive(Debug)]
    pub struct WglContext {
        glAccum: *mut c_void,
        glActiveTexture: *mut c_void,
        glAlphaFunc: *mut c_void,
        glAreTexturesResident: *mut c_void,
        glArrayElement: *mut c_void,
        glAttachShader: *mut c_void,
        glBegin: *mut c_void,
        glBeginConditionalRender: *mut c_void,
        glBeginQuery: *mut c_void,
        glBeginTransformFeedback: *mut c_void,
        glBindAttribLocation: *mut c_void,
        glBindBuffer: *mut c_void,
        glBindBufferBase: *mut c_void,
        glBindBufferRange: *mut c_void,
        glBindFragDataLocation: *mut c_void,
        glBindFragDataLocationIndexed: *mut c_void,
        glBindFramebuffer: *mut c_void,
        glBindRenderbuffer: *mut c_void,
        glBindSampler: *mut c_void,
        glBindTexture: *mut c_void,
        glBindVertexArray: *mut c_void,
        glBindVertexArrayAPPLE: *mut c_void,
        glBitmap: *mut c_void,
        glBlendBarrierKHR: *mut c_void,
        glBlendColor: *mut c_void,
        glBlendEquation: *mut c_void,
        glBlendEquationSeparate: *mut c_void,
        glBlendFunc: *mut c_void,
        glBlendFuncSeparate: *mut c_void,
        glBlitFramebuffer: *mut c_void,
        glBufferData: *mut c_void,
        glBufferStorage: *mut c_void,
        glBufferSubData: *mut c_void,
        glCallList: *mut c_void,
        glCallLists: *mut c_void,
        glCheckFramebufferStatus: *mut c_void,
        glClampColor: *mut c_void,
        glClear: *mut c_void,
        glClearAccum: *mut c_void,
        glClearBufferfi: *mut c_void,
        glClearBufferfv: *mut c_void,
        glClearBufferiv: *mut c_void,
        glClearBufferuiv: *mut c_void,
        glClearColor: *mut c_void,
        glClearDepth: *mut c_void,
        glClearIndex: *mut c_void,
        glClearStencil: *mut c_void,
        glClientActiveTexture: *mut c_void,
        glClientWaitSync: *mut c_void,
        glClipPlane: *mut c_void,
        glColor3b: *mut c_void,
        glColor3bv: *mut c_void,
        glColor3d: *mut c_void,
        glColor3dv: *mut c_void,
        glColor3f: *mut c_void,
        glColor3fv: *mut c_void,
        glColor3i: *mut c_void,
        glColor3iv: *mut c_void,
        glColor3s: *mut c_void,
        glColor3sv: *mut c_void,
        glColor3ub: *mut c_void,
        glColor3ubv: *mut c_void,
        glColor3ui: *mut c_void,
        glColor3uiv: *mut c_void,
        glColor3us: *mut c_void,
        glColor3usv: *mut c_void,
        glColor4b: *mut c_void,
        glColor4bv: *mut c_void,
        glColor4d: *mut c_void,
        glColor4dv: *mut c_void,
        glColor4f: *mut c_void,
        glColor4fv: *mut c_void,
        glColor4i: *mut c_void,
        glColor4iv: *mut c_void,
        glColor4s: *mut c_void,
        glColor4sv: *mut c_void,
        glColor4ub: *mut c_void,
        glColor4ubv: *mut c_void,
        glColor4ui: *mut c_void,
        glColor4uiv: *mut c_void,
        glColor4us: *mut c_void,
        glColor4usv: *mut c_void,
        glColorMask: *mut c_void,
        glColorMaski: *mut c_void,
        glColorMaterial: *mut c_void,
        glColorP3ui: *mut c_void,
        glColorP3uiv: *mut c_void,
        glColorP4ui: *mut c_void,
        glColorP4uiv: *mut c_void,
        glColorPointer: *mut c_void,
        glCompileShader: *mut c_void,
        glCompressedTexImage1D: *mut c_void,
        glCompressedTexImage2D: *mut c_void,
        glCompressedTexImage3D: *mut c_void,
        glCompressedTexSubImage1D: *mut c_void,
        glCompressedTexSubImage2D: *mut c_void,
        glCompressedTexSubImage3D: *mut c_void,
        glCopyBufferSubData: *mut c_void,
        glCopyImageSubData: *mut c_void,
        glCopyPixels: *mut c_void,
        glCopyTexImage1D: *mut c_void,
        glCopyTexImage2D: *mut c_void,
        glCopyTexSubImage1D: *mut c_void,
        glCopyTexSubImage2D: *mut c_void,
        glCopyTexSubImage3D: *mut c_void,
        glCreateProgram: *mut c_void,
        glCreateShader: *mut c_void,
        glCullFace: *mut c_void,
        glDebugMessageCallback: *mut c_void,
        glDebugMessageCallbackKHR: *mut c_void,
        glDebugMessageControl: *mut c_void,
        glDebugMessageControlKHR: *mut c_void,
        glDebugMessageInsert: *mut c_void,
        glDebugMessageInsertKHR: *mut c_void,
        glDeleteBuffers: *mut c_void,
        glDeleteFencesAPPLE: *mut c_void,
        glDeleteFramebuffers: *mut c_void,
        glDeleteLists: *mut c_void,
        glDeleteProgram: *mut c_void,
        glDeleteQueries: *mut c_void,
        glDeleteRenderbuffers: *mut c_void,
        glDeleteSamplers: *mut c_void,
        glDeleteShader: *mut c_void,
        glDeleteSync: *mut c_void,
        glDeleteTextures: *mut c_void,
        glDeleteVertexArrays: *mut c_void,
        glDeleteVertexArraysAPPLE: *mut c_void,
        glDepthFunc: *mut c_void,
        glDepthMask: *mut c_void,
        glDepthRange: *mut c_void,
        glDetachShader: *mut c_void,
        glDisable: *mut c_void,
        glDisableClientState: *mut c_void,
        glDisableVertexAttribArray: *mut c_void,
        glDisablei: *mut c_void,
        glDrawArrays: *mut c_void,
        glDrawArraysInstanced: *mut c_void,
        glDrawBuffer: *mut c_void,
        glDrawBuffers: *mut c_void,
        glDrawElements: *mut c_void,
        glDrawElementsBaseVertex: *mut c_void,
        glDrawElementsInstanced: *mut c_void,
        glDrawElementsInstancedBaseVertex: *mut c_void,
        glDrawPixels: *mut c_void,
        glDrawRangeElements: *mut c_void,
        glDrawRangeElementsBaseVertex: *mut c_void,
        glEdgeFlag: *mut c_void,
        glEdgeFlagPointer: *mut c_void,
        glEdgeFlagv: *mut c_void,
        glEnable: *mut c_void,
        glEnableClientState: *mut c_void,
        glEnableVertexAttribArray: *mut c_void,
        glEnablei: *mut c_void,
        glEnd: *mut c_void,
        glEndConditionalRender: *mut c_void,
        glEndList: *mut c_void,
        glEndQuery: *mut c_void,
        glEndTransformFeedback: *mut c_void,
        glEvalCoord1d: *mut c_void,
        glEvalCoord1dv: *mut c_void,
        glEvalCoord1f: *mut c_void,
        glEvalCoord1fv: *mut c_void,
        glEvalCoord2d: *mut c_void,
        glEvalCoord2dv: *mut c_void,
        glEvalCoord2f: *mut c_void,
        glEvalCoord2fv: *mut c_void,
        glEvalMesh1: *mut c_void,
        glEvalMesh2: *mut c_void,
        glEvalPoint1: *mut c_void,
        glEvalPoint2: *mut c_void,
        glFeedbackBuffer: *mut c_void,
        glFenceSync: *mut c_void,
        glFinish: *mut c_void,
        glFinishFenceAPPLE: *mut c_void,
        glFinishObjectAPPLE: *mut c_void,
        glFlush: *mut c_void,
        glFlushMappedBufferRange: *mut c_void,
        glFogCoordPointer: *mut c_void,
        glFogCoordd: *mut c_void,
        glFogCoorddv: *mut c_void,
        glFogCoordf: *mut c_void,
        glFogCoordfv: *mut c_void,
        glFogf: *mut c_void,
        glFogfv: *mut c_void,
        glFogi: *mut c_void,
        glFogiv: *mut c_void,
        glFramebufferRenderbuffer: *mut c_void,
        glFramebufferTexture: *mut c_void,
        glFramebufferTexture1D: *mut c_void,
        glFramebufferTexture2D: *mut c_void,
        glFramebufferTexture3D: *mut c_void,
        glFramebufferTextureLayer: *mut c_void,
        glFrontFace: *mut c_void,
        glFrustum: *mut c_void,
        glGenBuffers: *mut c_void,
        glGenFencesAPPLE: *mut c_void,
        glGenFramebuffers: *mut c_void,
        glGenLists: *mut c_void,
        glGenQueries: *mut c_void,
        glGenRenderbuffers: *mut c_void,
        glGenSamplers: *mut c_void,
        glGenTextures: *mut c_void,
        glGenVertexArrays: *mut c_void,
        glGenVertexArraysAPPLE: *mut c_void,
        glGenerateMipmap: *mut c_void,
        glGetActiveAttrib: *mut c_void,
        glGetActiveUniform: *mut c_void,
        glGetActiveUniformBlockName: *mut c_void,
        glGetActiveUniformBlockiv: *mut c_void,
        glGetActiveUniformName: *mut c_void,
        glGetActiveUniformsiv: *mut c_void,
        glGetAttachedShaders: *mut c_void,
        glGetAttribLocation: *mut c_void,
        glGetBooleani_v: *mut c_void,
        glGetBooleanv: *mut c_void,
        glGetBufferParameteri64v: *mut c_void,
        glGetBufferParameteriv: *mut c_void,
        glGetBufferPointerv: *mut c_void,
        glGetBufferSubData: *mut c_void,
        glGetClipPlane: *mut c_void,
        glGetCompressedTexImage: *mut c_void,
        glGetDebugMessageLog: *mut c_void,
        glGetDebugMessageLogKHR: *mut c_void,
        glGetDoublev: *mut c_void,
        glGetError: *mut c_void,
        glGetFloatv: *mut c_void,
        glGetFragDataIndex: *mut c_void,
        glGetFragDataLocation: *mut c_void,
        glGetFramebufferAttachmentParameteriv: *mut c_void,
        glGetInteger64i_v: *mut c_void,
        glGetInteger64v: *mut c_void,
        glGetIntegeri_v: *mut c_void,
        glGetIntegerv: *mut c_void,
        glGetLightfv: *mut c_void,
        glGetLightiv: *mut c_void,
        glGetMapdv: *mut c_void,
        glGetMapfv: *mut c_void,
        glGetMapiv: *mut c_void,
        glGetMaterialfv: *mut c_void,
        glGetMaterialiv: *mut c_void,
        glGetMultisamplefv: *mut c_void,
        glGetObjectLabel: *mut c_void,
        glGetObjectLabelKHR: *mut c_void,
        glGetObjectPtrLabel: *mut c_void,
        glGetObjectPtrLabelKHR: *mut c_void,
        glGetPixelMapfv: *mut c_void,
        glGetPixelMapuiv: *mut c_void,
        glGetPixelMapusv: *mut c_void,
        glGetPointerv: *mut c_void,
        glGetPointervKHR: *mut c_void,
        glGetPolygonStipple: *mut c_void,
        glGetProgramBinary: *mut c_void,
        glGetProgramInfoLog: *mut c_void,
        glGetProgramiv: *mut c_void,
        glGetQueryObjecti64v: *mut c_void,
        glGetQueryObjectiv: *mut c_void,
        glGetQueryObjectui64v: *mut c_void,
        glGetQueryObjectuiv: *mut c_void,
        glGetQueryiv: *mut c_void,
        glGetRenderbufferParameteriv: *mut c_void,
        glGetSamplerParameterIiv: *mut c_void,
        glGetSamplerParameterIuiv: *mut c_void,
        glGetSamplerParameterfv: *mut c_void,
        glGetSamplerParameteriv: *mut c_void,
        glGetShaderInfoLog: *mut c_void,
        glGetShaderSource: *mut c_void,
        glGetShaderiv: *mut c_void,
        glGetString: *mut c_void,
        glGetStringi: *mut c_void,
        glGetSynciv: *mut c_void,
        glGetTexEnvfv: *mut c_void,
        glGetTexEnviv: *mut c_void,
        glGetTexGendv: *mut c_void,
        glGetTexGenfv: *mut c_void,
        glGetTexGeniv: *mut c_void,
        glGetTexImage: *mut c_void,
        glGetTexLevelParameterfv: *mut c_void,
        glGetTexLevelParameteriv: *mut c_void,
        glGetTexParameterIiv: *mut c_void,
        glGetTexParameterIuiv: *mut c_void,
        glGetTexParameterPointervAPPLE: *mut c_void,
        glGetTexParameterfv: *mut c_void,
        glGetTexParameteriv: *mut c_void,
        glGetTransformFeedbackVarying: *mut c_void,
        glGetUniformBlockIndex: *mut c_void,
        glGetUniformIndices: *mut c_void,
        glGetUniformLocation: *mut c_void,
        glGetUniformfv: *mut c_void,
        glGetUniformiv: *mut c_void,
        glGetUniformuiv: *mut c_void,
        glGetVertexAttribIiv: *mut c_void,
        glGetVertexAttribIuiv: *mut c_void,
        glGetVertexAttribPointerv: *mut c_void,
        glGetVertexAttribdv: *mut c_void,
        glGetVertexAttribfv: *mut c_void,
        glGetVertexAttribiv: *mut c_void,
        glHint: *mut c_void,
        glIndexMask: *mut c_void,
        glIndexPointer: *mut c_void,
        glIndexd: *mut c_void,
        glIndexdv: *mut c_void,
        glIndexf: *mut c_void,
        glIndexfv: *mut c_void,
        glIndexi: *mut c_void,
        glIndexiv: *mut c_void,
        glIndexs: *mut c_void,
        glIndexsv: *mut c_void,
        glIndexub: *mut c_void,
        glIndexubv: *mut c_void,
        glInitNames: *mut c_void,
        glInsertEventMarkerEXT: *mut c_void,
        glInterleavedArrays: *mut c_void,
        glInvalidateBufferData: *mut c_void,
        glInvalidateBufferSubData: *mut c_void,
        glInvalidateFramebuffer: *mut c_void,
        glInvalidateSubFramebuffer: *mut c_void,
        glInvalidateTexImage: *mut c_void,
        glInvalidateTexSubImage: *mut c_void,
        glIsBuffer: *mut c_void,
        glIsEnabled: *mut c_void,
        glIsEnabledi: *mut c_void,
        glIsFenceAPPLE: *mut c_void,
        glIsFramebuffer: *mut c_void,
        glIsList: *mut c_void,
        glIsProgram: *mut c_void,
        glIsQuery: *mut c_void,
        glIsRenderbuffer: *mut c_void,
        glIsSampler: *mut c_void,
        glIsShader: *mut c_void,
        glIsSync: *mut c_void,
        glIsTexture: *mut c_void,
        glIsVertexArray: *mut c_void,
        glIsVertexArrayAPPLE: *mut c_void,
        glLightModelf: *mut c_void,
        glLightModelfv: *mut c_void,
        glLightModeli: *mut c_void,
        glLightModeliv: *mut c_void,
        glLightf: *mut c_void,
        glLightfv: *mut c_void,
        glLighti: *mut c_void,
        glLightiv: *mut c_void,
        glLineStipple: *mut c_void,
        glLineWidth: *mut c_void,
        glLinkProgram: *mut c_void,
        glListBase: *mut c_void,
        glLoadIdentity: *mut c_void,
        glLoadMatrixd: *mut c_void,
        glLoadMatrixf: *mut c_void,
        glLoadName: *mut c_void,
        glLoadTransposeMatrixd: *mut c_void,
        glLoadTransposeMatrixf: *mut c_void,
        glLogicOp: *mut c_void,
        glMap1d: *mut c_void,
        glMap1f: *mut c_void,
        glMap2d: *mut c_void,
        glMap2f: *mut c_void,
        glMapBuffer: *mut c_void,
        glMapBufferRange: *mut c_void,
        glMapGrid1d: *mut c_void,
        glMapGrid1f: *mut c_void,
        glMapGrid2d: *mut c_void,
        glMapGrid2f: *mut c_void,
        glMaterialf: *mut c_void,
        glMaterialfv: *mut c_void,
        glMateriali: *mut c_void,
        glMaterialiv: *mut c_void,
        glMatrixMode: *mut c_void,
        glMultMatrixd: *mut c_void,
        glMultMatrixf: *mut c_void,
        glMultTransposeMatrixd: *mut c_void,
        glMultTransposeMatrixf: *mut c_void,
        glMultiDrawArrays: *mut c_void,
        glMultiDrawElements: *mut c_void,
        glMultiDrawElementsBaseVertex: *mut c_void,
        glMultiTexCoord1d: *mut c_void,
        glMultiTexCoord1dv: *mut c_void,
        glMultiTexCoord1f: *mut c_void,
        glMultiTexCoord1fv: *mut c_void,
        glMultiTexCoord1i: *mut c_void,
        glMultiTexCoord1iv: *mut c_void,
        glMultiTexCoord1s: *mut c_void,
        glMultiTexCoord1sv: *mut c_void,
        glMultiTexCoord2d: *mut c_void,
        glMultiTexCoord2dv: *mut c_void,
        glMultiTexCoord2f: *mut c_void,
        glMultiTexCoord2fv: *mut c_void,
        glMultiTexCoord2i: *mut c_void,
        glMultiTexCoord2iv: *mut c_void,
        glMultiTexCoord2s: *mut c_void,
        glMultiTexCoord2sv: *mut c_void,
        glMultiTexCoord3d: *mut c_void,
        glMultiTexCoord3dv: *mut c_void,
        glMultiTexCoord3f: *mut c_void,
        glMultiTexCoord3fv: *mut c_void,
        glMultiTexCoord3i: *mut c_void,
        glMultiTexCoord3iv: *mut c_void,
        glMultiTexCoord3s: *mut c_void,
        glMultiTexCoord3sv: *mut c_void,
        glMultiTexCoord4d: *mut c_void,
        glMultiTexCoord4dv: *mut c_void,
        glMultiTexCoord4f: *mut c_void,
        glMultiTexCoord4fv: *mut c_void,
        glMultiTexCoord4i: *mut c_void,
        glMultiTexCoord4iv: *mut c_void,
        glMultiTexCoord4s: *mut c_void,
        glMultiTexCoord4sv: *mut c_void,
        glMultiTexCoordP1ui: *mut c_void,
        glMultiTexCoordP1uiv: *mut c_void,
        glMultiTexCoordP2ui: *mut c_void,
        glMultiTexCoordP2uiv: *mut c_void,
        glMultiTexCoordP3ui: *mut c_void,
        glMultiTexCoordP3uiv: *mut c_void,
        glMultiTexCoordP4ui: *mut c_void,
        glMultiTexCoordP4uiv: *mut c_void,
        glNewList: *mut c_void,
        glNormal3b: *mut c_void,
        glNormal3bv: *mut c_void,
        glNormal3d: *mut c_void,
        glNormal3dv: *mut c_void,
        glNormal3f: *mut c_void,
        glNormal3fv: *mut c_void,
        glNormal3i: *mut c_void,
        glNormal3iv: *mut c_void,
        glNormal3s: *mut c_void,
        glNormal3sv: *mut c_void,
        glNormalP3ui: *mut c_void,
        glNormalP3uiv: *mut c_void,
        glNormalPointer: *mut c_void,
        glObjectLabel: *mut c_void,
        glObjectLabelKHR: *mut c_void,
        glObjectPtrLabel: *mut c_void,
        glObjectPtrLabelKHR: *mut c_void,
        glOrtho: *mut c_void,
        glPassThrough: *mut c_void,
        glPixelMapfv: *mut c_void,
        glPixelMapuiv: *mut c_void,
        glPixelMapusv: *mut c_void,
        glPixelStoref: *mut c_void,
        glPixelStorei: *mut c_void,
        glPixelTransferf: *mut c_void,
        glPixelTransferi: *mut c_void,
        glPixelZoom: *mut c_void,
        glPointParameterf: *mut c_void,
        glPointParameterfv: *mut c_void,
        glPointParameteri: *mut c_void,
        glPointParameteriv: *mut c_void,
        glPointSize: *mut c_void,
        glPolygonMode: *mut c_void,
        glPolygonOffset: *mut c_void,
        glPolygonStipple: *mut c_void,
        glPopAttrib: *mut c_void,
        glPopClientAttrib: *mut c_void,
        glPopDebugGroup: *mut c_void,
        glPopDebugGroupKHR: *mut c_void,
        glPopGroupMarkerEXT: *mut c_void,
        glPopMatrix: *mut c_void,
        glPopName: *mut c_void,
        glPrimitiveRestartIndex: *mut c_void,
        glPrioritizeTextures: *mut c_void,
        glProgramBinary: *mut c_void,
        glProgramParameteri: *mut c_void,
        glProvokingVertex: *mut c_void,
        glPushAttrib: *mut c_void,
        glPushClientAttrib: *mut c_void,
        glPushDebugGroup: *mut c_void,
        glPushDebugGroupKHR: *mut c_void,
        glPushGroupMarkerEXT: *mut c_void,
        glPushMatrix: *mut c_void,
        glPushName: *mut c_void,
        glQueryCounter: *mut c_void,
        glRasterPos2d: *mut c_void,
        glRasterPos2dv: *mut c_void,
        glRasterPos2f: *mut c_void,
        glRasterPos2fv: *mut c_void,
        glRasterPos2i: *mut c_void,
        glRasterPos2iv: *mut c_void,
        glRasterPos2s: *mut c_void,
        glRasterPos2sv: *mut c_void,
        glRasterPos3d: *mut c_void,
        glRasterPos3dv: *mut c_void,
        glRasterPos3f: *mut c_void,
        glRasterPos3fv: *mut c_void,
        glRasterPos3i: *mut c_void,
        glRasterPos3iv: *mut c_void,
        glRasterPos3s: *mut c_void,
        glRasterPos3sv: *mut c_void,
        glRasterPos4d: *mut c_void,
        glRasterPos4dv: *mut c_void,
        glRasterPos4f: *mut c_void,
        glRasterPos4fv: *mut c_void,
        glRasterPos4i: *mut c_void,
        glRasterPos4iv: *mut c_void,
        glRasterPos4s: *mut c_void,
        glRasterPos4sv: *mut c_void,
        glReadBuffer: *mut c_void,
        glReadPixels: *mut c_void,
        glRectd: *mut c_void,
        glRectdv: *mut c_void,
        glRectf: *mut c_void,
        glRectfv: *mut c_void,
        glRecti: *mut c_void,
        glRectiv: *mut c_void,
        glRects: *mut c_void,
        glRectsv: *mut c_void,
        glRenderMode: *mut c_void,
        glRenderbufferStorage: *mut c_void,
        glRenderbufferStorageMultisample: *mut c_void,
        glRotated: *mut c_void,
        glRotatef: *mut c_void,
        glSampleCoverage: *mut c_void,
        glSampleMaski: *mut c_void,
        glSamplerParameterIiv: *mut c_void,
        glSamplerParameterIuiv: *mut c_void,
        glSamplerParameterf: *mut c_void,
        glSamplerParameterfv: *mut c_void,
        glSamplerParameteri: *mut c_void,
        glSamplerParameteriv: *mut c_void,
        glScaled: *mut c_void,
        glScalef: *mut c_void,
        glScissor: *mut c_void,
        glSecondaryColor3b: *mut c_void,
        glSecondaryColor3bv: *mut c_void,
        glSecondaryColor3d: *mut c_void,
        glSecondaryColor3dv: *mut c_void,
        glSecondaryColor3f: *mut c_void,
        glSecondaryColor3fv: *mut c_void,
        glSecondaryColor3i: *mut c_void,
        glSecondaryColor3iv: *mut c_void,
        glSecondaryColor3s: *mut c_void,
        glSecondaryColor3sv: *mut c_void,
        glSecondaryColor3ub: *mut c_void,
        glSecondaryColor3ubv: *mut c_void,
        glSecondaryColor3ui: *mut c_void,
        glSecondaryColor3uiv: *mut c_void,
        glSecondaryColor3us: *mut c_void,
        glSecondaryColor3usv: *mut c_void,
        glSecondaryColorP3ui: *mut c_void,
        glSecondaryColorP3uiv: *mut c_void,
        glSecondaryColorPointer: *mut c_void,
        glSelectBuffer: *mut c_void,
        glSetFenceAPPLE: *mut c_void,
        glShadeModel: *mut c_void,
        glShaderSource: *mut c_void,
        glShaderStorageBlockBinding: *mut c_void,
        glStencilFunc: *mut c_void,
        glStencilFuncSeparate: *mut c_void,
        glStencilMask: *mut c_void,
        glStencilMaskSeparate: *mut c_void,
        glStencilOp: *mut c_void,
        glStencilOpSeparate: *mut c_void,
        glTestFenceAPPLE: *mut c_void,
        glTestObjectAPPLE: *mut c_void,
        glTexBuffer: *mut c_void,
        glTexCoord1d: *mut c_void,
        glTexCoord1dv: *mut c_void,
        glTexCoord1f: *mut c_void,
        glTexCoord1fv: *mut c_void,
        glTexCoord1i: *mut c_void,
        glTexCoord1iv: *mut c_void,
        glTexCoord1s: *mut c_void,
        glTexCoord1sv: *mut c_void,
        glTexCoord2d: *mut c_void,
        glTexCoord2dv: *mut c_void,
        glTexCoord2f: *mut c_void,
        glTexCoord2fv: *mut c_void,
        glTexCoord2i: *mut c_void,
        glTexCoord2iv: *mut c_void,
        glTexCoord2s: *mut c_void,
        glTexCoord2sv: *mut c_void,
        glTexCoord3d: *mut c_void,
        glTexCoord3dv: *mut c_void,
        glTexCoord3f: *mut c_void,
        glTexCoord3fv: *mut c_void,
        glTexCoord3i: *mut c_void,
        glTexCoord3iv: *mut c_void,
        glTexCoord3s: *mut c_void,
        glTexCoord3sv: *mut c_void,
        glTexCoord4d: *mut c_void,
        glTexCoord4dv: *mut c_void,
        glTexCoord4f: *mut c_void,
        glTexCoord4fv: *mut c_void,
        glTexCoord4i: *mut c_void,
        glTexCoord4iv: *mut c_void,
        glTexCoord4s: *mut c_void,
        glTexCoord4sv: *mut c_void,
        glTexCoordP1ui: *mut c_void,
        glTexCoordP1uiv: *mut c_void,
        glTexCoordP2ui: *mut c_void,
        glTexCoordP2uiv: *mut c_void,
        glTexCoordP3ui: *mut c_void,
        glTexCoordP3uiv: *mut c_void,
        glTexCoordP4ui: *mut c_void,
        glTexCoordP4uiv: *mut c_void,
        glTexCoordPointer: *mut c_void,
        glTexEnvf: *mut c_void,
        glTexEnvfv: *mut c_void,
        glTexEnvi: *mut c_void,
        glTexEnviv: *mut c_void,
        glTexGend: *mut c_void,
        glTexGendv: *mut c_void,
        glTexGenf: *mut c_void,
        glTexGenfv: *mut c_void,
        glTexGeni: *mut c_void,
        glTexGeniv: *mut c_void,
        glTexImage1D: *mut c_void,
        glTexImage2D: *mut c_void,
        glTexImage2DMultisample: *mut c_void,
        glTexImage3D: *mut c_void,
        glTexImage3DMultisample: *mut c_void,
        glTexParameterIiv: *mut c_void,
        glTexParameterIuiv: *mut c_void,
        glTexParameterf: *mut c_void,
        glTexParameterfv: *mut c_void,
        glTexParameteri: *mut c_void,
        glTexParameteriv: *mut c_void,
        glTexStorage1D: *mut c_void,
        glTexStorage2D: *mut c_void,
        glTexStorage3D: *mut c_void,
        glTexSubImage1D: *mut c_void,
        glTexSubImage2D: *mut c_void,
        glTexSubImage3D: *mut c_void,
        glTextureRangeAPPLE: *mut c_void,
        glTransformFeedbackVaryings: *mut c_void,
        glTranslated: *mut c_void,
        glTranslatef: *mut c_void,
        glUniform1f: *mut c_void,
        glUniform1fv: *mut c_void,
        glUniform1i: *mut c_void,
        glUniform1iv: *mut c_void,
        glUniform1ui: *mut c_void,
        glUniform1uiv: *mut c_void,
        glUniform2f: *mut c_void,
        glUniform2fv: *mut c_void,
        glUniform2i: *mut c_void,
        glUniform2iv: *mut c_void,
        glUniform2ui: *mut c_void,
        glUniform2uiv: *mut c_void,
        glUniform3f: *mut c_void,
        glUniform3fv: *mut c_void,
        glUniform3i: *mut c_void,
        glUniform3iv: *mut c_void,
        glUniform3ui: *mut c_void,
        glUniform3uiv: *mut c_void,
        glUniform4f: *mut c_void,
        glUniform4fv: *mut c_void,
        glUniform4i: *mut c_void,
        glUniform4iv: *mut c_void,
        glUniform4ui: *mut c_void,
        glUniform4uiv: *mut c_void,
        glUniformBlockBinding: *mut c_void,
        glUniformMatrix2fv: *mut c_void,
        glUniformMatrix2x3fv: *mut c_void,
        glUniformMatrix2x4fv: *mut c_void,
        glUniformMatrix3fv: *mut c_void,
        glUniformMatrix3x2fv: *mut c_void,
        glUniformMatrix3x4fv: *mut c_void,
        glUniformMatrix4fv: *mut c_void,
        glUniformMatrix4x2fv: *mut c_void,
        glUniformMatrix4x3fv: *mut c_void,
        glUnmapBuffer: *mut c_void,
        glUseProgram: *mut c_void,
        glValidateProgram: *mut c_void,
        glVertex2d: *mut c_void,
        glVertex2dv: *mut c_void,
        glVertex2f: *mut c_void,
        glVertex2fv: *mut c_void,
        glVertex2i: *mut c_void,
        glVertex2iv: *mut c_void,
        glVertex2s: *mut c_void,
        glVertex2sv: *mut c_void,
        glVertex3d: *mut c_void,
        glVertex3dv: *mut c_void,
        glVertex3f: *mut c_void,
        glVertex3fv: *mut c_void,
        glVertex3i: *mut c_void,
        glVertex3iv: *mut c_void,
        glVertex3s: *mut c_void,
        glVertex3sv: *mut c_void,
        glVertex4d: *mut c_void,
        glVertex4dv: *mut c_void,
        glVertex4f: *mut c_void,
        glVertex4fv: *mut c_void,
        glVertex4i: *mut c_void,
        glVertex4iv: *mut c_void,
        glVertex4s: *mut c_void,
        glVertex4sv: *mut c_void,
        glVertexAttrib1d: *mut c_void,
        glVertexAttrib1dv: *mut c_void,
        glVertexAttrib1f: *mut c_void,
        glVertexAttrib1fv: *mut c_void,
        glVertexAttrib1s: *mut c_void,
        glVertexAttrib1sv: *mut c_void,
        glVertexAttrib2d: *mut c_void,
        glVertexAttrib2dv: *mut c_void,
        glVertexAttrib2f: *mut c_void,
        glVertexAttrib2fv: *mut c_void,
        glVertexAttrib2s: *mut c_void,
        glVertexAttrib2sv: *mut c_void,
        glVertexAttrib3d: *mut c_void,
        glVertexAttrib3dv: *mut c_void,
        glVertexAttrib3f: *mut c_void,
        glVertexAttrib3fv: *mut c_void,
        glVertexAttrib3s: *mut c_void,
        glVertexAttrib3sv: *mut c_void,
        glVertexAttrib4Nbv: *mut c_void,
        glVertexAttrib4Niv: *mut c_void,
        glVertexAttrib4Nsv: *mut c_void,
        glVertexAttrib4Nub: *mut c_void,
        glVertexAttrib4Nubv: *mut c_void,
        glVertexAttrib4Nuiv: *mut c_void,
        glVertexAttrib4Nusv: *mut c_void,
        glVertexAttrib4bv: *mut c_void,
        glVertexAttrib4d: *mut c_void,
        glVertexAttrib4dv: *mut c_void,
        glVertexAttrib4f: *mut c_void,
        glVertexAttrib4fv: *mut c_void,
        glVertexAttrib4iv: *mut c_void,
        glVertexAttrib4s: *mut c_void,
        glVertexAttrib4sv: *mut c_void,
        glVertexAttrib4ubv: *mut c_void,
        glVertexAttrib4uiv: *mut c_void,
        glVertexAttrib4usv: *mut c_void,
        glVertexAttribDivisor: *mut c_void,
        glVertexAttribI1i: *mut c_void,
        glVertexAttribI1iv: *mut c_void,
        glVertexAttribI1ui: *mut c_void,
        glVertexAttribI1uiv: *mut c_void,
        glVertexAttribI2i: *mut c_void,
        glVertexAttribI2iv: *mut c_void,
        glVertexAttribI2ui: *mut c_void,
        glVertexAttribI2uiv: *mut c_void,
        glVertexAttribI3i: *mut c_void,
        glVertexAttribI3iv: *mut c_void,
        glVertexAttribI3ui: *mut c_void,
        glVertexAttribI3uiv: *mut c_void,
        glVertexAttribI4bv: *mut c_void,
        glVertexAttribI4i: *mut c_void,
        glVertexAttribI4iv: *mut c_void,
        glVertexAttribI4sv: *mut c_void,
        glVertexAttribI4ubv: *mut c_void,
        glVertexAttribI4ui: *mut c_void,
        glVertexAttribI4uiv: *mut c_void,
        glVertexAttribI4usv: *mut c_void,
        glVertexAttribIPointer: *mut c_void,
        glVertexAttribP1ui: *mut c_void,
        glVertexAttribP1uiv: *mut c_void,
        glVertexAttribP2ui: *mut c_void,
        glVertexAttribP2uiv: *mut c_void,
        glVertexAttribP3ui: *mut c_void,
        glVertexAttribP3uiv: *mut c_void,
        glVertexAttribP4ui: *mut c_void,
        glVertexAttribP4uiv: *mut c_void,
        glVertexAttribPointer: *mut c_void,
        glVertexP2ui: *mut c_void,
        glVertexP2uiv: *mut c_void,
        glVertexP3ui: *mut c_void,
        glVertexP3uiv: *mut c_void,
        glVertexP4ui: *mut c_void,
        glVertexP4uiv: *mut c_void,
        glVertexPointer: *mut c_void,
        glViewport: *mut c_void,
        glWaitSync: *mut c_void,
        glWindowPos2d: *mut c_void,
        glWindowPos2dv: *mut c_void,
        glWindowPos2f: *mut c_void,
        glWindowPos2fv: *mut c_void,
        glWindowPos2i: *mut c_void,
        glWindowPos2iv: *mut c_void,
        glWindowPos2s: *mut c_void,
        glWindowPos2sv: *mut c_void,
        glWindowPos3d: *mut c_void,
        glWindowPos3dv: *mut c_void,
        glWindowPos3f: *mut c_void,
        glWindowPos3fv: *mut c_void,
        glWindowPos3i: *mut c_void,
        glWindowPos3iv: *mut c_void,
        glWindowPos3s: *mut c_void,
        glWindowPos3sv: *mut c_void,
    }

    pub enum WglContextError {
        FunctionNotFound(&'static str)
    }

    fn get_func(s: &str, opengl32_dll: HINSTANCE) -> *mut c_void {
        let mut func_name = encode_ascii(s);
        let addr1 = unsafe { wglGetProcAddress(func_name.as_mut_ptr()) };
        (if addr1 != ptr::null_mut() {
            addr1
        } else {
            unsafe { GetProcAddress(opengl32_dll, func_name.as_mut_ptr()) }
        }) as *mut c_void
    }

    impl WglContext {
        pub fn initialize(opengl32_dll: HINSTANCE) -> Self {
            WglContext {
                glAccum: get_func("glAccum", opengl32_dll),
                glActiveTexture: get_func("glActiveTexture", opengl32_dll),
                glAlphaFunc: get_func("glAlphaFunc", opengl32_dll),
                glAreTexturesResident: get_func("glAreTexturesResident", opengl32_dll),
                glArrayElement: get_func("glArrayElement", opengl32_dll),
                glAttachShader: get_func("glAttachShader", opengl32_dll),
                glBegin: get_func("glBegin", opengl32_dll),
                glBeginConditionalRender: get_func("glBeginConditionalRender", opengl32_dll),
                glBeginQuery: get_func("glBeginQuery", opengl32_dll),
                glBeginTransformFeedback: get_func("glBeginTransformFeedback", opengl32_dll),
                glBindAttribLocation: get_func("glBindAttribLocation", opengl32_dll),
                glBindBuffer: get_func("glBindBuffer", opengl32_dll),
                glBindBufferBase: get_func("glBindBufferBase", opengl32_dll),
                glBindBufferRange: get_func("glBindBufferRange", opengl32_dll),
                glBindFragDataLocation: get_func("glBindFragDataLocation", opengl32_dll),
                glBindFragDataLocationIndexed: get_func("glBindFragDataLocationIndexed", opengl32_dll),
                glBindFramebuffer: get_func("glBindFramebuffer", opengl32_dll),
                glBindRenderbuffer: get_func("glBindRenderbuffer", opengl32_dll),
                glBindSampler: get_func("glBindSampler", opengl32_dll),
                glBindTexture: get_func("glBindTexture", opengl32_dll),
                glBindVertexArray: get_func("glBindVertexArray", opengl32_dll),
                glBindVertexArrayAPPLE: get_func("glBindVertexArrayAPPLE", opengl32_dll),
                glBitmap: get_func("glBitmap", opengl32_dll),
                glBlendBarrierKHR: get_func("glBlendBarrierKHR", opengl32_dll),
                glBlendColor: get_func("glBlendColor", opengl32_dll),
                glBlendEquation: get_func("glBlendEquation", opengl32_dll),
                glBlendEquationSeparate: get_func("glBlendEquationSeparate", opengl32_dll),
                glBlendFunc: get_func("glBlendFunc", opengl32_dll),
                glBlendFuncSeparate: get_func("glBlendFuncSeparate", opengl32_dll),
                glBlitFramebuffer: get_func("glBlitFramebuffer", opengl32_dll),
                glBufferData: get_func("glBufferData", opengl32_dll),
                glBufferStorage: get_func("glBufferStorage", opengl32_dll),
                glBufferSubData: get_func("glBufferSubData", opengl32_dll),
                glCallList: get_func("glCallList", opengl32_dll),
                glCallLists: get_func("glCallLists", opengl32_dll),
                glCheckFramebufferStatus: get_func("glCheckFramebufferStatus", opengl32_dll),
                glClampColor: get_func("glClampColor", opengl32_dll),
                glClear: get_func("glClear", opengl32_dll),
                glClearAccum: get_func("glClearAccum", opengl32_dll),
                glClearBufferfi: get_func("glClearBufferfi", opengl32_dll),
                glClearBufferfv: get_func("glClearBufferfv", opengl32_dll),
                glClearBufferiv: get_func("glClearBufferiv", opengl32_dll),
                glClearBufferuiv: get_func("glClearBufferuiv", opengl32_dll),
                glClearColor: get_func("glClearColor", opengl32_dll),
                glClearDepth: get_func("glClearDepth", opengl32_dll),
                glClearIndex: get_func("glClearIndex", opengl32_dll),
                glClearStencil: get_func("glClearStencil", opengl32_dll),
                glClientActiveTexture: get_func("glClientActiveTexture", opengl32_dll),
                glClientWaitSync: get_func("glClientWaitSync", opengl32_dll),
                glClipPlane: get_func("glClipPlane", opengl32_dll),
                glColor3b: get_func("glColor3b", opengl32_dll),
                glColor3bv: get_func("glColor3bv", opengl32_dll),
                glColor3d: get_func("glColor3d", opengl32_dll),
                glColor3dv: get_func("glColor3dv", opengl32_dll),
                glColor3f: get_func("glColor3f", opengl32_dll),
                glColor3fv: get_func("glColor3fv", opengl32_dll),
                glColor3i: get_func("glColor3i", opengl32_dll),
                glColor3iv: get_func("glColor3iv", opengl32_dll),
                glColor3s: get_func("glColor3s", opengl32_dll),
                glColor3sv: get_func("glColor3sv", opengl32_dll),
                glColor3ub: get_func("glColor3ub", opengl32_dll),
                glColor3ubv: get_func("glColor3ubv", opengl32_dll),
                glColor3ui: get_func("glColor3ui", opengl32_dll),
                glColor3uiv: get_func("glColor3uiv", opengl32_dll),
                glColor3us: get_func("glColor3us", opengl32_dll),
                glColor3usv: get_func("glColor3usv", opengl32_dll),
                glColor4b: get_func("glColor4b", opengl32_dll),
                glColor4bv: get_func("glColor4bv", opengl32_dll),
                glColor4d: get_func("glColor4d", opengl32_dll),
                glColor4dv: get_func("glColor4dv", opengl32_dll),
                glColor4f: get_func("glColor4f", opengl32_dll),
                glColor4fv: get_func("glColor4fv", opengl32_dll),
                glColor4i: get_func("glColor4i", opengl32_dll),
                glColor4iv: get_func("glColor4iv", opengl32_dll),
                glColor4s: get_func("glColor4s", opengl32_dll),
                glColor4sv: get_func("glColor4sv", opengl32_dll),
                glColor4ub: get_func("glColor4ub", opengl32_dll),
                glColor4ubv: get_func("glColor4ubv", opengl32_dll),
                glColor4ui: get_func("glColor4ui", opengl32_dll),
                glColor4uiv: get_func("glColor4uiv", opengl32_dll),
                glColor4us: get_func("glColor4us", opengl32_dll),
                glColor4usv: get_func("glColor4usv", opengl32_dll),
                glColorMask: get_func("glColorMask", opengl32_dll),
                glColorMaski: get_func("glColorMaski", opengl32_dll),
                glColorMaterial: get_func("glColorMaterial", opengl32_dll),
                glColorP3ui: get_func("glColorP3ui", opengl32_dll),
                glColorP3uiv: get_func("glColorP3uiv", opengl32_dll),
                glColorP4ui: get_func("glColorP4ui", opengl32_dll),
                glColorP4uiv: get_func("glColorP4uiv", opengl32_dll),
                glColorPointer: get_func("glColorPointer", opengl32_dll),
                glCompileShader: get_func("glCompileShader", opengl32_dll),
                glCompressedTexImage1D: get_func("glCompressedTexImage1D", opengl32_dll),
                glCompressedTexImage2D: get_func("glCompressedTexImage2D", opengl32_dll),
                glCompressedTexImage3D: get_func("glCompressedTexImage3D", opengl32_dll),
                glCompressedTexSubImage1D: get_func("glCompressedTexSubImage1D", opengl32_dll),
                glCompressedTexSubImage2D: get_func("glCompressedTexSubImage2D", opengl32_dll),
                glCompressedTexSubImage3D: get_func("glCompressedTexSubImage3D", opengl32_dll),
                glCopyBufferSubData: get_func("glCopyBufferSubData", opengl32_dll),
                glCopyImageSubData: get_func("glCopyImageSubData", opengl32_dll),
                glCopyPixels: get_func("glCopyPixels", opengl32_dll),
                glCopyTexImage1D: get_func("glCopyTexImage1D", opengl32_dll),
                glCopyTexImage2D: get_func("glCopyTexImage2D", opengl32_dll),
                glCopyTexSubImage1D: get_func("glCopyTexSubImage1D", opengl32_dll),
                glCopyTexSubImage2D: get_func("glCopyTexSubImage2D", opengl32_dll),
                glCopyTexSubImage3D: get_func("glCopyTexSubImage3D", opengl32_dll),
                glCreateProgram: get_func("glCreateProgram", opengl32_dll),
                glCreateShader: get_func("glCreateShader", opengl32_dll),
                glCullFace: get_func("glCullFace", opengl32_dll),
                glDebugMessageCallback: get_func("glDebugMessageCallback", opengl32_dll),
                glDebugMessageCallbackKHR: get_func("glDebugMessageCallbackKHR", opengl32_dll),
                glDebugMessageControl: get_func("glDebugMessageControl", opengl32_dll),
                glDebugMessageControlKHR: get_func("glDebugMessageControlKHR", opengl32_dll),
                glDebugMessageInsert: get_func("glDebugMessageInsert", opengl32_dll),
                glDebugMessageInsertKHR: get_func("glDebugMessageInsertKHR", opengl32_dll),
                glDeleteBuffers: get_func("glDeleteBuffers", opengl32_dll),
                glDeleteFencesAPPLE: get_func("glDeleteFencesAPPLE", opengl32_dll),
                glDeleteFramebuffers: get_func("glDeleteFramebuffers", opengl32_dll),
                glDeleteLists: get_func("glDeleteLists", opengl32_dll),
                glDeleteProgram: get_func("glDeleteProgram", opengl32_dll),
                glDeleteQueries: get_func("glDeleteQueries", opengl32_dll),
                glDeleteRenderbuffers: get_func("glDeleteRenderbuffers", opengl32_dll),
                glDeleteSamplers: get_func("glDeleteSamplers", opengl32_dll),
                glDeleteShader: get_func("glDeleteShader", opengl32_dll),
                glDeleteSync: get_func("glDeleteSync", opengl32_dll),
                glDeleteTextures: get_func("glDeleteTextures", opengl32_dll),
                glDeleteVertexArrays: get_func("glDeleteVertexArrays", opengl32_dll),
                glDeleteVertexArraysAPPLE: get_func("glDeleteVertexArraysAPPLE", opengl32_dll),
                glDepthFunc: get_func("glDepthFunc", opengl32_dll),
                glDepthMask: get_func("glDepthMask", opengl32_dll),
                glDepthRange: get_func("glDepthRange", opengl32_dll),
                glDetachShader: get_func("glDetachShader", opengl32_dll),
                glDisable: get_func("glDisable", opengl32_dll),
                glDisableClientState: get_func("glDisableClientState", opengl32_dll),
                glDisableVertexAttribArray: get_func("glDisableVertexAttribArray", opengl32_dll),
                glDisablei: get_func("glDisablei", opengl32_dll),
                glDrawArrays: get_func("glDrawArrays", opengl32_dll),
                glDrawArraysInstanced: get_func("glDrawArraysInstanced", opengl32_dll),
                glDrawBuffer: get_func("glDrawBuffer", opengl32_dll),
                glDrawBuffers: get_func("glDrawBuffers", opengl32_dll),
                glDrawElements: get_func("glDrawElements", opengl32_dll),
                glDrawElementsBaseVertex: get_func("glDrawElementsBaseVertex", opengl32_dll),
                glDrawElementsInstanced: get_func("glDrawElementsInstanced", opengl32_dll),
                glDrawElementsInstancedBaseVertex: get_func("glDrawElementsInstancedBaseVertex", opengl32_dll),
                glDrawPixels: get_func("glDrawPixels", opengl32_dll),
                glDrawRangeElements: get_func("glDrawRangeElements", opengl32_dll),
                glDrawRangeElementsBaseVertex: get_func("glDrawRangeElementsBaseVertex", opengl32_dll),
                glEdgeFlag: get_func("glEdgeFlag", opengl32_dll),
                glEdgeFlagPointer: get_func("glEdgeFlagPointer", opengl32_dll),
                glEdgeFlagv: get_func("glEdgeFlagv", opengl32_dll),
                glEnable: get_func("glEnable", opengl32_dll),
                glEnableClientState: get_func("glEnableClientState", opengl32_dll),
                glEnableVertexAttribArray: get_func("glEnableVertexAttribArray", opengl32_dll),
                glEnablei: get_func("glEnablei", opengl32_dll),
                glEnd: get_func("glEnd", opengl32_dll),
                glEndConditionalRender: get_func("glEndConditionalRender", opengl32_dll),
                glEndList: get_func("glEndList", opengl32_dll),
                glEndQuery: get_func("glEndQuery", opengl32_dll),
                glEndTransformFeedback: get_func("glEndTransformFeedback", opengl32_dll),
                glEvalCoord1d: get_func("glEvalCoord1d", opengl32_dll),
                glEvalCoord1dv: get_func("glEvalCoord1dv", opengl32_dll),
                glEvalCoord1f: get_func("glEvalCoord1f", opengl32_dll),
                glEvalCoord1fv: get_func("glEvalCoord1fv", opengl32_dll),
                glEvalCoord2d: get_func("glEvalCoord2d", opengl32_dll),
                glEvalCoord2dv: get_func("glEvalCoord2dv", opengl32_dll),
                glEvalCoord2f: get_func("glEvalCoord2f", opengl32_dll),
                glEvalCoord2fv: get_func("glEvalCoord2fv", opengl32_dll),
                glEvalMesh1: get_func("glEvalMesh1", opengl32_dll),
                glEvalMesh2: get_func("glEvalMesh2", opengl32_dll),
                glEvalPoint1: get_func("glEvalPoint1", opengl32_dll),
                glEvalPoint2: get_func("glEvalPoint2", opengl32_dll),
                glFeedbackBuffer: get_func("glFeedbackBuffer", opengl32_dll),
                glFenceSync: get_func("glFenceSync", opengl32_dll),
                glFinish: get_func("glFinish", opengl32_dll),
                glFinishFenceAPPLE: get_func("glFinishFenceAPPLE", opengl32_dll),
                glFinishObjectAPPLE: get_func("glFinishObjectAPPLE", opengl32_dll),
                glFlush: get_func("glFlush", opengl32_dll),
                glFlushMappedBufferRange: get_func("glFlushMappedBufferRange", opengl32_dll),
                glFogCoordPointer: get_func("glFogCoordPointer", opengl32_dll),
                glFogCoordd: get_func("glFogCoordd", opengl32_dll),
                glFogCoorddv: get_func("glFogCoorddv", opengl32_dll),
                glFogCoordf: get_func("glFogCoordf", opengl32_dll),
                glFogCoordfv: get_func("glFogCoordfv", opengl32_dll),
                glFogf: get_func("glFogf", opengl32_dll),
                glFogfv: get_func("glFogfv", opengl32_dll),
                glFogi: get_func("glFogi", opengl32_dll),
                glFogiv: get_func("glFogiv", opengl32_dll),
                glFramebufferRenderbuffer: get_func("glFramebufferRenderbuffer", opengl32_dll),
                glFramebufferTexture: get_func("glFramebufferTexture", opengl32_dll),
                glFramebufferTexture1D: get_func("glFramebufferTexture1D", opengl32_dll),
                glFramebufferTexture2D: get_func("glFramebufferTexture2D", opengl32_dll),
                glFramebufferTexture3D: get_func("glFramebufferTexture3D", opengl32_dll),
                glFramebufferTextureLayer: get_func("glFramebufferTextureLayer", opengl32_dll),
                glFrontFace: get_func("glFrontFace", opengl32_dll),
                glFrustum: get_func("glFrustum", opengl32_dll),
                glGenBuffers: get_func("glGenBuffers", opengl32_dll),
                glGenFencesAPPLE: get_func("glGenFencesAPPLE", opengl32_dll),
                glGenFramebuffers: get_func("glGenFramebuffers", opengl32_dll),
                glGenLists: get_func("glGenLists", opengl32_dll),
                glGenQueries: get_func("glGenQueries", opengl32_dll),
                glGenRenderbuffers: get_func("glGenRenderbuffers", opengl32_dll),
                glGenSamplers: get_func("glGenSamplers", opengl32_dll),
                glGenTextures: get_func("glGenTextures", opengl32_dll),
                glGenVertexArrays: get_func("glGenVertexArrays", opengl32_dll),
                glGenVertexArraysAPPLE: get_func("glGenVertexArraysAPPLE", opengl32_dll),
                glGenerateMipmap: get_func("glGenerateMipmap", opengl32_dll),
                glGetActiveAttrib: get_func("glGetActiveAttrib", opengl32_dll),
                glGetActiveUniform: get_func("glGetActiveUniform", opengl32_dll),
                glGetActiveUniformBlockName: get_func("glGetActiveUniformBlockName", opengl32_dll),
                glGetActiveUniformBlockiv: get_func("glGetActiveUniformBlockiv", opengl32_dll),
                glGetActiveUniformName: get_func("glGetActiveUniformName", opengl32_dll),
                glGetActiveUniformsiv: get_func("glGetActiveUniformsiv", opengl32_dll),
                glGetAttachedShaders: get_func("glGetAttachedShaders", opengl32_dll),
                glGetAttribLocation: get_func("glGetAttribLocation", opengl32_dll),
                glGetBooleani_v: get_func("glGetBooleani_v", opengl32_dll),
                glGetBooleanv: get_func("glGetBooleanv", opengl32_dll),
                glGetBufferParameteri64v: get_func("glGetBufferParameteri64v", opengl32_dll),
                glGetBufferParameteriv: get_func("glGetBufferParameteriv", opengl32_dll),
                glGetBufferPointerv: get_func("glGetBufferPointerv", opengl32_dll),
                glGetBufferSubData: get_func("glGetBufferSubData", opengl32_dll),
                glGetClipPlane: get_func("glGetClipPlane", opengl32_dll),
                glGetCompressedTexImage: get_func("glGetCompressedTexImage", opengl32_dll),
                glGetDebugMessageLog: get_func("glGetDebugMessageLog", opengl32_dll),
                glGetDebugMessageLogKHR: get_func("glGetDebugMessageLogKHR", opengl32_dll),
                glGetDoublev: get_func("glGetDoublev", opengl32_dll),
                glGetError: get_func("glGetError", opengl32_dll),
                glGetFloatv: get_func("glGetFloatv", opengl32_dll),
                glGetFragDataIndex: get_func("glGetFragDataIndex", opengl32_dll),
                glGetFragDataLocation: get_func("glGetFragDataLocation", opengl32_dll),
                glGetFramebufferAttachmentParameteriv: get_func("glGetFramebufferAttachmentParameteriv", opengl32_dll),
                glGetInteger64i_v: get_func("glGetInteger64i_v", opengl32_dll),
                glGetInteger64v: get_func("glGetInteger64v", opengl32_dll),
                glGetIntegeri_v: get_func("glGetIntegeri_v", opengl32_dll),
                glGetIntegerv: get_func("glGetIntegerv", opengl32_dll),
                glGetLightfv: get_func("glGetLightfv", opengl32_dll),
                glGetLightiv: get_func("glGetLightiv", opengl32_dll),
                glGetMapdv: get_func("glGetMapdv", opengl32_dll),
                glGetMapfv: get_func("glGetMapfv", opengl32_dll),
                glGetMapiv: get_func("glGetMapiv", opengl32_dll),
                glGetMaterialfv: get_func("glGetMaterialfv", opengl32_dll),
                glGetMaterialiv: get_func("glGetMaterialiv", opengl32_dll),
                glGetMultisamplefv: get_func("glGetMultisamplefv", opengl32_dll),
                glGetObjectLabel: get_func("glGetObjectLabel", opengl32_dll),
                glGetObjectLabelKHR: get_func("glGetObjectLabelKHR", opengl32_dll),
                glGetObjectPtrLabel: get_func("glGetObjectPtrLabel", opengl32_dll),
                glGetObjectPtrLabelKHR: get_func("glGetObjectPtrLabelKHR", opengl32_dll),
                glGetPixelMapfv: get_func("glGetPixelMapfv", opengl32_dll),
                glGetPixelMapuiv: get_func("glGetPixelMapuiv", opengl32_dll),
                glGetPixelMapusv: get_func("glGetPixelMapusv", opengl32_dll),
                glGetPointerv: get_func("glGetPointerv", opengl32_dll),
                glGetPointervKHR: get_func("glGetPointervKHR", opengl32_dll),
                glGetPolygonStipple: get_func("glGetPolygonStipple", opengl32_dll),
                glGetProgramBinary: get_func("glGetProgramBinary", opengl32_dll),
                glGetProgramInfoLog: get_func("glGetProgramInfoLog", opengl32_dll),
                glGetProgramiv: get_func("glGetProgramiv", opengl32_dll),
                glGetQueryObjecti64v: get_func("glGetQueryObjecti64v", opengl32_dll),
                glGetQueryObjectiv: get_func("glGetQueryObjectiv", opengl32_dll),
                glGetQueryObjectui64v: get_func("glGetQueryObjectui64v", opengl32_dll),
                glGetQueryObjectuiv: get_func("glGetQueryObjectuiv", opengl32_dll),
                glGetQueryiv: get_func("glGetQueryiv", opengl32_dll),
                glGetRenderbufferParameteriv: get_func("glGetRenderbufferParameteriv", opengl32_dll),
                glGetSamplerParameterIiv: get_func("glGetSamplerParameterIiv", opengl32_dll),
                glGetSamplerParameterIuiv: get_func("glGetSamplerParameterIuiv", opengl32_dll),
                glGetSamplerParameterfv: get_func("glGetSamplerParameterfv", opengl32_dll),
                glGetSamplerParameteriv: get_func("glGetSamplerParameteriv", opengl32_dll),
                glGetShaderInfoLog: get_func("glGetShaderInfoLog", opengl32_dll),
                glGetShaderSource: get_func("glGetShaderSource", opengl32_dll),
                glGetShaderiv: get_func("glGetShaderiv", opengl32_dll),
                glGetString: get_func("glGetString", opengl32_dll),
                glGetStringi: get_func("glGetStringi", opengl32_dll),
                glGetSynciv: get_func("glGetSynciv", opengl32_dll),
                glGetTexEnvfv: get_func("glGetTexEnvfv", opengl32_dll),
                glGetTexEnviv: get_func("glGetTexEnviv", opengl32_dll),
                glGetTexGendv: get_func("glGetTexGendv", opengl32_dll),
                glGetTexGenfv: get_func("glGetTexGenfv", opengl32_dll),
                glGetTexGeniv: get_func("glGetTexGeniv", opengl32_dll),
                glGetTexImage: get_func("glGetTexImage", opengl32_dll),
                glGetTexLevelParameterfv: get_func("glGetTexLevelParameterfv", opengl32_dll),
                glGetTexLevelParameteriv: get_func("glGetTexLevelParameteriv", opengl32_dll),
                glGetTexParameterIiv: get_func("glGetTexParameterIiv", opengl32_dll),
                glGetTexParameterIuiv: get_func("glGetTexParameterIuiv", opengl32_dll),
                glGetTexParameterPointervAPPLE: get_func("glGetTexParameterPointervAPPLE", opengl32_dll),
                glGetTexParameterfv: get_func("glGetTexParameterfv", opengl32_dll),
                glGetTexParameteriv: get_func("glGetTexParameteriv", opengl32_dll),
                glGetTransformFeedbackVarying: get_func("glGetTransformFeedbackVarying", opengl32_dll),
                glGetUniformBlockIndex: get_func("glGetUniformBlockIndex", opengl32_dll),
                glGetUniformIndices: get_func("glGetUniformIndices", opengl32_dll),
                glGetUniformLocation: get_func("glGetUniformLocation", opengl32_dll),
                glGetUniformfv: get_func("glGetUniformfv", opengl32_dll),
                glGetUniformiv: get_func("glGetUniformiv", opengl32_dll),
                glGetUniformuiv: get_func("glGetUniformuiv", opengl32_dll),
                glGetVertexAttribIiv: get_func("glGetVertexAttribIiv", opengl32_dll),
                glGetVertexAttribIuiv: get_func("glGetVertexAttribIuiv", opengl32_dll),
                glGetVertexAttribPointerv: get_func("glGetVertexAttribPointerv", opengl32_dll),
                glGetVertexAttribdv: get_func("glGetVertexAttribdv", opengl32_dll),
                glGetVertexAttribfv: get_func("glGetVertexAttribfv", opengl32_dll),
                glGetVertexAttribiv: get_func("glGetVertexAttribiv", opengl32_dll),
                glHint: get_func("glHint", opengl32_dll),
                glIndexMask: get_func("glIndexMask", opengl32_dll),
                glIndexPointer: get_func("glIndexPointer", opengl32_dll),
                glIndexd: get_func("glIndexd", opengl32_dll),
                glIndexdv: get_func("glIndexdv", opengl32_dll),
                glIndexf: get_func("glIndexf", opengl32_dll),
                glIndexfv: get_func("glIndexfv", opengl32_dll),
                glIndexi: get_func("glIndexi", opengl32_dll),
                glIndexiv: get_func("glIndexiv", opengl32_dll),
                glIndexs: get_func("glIndexs", opengl32_dll),
                glIndexsv: get_func("glIndexsv", opengl32_dll),
                glIndexub: get_func("glIndexub", opengl32_dll),
                glIndexubv: get_func("glIndexubv", opengl32_dll),
                glInitNames: get_func("glInitNames", opengl32_dll),
                glInsertEventMarkerEXT: get_func("glInsertEventMarkerEXT", opengl32_dll),
                glInterleavedArrays: get_func("glInterleavedArrays", opengl32_dll),
                glInvalidateBufferData: get_func("glInvalidateBufferData", opengl32_dll),
                glInvalidateBufferSubData: get_func("glInvalidateBufferSubData", opengl32_dll),
                glInvalidateFramebuffer: get_func("glInvalidateFramebuffer", opengl32_dll),
                glInvalidateSubFramebuffer: get_func("glInvalidateSubFramebuffer", opengl32_dll),
                glInvalidateTexImage: get_func("glInvalidateTexImage", opengl32_dll),
                glInvalidateTexSubImage: get_func("glInvalidateTexSubImage", opengl32_dll),
                glIsBuffer: get_func("glIsBuffer", opengl32_dll),
                glIsEnabled: get_func("glIsEnabled", opengl32_dll),
                glIsEnabledi: get_func("glIsEnabledi", opengl32_dll),
                glIsFenceAPPLE: get_func("glIsFenceAPPLE", opengl32_dll),
                glIsFramebuffer: get_func("glIsFramebuffer", opengl32_dll),
                glIsList: get_func("glIsList", opengl32_dll),
                glIsProgram: get_func("glIsProgram", opengl32_dll),
                glIsQuery: get_func("glIsQuery", opengl32_dll),
                glIsRenderbuffer: get_func("glIsRenderbuffer", opengl32_dll),
                glIsSampler: get_func("glIsSampler", opengl32_dll),
                glIsShader: get_func("glIsShader", opengl32_dll),
                glIsSync: get_func("glIsSync", opengl32_dll),
                glIsTexture: get_func("glIsTexture", opengl32_dll),
                glIsVertexArray: get_func("glIsVertexArray", opengl32_dll),
                glIsVertexArrayAPPLE: get_func("glIsVertexArrayAPPLE", opengl32_dll),
                glLightModelf: get_func("glLightModelf", opengl32_dll),
                glLightModelfv: get_func("glLightModelfv", opengl32_dll),
                glLightModeli: get_func("glLightModeli", opengl32_dll),
                glLightModeliv: get_func("glLightModeliv", opengl32_dll),
                glLightf: get_func("glLightf", opengl32_dll),
                glLightfv: get_func("glLightfv", opengl32_dll),
                glLighti: get_func("glLighti", opengl32_dll),
                glLightiv: get_func("glLightiv", opengl32_dll),
                glLineStipple: get_func("glLineStipple", opengl32_dll),
                glLineWidth: get_func("glLineWidth", opengl32_dll),
                glLinkProgram: get_func("glLinkProgram", opengl32_dll),
                glListBase: get_func("glListBase", opengl32_dll),
                glLoadIdentity: get_func("glLoadIdentity", opengl32_dll),
                glLoadMatrixd: get_func("glLoadMatrixd", opengl32_dll),
                glLoadMatrixf: get_func("glLoadMatrixf", opengl32_dll),
                glLoadName: get_func("glLoadName", opengl32_dll),
                glLoadTransposeMatrixd: get_func("glLoadTransposeMatrixd", opengl32_dll),
                glLoadTransposeMatrixf: get_func("glLoadTransposeMatrixf", opengl32_dll),
                glLogicOp: get_func("glLogicOp", opengl32_dll),
                glMap1d: get_func("glMap1d", opengl32_dll),
                glMap1f: get_func("glMap1f", opengl32_dll),
                glMap2d: get_func("glMap2d", opengl32_dll),
                glMap2f: get_func("glMap2f", opengl32_dll),
                glMapBuffer: get_func("glMapBuffer", opengl32_dll),
                glMapBufferRange: get_func("glMapBufferRange", opengl32_dll),
                glMapGrid1d: get_func("glMapGrid1d", opengl32_dll),
                glMapGrid1f: get_func("glMapGrid1f", opengl32_dll),
                glMapGrid2d: get_func("glMapGrid2d", opengl32_dll),
                glMapGrid2f: get_func("glMapGrid2f", opengl32_dll),
                glMaterialf: get_func("glMaterialf", opengl32_dll),
                glMaterialfv: get_func("glMaterialfv", opengl32_dll),
                glMateriali: get_func("glMateriali", opengl32_dll),
                glMaterialiv: get_func("glMaterialiv", opengl32_dll),
                glMatrixMode: get_func("glMatrixMode", opengl32_dll),
                glMultMatrixd: get_func("glMultMatrixd", opengl32_dll),
                glMultMatrixf: get_func("glMultMatrixf", opengl32_dll),
                glMultTransposeMatrixd: get_func("glMultTransposeMatrixd", opengl32_dll),
                glMultTransposeMatrixf: get_func("glMultTransposeMatrixf", opengl32_dll),
                glMultiDrawArrays: get_func("glMultiDrawArrays", opengl32_dll),
                glMultiDrawElements: get_func("glMultiDrawElements", opengl32_dll),
                glMultiDrawElementsBaseVertex: get_func("glMultiDrawElementsBaseVertex", opengl32_dll),
                glMultiTexCoord1d: get_func("glMultiTexCoord1d", opengl32_dll),
                glMultiTexCoord1dv: get_func("glMultiTexCoord1dv", opengl32_dll),
                glMultiTexCoord1f: get_func("glMultiTexCoord1f", opengl32_dll),
                glMultiTexCoord1fv: get_func("glMultiTexCoord1fv", opengl32_dll),
                glMultiTexCoord1i: get_func("glMultiTexCoord1i", opengl32_dll),
                glMultiTexCoord1iv: get_func("glMultiTexCoord1iv", opengl32_dll),
                glMultiTexCoord1s: get_func("glMultiTexCoord1s", opengl32_dll),
                glMultiTexCoord1sv: get_func("glMultiTexCoord1sv", opengl32_dll),
                glMultiTexCoord2d: get_func("glMultiTexCoord2d", opengl32_dll),
                glMultiTexCoord2dv: get_func("glMultiTexCoord2dv", opengl32_dll),
                glMultiTexCoord2f: get_func("glMultiTexCoord2f", opengl32_dll),
                glMultiTexCoord2fv: get_func("glMultiTexCoord2fv", opengl32_dll),
                glMultiTexCoord2i: get_func("glMultiTexCoord2i", opengl32_dll),
                glMultiTexCoord2iv: get_func("glMultiTexCoord2iv", opengl32_dll),
                glMultiTexCoord2s: get_func("glMultiTexCoord2s", opengl32_dll),
                glMultiTexCoord2sv: get_func("glMultiTexCoord2sv", opengl32_dll),
                glMultiTexCoord3d: get_func("glMultiTexCoord3d", opengl32_dll),
                glMultiTexCoord3dv: get_func("glMultiTexCoord3dv", opengl32_dll),
                glMultiTexCoord3f: get_func("glMultiTexCoord3f", opengl32_dll),
                glMultiTexCoord3fv: get_func("glMultiTexCoord3fv", opengl32_dll),
                glMultiTexCoord3i: get_func("glMultiTexCoord3i", opengl32_dll),
                glMultiTexCoord3iv: get_func("glMultiTexCoord3iv", opengl32_dll),
                glMultiTexCoord3s: get_func("glMultiTexCoord3s", opengl32_dll),
                glMultiTexCoord3sv: get_func("glMultiTexCoord3sv", opengl32_dll),
                glMultiTexCoord4d: get_func("glMultiTexCoord4d", opengl32_dll),
                glMultiTexCoord4dv: get_func("glMultiTexCoord4dv", opengl32_dll),
                glMultiTexCoord4f: get_func("glMultiTexCoord4f", opengl32_dll),
                glMultiTexCoord4fv: get_func("glMultiTexCoord4fv", opengl32_dll),
                glMultiTexCoord4i: get_func("glMultiTexCoord4i", opengl32_dll),
                glMultiTexCoord4iv: get_func("glMultiTexCoord4iv", opengl32_dll),
                glMultiTexCoord4s: get_func("glMultiTexCoord4s", opengl32_dll),
                glMultiTexCoord4sv: get_func("glMultiTexCoord4sv", opengl32_dll),
                glMultiTexCoordP1ui: get_func("glMultiTexCoordP1ui", opengl32_dll),
                glMultiTexCoordP1uiv: get_func("glMultiTexCoordP1uiv", opengl32_dll),
                glMultiTexCoordP2ui: get_func("glMultiTexCoordP2ui", opengl32_dll),
                glMultiTexCoordP2uiv: get_func("glMultiTexCoordP2uiv", opengl32_dll),
                glMultiTexCoordP3ui: get_func("glMultiTexCoordP3ui", opengl32_dll),
                glMultiTexCoordP3uiv: get_func("glMultiTexCoordP3uiv", opengl32_dll),
                glMultiTexCoordP4ui: get_func("glMultiTexCoordP4ui", opengl32_dll),
                glMultiTexCoordP4uiv: get_func("glMultiTexCoordP4uiv", opengl32_dll),
                glNewList: get_func("glNewList", opengl32_dll),
                glNormal3b: get_func("glNormal3b", opengl32_dll),
                glNormal3bv: get_func("glNormal3bv", opengl32_dll),
                glNormal3d: get_func("glNormal3d", opengl32_dll),
                glNormal3dv: get_func("glNormal3dv", opengl32_dll),
                glNormal3f: get_func("glNormal3f", opengl32_dll),
                glNormal3fv: get_func("glNormal3fv", opengl32_dll),
                glNormal3i: get_func("glNormal3i", opengl32_dll),
                glNormal3iv: get_func("glNormal3iv", opengl32_dll),
                glNormal3s: get_func("glNormal3s", opengl32_dll),
                glNormal3sv: get_func("glNormal3sv", opengl32_dll),
                glNormalP3ui: get_func("glNormalP3ui", opengl32_dll),
                glNormalP3uiv: get_func("glNormalP3uiv", opengl32_dll),
                glNormalPointer: get_func("glNormalPointer", opengl32_dll),
                glObjectLabel: get_func("glObjectLabel", opengl32_dll),
                glObjectLabelKHR: get_func("glObjectLabelKHR", opengl32_dll),
                glObjectPtrLabel: get_func("glObjectPtrLabel", opengl32_dll),
                glObjectPtrLabelKHR: get_func("glObjectPtrLabelKHR", opengl32_dll),
                glOrtho: get_func("glOrtho", opengl32_dll),
                glPassThrough: get_func("glPassThrough", opengl32_dll),
                glPixelMapfv: get_func("glPixelMapfv", opengl32_dll),
                glPixelMapuiv: get_func("glPixelMapuiv", opengl32_dll),
                glPixelMapusv: get_func("glPixelMapusv", opengl32_dll),
                glPixelStoref: get_func("glPixelStoref", opengl32_dll),
                glPixelStorei: get_func("glPixelStorei", opengl32_dll),
                glPixelTransferf: get_func("glPixelTransferf", opengl32_dll),
                glPixelTransferi: get_func("glPixelTransferi", opengl32_dll),
                glPixelZoom: get_func("glPixelZoom", opengl32_dll),
                glPointParameterf: get_func("glPointParameterf", opengl32_dll),
                glPointParameterfv: get_func("glPointParameterfv", opengl32_dll),
                glPointParameteri: get_func("glPointParameteri", opengl32_dll),
                glPointParameteriv: get_func("glPointParameteriv", opengl32_dll),
                glPointSize: get_func("glPointSize", opengl32_dll),
                glPolygonMode: get_func("glPolygonMode", opengl32_dll),
                glPolygonOffset: get_func("glPolygonOffset", opengl32_dll),
                glPolygonStipple: get_func("glPolygonStipple", opengl32_dll),
                glPopAttrib: get_func("glPopAttrib", opengl32_dll),
                glPopClientAttrib: get_func("glPopClientAttrib", opengl32_dll),
                glPopDebugGroup: get_func("glPopDebugGroup", opengl32_dll),
                glPopDebugGroupKHR: get_func("glPopDebugGroupKHR", opengl32_dll),
                glPopGroupMarkerEXT: get_func("glPopGroupMarkerEXT", opengl32_dll),
                glPopMatrix: get_func("glPopMatrix", opengl32_dll),
                glPopName: get_func("glPopName", opengl32_dll),
                glPrimitiveRestartIndex: get_func("glPrimitiveRestartIndex", opengl32_dll),
                glPrioritizeTextures: get_func("glPrioritizeTextures", opengl32_dll),
                glProgramBinary: get_func("glProgramBinary", opengl32_dll),
                glProgramParameteri: get_func("glProgramParameteri", opengl32_dll),
                glProvokingVertex: get_func("glProvokingVertex", opengl32_dll),
                glPushAttrib: get_func("glPushAttrib", opengl32_dll),
                glPushClientAttrib: get_func("glPushClientAttrib", opengl32_dll),
                glPushDebugGroup: get_func("glPushDebugGroup", opengl32_dll),
                glPushDebugGroupKHR: get_func("glPushDebugGroupKHR", opengl32_dll),
                glPushGroupMarkerEXT: get_func("glPushGroupMarkerEXT", opengl32_dll),
                glPushMatrix: get_func("glPushMatrix", opengl32_dll),
                glPushName: get_func("glPushName", opengl32_dll),
                glQueryCounter: get_func("glQueryCounter", opengl32_dll),
                glRasterPos2d: get_func("glRasterPos2d", opengl32_dll),
                glRasterPos2dv: get_func("glRasterPos2dv", opengl32_dll),
                glRasterPos2f: get_func("glRasterPos2f", opengl32_dll),
                glRasterPos2fv: get_func("glRasterPos2fv", opengl32_dll),
                glRasterPos2i: get_func("glRasterPos2i", opengl32_dll),
                glRasterPos2iv: get_func("glRasterPos2iv", opengl32_dll),
                glRasterPos2s: get_func("glRasterPos2s", opengl32_dll),
                glRasterPos2sv: get_func("glRasterPos2sv", opengl32_dll),
                glRasterPos3d: get_func("glRasterPos3d", opengl32_dll),
                glRasterPos3dv: get_func("glRasterPos3dv", opengl32_dll),
                glRasterPos3f: get_func("glRasterPos3f", opengl32_dll),
                glRasterPos3fv: get_func("glRasterPos3fv", opengl32_dll),
                glRasterPos3i: get_func("glRasterPos3i", opengl32_dll),
                glRasterPos3iv: get_func("glRasterPos3iv", opengl32_dll),
                glRasterPos3s: get_func("glRasterPos3s", opengl32_dll),
                glRasterPos3sv: get_func("glRasterPos3sv", opengl32_dll),
                glRasterPos4d: get_func("glRasterPos4d", opengl32_dll),
                glRasterPos4dv: get_func("glRasterPos4dv", opengl32_dll),
                glRasterPos4f: get_func("glRasterPos4f", opengl32_dll),
                glRasterPos4fv: get_func("glRasterPos4fv", opengl32_dll),
                glRasterPos4i: get_func("glRasterPos4i", opengl32_dll),
                glRasterPos4iv: get_func("glRasterPos4iv", opengl32_dll),
                glRasterPos4s: get_func("glRasterPos4s", opengl32_dll),
                glRasterPos4sv: get_func("glRasterPos4sv", opengl32_dll),
                glReadBuffer: get_func("glReadBuffer", opengl32_dll),
                glReadPixels: get_func("glReadPixels", opengl32_dll),
                glRectd: get_func("glRectd", opengl32_dll),
                glRectdv: get_func("glRectdv", opengl32_dll),
                glRectf: get_func("glRectf", opengl32_dll),
                glRectfv: get_func("glRectfv", opengl32_dll),
                glRecti: get_func("glRecti", opengl32_dll),
                glRectiv: get_func("glRectiv", opengl32_dll),
                glRects: get_func("glRects", opengl32_dll),
                glRectsv: get_func("glRectsv", opengl32_dll),
                glRenderMode: get_func("glRenderMode", opengl32_dll),
                glRenderbufferStorage: get_func("glRenderbufferStorage", opengl32_dll),
                glRenderbufferStorageMultisample: get_func("glRenderbufferStorageMultisample", opengl32_dll),
                glRotated: get_func("glRotated", opengl32_dll),
                glRotatef: get_func("glRotatef", opengl32_dll),
                glSampleCoverage: get_func("glSampleCoverage", opengl32_dll),
                glSampleMaski: get_func("glSampleMaski", opengl32_dll),
                glSamplerParameterIiv: get_func("glSamplerParameterIiv", opengl32_dll),
                glSamplerParameterIuiv: get_func("glSamplerParameterIuiv", opengl32_dll),
                glSamplerParameterf: get_func("glSamplerParameterf", opengl32_dll),
                glSamplerParameterfv: get_func("glSamplerParameterfv", opengl32_dll),
                glSamplerParameteri: get_func("glSamplerParameteri", opengl32_dll),
                glSamplerParameteriv: get_func("glSamplerParameteriv", opengl32_dll),
                glScaled: get_func("glScaled", opengl32_dll),
                glScalef: get_func("glScalef", opengl32_dll),
                glScissor: get_func("glScissor", opengl32_dll),
                glSecondaryColor3b: get_func("glSecondaryColor3b", opengl32_dll),
                glSecondaryColor3bv: get_func("glSecondaryColor3bv", opengl32_dll),
                glSecondaryColor3d: get_func("glSecondaryColor3d", opengl32_dll),
                glSecondaryColor3dv: get_func("glSecondaryColor3dv", opengl32_dll),
                glSecondaryColor3f: get_func("glSecondaryColor3f", opengl32_dll),
                glSecondaryColor3fv: get_func("glSecondaryColor3fv", opengl32_dll),
                glSecondaryColor3i: get_func("glSecondaryColor3i", opengl32_dll),
                glSecondaryColor3iv: get_func("glSecondaryColor3iv", opengl32_dll),
                glSecondaryColor3s: get_func("glSecondaryColor3s", opengl32_dll),
                glSecondaryColor3sv: get_func("glSecondaryColor3sv", opengl32_dll),
                glSecondaryColor3ub: get_func("glSecondaryColor3ub", opengl32_dll),
                glSecondaryColor3ubv: get_func("glSecondaryColor3ubv", opengl32_dll),
                glSecondaryColor3ui: get_func("glSecondaryColor3ui", opengl32_dll),
                glSecondaryColor3uiv: get_func("glSecondaryColor3uiv", opengl32_dll),
                glSecondaryColor3us: get_func("glSecondaryColor3us", opengl32_dll),
                glSecondaryColor3usv: get_func("glSecondaryColor3usv", opengl32_dll),
                glSecondaryColorP3ui: get_func("glSecondaryColorP3ui", opengl32_dll),
                glSecondaryColorP3uiv: get_func("glSecondaryColorP3uiv", opengl32_dll),
                glSecondaryColorPointer: get_func("glSecondaryColorPointer", opengl32_dll),
                glSelectBuffer: get_func("glSelectBuffer", opengl32_dll),
                glSetFenceAPPLE: get_func("glSetFenceAPPLE", opengl32_dll),
                glShadeModel: get_func("glShadeModel", opengl32_dll),
                glShaderSource: get_func("glShaderSource", opengl32_dll),
                glShaderStorageBlockBinding: get_func("glShaderStorageBlockBinding", opengl32_dll),
                glStencilFunc: get_func("glStencilFunc", opengl32_dll),
                glStencilFuncSeparate: get_func("glStencilFuncSeparate", opengl32_dll),
                glStencilMask: get_func("glStencilMask", opengl32_dll),
                glStencilMaskSeparate: get_func("glStencilMaskSeparate", opengl32_dll),
                glStencilOp: get_func("glStencilOp", opengl32_dll),
                glStencilOpSeparate: get_func("glStencilOpSeparate", opengl32_dll),
                glTestFenceAPPLE: get_func("glTestFenceAPPLE", opengl32_dll),
                glTestObjectAPPLE: get_func("glTestObjectAPPLE", opengl32_dll),
                glTexBuffer: get_func("glTexBuffer", opengl32_dll),
                glTexCoord1d: get_func("glTexCoord1d", opengl32_dll),
                glTexCoord1dv: get_func("glTexCoord1dv", opengl32_dll),
                glTexCoord1f: get_func("glTexCoord1f", opengl32_dll),
                glTexCoord1fv: get_func("glTexCoord1fv", opengl32_dll),
                glTexCoord1i: get_func("glTexCoord1i", opengl32_dll),
                glTexCoord1iv: get_func("glTexCoord1iv", opengl32_dll),
                glTexCoord1s: get_func("glTexCoord1s", opengl32_dll),
                glTexCoord1sv: get_func("glTexCoord1sv", opengl32_dll),
                glTexCoord2d: get_func("glTexCoord2d", opengl32_dll),
                glTexCoord2dv: get_func("glTexCoord2dv", opengl32_dll),
                glTexCoord2f: get_func("glTexCoord2f", opengl32_dll),
                glTexCoord2fv: get_func("glTexCoord2fv", opengl32_dll),
                glTexCoord2i: get_func("glTexCoord2i", opengl32_dll),
                glTexCoord2iv: get_func("glTexCoord2iv", opengl32_dll),
                glTexCoord2s: get_func("glTexCoord2s", opengl32_dll),
                glTexCoord2sv: get_func("glTexCoord2sv", opengl32_dll),
                glTexCoord3d: get_func("glTexCoord3d", opengl32_dll),
                glTexCoord3dv: get_func("glTexCoord3dv", opengl32_dll),
                glTexCoord3f: get_func("glTexCoord3f", opengl32_dll),
                glTexCoord3fv: get_func("glTexCoord3fv", opengl32_dll),
                glTexCoord3i: get_func("glTexCoord3i", opengl32_dll),
                glTexCoord3iv: get_func("glTexCoord3iv", opengl32_dll),
                glTexCoord3s: get_func("glTexCoord3s", opengl32_dll),
                glTexCoord3sv: get_func("glTexCoord3sv", opengl32_dll),
                glTexCoord4d: get_func("glTexCoord4d", opengl32_dll),
                glTexCoord4dv: get_func("glTexCoord4dv", opengl32_dll),
                glTexCoord4f: get_func("glTexCoord4f", opengl32_dll),
                glTexCoord4fv: get_func("glTexCoord4fv", opengl32_dll),
                glTexCoord4i: get_func("glTexCoord4i", opengl32_dll),
                glTexCoord4iv: get_func("glTexCoord4iv", opengl32_dll),
                glTexCoord4s: get_func("glTexCoord4s", opengl32_dll),
                glTexCoord4sv: get_func("glTexCoord4sv", opengl32_dll),
                glTexCoordP1ui: get_func("glTexCoordP1ui", opengl32_dll),
                glTexCoordP1uiv: get_func("glTexCoordP1uiv", opengl32_dll),
                glTexCoordP2ui: get_func("glTexCoordP2ui", opengl32_dll),
                glTexCoordP2uiv: get_func("glTexCoordP2uiv", opengl32_dll),
                glTexCoordP3ui: get_func("glTexCoordP3ui", opengl32_dll),
                glTexCoordP3uiv: get_func("glTexCoordP3uiv", opengl32_dll),
                glTexCoordP4ui: get_func("glTexCoordP4ui", opengl32_dll),
                glTexCoordP4uiv: get_func("glTexCoordP4uiv", opengl32_dll),
                glTexCoordPointer: get_func("glTexCoordPointer", opengl32_dll),
                glTexEnvf: get_func("glTexEnvf", opengl32_dll),
                glTexEnvfv: get_func("glTexEnvfv", opengl32_dll),
                glTexEnvi: get_func("glTexEnvi", opengl32_dll),
                glTexEnviv: get_func("glTexEnviv", opengl32_dll),
                glTexGend: get_func("glTexGend", opengl32_dll),
                glTexGendv: get_func("glTexGendv", opengl32_dll),
                glTexGenf: get_func("glTexGenf", opengl32_dll),
                glTexGenfv: get_func("glTexGenfv", opengl32_dll),
                glTexGeni: get_func("glTexGeni", opengl32_dll),
                glTexGeniv: get_func("glTexGeniv", opengl32_dll),
                glTexImage1D: get_func("glTexImage1D", opengl32_dll),
                glTexImage2D: get_func("glTexImage2D", opengl32_dll),
                glTexImage2DMultisample: get_func("glTexImage2DMultisample", opengl32_dll),
                glTexImage3D: get_func("glTexImage3D", opengl32_dll),
                glTexImage3DMultisample: get_func("glTexImage3DMultisample", opengl32_dll),
                glTexParameterIiv: get_func("glTexParameterIiv", opengl32_dll),
                glTexParameterIuiv: get_func("glTexParameterIuiv", opengl32_dll),
                glTexParameterf: get_func("glTexParameterf", opengl32_dll),
                glTexParameterfv: get_func("glTexParameterfv", opengl32_dll),
                glTexParameteri: get_func("glTexParameteri", opengl32_dll),
                glTexParameteriv: get_func("glTexParameteriv", opengl32_dll),
                glTexStorage1D: get_func("glTexStorage1D", opengl32_dll),
                glTexStorage2D: get_func("glTexStorage2D", opengl32_dll),
                glTexStorage3D: get_func("glTexStorage3D", opengl32_dll),
                glTexSubImage1D: get_func("glTexSubImage1D", opengl32_dll),
                glTexSubImage2D: get_func("glTexSubImage2D", opengl32_dll),
                glTexSubImage3D: get_func("glTexSubImage3D", opengl32_dll),
                glTextureRangeAPPLE: get_func("glTextureRangeAPPLE", opengl32_dll),
                glTransformFeedbackVaryings: get_func("glTransformFeedbackVaryings", opengl32_dll),
                glTranslated: get_func("glTranslated", opengl32_dll),
                glTranslatef: get_func("glTranslatef", opengl32_dll),
                glUniform1f: get_func("glUniform1f", opengl32_dll),
                glUniform1fv: get_func("glUniform1fv", opengl32_dll),
                glUniform1i: get_func("glUniform1i", opengl32_dll),
                glUniform1iv: get_func("glUniform1iv", opengl32_dll),
                glUniform1ui: get_func("glUniform1ui", opengl32_dll),
                glUniform1uiv: get_func("glUniform1uiv", opengl32_dll),
                glUniform2f: get_func("glUniform2f", opengl32_dll),
                glUniform2fv: get_func("glUniform2fv", opengl32_dll),
                glUniform2i: get_func("glUniform2i", opengl32_dll),
                glUniform2iv: get_func("glUniform2iv", opengl32_dll),
                glUniform2ui: get_func("glUniform2ui", opengl32_dll),
                glUniform2uiv: get_func("glUniform2uiv", opengl32_dll),
                glUniform3f: get_func("glUniform3f", opengl32_dll),
                glUniform3fv: get_func("glUniform3fv", opengl32_dll),
                glUniform3i: get_func("glUniform3i", opengl32_dll),
                glUniform3iv: get_func("glUniform3iv", opengl32_dll),
                glUniform3ui: get_func("glUniform3ui", opengl32_dll),
                glUniform3uiv: get_func("glUniform3uiv", opengl32_dll),
                glUniform4f: get_func("glUniform4f", opengl32_dll),
                glUniform4fv: get_func("glUniform4fv", opengl32_dll),
                glUniform4i: get_func("glUniform4i", opengl32_dll),
                glUniform4iv: get_func("glUniform4iv", opengl32_dll),
                glUniform4ui: get_func("glUniform4ui", opengl32_dll),
                glUniform4uiv: get_func("glUniform4uiv", opengl32_dll),
                glUniformBlockBinding: get_func("glUniformBlockBinding", opengl32_dll),
                glUniformMatrix2fv: get_func("glUniformMatrix2fv", opengl32_dll),
                glUniformMatrix2x3fv: get_func("glUniformMatrix2x3fv", opengl32_dll),
                glUniformMatrix2x4fv: get_func("glUniformMatrix2x4fv", opengl32_dll),
                glUniformMatrix3fv: get_func("glUniformMatrix3fv", opengl32_dll),
                glUniformMatrix3x2fv: get_func("glUniformMatrix3x2fv", opengl32_dll),
                glUniformMatrix3x4fv: get_func("glUniformMatrix3x4fv", opengl32_dll),
                glUniformMatrix4fv: get_func("glUniformMatrix4fv", opengl32_dll),
                glUniformMatrix4x2fv: get_func("glUniformMatrix4x2fv", opengl32_dll),
                glUniformMatrix4x3fv: get_func("glUniformMatrix4x3fv", opengl32_dll),
                glUnmapBuffer: get_func("glUnmapBuffer", opengl32_dll),
                glUseProgram: get_func("glUseProgram", opengl32_dll),
                glValidateProgram: get_func("glValidateProgram", opengl32_dll),
                glVertex2d: get_func("glVertex2d", opengl32_dll),
                glVertex2dv: get_func("glVertex2dv", opengl32_dll),
                glVertex2f: get_func("glVertex2f", opengl32_dll),
                glVertex2fv: get_func("glVertex2fv", opengl32_dll),
                glVertex2i: get_func("glVertex2i", opengl32_dll),
                glVertex2iv: get_func("glVertex2iv", opengl32_dll),
                glVertex2s: get_func("glVertex2s", opengl32_dll),
                glVertex2sv: get_func("glVertex2sv", opengl32_dll),
                glVertex3d: get_func("glVertex3d", opengl32_dll),
                glVertex3dv: get_func("glVertex3dv", opengl32_dll),
                glVertex3f: get_func("glVertex3f", opengl32_dll),
                glVertex3fv: get_func("glVertex3fv", opengl32_dll),
                glVertex3i: get_func("glVertex3i", opengl32_dll),
                glVertex3iv: get_func("glVertex3iv", opengl32_dll),
                glVertex3s: get_func("glVertex3s", opengl32_dll),
                glVertex3sv: get_func("glVertex3sv", opengl32_dll),
                glVertex4d: get_func("glVertex4d", opengl32_dll),
                glVertex4dv: get_func("glVertex4dv", opengl32_dll),
                glVertex4f: get_func("glVertex4f", opengl32_dll),
                glVertex4fv: get_func("glVertex4fv", opengl32_dll),
                glVertex4i: get_func("glVertex4i", opengl32_dll),
                glVertex4iv: get_func("glVertex4iv", opengl32_dll),
                glVertex4s: get_func("glVertex4s", opengl32_dll),
                glVertex4sv: get_func("glVertex4sv", opengl32_dll),
                glVertexAttrib1d: get_func("glVertexAttrib1d", opengl32_dll),
                glVertexAttrib1dv: get_func("glVertexAttrib1dv", opengl32_dll),
                glVertexAttrib1f: get_func("glVertexAttrib1f", opengl32_dll),
                glVertexAttrib1fv: get_func("glVertexAttrib1fv", opengl32_dll),
                glVertexAttrib1s: get_func("glVertexAttrib1s", opengl32_dll),
                glVertexAttrib1sv: get_func("glVertexAttrib1sv", opengl32_dll),
                glVertexAttrib2d: get_func("glVertexAttrib2d", opengl32_dll),
                glVertexAttrib2dv: get_func("glVertexAttrib2dv", opengl32_dll),
                glVertexAttrib2f: get_func("glVertexAttrib2f", opengl32_dll),
                glVertexAttrib2fv: get_func("glVertexAttrib2fv", opengl32_dll),
                glVertexAttrib2s: get_func("glVertexAttrib2s", opengl32_dll),
                glVertexAttrib2sv: get_func("glVertexAttrib2sv", opengl32_dll),
                glVertexAttrib3d: get_func("glVertexAttrib3d", opengl32_dll),
                glVertexAttrib3dv: get_func("glVertexAttrib3dv", opengl32_dll),
                glVertexAttrib3f: get_func("glVertexAttrib3f", opengl32_dll),
                glVertexAttrib3fv: get_func("glVertexAttrib3fv", opengl32_dll),
                glVertexAttrib3s: get_func("glVertexAttrib3s", opengl32_dll),
                glVertexAttrib3sv: get_func("glVertexAttrib3sv", opengl32_dll),
                glVertexAttrib4Nbv: get_func("glVertexAttrib4Nbv", opengl32_dll),
                glVertexAttrib4Niv: get_func("glVertexAttrib4Niv", opengl32_dll),
                glVertexAttrib4Nsv: get_func("glVertexAttrib4Nsv", opengl32_dll),
                glVertexAttrib4Nub: get_func("glVertexAttrib4Nub", opengl32_dll),
                glVertexAttrib4Nubv: get_func("glVertexAttrib4Nubv", opengl32_dll),
                glVertexAttrib4Nuiv: get_func("glVertexAttrib4Nuiv", opengl32_dll),
                glVertexAttrib4Nusv: get_func("glVertexAttrib4Nusv", opengl32_dll),
                glVertexAttrib4bv: get_func("glVertexAttrib4bv", opengl32_dll),
                glVertexAttrib4d: get_func("glVertexAttrib4d", opengl32_dll),
                glVertexAttrib4dv: get_func("glVertexAttrib4dv", opengl32_dll),
                glVertexAttrib4f: get_func("glVertexAttrib4f", opengl32_dll),
                glVertexAttrib4fv: get_func("glVertexAttrib4fv", opengl32_dll),
                glVertexAttrib4iv: get_func("glVertexAttrib4iv", opengl32_dll),
                glVertexAttrib4s: get_func("glVertexAttrib4s", opengl32_dll),
                glVertexAttrib4sv: get_func("glVertexAttrib4sv", opengl32_dll),
                glVertexAttrib4ubv: get_func("glVertexAttrib4ubv", opengl32_dll),
                glVertexAttrib4uiv: get_func("glVertexAttrib4uiv", opengl32_dll),
                glVertexAttrib4usv: get_func("glVertexAttrib4usv", opengl32_dll),
                glVertexAttribDivisor: get_func("glVertexAttribDivisor", opengl32_dll),
                glVertexAttribI1i: get_func("glVertexAttribI1i", opengl32_dll),
                glVertexAttribI1iv: get_func("glVertexAttribI1iv", opengl32_dll),
                glVertexAttribI1ui: get_func("glVertexAttribI1ui", opengl32_dll),
                glVertexAttribI1uiv: get_func("glVertexAttribI1uiv", opengl32_dll),
                glVertexAttribI2i: get_func("glVertexAttribI2i", opengl32_dll),
                glVertexAttribI2iv: get_func("glVertexAttribI2iv", opengl32_dll),
                glVertexAttribI2ui: get_func("glVertexAttribI2ui", opengl32_dll),
                glVertexAttribI2uiv: get_func("glVertexAttribI2uiv", opengl32_dll),
                glVertexAttribI3i: get_func("glVertexAttribI3i", opengl32_dll),
                glVertexAttribI3iv: get_func("glVertexAttribI3iv", opengl32_dll),
                glVertexAttribI3ui: get_func("glVertexAttribI3ui", opengl32_dll),
                glVertexAttribI3uiv: get_func("glVertexAttribI3uiv", opengl32_dll),
                glVertexAttribI4bv: get_func("glVertexAttribI4bv", opengl32_dll),
                glVertexAttribI4i: get_func("glVertexAttribI4i", opengl32_dll),
                glVertexAttribI4iv: get_func("glVertexAttribI4iv", opengl32_dll),
                glVertexAttribI4sv: get_func("glVertexAttribI4sv", opengl32_dll),
                glVertexAttribI4ubv: get_func("glVertexAttribI4ubv", opengl32_dll),
                glVertexAttribI4ui: get_func("glVertexAttribI4ui", opengl32_dll),
                glVertexAttribI4uiv: get_func("glVertexAttribI4uiv", opengl32_dll),
                glVertexAttribI4usv: get_func("glVertexAttribI4usv", opengl32_dll),
                glVertexAttribIPointer: get_func("glVertexAttribIPointer", opengl32_dll),
                glVertexAttribP1ui: get_func("glVertexAttribP1ui", opengl32_dll),
                glVertexAttribP1uiv: get_func("glVertexAttribP1uiv", opengl32_dll),
                glVertexAttribP2ui: get_func("glVertexAttribP2ui", opengl32_dll),
                glVertexAttribP2uiv: get_func("glVertexAttribP2uiv", opengl32_dll),
                glVertexAttribP3ui: get_func("glVertexAttribP3ui", opengl32_dll),
                glVertexAttribP3uiv: get_func("glVertexAttribP3uiv", opengl32_dll),
                glVertexAttribP4ui: get_func("glVertexAttribP4ui", opengl32_dll),
                glVertexAttribP4uiv: get_func("glVertexAttribP4uiv", opengl32_dll),
                glVertexAttribPointer: get_func("glVertexAttribPointer", opengl32_dll),
                glVertexP2ui: get_func("glVertexP2ui", opengl32_dll),
                glVertexP2uiv: get_func("glVertexP2uiv", opengl32_dll),
                glVertexP3ui: get_func("glVertexP3ui", opengl32_dll),
                glVertexP3uiv: get_func("glVertexP3uiv", opengl32_dll),
                glVertexP4ui: get_func("glVertexP4ui", opengl32_dll),
                glVertexP4uiv: get_func("glVertexP4uiv", opengl32_dll),
                glVertexPointer: get_func("glVertexPointer", opengl32_dll),
                glViewport: get_func("glViewport", opengl32_dll),
                glWaitSync: get_func("glWaitSync", opengl32_dll),
                glWindowPos2d: get_func("glWindowPos2d", opengl32_dll),
                glWindowPos2dv: get_func("glWindowPos2dv", opengl32_dll),
                glWindowPos2f: get_func("glWindowPos2f", opengl32_dll),
                glWindowPos2fv: get_func("glWindowPos2fv", opengl32_dll),
                glWindowPos2i: get_func("glWindowPos2i", opengl32_dll),
                glWindowPos2iv: get_func("glWindowPos2iv", opengl32_dll),
                glWindowPos2s: get_func("glWindowPos2s", opengl32_dll),
                glWindowPos2sv: get_func("glWindowPos2sv", opengl32_dll),
                glWindowPos3d: get_func("glWindowPos3d", opengl32_dll),
                glWindowPos3dv: get_func("glWindowPos3dv", opengl32_dll),
                glWindowPos3f: get_func("glWindowPos3f", opengl32_dll),
                glWindowPos3fv: get_func("glWindowPos3fv", opengl32_dll),
                glWindowPos3i: get_func("glWindowPos3i", opengl32_dll),
                glWindowPos3iv: get_func("glWindowPos3iv", opengl32_dll),
                glWindowPos3s: get_func("glWindowPos3s", opengl32_dll),
                glWindowPos3sv: get_func("glWindowPos3sv", opengl32_dll),
            }
        }
    }

    impl gleam::gl::Gl for WglContext {
        fn get_type(&self) -> gleam::gl::GlType { gleam::gl::GlType::Gl }

        fn buffer_data_untyped(
            &self,
            target: GLenum,
            size: GLsizeiptr,
            data: *const GLvoid,
            usage: GLenum,
        ) {
            if self.glBufferData == ptr::null_mut() {
                _gl_impl_panic("glBufferData");
                return;
            }

            unsafe {
                let func: extern "C" fn(GLenum, GLsizeiptr, *const GLvoid, GLenum) = mem::transmute(self.glBufferData);
                (func)(target, size, data, usage)
            }
        }

        fn buffer_sub_data_untyped(
            &self,
            target: GLenum,
            offset: isize,
            size: GLsizeiptr,
            data: *const GLvoid,
        ) {
            if self.glBufferSubData == ptr::null_mut() {
                _gl_impl_panic("glBufferSubData");
                return;
            }

            unsafe {
                let func: extern "C" fn(GLenum, isize, GLsizeiptr, *const GLvoid) = mem::transmute(self.glBufferSubData);
                (func)(target, offset, size, data)
            }
        }

        fn map_buffer(&self, target: GLenum, access: GLbitfield) -> *mut GLvoid {
            if self.glMapBuffer == ptr::null_mut() {
                _gl_impl_panic("glMapBuffer");
                return ptr::null_mut();
            }

            unsafe {
                let func: extern "C" fn(GLenum, GLbitfield) -> *mut GLvoid = mem::transmute(self.glMapBuffer);
                (func)(target, access)
            }
        }

        fn map_buffer_range(
            &self,
            target: GLenum,
            offset: GLintptr,
            length: GLsizeiptr,
            access: GLbitfield,
        ) -> *mut GLvoid {
            if self.glMapBufferRange == ptr::null_mut() {
                _gl_impl_panic("glMapBufferRange");
                return ptr::null_mut();
            }

            unsafe {
                let func: extern "C" fn( GLenum, GLintptr, GLsizeiptr, GLbitfield) -> *mut GLvoid = mem::transmute(self.glMapBufferRange);
                (func)(target, offset, length, access)
            }
        }

        fn unmap_buffer(&self, target: GLenum) -> GLboolean {
            if self.glUnmapBuffer == ptr::null_mut() {
                _gl_impl_panic("glUnmapBuffer");
                return 1;
            }
            unsafe {
                let func: extern "C" fn(GLenum) -> GLboolean = mem::transmute(self.glUnmapBuffer);
                (func)(target)
            }
        }

        fn tex_buffer(&self, target: GLenum, internal_format: GLenum, buffer: GLuint) {
            if self.glTexBuffer == ptr::null_mut() {
                _gl_impl_panic("glTexBuffer");
                return;
            }
            unsafe {
                let func: extern "C" fn(GLenum, GLenum, GLuint) = mem::transmute(self.glTexBuffer);
                (func)(target, internal_format, buffer)
            }
        }

        fn shader_source(&self, shader: GLuint, strings: &[&[u8]]) {
            if self.glShaderSource == ptr::null_mut() {
                _gl_impl_panic("glShaderSource");
                return;
            }
            unsafe {
                let func: extern "C" fn(GLuint, &[&[u8]]) = mem::transmute(self.glShaderSource);
                (func)(shader, strings)
            }
        }

        fn read_buffer(&self, mode: GLenum) {
            if self.glReadBuffer == ptr::null_mut() {
                _gl_impl_panic("glReadBuffer");
                return;
            }
            unsafe {
                let func: extern "C" fn(GLenum) = mem::transmute(self.glReadBuffer);
                (func)(mode)
            }
        }

        fn read_pixels_into_buffer(
            &self,
            x: GLint,
            y: GLint,
            width: GLsizei,
            height: GLsizei,
            format: GLenum,
            pixel_type: GLenum,
            dst_buffer: &mut [u8],
        ) {
            if self.glReadPixels == ptr::null_mut() {
                _gl_impl_panic("glReadPixels");
                return;
            }

            if self.glPixelStorei == ptr::null_mut() {
                _gl_impl_panic("glPixelStorei");
                return;
            }

            unsafe {
                let glPixelStorei: extern "C" fn(GLenum, GLint) = mem::transmute(self.glPixelStorei);
                (glPixelStorei)(gleam::gl::PACK_ALIGNMENT, 1);

                let func: extern "C" fn(GLint, GLint, GLsizei, GLsizei, GLenum, GLenum, *mut u8) = mem::transmute(self.glReadPixels);
                (func)(x, y, width, height, format, pixel_type, dst_buffer.as_mut_ptr())
            }
        }

        fn read_pixels(
            &self,
            x: GLint,
            y: GLint,
            width: GLsizei,
            height: GLsizei,
            format: GLenum,
            pixel_type: GLenum,
        ) -> Vec<u8> {

            use gleam::gl::*;

            let bit_depth = match pixel_type {

                UNSIGNED_BYTE |
                BYTE |
                UNSIGNED_BYTE_3_3_2 |
                UNSIGNED_BYTE_2_3_3_REV => 1,

                UNSIGNED_SHORT |
                UNSIGNED_SHORT_5_6_5 |
                UNSIGNED_SHORT_5_6_5_REV |
                UNSIGNED_SHORT_4_4_4_4 |
                UNSIGNED_SHORT_4_4_4_4_REV |
                UNSIGNED_SHORT_5_5_5_1 |
                UNSIGNED_SHORT_1_5_5_5_REV |
                SHORT => 2,

                UNSIGNED_INT |
                UNSIGNED_INT_8_8_8_8 |
                UNSIGNED_INT_8_8_8_8_REV |
                UNSIGNED_INT_10_10_10_2 |
                UNSIGNED_INT_2_10_10_10_REV |
                UNSIGNED_INT_24_8 |
                UNSIGNED_INT_10F_11F_11F_REV |
                UNSIGNED_INT_5_9_9_9_REV |
                INT => 4,

                HALF_FLOAT => 2,

                FLOAT |
                FLOAT_32_UNSIGNED_INT_24_8_REV => 4,

                _ => 0,
            };

            let mut v = vec![0;width as usize * height as usize * bit_depth];
            self.read_pixels_into_buffer(x, y, width, height, format, pixel_type, &mut v[..]);
            v
        }

        unsafe fn read_pixels_into_pbo(
            &self,
            x: GLint,
            y: GLint,
            width: GLsizei,
            height: GLsizei,
            format: GLenum,
            pixel_type: GLenum,
        ) {
            if self.glReadPixels == ptr::null_mut() {
                _gl_impl_panic("glReadPixels");
                return;
            }

            unsafe {
                let func: extern "C" fn(GLint, GLint, GLsizei, GLsizei, GLenum, GLenum, *mut u8) = mem::transmute(self.glReadPixels);
                (func)(x, y, width, height, format, pixel_type, ptr::null_mut())
            }
        }

        fn sample_coverage(&self, value: GLclampf, invert: bool) {
            if self.glSampleCoverage == ptr::null_mut() {
                _gl_impl_panic("glSampleCoverage");
                return;
            }

            unsafe {
                let func: extern "C" fn(GLclampf, bool) = mem::transmute(self.glSampleCoverage);
                (func)(value, invert)
            }
        }

        fn polygon_offset(&self, factor: GLfloat, units: GLfloat) {
            if self.glPolygonOffset == ptr::null_mut() {
                _gl_impl_panic("glPolygonOffset");
                return;
            }

            unsafe {
                let func: extern "C" fn(GLfloat, GLfloat) = mem::transmute(self.glPolygonOffset);
                (func)(factor, units)
            }
        }

        fn pixel_store_i(&self, name: GLenum, param: GLint) {
            if self.glPixelStorei == ptr::null_mut() {
                _gl_impl_panic("glPixelStorei");
                return;
            }

            unsafe {
                let func: extern "C" fn(GLenum, GLint) = mem::transmute(self.glPixelStorei);
                (func)(name, param)
            }
        }

        fn gen_buffers(&self, n: GLsizei) -> Vec<GLuint> {
            if self.glGenBuffers == ptr::null_mut() {
                _gl_impl_panic("glGenBuffers");
                return Vec::new();
            }

            let mut v = vec![0;n.max(0) as usize];
            unsafe {
                let func: extern "C" fn(GLsizei, *mut GLuint) = mem::transmute(self.glGenBuffers);
                (func)(n, v.as_mut_ptr());
            }
            v
        }

        fn gen_renderbuffers(&self, n: GLsizei) -> Vec<GLuint> {
            if self.glGenRenderbuffers == ptr::null_mut() {
                _gl_impl_panic("glGenRenderbuffers");
                return Vec::new();
            }

            let mut v = vec![0;n.max(0) as usize];
            unsafe {
                let func: extern "C" fn(GLsizei, *mut GLuint) = mem::transmute(self.glGenRenderbuffers);
                (func)(n, v.as_mut_ptr());
            }
            v
        }

        fn gen_framebuffers(&self, n: GLsizei) -> Vec<GLuint> {
            if self.glGenFramebuffers == ptr::null_mut() {
                _gl_impl_panic("glGenFramebuffers");
                return Vec::new();
            }

            let mut v = vec![0;n.max(0) as usize];
            unsafe {
                let func: extern "C" fn(GLsizei, *mut GLuint) = mem::transmute(self.glGenFramebuffers);
                (func)(n, v.as_mut_ptr());
            }
            v
        }

        fn gen_textures(&self, n: GLsizei) -> Vec<GLuint> {
            if self.glGenTextures == ptr::null_mut() {
                _gl_impl_panic("glGenTextures");
                return Vec::new();
            }

            let mut v = vec![0;n.max(0) as usize];
            unsafe {
                let func: extern "C" fn(GLsizei, *mut GLuint) = mem::transmute(self.glGenTextures);
                (func)(n, v.as_mut_ptr());
            }
            v
        }

        fn gen_vertex_arrays(&self, n: GLsizei) -> Vec<GLuint> {
            if self.glGenVertexArrays == ptr::null_mut() {
                _gl_impl_panic("glGenVertexArrays");
                return Vec::new();
            }

            let mut v = vec![0;n.max(0) as usize];
            unsafe {
                let func: extern "C" fn(GLsizei, *mut GLuint) = mem::transmute(self.glGenVertexArrays);
                (func)(n, v.as_mut_ptr());
            }
            v
        }

        fn gen_vertex_arrays_apple(&self, n: GLsizei) -> Vec<GLuint> {
            if self.glGenVertexArraysAPPLE == ptr::null_mut() {
                _gl_impl_panic("glGenVertexArraysAPPLE");
                return Vec::new();
            }

            let mut v = vec![0;n.max(0) as usize];
            unsafe {
                let func: extern "C" fn(GLsizei, *mut GLuint) = mem::transmute(self.glGenVertexArraysAPPLE);
                (func)(n, v.as_mut_ptr());
            }
            v
        }

        fn gen_queries(&self, n: GLsizei) -> Vec<GLuint> {
            if self.glGenQueries == ptr::null_mut() {
                _gl_impl_panic("glGenQueries");
                return Vec::new();
            }

            let mut v = vec![0;n.max(0) as usize];
            unsafe {
                let func: extern "C" fn(GLsizei, *mut GLuint) = mem::transmute(self.glGenQueries);
                (func)(n, v.as_mut_ptr());
            }
            v
        }

        fn begin_query(&self, target: GLenum, id: GLuint) {
            if self.glBeginQuery == ptr::null_mut() {
                _gl_impl_panic("glBeginQuery");
                return;
            }

            unsafe {
                let func: extern "C" fn(GLenum, GLuint) = mem::transmute(self.glBeginQuery);
                (func)(target, id)
            }
        }

        fn end_query(&self, target: GLenum) {
            if self.glEndQuery == ptr::null_mut() {
                _gl_impl_panic("glEndQuery");
                return;
            }

            unsafe {
                let func: extern "C" fn(GLenum) = mem::transmute(self.glEndQuery);
                (func)(target)
            }
        }

        fn query_counter(&self, id: GLuint, target: GLenum) {
            if self.glQueryCounter == ptr::null_mut() {
                _gl_impl_panic("glQueryCounter");
                return;
            }

            unsafe {
                let func: extern "C" fn(GLuint, GLenum) = mem::transmute(self.glQueryCounter);
                (func)(id, target)
            }
        }

        fn get_query_object_iv(&self, id: GLuint, pname: GLenum) -> i32 {
            if self.glGetQueryObjectiv == ptr::null_mut() {
                _gl_impl_panic("glGetQueryObjectiv");
                return 0;
            }

            unsafe {
                let func: extern "C" fn(GLuint, GLenum) -> i32 = mem::transmute(self.glGetQueryObjectiv);
                (func)(id, pname)
            }
        }

        fn get_query_object_uiv(&self, id: GLuint, pname: GLenum) -> u32 {
            if self.glGetQueryObjectuiv == ptr::null_mut() {
                _gl_impl_panic("glGetQueryObjectuiv");
                return 0;
            }

            unsafe {
                let func: extern "C" fn(GLuint, GLenum) -> u32 = mem::transmute(self.glGetQueryObjectuiv);
                (func)(id, pname)
            }
        }

        fn get_query_object_i64v(&self, id: GLuint, pname: GLenum) -> i64 {
            if self.glGetQueryObjecti64v == ptr::null_mut() {
                _gl_impl_panic("glGetQueryObjecti64v");
                return 0;
            }

            unsafe {
                let func: extern "C" fn(GLuint, GLenum) -> i64 = mem::transmute(self.glGetQueryObjecti64v);
                (func)(id, pname)
            }
        }

        fn get_query_object_ui64v(&self, id: GLuint, pname: GLenum) -> u64 {
            if self.glGetQueryObjectui64v == ptr::null_mut() {
                _gl_impl_panic("glGetQueryObjectui64v");
                return 0;
            }

            unsafe {
                let func: extern "C" fn(GLuint, GLenum) -> u64 = mem::transmute(self.glGetQueryObjectui64v);
                (func)(id, pname)
            }
        }

        fn delete_queries(&self, queries: &[GLuint]) {
            if self.glDeleteQueries == ptr::null_mut() {
                _gl_impl_panic("glDeleteQueries");
                return;
            }

            unsafe {
                let func: extern "C" fn(GLsizei, *const GLuint) = mem::transmute(self.glDeleteQueries);
                (func)(queries.len() as GLsizei, queries.as_ptr())
            }
        }

        fn delete_vertex_arrays(&self, vertex_arrays: &[GLuint]) {
            if self.glDeleteVertexArrays == ptr::null_mut() {
                _gl_impl_panic("glDeleteVertexArrays");
                return;
            }

            unsafe {
                let func: extern "C" fn(GLsizei, *const GLuint) = mem::transmute(self.glDeleteVertexArrays);
                (func)(vertex_arrays.len() as GLsizei, vertex_arrays.as_ptr())
            }
        }

        fn delete_vertex_arrays_apple(&self, vertex_arrays: &[GLuint]) {
            if self.glDeleteVertexArraysAPPLE == ptr::null_mut() {
                _gl_impl_panic("glDeleteVertexArraysAPPLE");
                return;
            }

            unsafe {
                let func: extern "C" fn(GLsizei, *const GLuint) = mem::transmute(self.glDeleteVertexArraysAPPLE);
                (func)(vertex_arrays.len() as GLsizei, vertex_arrays.as_ptr())
            }
        }

        fn delete_buffers(&self, buffers: &[GLuint]) {
            if self.glDeleteBuffers == ptr::null_mut() {
                _gl_impl_panic("glDeleteBuffers");
                return;
            }

            unsafe {
                let func: extern "C" fn(GLsizei, *const GLuint) = mem::transmute(self.glDeleteBuffers);
                (func)(buffers.len() as GLsizei, buffers.as_ptr())
            }
        }

        fn delete_renderbuffers(&self, renderbuffers: &[GLuint]) {
            if self.glDeleteRenderbuffers == ptr::null_mut() {
                _gl_impl_panic("glDeleteRenderbuffers");
                return;
            }

            unsafe {
                let func: extern "C" fn(GLsizei, *const GLuint) = mem::transmute(self.glDeleteRenderbuffers);
                (func)(renderbuffers.len() as GLsizei, renderbuffers.as_ptr())
            }
        }

        fn delete_framebuffers(&self, framebuffers: &[GLuint]) {
            if self.glDeleteFramebuffers == ptr::null_mut() {
                _gl_impl_panic("glDeleteFramebuffers");
                return;
            }

            unsafe {
                let func: extern "C" fn(GLsizei, *const GLuint) = mem::transmute(self.glDeleteFramebuffers);
                (func)(framebuffers.len() as GLsizei, framebuffers.as_ptr())
            }
        }

        fn delete_textures(&self, textures: &[GLuint]) {
            if self.glDeleteTextures == ptr::null_mut() {
                _gl_impl_panic("glDeleteTextures");
                return;
            }

            unsafe {
                let func: extern "C" fn(GLsizei, *const GLuint) = mem::transmute(self.glDeleteTextures);
                (func)(textures.len() as GLsizei, textures.as_ptr())
            }
        }

    }

    fn _gl_impl_panic(s: &str) { println!("OpenGL function not loaded: {}", s); }
}

// #[no_mangle]
/* pub extern "C" */ fn main(/*_argc: isize, _argv: *const *const u8*/) /* -> isize */ {

    let (hwnd, context_arc) = match create_window() {
        Ok(o) => o,
        Err(_) => return, // return -1,
    };

    if let Err(_) = create_opengl_context(hwnd, context_arc) {
        // log error but do not panic: opengl context is not required for running
        // println!("ERROR initializing OpenGL, continuing in software rendering mode: {}", e.message());
    }

    if let Err(_) = unsafe { set_window_transparency_and_region(hwnd, /* transparent window */ true, /* no decorations */ true) } {
        // println!("ERROR setting transparency and region: {}", e.message());
    }


    let mut msg: MSG = unsafe { mem::zeroed() };

    unsafe {
        ShowWindow(hwnd, SW_SHOWNORMAL | SW_MAXIMIZE);

        while GetMessageW(&mut msg, hwnd, 0, 0) > 0 {
            TranslateMessage(&msg);
            DispatchMessageW(&msg);
        }
    }

    // msg.wParam as isize
}