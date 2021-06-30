#![allow(non_snake_case)]
#![windows_subsystem = "windows"]
#![cfg_attr(not(feature = "std"), feature(default_alloc_error_handler))]

pub const COLOR_BUFFER_BIT: GLenum = 0x00004000;
pub const DEPTH_BUFFER_BIT: GLenum = 0x00000100;

extern crate core;
extern crate alloc;
extern crate gl_context_loader as gl;

use gl::{
    GenericGlContext as WglContext,
    GLenum,
};

use core::{ptr, mem};
use core::cell::RefCell;
#[cfg(not(feature = "std"))]
use core::panic::PanicInfo;

use alloc::rc::Rc;
use alloc::vec::Vec;
use alloc::boxed::Box;

use libc_alloc::LibcAlloc;

#[global_allocator]
static ALLOCATOR: LibcAlloc = LibcAlloc;

use winapi::{
    ctypes::c_void,
    shared::{
        windef::{HWND, RECT, HGLRC, HDC},
        ntdef::HRESULT,
        minwindef::{LPARAM, WPARAM, LRESULT, BOOL, HINSTANCE, TRUE},
    },
    um::{
        errhandlingapi::GetLastError,
        libloaderapi::{LoadLibraryW, FreeLibrary, GetModuleHandleW, GetProcAddress},
        winuser::{
            RegisterClassW, ShowWindow, CreateWindowExW, DefWindowProcW,
            GetMessageW, TranslateMessage, DispatchMessageW, GetClientRect,
            PostQuitMessage, GetWindowLongPtrW, SetWindowLongPtrW,
            WNDCLASSW, CS_HREDRAW, CS_VREDRAW, CS_OWNDC, WM_ERASEBKGND,
            WS_OVERLAPPEDWINDOW, WS_POPUP, CW_USEDEFAULT, SW_MAXIMIZE, SW_SHOWNORMAL,
            MSG, WS_EX_APPWINDOW, CREATESTRUCTW, GWLP_USERDATA, WM_RBUTTONDOWN, WM_COMMAND,
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
    gl: Rc<WglContext>,
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

    let mut func_name = encode_ascii("wglSwapIntervalEXT");
    let wglSwapIntervalEXT = unsafe {
        let proc_address = wglGetProcAddress(func_name.as_mut_ptr());
        if proc_address == ptr::null_mut() {
            None
        } else {
            let w: unsafe extern "system" fn(i32) -> i32 = mem::transmute(proc_address);
            Some(w)
        }
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
        let loaded = load_gl_context(opengl32_dll);
        if let Some(wglSwapIntervalEXT) = wglSwapIntervalEXT {
            unsafe { wglSwapIntervalEXT(0) }; // disable vsync
        }
        unsafe { wglMakeCurrent(ptr::null_mut(), ptr::null_mut()) };

        *lock = Some(WindowsGlContext {
            hrc: hRC,
            opengl32_dll,
            gl: Rc::new(loaded),
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

    let mut os_vinfo: OSVERSIONINFOW = mem::zeroed();
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
            WM_ERASEBKGND => return 1,
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
            WM_RBUTTONDOWN => {
                use winapi::um::winuser::{
                    CreatePopupMenu, InsertMenuW, TrackPopupMenu, SetForegroundWindow,
                    GetCursorPos,
                    MF_BYPOSITION, MF_STRING, TPM_TOPALIGN, TPM_LEFTALIGN
                };
                use winapi::shared::windef::POINT;
                let mut pos: POINT = POINT { x: 0, y: 0 };
                GetCursorPos(&mut pos);
                let hPopupMenu = CreatePopupMenu();
                let mut a = encode_wide("Exit");
                let mut b = encode_wide("Play");
                InsertMenuW(hPopupMenu, 0, MF_BYPOSITION | MF_STRING, 0, a.as_mut_ptr());
                InsertMenuW(hPopupMenu, 0, MF_BYPOSITION | MF_STRING, 0, b.as_mut_ptr());
                SetForegroundWindow(hwnd);
                TrackPopupMenu(hPopupMenu, TPM_TOPALIGN | TPM_LEFTALIGN, pos.x, pos.y, 0, hwnd, ptr::null_mut());
            },
            WM_COMMAND => {

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

                        // #[cfg(feature = "std")]
                        // use gleam::gl::Gl;

                        context.gl.viewport(0, 0, rect.right, rect.bottom);
                        context.gl.clear_color(0.0, 0.0, 1.0, 0.0);
                        context.gl.clear_depth(1.0);
                        context.gl.clear(COLOR_BUFFER_BIT | DEPTH_BUFFER_BIT);

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

                        context.gl.flush();
                        SwapBuffers(hDC);
                        // context.gl.finish(); // TODO: OpenGL 3?

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

pub fn load_gl_context(opengl32_dll: HINSTANCE) -> WglContext {

    fn get_func(s: &str, opengl32_dll: HINSTANCE) -> *mut gl::c_void {
        let mut func_name = encode_ascii(s);
        let addr1 = unsafe { wglGetProcAddress(func_name.as_mut_ptr()) };
        (if addr1 != ptr::null_mut() {
            addr1
        } else {
            unsafe { GetProcAddress(opengl32_dll, func_name.as_mut_ptr()) }
        }) as *mut gl::c_void
    }

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

fn main() {
    main_inner();
}

fn main_inner() -> isize {

    let (hwnd, context_arc) = match create_window() {
        Ok(o) => o,
        Err(_) => return -1,
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

    msg.wParam as isize
}