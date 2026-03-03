//! Integration tests for JIT memory allocation on macOS aarch64.
//! Tests that Cranelift can allocate executable memory and our JIT
//! engine initializes correctly.

use std::ptr;

extern "C" {
    fn sys_icache_invalidate(start: *mut libc::c_void, size: libc::size_t);
}

#[test]
fn test_raw_jit_mmap() {
    unsafe {
        let size = 4096usize;

        #[cfg(target_os = "macos")]
        let flags = libc::MAP_PRIVATE | libc::MAP_ANON | libc::MAP_JIT;
        #[cfg(not(target_os = "macos"))]
        let flags = libc::MAP_PRIVATE | libc::MAP_ANON;

        let ptr = libc::mmap(
            ptr::null_mut(),
            size,
            libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC,
            flags,
            -1,
            0,
        );
        assert_ne!(ptr, libc::MAP_FAILED, "mmap with MAP_JIT failed");

        #[cfg(target_arch = "aarch64")]
        {
            libc::pthread_jit_write_protect_np(0);
            *(ptr as *mut u32) = 0xD65F03C0; // RET
            libc::pthread_jit_write_protect_np(1);
            sys_icache_invalidate(ptr, size);
        }

        assert_eq!(libc::munmap(ptr, size), 0);
    }
}

#[test]
fn test_cranelift_jit_module_creation() {
    use cranelift_codegen::isa;
    use cranelift_codegen::settings::{self, Configurable};
    use cranelift_jit::{JITBuilder, JITModule};
    use target_lexicon::Triple;

    let mut flag_builder = settings::builder();
    flag_builder.set("opt_level", "speed").unwrap();
    flag_builder.set("is_pic", "false").unwrap();

    let isa_builder = isa::lookup(Triple::host()).expect("unsupported host");
    let isa = isa_builder.finish(settings::Flags::new(flag_builder)).unwrap();
    let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
    let _module = JITModule::new(builder);
}

#[test]
fn test_jit_engine_new() {
    let engine = haskellboy_jit::engine::JITEngine::new();
    assert!(!engine.has_block(0x0100));
}
