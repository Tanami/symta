ffi_begin gfx_ '/Users/nikita/Documents/git/symta/bindings/gfx/lib/main'
ffi gfx_load_png.ptr Filename.text
ffi gfx_save_png.void Filename.text Gfx.ptr
ffi gfx_w.uint32_t Gfx.ptr
ffi gfx_h.uint32_t Gfx.ptr
ffi gfx_type.uint32_t Gfx.ptr

dummy = Void

export 'dummy'
