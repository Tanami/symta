/*
ffi_begin gfx_ '/Users/nikita/Documents/git/symta/bindings/gfx/lib/main'
ffi gfx_load_png.ptr Filename.text
ffi gfx_save_png.void Filename.text Gfx.ptr
ffi gfx_w.uint32_t Gfx.ptr
ffi gfx_h.uint32_t Gfx.ptr


data gfx handle

gfx.w = gfx_w Me.handle
gfx.h = gfx_h Me.handle

load_png Filename =
| Handle = gfx_load_png Filename
| new_gfx Handle

save_png Filename Gfx = gfx_save_png Filename Gfx.handle
*/

export load_png save_png
