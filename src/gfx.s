/*data gfx handle


Lib = '/Users/nikita/Documents/git/symta/bindings/gfx/lib/main'
G_load_png = ffi_load Lib load_png
G_save_png = ffi_load Lib save_png
G_gfx_w = ffi_load Lib gfx_w
G_gfx_h = ffi_load Lib gfx_h

gfx.w = _ffi_call \(uint32_t ptr) G_gfx_w Me.handle
gfx.h = _ffi_call \(uint32_t ptr) G_gfx_h Me.handle

load_png Filename =
| Handle = _ffi_call \(ptr text) G_load_png Filename
| new_gfx Handle

save_png Filename Gfx = _ffi_call \(void text ptr) G_save_png Filename Gfx.handle
*/

/*ffi Lib load_png ptr text
ffi Lib save_png void text ptr*/

export load_png save_png
