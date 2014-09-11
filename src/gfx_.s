ffi_begin gfx_ '/Users/nikita/Documents/git/symta/bindings/gfx/lib/main'
ffi new_gfx_: new_gfx.ptr W.uint32_t H.uint32_t
ffi gfx_load_png.ptr Filename.text
ffi gfx_save_png.void Filename.text Gfx.ptr
ffi gfx_w.uint32_t Gfx.ptr
ffi gfx_h.uint32_t Gfx.ptr
ffi gfx_hotspot_x.uint32_t Gfx.ptr
ffi gfx_hotspot_y.uint32_t Gfx.ptr
ffi gfx_get.uint32_t Gfx.ptr X.int Y.int
ffi gfx_set.void Gfx.ptr X.int Y.int Color.uint32_t
ffi gfx_clear.void Gfx.ptr Color.uint32_t
ffi gfx_line.void Gfx.ptr Color.uint32_t SX.int SY.int DX.int DY.int
ffi gfx_rect.void Gfx.ptr Color.uint32_t Fill.int X.int Y.int W.int H.int
ffi gfx_circle.void Gfx.ptr Color.uint32_t Fill.int X.int Y.int R.int
ffi gfx_cmap.ptr Gfx.ptr
ffi gfx_set_cmap.void Gfx.ptr CMap.ptr
ffi gfx_resize.void Gfx.ptr W.int H.int
ffi gfx_blit.void Gfx.ptr X.int Y.int Src.ptr SX.int SY.int W.int H.int
                  FlipX.int FlipY.int Map.ptr

dummy = Void

export 'dummy'
