ffi_begin gfx
ffi new_gfx_: new_gfx.ptr W.u4 H.u4
ffi free_gfx.void Gfx.ptr
ffi gfx_load_png.ptr Filename.text
ffi gfx_save_png.void Filename.text Gfx.ptr
ffi gfx_w.u4 Gfx.ptr
ffi gfx_h.u4 Gfx.ptr
ffi gfx_x.int Gfx.ptr
ffi gfx_y.int Gfx.ptr
ffi gfx_set_xy.void Gfx.ptr X.int Y.int
ffi gfx_get.u4 Gfx.ptr X.int Y.int
ffi gfx_set.void Gfx.ptr X.int Y.int Color.u4
ffi gfx_clear.void Gfx.ptr Color.u4
ffi gfx_line.void Gfx.ptr Color.u4 SX.int SY.int DX.int DY.int
ffi gfx_rect.void Gfx.ptr Color.u4 Fill.int X.int Y.int W.int H.int
ffi gfx_circle.void Gfx.ptr Color.u4 Fill.int X.int Y.int R.int
ffi gfx_triangle.void Gfx.ptr Color.u4 AX.int AY.int BX.int BY.int CX.int CY.int
ffi gfx_cmap.ptr Gfx.ptr
ffi gfx_enable_cmap.ptr Gfx.ptr
ffi gfx_resize.void Gfx.ptr W.int H.int
ffi gfx_set_bflags_flip_x.void Gfx.ptr
ffi gfx_set_bflags_flip_y.void Gfx.ptr
ffi gfx_set_blit_bright.void Gfx.ptr Amount.int
ffi gfx_set_blit_dither.void Gfx.ptr Amount.int
ffi gfx_set_blit_rect.void Gfx.ptr BX.int BY.int BW.int BH.int
ffi gfx_set_recolor_map.void Gfx.ptr CMap.ptr
ffi gfx_set_zdata.void Gfx.ptr ZData.ptr
ffi gfx_blit.void Gfx.ptr X.int Y.int Src.ptr
ffi gfx_margins.ptr Gfx.ptr
ffi ffi_alloc_.ptr Size.int
ffi ffi_free_.void Ptr.ptr
ffi ffi_memset_.void Ptr.ptr Value.int Count.u4

dummy = No

export 'dummy'
