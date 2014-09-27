use gfx_
data gfx handle

GFX_RGB  = 0
GFX_RGBA = 1
GFX_MAP  = 2

ffi_alloc Size = ffi_alloc_ Size
ffi_free Ptr = ffi_free_ Ptr

new_cmap Xs =
| when Xs.size > 256: bad "cant set color map larger than 256"
| P = ffi_alloc Xs.size*4
| for [I E] Xs.i: _ffi_set uint32_t P I E
| P

gfx W H = new_gfx (new_gfx_ W H)
gfx.free = free_gfx Me.handle
gfx.w = gfx_w Me.handle
gfx.h = gfx_h Me.handle
gfx.hotspot = [(gfx_hotspot_x Me.handle) (gfx_hotspot_y Me.handle)]
gfx.set_hotspot [X Y] = gfx_set_hotspot Me.handle X Y
gfx.get X Y = gfx_get Me.handle X Y
gfx.set X Y Color = gfx_set Me.handle X Y Color
gfx.clear Color = gfx_clear Me.handle Color
gfx.line Color A B = gfx_line Me.handle Color A.0 A.1 B.0 B.1
gfx.rect Color Fill X Y W H = gfx_rect Me.handle Color Fill X Y W H
gfx.circle Color Fill C R = gfx_circle Me.handle Color Fill C.0 C.1 R
gfx.resize W H = gfx_resize Me.handle W H
gfx.cmap =
| P = gfx_cmap Me.handle
| less P: leave 0
| dup I 256: _ffi_get uint32_t P I
gfx.set_cmap NewCM =
| when NewCM.size > 256: bad "cant set color map larger than 256"
| P = gfx_enable_cmap Me.handle
| for [I E] NewCM.i: _ffi_set uint32_t P I E
gfx.blit P Src rect/0 flipX/0 flipY/0 map/0 =
| less Src.is_gfx:
  | Src.draw{Me P}
  | leave 0
| [SX SY SW SH] = if Rect then Rect else [0 0 Src.w Src.h]
| gfx_blit Me.handle P.0 P.1 Src.handle SX SY SW SH FlipX FlipY Map
gfx.margins =
| P = gfx_margins Me.handle
| [(_ffi_get uint32_t P 0) (_ffi_get uint32_t P 1)
   (_ffi_get uint32_t P 2) (_ffi_get uint32_t P 3)]
gfx.cut X Y W H =
| G = gfx W H
| G.clear{0}
| CMap = Me.cmap
| when CMap: G.cmap <= CMap
| G.blit{[0 0] Me rect [X Y W H]}
| G
gfx.copy = Me.cut{0 0 Me.w Me.h}
gfx.frames W H =
| GW = Me.w
| dup I GW*Me.h/(W*H): Me.cut{I*W%GW I*W/GW*H W H}
gfx.render = Me
gfx.as_text = "#gfx{[Me.w] [Me.h]}"

load_png Filename =
| Handle = gfx_load_png Filename
| new_gfx Handle

save_png Filename Gfx = gfx_save_png Filename Gfx.handle

gfx_load Filename = load_png Filename

rgb R G B = form R*#10000 + G*#100 + B
rgba R G B A = form A*#1000000 + R*#10000 + G*#100 + B

export gfx load_png save_png gfx_load new_cmap ffi_alloc ffi_free 'rgb' 'rgba' 'GFX_RGB' 'GFX_RGBA' 'GFX_MAP'
