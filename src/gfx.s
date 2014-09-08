use gfx_
data gfx handle

GFX_RGB  = 0
GFX_RGBA = 1
GFX_MAP  = 2

gfx W H = new_gfx (new_gfx_ W H)
gfx.w = gfx_w Me.handle
gfx.h = gfx_h Me.handle
gfx.hotspot = [(gfx_hotspot_x Me.handle) (gfx_hotspot_y Me.handle)]
gfx.get X Y = gfx_get Me.handle X Y
gfx.set X Y Color = gfx_set Me.handle X Y Color
gfx.clear Color = gfx_clear Me.handle Color
gfx.line Color A B = gfx_line Me.handle Color A.0 A.1 B.0 B.1
gfx.rect Color Fill A B = gfx_rect Me.handle Color Fill A.0 A.1 B.0 B.1
gfx.circle Color Fill C R = gfx_circle Me.handle Color Fill C.0 C.1 R
gfx.resize W H = gfx_resize Me.handle W H
gfx.blit P Gfx Rect FlipX FlipY =
| Map = 0
| [SX SY SW SH] = if Rect then Rect else [0 0 Gfx.w Gfx.h]
| gfx_blit Me.handle P.0 P.1 Gfx.handle SX SY SW SH FlipX FlipY Map

load_png Filename =
| Handle = gfx_load_png Filename
| new_gfx Handle

save_png Filename Gfx = gfx_save_png Filename Gfx.handle

rgb R G B = form R*#10000 + G*#100 + B
rgba R G B A = form A*#1000000 + R*#10000 + G*#100 + B

export gfx load_png save_png 'rgb' 'rgba' 'GFX_RGB' 'GFX_RGBA' 'GFX_MAP'
