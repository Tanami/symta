use gfx_
data gfx handle

/*GFX_RGB  = 0
GFX_RGBA = 1
GFX_MAP  = 2*/

gfx.w = gfx_w Me.handle
gfx.h = gfx_h Me.handle
gfx.type = gfx_type Me.handle

load_png Filename =
| Handle = gfx_load_png Filename
| new_gfx Handle

save_png Filename Gfx = gfx_save_png Filename Gfx.handle

abc X = X+1

export load_png save_png 'abc' //'GFX_RGB' 'GFX_RGBA' 'GFX_MAP'
