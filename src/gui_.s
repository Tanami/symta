ffi_begin gui
ffi show_gfx.text Gfx.ptr
ffi show_close.void
ffi show_get_events.text
ffi show_cursor.void State.int
ffi show_sleep.void Milliseconds.u4
ffi show_get_ticks.u4

ffi show_sound_load.int Filename.text Music.int
ffi show_sound_free.void Id.int
ffi show_sound_play.int Id.int Channel.int Loop.int
ffi show_sound_playing.int Channel.int

dummy = No

export 'dummy'
