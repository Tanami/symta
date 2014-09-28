use show_ reader

// loop body made a separate routine, so that GC will free all per-frame data
event_loop F =
| Events = @parse: show_get_events
| when got Events.locate{? >< quit}: leave [Void]
| G = F Events
| less G.is_gfx: leave [G]
| Result = show_gfx G.handle
| when Result <> '': bad "show: [Result]"
| Void

show F =
| while 1
  | R = event_loop F
  | when R.is_list
    | show_close
    | leave R.0

export show
