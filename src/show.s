use show_ reader

show F =
| Events = []
| while no Events.locate{? >< quit}
  | G = F Events
  | Result = show_gfx G.handle
  | when Result <> '': bad "show: [Result]"
  | Events <= parse: show_get_events
| show_close
| Void

export show
