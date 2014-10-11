use common gfx gui

type view.widget{W H M} w/W h/H main/M world paused/1 sel/[] last_click/[]
                        ack a m/[0 0] g visible notes speed/20
| $g <= gfx W H
| $world <= $main.world

view.clear_clicks = $last_click <= [[Void 0] 0]
view.notify Player Text life/6.0 =
| when Player >< $world.this_player
  | $notes <= [@$notes [(clock)+Life Text]]

view.render =
| G = $g
| Cs = $world.units
| TN = $world.tileset_name
| TP = $world.this_player
//| Side = TP.side
| [CX CY] = $world.top_left
| [IX IY] = [0 0]
| CW = @int: @ceil ($w.float+31.0)/32.0
| CH = @int: @ceil ($h.float+31.0)/32.0
| W = $world.w
| H = $world.h
| Vs = []
| EX = @clip 0 W CX+CW
| EY = @clip 0 H CY+CH
| Y = CY
| PY = IY
| while Y < EY
  | X = CX + Y*W
  | EX = EX + Y*W
  | PX = IX
  | while X < EX
    | C = Cs.X
    | G.blit{[PX PY] C.gfx}
    | !PX + 32
    | !X + 1
  | !PY + 32
  | !Y + 1
| G


export view
