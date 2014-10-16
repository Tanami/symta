use common gfx gui

type view.widget{W H M} w/W h/H main/M paused/1 sel/[] last_click/[]
                        ack a m/[0 0] g visible notes speed/20 frame
                        sel_blink/[0 0 0]
| $g <= gfx W H
view.world = $main.world
view.clear_clicks = $last_click <= [[Void 0] 0]
view.notify Player Text life/6.0 =
| when Player >< $world.this_player
  | $notes <= [@$notes [(clock)+Life Text]]

view.draw_unit U =
| Cycle = $world.cycle
| when U.last_drawn >< Cycle: leave 0
| U.last_drawn <= Cycle
| G = $g
| TN = $world.tileset_name
| SO = $world.this_player.view
| [BId BT1 BT2] = $sel_blink
| Blink = Cycle < BT1 or (Cycle > BT2 and Cycle < BT2 + 12)
| Id = U.id
| Col = $world.tints.(U.color)
| D = dirN U.dir
| UG = U.sprite.TN.(U.frame).D
| [X Y] = U.disp - SO + U.size*16
| [SW SH] = U.selection
| RX = X - SW/2
| RY = Y - SH/2
| G.blit{[X-UG.w/2 Y-UG.h/2] UG map(Col) flipX(D > 4 and not U.building)}
| !$world.cycle + 1

view.normalize_view =
| SO = $world.this_player.view
| [X Y] = SO
| M = $world.margin*32
| WW = $world.w*32
| WH = $world.h*32
| NewSO = [X.clip{M M+WW-$g.w} Y.clip{M M+WW+$g.h}]
| when SO <> NewSO: $world.this_player.view <= NewSO

view.render =
| $normalize_view
| G = $g
| Cs = $world.units
| TN = $world.tileset_name
| TP = $world.this_player
| Cycle = $world.cycle
//| Side = TP.side
| SO = TP.view
| [IX IY] = -(SO%32)
| [CX CY] = SO/32
| CW = @int: @ceil ($w.float+31.0)/32.0
| CH = @int: @ceil ($h.float+31.0)/32.0
| W = $world.w
| H = $world.h
| Vs = [] // visible units
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
    | when!it C.content: [it@!Vs]
    | G.blit{[PX PY] C.gfx}
    | !PX + 32
    | !X + 1
  | !PY + 32
  | !Y + 1
| Vs = Vs{@r$[] V<-&0 => [V @V.content_next^r]}.join
| Vs = Vs.sort{[?layer ?disp.1] < [??layer ??disp.1]}
| for X Vs.keep{?building}: $draw_unit{X}
| for X Vs.skip{?building}: $draw_unit{X}
//| Vs{[?type.id ?building]}^say
| G


export view
