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
| $g

export view
