use gfx reader gui_

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


//FIXME: create a default widgets

GUI = Void

widget.input @E =
widget.items = Void
widget.render = Me
widget.draw G P =
widget.popup = Void
widget.cursor = \default
widget.parent = 
widget.`!parent` P = 
widget.itemAt Point XY WH =
| Items = $items
| when no Items: leave [Me XY WH]
| Item = Items.find{I => Point.in{I.meta_}}
| when no Item: leave [Me XY WH]
| [IX IY W H] = Item.meta_
| Item.itemAt{Point-[IX IY] XY+[IX IY] [W H]}
widget.x = 0
widget.y = 0
widget.w = 0
widget.h = 0
widget.above_all = 0
widget.wants_focus = 0
widget.wants_focus_rect = 0

type spacer.widget{W H} w/W h/H
spacer.as_text = "#spacer{[$w] [$h]}"

type tabs.~{Init Tabs} tab all/Tabs | $pick{Init}
tabs.pick TabName =
| when $tab: (get_gui).focus_widget <= Void
| $tab <= $all.TabName
| when no $tab: bad "tabs.pick: no [TabName]"
tabs.as_text = "#tabs{[$tab]}"
tabs._ Method Args =
| Args.0 <= Args.0.tab
| Args.apply_method{Method}

type canvas.widget{W H P} w/W h/H paint/P
canvas.draw G P = case Me (F<~).paint: F G P $w $h 

type lay.widget{D S Xs} w/1 h/1 dir/D spacing/S items/Xs{(new_meta ? [0 0 1 1])}
lay.draw G P =
| D = $dir
| S = $spacing
| Is = $items
| Rs = Is{?render}
| case $dir v | $w <= Rs{?w}.max; $h <= Rs{?h}.infix{S}.sum
            h | $h <= Rs{?h}.max; $w <= Rs{?w}.infix{S}.sum
| N = 0
| for R Rs
  | W = R.w
  | H = R.h
  | Rect = Is^pop.meta_
  | RX = case D v(0) h(N)
  | RY = case D v(N) h(0)
  | G.blit{P+[RX RY] R}
  | Rect.init{[RX RY W H]}
  | N <= case D v(N+H+S) h(N+W+S)

type dlg.widget{Xs w/Void h/Void} w/W h/H ws items rs
| $ws <= Xs{[X Y W]=>[X Y (new_meta W [0 0 1 1])]}
| $items <= $ws{}{?2}.flip
dlg.render =
| when got!it $items.locate{?above_all}:
  | swap $items.0 $items.it
  | swap $ws.($ws.size-1) $ws.($ws.size-it-1)
  | $items.0.above_all <= 0
| have $w: $ws{}{?0 + ?2.render.w}.max
| have $h: $ws{}{?1 + ?2.render.h}.max
| Me
dlg.draw G P =
| for [X Y W] $ws
  | R = W.render
  | Rect = W.meta_
  | !X + R.x
  | !Y + R.y
  | Rect.init{[X Y R.w R.h]}
  | G.blit{P+[X Y] R}

type gui{Root cursor/host}
  root/Root timers/[] mice_xy/[0 0] widget_cursor/default result/Void fb/Void
  keys/(t) popup/Void last_widget/(widget) focus_widget/Void
  focus_xy/[0 0] focus_wh/[0 0] mice_focus mice_focus_xy/[0 0] click_time/(t)
  cursor/Cursor host_cursor/0
| GUI <= Me
| $fb <= gfx 1 1
| show: Es => | GUI.input{Es}
              | GUI.render
| when got $fb
  | $fb.free
  | $fb <= Void
| R = $result
| $result <= Void
| GUI <= Void
| leave R
gui.render =
| FB = $fb
| when no FB: leave Void
| R = $root.render
| W = R.w
| H = R.h
| when W <> FB.w or H <> FB.h:
  | FB.free
  | FB <= gfx W H
  | $fb <= FB
| FB.blit{[0 0] R}
| when got!fw $focus_widget:
  | when fw.wants_focus_rect
    | P = $focus_xy+[fw.x fw.y]
    | WH = if fw.w and fw.h then [fw.w fw.h] else $focus_wh
    | FB.rect{#FFFF00 0 P.0-1 P.1-1 WH.0+2 WH.1+2}
| C = $widget_cursor
| when got C
  | XY = GUI.mice_xy
  | CG = if C >< default then $cursor else C
  | when got CG and host <> CG:
    | when $host_cursor: show_cursor 0
    | $host_cursor <= 0
    | FB.blit{XY-CG.hotspot CG}
  | when host >< CG and not $host_cursor:
    | show_cursor 1
    | $host_cursor <= 1
  | Pop = $popup
  | when got Pop: FB.blit{XY-[0 Pop.h] Pop}
| FB
gui.add_timer Interval Handler =
| [@!Me.timers [Interval (clock)+Interval Handler]]
gui.update_timers Time =
| Ts = $timers
| less Ts.size: leave 0
| $timers <= [] // user code can insert additional timers
| Remove = []
| for [N T] Ts.i: case T [Interval Expiration Fn]:
  | when Time >> Expiration
    | if got Fn{} then Ts.N.1 <= (Time)+Interval
      else push N Remove
| when Remove.size
  | N = -1
  | Ts <= Ts.skip{X=>Remove.locate{!N+1}^got}
| when $timers.size: Ts <= [@Ts @$timers]
| $timers <= Ts
| 0
gui.input Es =
| T = clock
| $update_timers{T}
| [NW NW_XY NW_WH] = $root.itemAt{$mice_xy [0 0] [0 0]} //new widget
| $popup <= NW.popup
| $widget_cursor <= NW.cursor
| for E Es: case E
  [mice_move XY]
    | $mice_xy.init{XY}
    | if $mice_focus
      then $mice_focus.input{mice_move XY XY-$mice_focus_xy}
      else NW.input{mice_move XY XY-NW_XY}
    | LW = $last_widget
    | when LW^address <> NW^address:
      | when got LW: LW.input{mice over 0 XY}
      | $last_widget <= NW
      | NW.input{mice over 1 XY}
  [mice Button State]
    | MP = $mice_xy
    | if $mice_focus
      then | LastClickTime = $click_time.Button
           | when got LastClickTime and T-LastClickTime < 0.25:
             | NW.input{mice "double_[Button]" 1 MP-NW_XY}
           | $click_time.Button <= T
           | $mice_focus.input{mice Button State MP-NW_XY}
           | less State: $mice_focus <= 0
      else | $mice_focus <= NW
           | $mice_focus_xy.init{NW_XY}
           | NW.input{mice Button State MP-NW_XY}
    | when State and NW.wants_focus:
      | $focus_xy <= NW_XY
      | $focus_wh <= NW_WH
      | FW = $focus_widget
      | when FW^address <> NW^address:
        | when got FW: FW.input{focus 0 MP-$focus_xy}
        | $focus_widget <= NW
        | NW.input{focus 1 MP-NW_XY}
  [key Key State] | $keys.Key <= State
                  | D = if got $focus_widget then $focus_widget else NW
                  | D.input{key Key State}
  Else |
| Void
gui.exit @Result =
| $result <= case Result [R](R) Else(Void)
| $fb <= Void

get_gui = GUI

export gui get_gui tabs lay dlg spacer
       ffi_alloc ffi_free new_cmap gfx rgb rgba
