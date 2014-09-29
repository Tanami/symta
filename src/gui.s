use gfx show

int.in Start End = Start << Me and Me < End
list.in [RX RY RW RH] = $0.in{RX RX+RW} and $1.in{RY RY+RH}
rects_intersect [AX AY AW AH] [BX BY BW BH] = AX<BX+BW and AY<BY+BH and BX<AX+AW and BY<AY+AH

GUI = Void

Skin = Void
SkinCache = Void
ImgCache = Void
FontCache = Void
FontTints = Void

data widget

widget.input @E =
widget.items = Void
widget.render = Me
widget.draw G P =
widget.popup = Void
widget.cursor = \point
widget.itemAt Point XY =
| Items = $items
| when no Items: leave [XY Me]
| Item = Items.find{I => Point.in{I.meta_}}
| when no Item: leave [XY Me]
| ItemXY = Item.meta_.take{2}
| Item.itemAt{Point-ItemXY XY+ItemXY}

cfg P = P.get.utf8.lines{}{?parse}.skip{is.[]}

setSkin Path =
| Skin <= Path
| SkinCache <= m
| ImgCache <= m
| FontCache <= m
| FontTints <= "[Skin]/font/tints.txt"^cfg{}.map{[?0 ?.tail.pad{256 #FF000000}^new_cmap]}.as_map

data font glyphs widths height

font N = init FontCache.N:
| Path = "[Skin]/font/[N]"
| G = gfx_load "[Path].png"
| [W H] = "[Path].txt".get.utf8.parse
| Glyphs = G.frames{W H}
| Ws = Glyphs{[X Y W H].margins=>X+W}
| Ws.0 <= W/2
| new_font Glyphs Ws H

font.width Line = | Ws = $widths; Line{C.code => Ws.(C-' '.code)+1}.sum

font.draw G X Y Tint Text =
| Ls = Text.lines
| Palette = FontTints.Tint
| when no Palette: bad "undefined font tint `[Tint]`; check tints.txt"
| Ws = $widths
| Gs = $glyphs
| H = $height
| CodePoint = ' '.code
| CY = Y
| for L Ls
  | CX = X
  | for C L
    | I = C.code-CodePoint
    | W = Ws.I
    | G.blit{[CX CY] Gs.I map(Palette)}
    | W+1+!CX
  | !CY + H

data txt.widget g value_ size tint font
txt Value size/small tint/white =
| R = new_txt 0 '' Size Tint Size^font
| R.value <= Value
| R
txt.render = $g
txt.as_text = "#txt{[$value]}"
txt.value = $value_
txt.set_value Text =
| Text <= "[Text]"
| $value_ <= Text
| F = $font
| W = Text.lines{}{L => F.width{L}}.max
| H = F.height
| G = gfx{W H}
| G.clear{#FFFFFFFF}
| F.draw{G 0 0 $tint Text}
| OldG = $g
| when OldG: OldG.free
| $g <= G

font.as_text = "#font{}"

skin F = init SkinCache.F: gfx_load "[Skin]/[F].png"

cursor F =
| F = "cursor/[F]"
| init SkinCache.F: leave
  | Gfx = skin F
  | Gfx.hotspot <= "[Skin]/[F].txt".get.utf8.parse
  | Gfx

data spacer.widget w h
spacer W H = new_spacer W H
spacer.as_text = "#spacer{[$w] [$h]}"

data pic.widget value
pic Path = new_pic value
pic.render = if $value.is_text then skin $value else $value
pic.as_text = "#pic{[$value]}"

data tabs.~.widget tab all
tabs Initial Tabs = new_tabs Tabs.Initial Tabs
tabs.pick TabName = $tab <= $all.TabName
tabs.as_text = "#tabs{[$tab]}"
tabs._ Method Args =
| Args.0 <= Args.0.tab
| Args.apply_method{Method}

data canvas.widget w h paint
canvas W H Paint = new_canvas W H Paint
canvas.draw G P = case Me (F<~).paint: F G P $w $h 

data bar.widget value_ bg
bar InitialValue = new_bar InitialValue.clip{0 100} (skin "bar/bg")
bar.render = Me
bar.value = $value_
bar.set_value New = $value_ <= New.clip{0 100}
bar.draw G P =
| G.blit{P $bg}
| G.rect{#347004 1 P+[3 3] [152*$value_/100 14]}

data button.widget value on_click state over w_size h_size skin cache
button Text Fn state/normal w_size/large h_size/medium =
| new_button Text Fn State 0 W_size H_size Void (m)
button.reskin =
| Cache = $cache
| $skin <= Skin
| when got Cache.Skin: leave 0
| WSize = $w_size
| HSize = $h_size
| Text = $value
| Cache.Skin <= @as_map: map N [normal over pressed disabled]: _list N
  | File = "button/[HSize]-[WSize]-[case N over normal _ N]"
  | G = File^skin.copy
  | P = case N pressed 2 _ 0
  | Tint = case N pressed+over | \white
                  disabled | \gray
                  _ | \yellow
  | F = font HSize
  | FW = F.width{Text}
  | FH = F.height
  | X = G.w/2-FW/2+P
  | Y = G.h/2-FH/2+P
  | F.draw{G X Y Tint Text}
  | G
button.render =
| when $skin <> Skin: $reskin
| State = $state
| when State >< normal and $over: State <= \over
| $cache.Skin.State
button.input @In = case In
  [mice over S P] | $over <= S
  [mice left 1 P] | case $state normal: Me.state <= \pressed
  [mice left 0 P] | case $state pressed
                    | when $over: $on_click{}{}
                    | $state <= \normal
button.as_text = "#button{[$value]}"

data arrow.widget direction on_click state
arrow Direction Fn state/normal = new_arrow Direction Fn State
arrow.render = skin "arrow/[$direction]-[$state]"
arrow.input @In = case In
  [mice left 1 P] | case $state normal
                    | $state <= \pressed
                    | Repeat = => when $state >< pressed
                                  | $on_click{}{}
                                  | 1
                    | GUI.add_timer{0.25 Repeat}
  [mice left 0 P] | case $state pressed
                    | $on_click{}{}
                    | $state <= \normal
arrow.as_text = "#arrow{[$direction] state([$state])}"

data box.widget w h dir spacing items rendered
box Direction Spacing @Xs =
| Items = for X Xs: new_meta X [0 0 1 1]
| new_box 0 0 Direction Spacing Items Void
box.render =
| S = $spacing
| Rs = $items{}{?render}
| case $dir v | $w <= Rs{?w}.max; $h <= Rs{?h}.infix{S}.sum
            h | $h <= Rs{?h}.max; $w <= Rs{?w}.infix{S}.sum
| $rendered <= Rs
| Me
box.draw G P =
| D = $dir
| S = $spacing
| Is = $items
| N = 0
| for R $rendered
  | W = R.w
  | H = R.h
  | Rect = Is^pop.meta_
  | RX = case D v(0) h(N)
  | RY = case D v(N) h(0)
  | G.blit{P+[RX RY] R}
  | Rect.0 <= RX
  | Rect.1 <= RY
  | Rect.2 <= W
  | Rect.3 <= H
  | N <= case D v(N+H+S) h(N+W+S)

data gui root timers mice_xy cursor result fb keys popup
         last_widget focus_widget focus_xy click_time
gui.render =
| FB = $fb
| when no FB: leave Void
| R = $root.render
| W = R.w
| H = R.h
| when not R.is_gfx or W <> FB.w or H <> FB.h:
  | FB.free
  | FB <= gfx W H
  | $fb <= FB
| FB.blit{[0 0] R}
| C = $cursor
| when got C
  | CG = cursor C
  | XY = GUI.mice_xy-CG.hotspot
  | FB.blit{XY CG}
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
    | if Fn{} then $timers.N.1 <= (Time)+Interval
      else push N Remove
| when Remove.size
  | N = -1
  | $timers <= $timers.skip{X=>Remove.locate{!N+1}^got}
| 0
gui.input Es =
| T = clock
| $update_timers{T}
| [NW_XY NW] = $root.itemAt{$mice_xy [0 0]} //new widget under cursor
| $popup <= NW.popup
| $cursor <= NW.cursor
| for E Es: case E
  [mice_move XY]
    | $mice_xy <= XY
    | NW.input{mice_move XY XY-NW_XY}
    | LW = $last_widget
    | when LW^address <> NW^address:
      | when got LW: LW.input{mice over 0 XY}
      | $last_widget <= NW
      | NW.input{mice over 1 XY}
  [mice Button State]
    | MP = $mice_xy
    | NW.input{mice Button State MP-NW_XY}
    | FW = $focus_widget
    | when FW^address <> NW^address
      | when got FW: FW.input{focus 0 MP-$focus_xy}
      | $focus_widget <= NW
      | $focus_xy <= NW_XY
      | FW.input{focus 1 MP-NW_XY}
    | LastClickTime = $click_time.Button
    | when got LastClickTime and T-LastClickTime < 0.25:
      | NW.input{mice "double_[Button]" 1 MP-NW_XY}
    | $click_time.Button <= T
  [key Key State] | $keys.Key <= State
                  | NW.input{key Key State $mice_xy-$focus_xy}
  Else |
| Void
gui.exit Result =
| $result <= Result
| $fb <= Void
gui Root = //FIXME: create a default skin and allow picking user defined skins
| setSkin '/Users/nikita/Documents/git/symta/build/test_macro/data/ui'
| GUI <= new_gui Root [] [0 0] point Void gfx{1 1} (m) Void
                 (new_widget) (new_widget) [0 0] (m)
| show: Es => | GUI.input{Es}
              | GUI.render
| R = GUI.result
| GUI.fb.free
| GUI <= Void
| R


export gui button
