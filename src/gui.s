use gfx show

int.in Start End = Start << Me and Me < End
list.in [RX RY RW RH] = Me.0.in{RX RX+RW} and Me.1.in{RY RY+RH}
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
| Items = Me.items
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

font.width Line = | Ws = Me.widths; Line{C.code => Ws.(C-' '.code)+1}.sum

font.draw G X Y Tint Text =
| Ls = Text.lines
| Palette = FontTints.Tint
| when no Palette: bad "undefined font tint `[Tint]`; check tints.txt"
| Ws = Me.widths
| Gs = Me.glyphs
| H = Me.height
| CodePoint = ' '.code
| CY = Y
| for L Ls
  | CX = X
  | for C L
    | I = C.code-CodePoint
    | W = Ws.I
    | G.blit{[CX CY] Gs.I map(Palette)}
    | CX <= CX+W+1
  | CY !+ H

data txt.widget g value_ size tint font
txt Value size/small tint/white =
| R = new_txt 0 '' Size Tint Size^font
| R.value <= Value
| R
txt.render = Me.g
txt.as_text = "#txt{[Me.value]}"
txt.value = Me.value_
txt.set_value Text =
| Text <= "[Text]"
| Me.value_ <= Text
| F = Me.font
| W = Text.lines{}{L => F.width{L}}.max
| H = F.height
| G = gfx{W H}
| G.clear{#FFFFFFFF}
| F.draw{G 0 0 Me.tint Text}
| OldG = Me.g
| when OldG: OldG.free
| Me.g <= G

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
spacer.as_text = "#spacer{[Me.w] [Me.h]}"

data pic.widget value
pic Path = new_pic value
pic.render = if Me.value.is_text then skin Me.value else Me.value
pic.as_text = "#pic{[Me.value]}"

data tabs.widget tab all
tabs Initial Tabs = new_tabs Tabs.Initial Tabs
tabs.pick TabName = Me.tab <= Me.all.TabName
tabs.as_text = "#tabs{[Me.tab]}"
tabs._ Name =
| M = _this_method
| Me.0 <= Me.0.tab
| Me.apply_method{M}


data canvas.widget w h paint
canvas W H Paint = new_canvas W H Paint
canvas.draw G P = case Me (F<~).paint: F G P Me.w Me.h 

data bar.widget value_ bg
bar InitialValue = new_bar InitialValue.clip{0 100} (skin "bar/bg")
bar.render = Me
bar.value = Me.value_
bar.set_value New = Me.value_ <= New.clip{0 100}
bar.draw G P =
| G.blit{P Me.bg}
| G.rect{#347004 1 P+[3 3] [152*Me.value_/100 14]}

data button.widget value on_click state over w_size h_size skin cache
button Text Fn state/normal w_size/large h_size/medium =
| new_button Text Fn State 0 W_size H_size Void (m)
button.reskin =
| Cache = Me.cache
| Me.skin <= Skin
| when got Cache.Skin: leave 0
| WSize = Me.w_size
| HSize = Me.h_size
| Text = Me.value
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
| when Me.skin <> Skin: Me.reskin
| State = Me.state
| when State >< normal and Me.over: State <= \over
| Me.cache.Skin.State
button.input @In = case In
  [mice over S P] | Me.over <= S
  [mice left 1 P] | case Me.state normal: Me.state <= \pressed
  [mice left 0 P] | case Me.state pressed
                    | when Me.over: Me.on_click{}{}
                    | Me.state <= \normal
button.as_text = "#button{[Me.value]}"

data arrow.widget direction on_click state
arrow Direction Fn state/normal = new_arrow Direction Fn State
arrow.render = skin "arrow/[Me.direction]-[Me.state]"
arrow.input @In = case In
  [mice left 1 P] | case Me.state normal
                    | Me.state <= \pressed
                    | Repeat = => when Me.state >< pressed
                                  | Me.on_click{}{}
                                  | 1
                    | GUI.add_timer{0.25 Repeat}
  [mice left 0 P] | case Me.state pressed
                    | Me.on_click{}{}
                    | Me.state <= \normal
arrow.as_text = "#arrow{[Me.direction] state([Me.state])}"

data box.widget w h dir spacing items rendered
box Direction Spacing @Xs =
| Items = for X Xs: new_meta X [0 0 1 1]
| new_box 0 0 Direction Spacing Items Void
box.render =
| S = Me.spacing
| Rs = Me.items{}{?render}
| case Me.dir v | Me.w <= Rs{?w}.max; Me.h <= Rs{?h}.infix{S}.sum
              h | Me.h <= Rs{?h}.max; Me.w <= Rs{?w}.infix{S}.sum
| Me.rendered <= Rs
| Me
box.draw G P =
| D = Me.dir
| S = Me.spacing
| Is = Me.items
| N = 0
| for R Me.rendered
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
| FB = Me.fb
| when no FB: leave Void
| R = Me.root.render
| W = R.w
| H = R.h
| when not R.is_gfx or W <> FB.w or H <> FB.h:
  | FB.free
  | FB <= gfx W H
  | Me.fb <= FB
| FB.blit{[0 0] R}
| C = Me.cursor
| when got C
  | CG = cursor C
  | XY = GUI.mice_xy-CG.hotspot
  | FB.blit{XY CG}
  | Pop = Me.popup
  | when got Pop: FB.blit{XY-[0 Pop.h] Pop}
| FB
gui.add_timer Interval Handler =
| [@Me.timers! [Interval (clock)+Interval Handler]]
gui.update_timers Time =
| Ts = Me.timers
| less Ts.size: leave 0
| Me.timers <= [] // user code can insert additional timers
| Remove = []
| for [N T] Ts.i: case T [Interval Expiration Fn]:
  | when Time >> Expiration
    | if Fn{} then Me.timers.N.1 <= (Time)+Interval
      else push N Remove
| when Remove.size
  | N = -1
  | Me.timers <= Me.timers.skip{X=>Remove.locate{N!+1}^got}
| 0
gui.input Es =
| T = clock
| Me.update_timers{T}
| [NW_XY NW] = Me.root.itemAt{Me.mice_xy [0 0]} //new widget under cursor
| Me.popup <= NW.popup
| Me.cursor <= NW.cursor
| for E Es: case E
  [mice_move XY]
    | Me.mice_xy <= XY
    | NW.input{mice_move XY XY-NW_XY}
    | LW = Me.last_widget
    | when LW^address <> NW^address:
      | when got LW: LW.input{mice over 0 XY}
      | Me.last_widget <= NW
      | NW.input{mice over 1 XY}
  [mice Button State]
    | MP = Me.mice_xy
    | NW.input{mice Button State MP-NW_XY}
    | FW = Me.focus_widget
    | when FW^address <> NW^address
      | when got FW: FW.input{focus 0 MP-Me.focus_xy}
      | Me.focus_widget <= NW
      | Me.focus_xy <= NW_XY
      | FW.input{focus 1 MP-NW_XY}
    | LastClickTime = Me.click_time.Button
    | when got LastClickTime and T-LastClickTime < 0.25:
      | NW.input{mice "double_[Button]" 1 MP-NW_XY}
    | Me.click_time.Button <= T
  [key Key State] | Me.keys.Key <= State
                  | NW.input{key Key State Me.mice_xy-Me.focus_xy}
  Else |
| Void
gui.exit Result =
| Me.result <= Result
| Me.fb <= Void
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
