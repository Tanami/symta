use gfx show

GUI = Void

Skin = Void
SkinCache = Void
ImgCache = Void
FontCache = Void
FontTints = Void

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

data txt g value_ size tint font
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

timer Interval Handler = [@GUI.timers! [Interval (clock)+Interval Handler]]

// FIXME: expose inheritance and make them part of `widget` superclass
_.input E =
_.items =
_.render = Me
_.draw G P =
_.popup = Me
_.cursor = Void
_.itemAt Where XY = [0 0] // now Me is the Widget

data spacer w h
spacer W H = new_spacer W H
spacer.as_text = "#spacer{[Me.w] [Me.h]}"

data pic value
pic Path = new_pic value
pic.render = if Me.value.is_text then skin Me.value else Me.value
pic.as_text = "#pic{[Me.value]}"

data tabs tab all
tabs Initial Tabs = new_tabs Tabs.Initial Tabs
tabs.items = Me.tab.items
tabs.render = Me.tab.render
tabs.input E = Me.tab.input{E}
tabs.pick TabName = Me.tab <= Me.all.TabName
tabs.as_text = "#tabs{[Me.tab]}"

data canvas w h paint
canvas W H Paint = new_canvas W H Paint
canvas.draw G P = case Me (F<~).paint: F G P Me.w Me.h 

data bar value_ bg
bar InitialValue = new_bar InitialValue.clip{0 100} (skin "bar/bg")
bar.render = Me
bar.value = Me.value_
bar.set_value New = Me.value_ <= New.clip{0 100}
bar.draw G P =
| G.blit{P Me.bg}
| G.rect{#347004 1 P+[3 3] [152*Me.value_/100 14]}

data button value on_click state over w_size h_size skin cache
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
button.input In = case In
  [mice_over S P] | Me.over <= S
  [mice_left 1 P] | case Me.state normal: Me.state <= \pressed
  [mice_left 0 P] | case Me.state pressed
                    | when Me.over: Me.on_click{}{}
                    | Me.state <= \normal
button.as_text = "#button{[Me.value]}"

data arrow direction on_click state
arrow Direction Fn state/normal = new_arrow Direction Fn State
arrow.render = skin "arrow/[Me.direction]-[Me.state]"
arrow.input In = case In
  [mice_left 1 P] | case Me.state normal
                    | Me.state <= \pressed
                    | timer 0.25: => when Me.state >< pressed
                                     | Me.on_click{}{}
                                     | 1
  [mice_left 0 P] | case Me.state pressed
                    | Me.on_click{}{}
                    | Me.state <= \normal
arrow.as_text = "#arrow{[Me.direction] state([Me.state])}"

data box w h dir spacing items rendered
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
gui.input Es =
| T = clock
| Ts = Me.timers
| Me.timers <= [] // user code can insert additional timers
| Ts = map [Interval Expiration Fn] Ts
  | if T < Expiration then [Interval Expiration Fn]
    else if Fn{} then [Interval (clock)+Interval Fn]
    else 0
| [@Ts.skip{0} @Me.timers!]
| [NP NW] = Me.root.itemAt{Me.mice_xy [0 0]} //used to determine when mouse leaves widget
| for E Es: case E
  [mice_move XY] | Me.mice_xy <= XY
  [mice Button State] | 
  [key Key State] | 
  Else |
gui.exit Result =
| Me.result <= Result
| Me.fb <= Void
gui Root = //FIXME: create a default skin and allow picking user defined skins
| setSkin '/Users/nikita/Documents/git/symta/build/test_macro/data/ui'
| GUI <= new_gui Root [] [0 0] point Void gfx{1 1} (m) Void
| show: Es => | GUI.input{Es}
              | GUI.render
| R = GUI.result
| GUI <= Void
| R


export gui button
