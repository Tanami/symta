use gfx show

rects_intersect [AX AY AW AH] [BX BY BW BH] = AX<BX+BW and AY<BY+BH and BX<AX+AW and BY<AY+AH

GUI = Void

Skin = Void
SkinCache = Void
ImgCache = Void
FontCache = Void
FontTints = Void

widget.input @E =
widget.items = Void
widget.render = Me
widget.draw G P =
widget.popup = Void
widget.cursor = \point
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

cfg P = P.get.utf8.lines{}{?parse}.skip{is.[]}

setSkin Path =
| Skin <= Path
| SkinCache <= t
| ImgCache <= t
| FontCache <= t
| FontTints <= "[Skin]/font/tints.txt"^cfg{}.map{[?0 ?.tail.pad{256 #FF000000}^new_cmap]}.table

type font{new_font Gs W H} glyphs/Gs widths/W height/H

font N = have FontCache.N:
| Path = "[Skin]/font/[N]"
| G = gfx "[Path].png"
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

type txt.widget{Value size/small tint/white}
     g value_/Value size/Size tint/Tint font
txt.render =
| less $font
  | $font <= font $size
  | $value <= $value_
| $g
txt.as_text = "#txt{[$value]}"
txt.value = $value_
txt.`!value` Text =
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

skin F = have SkinCache.F: gfx "[Skin]/[F].png"

cursor F =
| F = "cursor/[F]"
| have SkinCache.F: leave
  | Gfx = skin F
  | Gfx.hotspot <= "[Skin]/[F].txt".get.utf8.parse
  | Gfx

type spacer.widget{W H} w/W h/H
spacer.as_text = "#spacer{[$w] [$h]}"

type pic.widget{Path} value/Path
pic.render = if $value.is_text then skin $value else $value
pic.as_text = "#pic{[$value]}"

type tabs.~{Init Tabs} tab/Tabs.Init all/Tabs
tabs.pick TabName =
| (get_gui).focus_widget <= Void
| $tab <= $all.TabName
tabs.as_text = "#tabs{[$tab]}"
tabs._ Method Args =
| Args.0 <= Args.0.tab
| Args.apply_method{Method}

type canvas.widget{W H P} w/W h/H paint/P
canvas.draw G P = case Me (F<~).paint: F G P $w $h 

type bar.widget{V} value_/V.clip{0 100} bg/Void
bar.render =
| have $bg: skin."bar/bg"
| Me
bar.value = $value_
bar.set_value New = $value_ <= New.clip{0 100}
bar.draw G P =
| G.blit{P $bg}
| G.rect{#347004 1 P+[3 3] [152*$value_/100 14]}

type button.widget{Text Fn on/1 w_size/large h_size/medium}
  value/Text on_click/Fn state over w_size/W_size h_size/H_size skin/Void cache/(t)
| $state <= if On then \normal else \disabled
button.reskin =
| Cache = $cache
| $skin <= Skin
| when got Cache.Skin: leave 0
| WSize = $w_size
| HSize = $h_size
| Text = $value
| Cache.Skin <= @table: map N [normal over pressed disabled]: list N
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

type arrow.widget{D Fn state/normal} direction/D on_click/Fn state/State
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
  | Rect.0 <= RX
  | Rect.1 <= RY
  | Rect.2 <= W
  | Rect.3 <= H
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
  | Rect.0 <= X
  | Rect.1 <= Y
  | Rect.2 <= R.w
  | Rect.3 <= R.h
  | G.blit{P+[X Y] R}

type litem.widget{Text w/140 on/1} text_/Text w/W h state font fw fh init
| $state <= if On then \normal else \disabled
litem.render =
| less $init
  | $h <= "litem/normal"^skin.h
  | $font <= font small
  | $fw <= $font.width{$text_}
  | $fh <= $font.height
  | $init <= 1
| Me
litem.text = $text_
litem.`!text` Text =
| $init <= 0
| $text_ <= Text
litem.draw G P =
| BG = "litem/[$state]"^skin
| G.blit{P BG rect/[0 0 $w BG.h]}
| Tint = case $state picked(\white) disabled(\gray) _(\yellow)
| X = 2
| Y = BG.h/2-$fh/2
| $font.draw{G P.0+X P.1+Y Tint $text_}
litem.input @In = case In
  [mice left 1 P] | $state <= case $state normal(\picked) picked(\normal) X(X)

type droplist.widget{Xs w/140} w/W h/1 y ih xs/[] drop rs picked over above_all p
| less Xs.size: Xs <= [' ']
| $xs <= Xs{(litem ? w/$w)}
droplist.text = $xs.($picked).text
droplist.`!text` T = $xs.($picked).text <= T
droplist.render =
| $rs <= map X $xs X.render
| case $rs [R@_]: $ih <= R.h
| when $drop
  | $y <= -$ih*$picked
  | $h <= $ih*$rs.size
| less $drop
  | $y <= 0
  | $h <= $ih
| Me
droplist.draw G P =
| when $drop
  | Y = 0
  | for R $rs
    | G.blit{P+[0 Y] R}
    | !Y + R.h
| less $drop
  | G.blit{P $rs.($picked)}
  | A = skin "arrow/down-normal"
  | G.blit{P+[$w-A.w 0] A}
| $rs <= 0
| Void
droplist.input @In = case In
  [mice over S P] | $over <= S
                  | $xs.($p).state <= case S 1(\picked) 0(\normal)
  [mice_move _ P] | when $drop
                    | $xs.($p).state <= \normal
                    | $p <= (P.1/$ih).clip{0 $xs.size-1}
                    | $xs.($p).state <= \picked
  [mice left 1 P] | $drop <= 1
                  | $p <= $picked
                  | $above_all <= 1
                  | $xs.($p).state <= \picked
  [mice left 0 P] | $drop <= 0
                  | $xs.($p).state <= \normal
                  | $picked <= $p

//  | when $over: G.rect{#FFFF00 0 P.0-1 P.1-1 $w+1 $h+1}

//FIXME: create a default skin and allow picking user defined skins
type gui{Root} root/Root timers/[] mice_xy/[0 0] cursor/point result/Void fb/Void
               keys/(t) popup/Void last_widget/(widget) focus_widget/Void
               focus_xy/[0 0] focus_wh/[0 0] last_clicked/(widget) click_time/(t)
| setSkin '/Users/nikita/Documents/git/symta/build/test_macro/data/ui'
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
  | P = $focus_xy+[fw.x fw.y]
  | WH = if fw.w and fw.h then [fw.w fw.h] else $focus_wh
  | FB.rect{#FFFF00 0 P.0-1 P.1-1 WH.0+2 WH.1+2}
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
| [NW NW_XY NW_WH] = $root.itemAt{$mice_xy [0 0] [0 0]} //new widget
| $popup <= NW.popup
| $cursor <= NW.cursor
| for E Es: case E
  [mice_move XY]
    | $mice_xy.0 <= XY.0
    | $mice_xy.1 <= XY.1
    | NW.input{mice_move XY XY-NW_XY}
    | LW = $last_widget
    | when LW^address <> NW^address:
      | when got LW: LW.input{mice over 0 XY}
      | $last_widget <= NW
      | NW.input{mice over 1 XY}
  [mice Button State]
    | MP = $mice_xy
    | if State
      then | $last_clicked <= NW
           | NW.input{mice Button State MP-NW_XY}
      else $last_clicked.input{mice Button State MP-NW_XY}
    | when State and NW.wants_focus:
      | $focus_xy <= NW_XY
      | $focus_wh <= NW_WH
      | FW = $focus_widget
      | when FW^address <> NW^address:
        | when got FW: FW.input{focus 0 MP-$focus_xy}
        | $focus_widget <= NW
        | NW.input{focus 1 MP-NW_XY}
    | LastClickTime = $click_time.Button
    | when got LastClickTime and T-LastClickTime < 0.25:
      | NW.input{mice "double_[Button]" 1 MP-NW_XY}
    | $click_time.Button <= T
  [key Key State] | $keys.Key <= State
                  | NW.input{key Key State $mice_xy-$focus_xy}
  Else |
| Void
gui.exit @Result =
| $result <= case Result [R](R) Else(Void)
| $fb <= Void

get_gui = GUI

export gui get_gui button spacer pic txt lay dlg litem droplist tabs skin font
