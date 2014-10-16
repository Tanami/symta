use common gfx gui 

Skin = Void
SkinCache = Void
FontCache = Void
FontTints = Void

set_skin Path =
| Skin <= Path
| SkinCache <= t
| FontCache <= t
| FontTints <= "[Skin]/font/tints.txt"^cfg{}.map{[?0 ?.tail.pad{256 #FF000000}^new_cmap]}.table

skin F = have SkinCache.F: gfx "[Skin]/[F].png"

skin_cursor F =
| F = "cursor/[F]"
| have SkinCache.F: leave
  | Gfx = skin F
  | Gfx.hotspot <= "[Skin]/[F].txt".get.utf8.parse
  | Gfx

type font{new_font Gs W H} glyphs/Gs widths/W height/H
font.as_text = "#font{}"
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

type litems.~{Xs w/300 lines/5 f/(V=>)} f/F ih/Void lines/Lines xs/Xs box picked o/Void
| $box <= lay v 0: dup $lines: litem '' w/W
| $offset <= 0
litems._ Method Args =
| Args.0 <= Args.0.box
| Args.apply_method{Method}
litems.offset = $o
litems.`!offset` NO =
| when NO >< $o: leave 0
| $o <= max 0: @clip 0 $xs.size-1 NO
| times K $lines
  | I = $o + K
  | Item = $box.items.K
  | if I < $xs.size
    then | Item.text <= $xs.I
         | Item.state <= if I >< $picked then \picked else \normal
    else | Item.text <= ''
         | Item.state <= \disabled
litems.value = $xs.($picked)
litems.data = $xs
litems.`!data` Ys =
| $xs <= Ys
| $picked <= 0
| $o <= Void
| $offset <= 0
litems.input @In = case In
  [mice left 1 P] | have $ih: $box.items.0.render.h
                  | NP = @clip 0 $xs.size-1 P.1/$ih+$o
                  | when NP <> $picked
                    | K = $picked - $o
                    | when K >> 0 and K < $lines:
                      | $box.items.K.state <= \normal
                    | $picked <= NP
                    | $f $xs.NP
                    | $box.items.(NP-$o).state <= \picked
litems.itemAt Point XY WH = [Me XY WH] //override lay's method

folder_nomalized Path = 
| Xs = Path.items
| Folders = Xs.keep{is."[_]/"}
| Files = Xs.skip{is."[_]/"}
| Parent = if Path >< '/' or Path.last >< ':' then [] else ['../']
| [@Parent @Folders @Files]

type slider_.widget{D f/(N=>) size/124 value/0.0 on/1 state/normal delta/Void}
     dir/D f/F size/Size pos/0.0 state/State skin/Void w/1 h/1 delta/Delta
| have $delta (10.0/$size.float)
| $value <= Value
slider_.value = $pos/$size.float
slider_.`!value` V =
| OV = $value
| $pos <= (V*$size.float).clip{0.0 $size.float}
| when $value <> OV: $f $value
slider_.render =
| S = skin "slider/[$dir]-normal"
| $w <= S.w
| $h <= S.h
| if $dir >< v then $h <= $size else $w <= $size
| Me
slider_.inc = !$value + $delta
slider_.dec = !$value - $delta
slider_.draw G P =
| BG = skin "slider/[$dir]-normal"
| K = skin "slider/knob"
| I = 0
| when $dir >< v
  | while I < $size
    | G.blit{P+[0 I] BG rect/[0 0 BG.w (min BG.h $size-I)]}
    | !I + BG.h
  | G.blit{P+[1 $pos.int*($size-K.h)/$size+1] K}
| when $dir >< h
  | while I < $size
    | G.blit{P+[I 0] BG rect/[0 0 (min BG.w $size-I) BG.h]}
    | !I + BG.w
  | G.blit{P+[$pos.int*($size-K.w)/$size+1 1] K}
slider_.input @In = case In
  [mice_move _ P] | when $state >< pressed
                    | NP = @clip 0 $size: if $dir >< v then P.1 else P.0
                    | when NP <> $pos.int
                      | $pos <= NP.float
                      | $f $value
  [mice left 1 P] | when $state >< normal
                    | $state <= \pressed
                    | $input{mice_move P P}
  [mice left 0 P] | when $state >< pressed: $state <= \normal

type arrow.widget{D Fn state/normal} direction/D on_click/Fn state/State
arrow.render = skin "arrow/[$direction]-[$state]"
arrow.input @In = case In
  [mice left 1 P] | when $state >< normal
                    | $state <= \pressed
                    | Repeat = => when $state >< pressed
                                  | $on_click{}{}
                                  | 1
                    | (get_gui).add_timer{0.25 Repeat}
  [mice left 0 P] | when $state >< pressed
                    | $on_click{}{}
                    | $state <= \normal
arrow.as_text = "#arrow{[$direction] state([$state])}"

slider D @Rest =
| S = slider_ D @Rest
| if D >< v
  then lay D 0 [(arrow up (=>S.dec)) S (arrow down (=>S.inc))]
  else lay D 0 [(arrow left (=>!S.dec)) S (arrow right (=>S.inc))]

type folder_litems.~{Root f/(V=>)} root/Root f/F litems
| when $root.last <> '/': $root <= "[$root]/"
| $litems <= litems lines/9 f/(N => F "[$root][N]") $root^folder_nomalized
folder_litems._ Method Args =
| Args.0 <= Args.0.litems
| Args.apply_method{Method}
folder_litems.input @In = case In
  [mice double_left 1 P] | R = if $litems.value >< '../'
                               then "[$root.lead.url.0]"
                               else "[$root][$litems.value]"
                         | when R.folder
                           | $root <= R
                           | $litems.data <= $root^folder_nomalized
  Else | $litems.input{@In}
folder_litems.itemAt Point XY WH = [Me XY WH]

folder_widget Root f/(V=>) =
| FL = folder_litems Root f/F
| S = slider size/124 v f/(N => FL.offset <= @int N*FL.data.size.float)
| lay h 0 [FL S]


type view.widget{W H M} w/W h/H main/M world paused/1 sel/[] last_click/[]
                        ack a m/[0 0] g visible notes speed/20 frame
                        sel_blink/[0 0 0]
| $g <= gfx W H
| $world <= $main.world

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


export set_skin skin skin_cursor font txt button droplist slider folder_widget view
