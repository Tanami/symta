use util gfx

type unit
    id type xy disp owner color team name side hits mana
    frame dir/Dirs.0 resources/(t size/6)
    enemies nobody playable rescueable passive view
    world content_next sensor_next last_drawn/-1 mm_color
heir unit $type
unit.as_text = "#unit{[$type.id]}"

type world{Main}
   main/Main w h rect owned/(dup 32 []) units cycle scheds vs vs_i
   nqs trans orders free_ids used_ids new_units del_units
   max_units/1200 max_w/300 max_h/300 max_cells
   tileset tileset_name tiles gfxes palette tints
   players/16^dup this_player/Void minimap_dim minimap/Main.minimap
   minimap_cells
| WxH = $max_w*$max_h
| $max_cells <= WxH
| $units <= dup $max_units+WxH
| $free_ids <= ($max_units){?+WxH}
| for Id $free_ids
  | U = unit
  | U.id <= Id
  | U.world <= Me
  | $units.Id <= U
| $vs <= dup $units.size [] // visible units
| $minimap_cells <= dup $minimap.w*$minimap.h
| $main.world <= Me

world.cell_id P = $w*P.1 + P.0

world.init_minimap =
| [MW MH] = [$minimap.w $minimap.h]
| [W H] = [$w $h]
| for [MX MY] [0 0 MW MH].xy
  | X = MX*W/MW
  | Y = MY*H/MH
  | $minimap_cells.(MW*MY+MX) <= $units.(Y*W+X)
| $upd_minimap

world.init_dimensions W H =
| $w <= W
| $h <= H
| $rect <= [0 0 W H]
| $minimap_dim <= 128/W //FIXME: breaks for 96x96 maps

world.upd_minimap =
| [MW MH] = [$minimap.w $minimap.h]
| for [MX MY] [0 0 MW MH].xy
  | C = $minimap_cells.(MY*MW + MX)
  | U = C.content
  | if U then $minimap.set{MX MY U.mm_color}
    else $minimap.set{MX MY C.mm_color}

world.init_cell XY Tile =
| C = $tiles.Tile.copy
| Id = $cell_id{XY}
| C.id <= Id
| C.xy <= XY
| C.disp <= XY*32
| C.neibs <= Dirs{?+XY}.keep{?.in{$rect}}
| $units.Id <= C

world.new O T delay/6.rand =
| T = if T.is_utype then T else $main.types.T
| Id = $free_ids.($used_ids)
| !$used_ids + 1
| U = $units.Id
| U.type <= T
| U.mana <= T.mana
| U.owner <= O
| U.color <= if O then O.color else \yellow
| U.mm_color <= $main.ui_colors.(U.color).0
| U.resources <= T.resources.copy
| U.frame <= 0
| U

world.upd_area Rect F =
| for [X Y] Rect.xy: when [X Y].in{$rect}: F $units.(Y*$w + X)

unit.mark =
| S = $sight
| $world.upd_area{[@$xy @$size]
  | C => | $content_next <= C.content
         | C.content <= Me
         | @xor !C.mask $layer}
| $world.upd_area{[@($xy-[S S]) @($size+[2*S 2*S])]
  | C => | $sensor_next <= C.sensors
         | C.sensors <= Me}

unit.deploy P =
| $xy <= P
| $disp <= P*32
| $mark

PudTilesets = [summer winter wasteland swamp]
PudTeams = t nobody(0) neutral(0) capturable(0) computer(1) person(2) rescueable(2)
PudPlayers = [0 0 neutral 0 computer person capturable rescueable]
Critters = t summer\sheep wasteland\boar winter\seal swamp\hellhog
PudSides = [human orc neutral]

recolor Offset Pal Cs =
| Pal = Pal.copy
| for [I C] Cs.i: Pal.(Offset+I) <= C
| new_cmap Pal

world.load_pud Path =
| Units = []
| era [N @_] =
  | $tileset_name <= PudTilesets.N
  | $tileset <= $main.tilesets.($tileset_name)
  | $tiles <= $tileset.tiles
| sres N Xs = for [I A] Xs.group{2}{?u2}.i
  | $players.I.resources.N <= A
| Handlers = t
  'DESC' | Xs => //$description <= Xs.take{Xs.locate{0}^supply{32}}.utf8
  'OWNR' | Xs => for [I P] Xs{PudPlayers.?}.i
                 | U = $new{0 player}
                 | less P: U.nobody <= 1
                 | U.color <= case P neutral yellow _ $main.player_colors.I
                 | U.name <= "Player[I]"
                 | U.playable <= P >< person
                 | U.rescueable <= case P capturable+rescueable 1
                 | U.xy <= [0 0]
                 | U.team <= PudTeams.P
                 | $players.I <= U
  'ERA ' | Xs => era Xs
  'ERAX' | Xs => era Xs
  'DIM ' | [2/W.u2 2/H.u2 @_] => $init_dimensions{W H}
  'SIDE' | Xs => for [I S] Xs{PudSides.?}.i: $players.I.side <= S
  'SGLD' | Xs => sres gold Xs
  'SLBR' | Xs => sres wood Xs
  'SOIL' | Xs => sres oil Xs
  'AIPL' | Xs => Xs.i{}{$0[I 1]=>$players.I.passive<=1}
  'MTXM' | Xs => | M = Xs.group{2}{?u2}
                 | I = -1
                 | for P $rect.xy: $init_cell{P M.(!I+1)}
  'UNIT' | @r$0 [2/X.u2 2/Y.u2 I O 2/D.u2 @Xs] =>
           | XY = [X Y]
           | T = case I 57 Critters.($tileset_name) _ $main.pud.I
           | case T
             Void | bad "Invalid unit slot: [I]"
             player | $players.O.xy <= XY
                    | $players.O.view <= XY*32 - [224 224]
             _ | [[XY O T D] @!Units]
           | r Xs
| Cs = Path.get^(@r$[] [4/M.utf8 4/L.u4 L/D @Xs] => [[M D] @Xs^r])
| less Cs^is{[[\TYPE _]@_]}: bad "Invalid PUD file: [Path]"
| for [T D] Cs: when got!it Handlers.T: it D
| for U $players
  | U.owner <= U.id
  | U.enemies <= $players.skip{?id >< U.id}.keep{P =>
    | (P.team <> U.team and P.team <> 0 and U.team <> 0)
      or (P.playable and U.playable)}
  | $units.(U.id) <= U
  | if no $this_player and U.playable then $this_player <= U
    else if U.nobody then
    else //U.order{plan}
| for [XY O T D] Units
  | P = $players.O
  | U = $new{P T}
  | when!it U.resource: U.resources.it <= D*2500
  | Rs = P.resources
  | have Rs.food 0
  | !Rs.food + (U.supply - U.cost.food)
  | less U.building: U.dir <= Dirs.rand
  | U.deploy{XY}
| $palette <= $tileset.tiles.0.gfx.cmap
| $tints <= @table: map [K C] $main.ui_colors [K (recolor 208 $palette C)]
| $init_minimap
| Void

export world
