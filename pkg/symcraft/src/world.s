use common tile

type world w h rect owned/(dup 32 []) units cycle scheds vs vs_i
           nqs trans orders free_ids new_units del_units margin/10 
           top_left/[10 10] max_units/1200 max_w/300 max_h/300
           tileset tileset_name tiles gfxes
           players/16^dup this_player minimap_dim
| WxH = $max_w*$max_h
| $units <= dup $max_units+WxH
| $free_ids <= ($max_units){?+WxH}
| $vs <= dup $units.size [] // visible units
world.cell_id P = $w*P.1 + P.0
world.init_cell XY Tile =
| C = $tiles.Tile.copy
| Id = $cell_id{XY}
| C.id <= Id
| C.xy <= XY
| C.disp <= XY*32
| C.neibs <= Dirs{?+XY}.keep{?.in{$rect}}
| $units.Id <= C

main.new O T delay/6.rand =
| U = if T.is_unit then T.copy else $types.T.copy
| U.owner <= O
| U.resources <= t
| U

PudTilesets = [summer winter wasteland swamp]
PudTeams = t nobody(0) neutral(0) capturable(0) computer(1) person(2) rescueable(2)
PudPlayers = [0 0 neutral 0 computer person capturable rescueable]
Critters = t summer\sheep wasteland\boar winter\seal swamp\hellhog
PudSides = [human orc neutral]

main.load_pud Path =
| R = world
| $world <= R
| Units = []
| era [N @_] =
  | R.tileset_name <= PudTilesets.N
  | R.tileset <= $tilesets.(R.tileset_name)
  | R.tiles <= R.tileset.tiles
| sres N Xs = for [I A] Xs.group{2}{?u2}.i: R.players.I.resources.N <= A
| Handlers = t
  'DESC' | Xs => //R.description <= Xs.take{Xs.locate{0}^supply{32}}.utf8
  'OWNR' | Xs => for [I P] Xs{PudPlayers.?}.i
                 | U = $new{0 player}
                 | less P: U.nobody <= 1
                 | U.color <= case P neutral yellow _ $player_colors.I
                 | U.name <= "Player[I]"
                 | U.playable <= P >< person
                 | U.rescueable <= case P capturable+rescueable 1
                 | U.xy <= R.top_left
                 | U.team <= PudTeams.P
                 | R.players.I <= U
  'ERA ' | Xs => era Xs
  'ERAX' | Xs => era Xs
  'DIM ' | [2/W.u2 2/H.u2 @_] =>
            | R.w <= W + 2*R.margin
            | R.h <= H + 2*R.margin
            | R.rect <= [R.margin R.margin W H]
            | R.minimap_dim <= 128/W //FIXME: breaks for 96x96 maps
  'SIDE' | Xs => for [I S] Xs{PudSides.?}.i: R.players.I.side <= S
  'SGLD' | Xs => sres gold Xs
  'SLBR' | Xs => sres wood Xs
  'SOIL' | Xs => sres oil Xs
  'AIPL' | Xs => Xs.i{}{$0[1 I]=>R.players.I.passive<=1}
  'MTXM' | Xs => | M = Xs.group{2}{?u2}
                 | I = -1
                 | for P R.rect.xy: R.init_cell{P M.(!I+1)}
  'UNIT' | @r$0 [2/X.u2 2/Y.u2 I O 2/D.u2 @Xs] =>
           | XY = [X Y] + R.top_left
           | T = case I 57 Critters.(R.tileset_name) _ $pud.I
           | case T
             Void | bad "Invalid unit slot: [I]"
             player | R.players.O.xy <= XY
                    | R.players.O.view <= XY*32 - [224 224]
             _ | [[XY O+1 T D] @!Units]
           | r Xs
| Cs = Path.get^(@r$[] [4/M.utf8 4/L.u4 L/D @Xs] => [[M D] @Xs^r])
| less Cs^is{[[\TYPE _]@_]}: bad "Invalid PUD file: [Path]"
| for [T D] Cs: when got!it Handlers.T: it D
| Void
