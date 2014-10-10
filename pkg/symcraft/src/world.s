use common tile

PudTilesets = [summer winter wasteland swamp]
PudTeams = t nobody(0) neutral(0) capturable(0) computer(1) person(2) rescueable(2)
PudPlayers = [0 0 neutral 0 computer person capturable rescueable]
Critters = t summer\sheep wasteland\boar winter\seal swamp\hellhog

type world w h owned/(dup 32 []) units cycle scheds vs vs_i
           nqs trans orders free_ids new_units del_units margin/10 
           margin_origin/[10 10] max_units/1200 max_w/300 max_h/300
           tileset tileset_name world_rect tiles gfxes
           players this_player
| WxH = $max_w*$max_h
| $units <= dup $max_units+WxH
| $free_ids <= ($max_units){?+WxH}
| $vs <= dup $units.size [] // visible units
world.cellIndex P = $w*P.1 + P.0
world.setTile P I =
| C = $tiles.copy
| Id = $cell_id{P}
| C.id <= Id
| C.disp <= P*32
| C.neibs <= Dirs{?+P}.keep{?.in{$world_rect}}
| $units.Id <= C

main.load_pud Path =
| W = world
| era [N @_] =
  | W.tileset_name <= PudTilesets.N
  | W.tileset <= $tilesets.(W.tileset_name)
| Handlers = t
  'DESC' | Xs => //W.description <= Xs.take{Xs.locate{0}^supply{32}}.utf8
  'ERA ' | Xs => era Xs
  'ERAX' | Xs => era Xs
  'DIM ' | Xs =>
  'OWNR' | Xs =>
  'SIDE' | Xs =>
  'SGLD' | Xs => 
  'AIPL' | Xs => 
  'UNIT' | Xs =>
| Cs = Path.get^(@r$[] [4/M.utf8 4/L.u4 L/D @Xs] => [[M D] @Xs^r])
| less Cs^is{[[\TYPE _]@_]}: bad "Invalid PUD file: [Path]"
| for [T D] Cs: when got!it Handlers.T: it D
| Cs{?0}
