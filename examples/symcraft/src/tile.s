use gfx

Data = Void
I2E = Void //index to edge
E2I = Void //edge to index
Tilesets

MCs = | C = [water land plain air forest wall rock dead invuln 0 1 2 3 4 5 6 7 8 9 10]
      | (C.i){[?1 2**?0]}.as_map

TTypes = m // tile types
  block   | m base 0       mc 0
  plainL  | m base block   mc [air land plain]
  plainD  | m base plainL  mc [air land plain]
  forest  | m base plainL  mc [air forest]
              rm forestR  hp 1  wood 100  resource wood
  mudL    | m base plainL  mc [air land]
  mudD    | m base mudL    mc [air land]
  waterL  | m base mudL    mc [air water]
  waterD  | m base waterL  mc [air water]
  rock    | m base mudL    mc [air rock] rm rockR  hp 1
  wallH   | m base plainL  mc [air wall] rm wallR  hp 100 armor 20
  wallO   | m base plainL  mc [air wall] rm wallR  hp 100 armor 20
  wallCH  | m base plainL  mc [air wall] rm wallCR hp 100 armor 20
  wallCO  | m base plainL  mc [air wall] rm wallCR hp 100 armor 20
  rockR   | m base plainL  mc [air land] rm 0
  forestR | m base block   mc [air land plain] rm 0
  wallR   | m base block   mc [air land] rm 0
  wallCR  | m base block   mc [air land] rm 0


foldEdges X = X.i.map{[I V]=>V.digits{2}.shl{3*I}}.fold{0 (@ior ? ??)}

calcEdges X =
| T = transpose X
| [X.0 T.2 X.2 T.0]^foldEdges.ior{X.1.1.shl{12}}

cfg P = P.get.utf8.lines{}{?parse}.skip{is.[]}

data entity
entity.size = [1 1]
entity.owner = 0
entity.hp = 0
entity.armor = 0
entity.rm = Void
entity.resource = Void

data cell.entity type base rm mask tileId gfxId gfx edges mc hp resource gold wood
cell = new_cell 0 0 0 0 0 0 0 0 0 0

data tileset name tiles trns

loadTileset P =
| Frames = "[P]/gfx.png"^load_gfx.frames{32 32}
| Ts = dup 4096
| Tr = m
| N = 0
| for [K Type @Gs] "[P]/tiles.txt"^cfg
  | Tr."[K]_[Type]" <= N
  | T = TTypes.Type
  | Mask = T.mc{}{MCs.?}.fold{T.hp^(&0 0=>MCs.dead) (@ior ? ??)}
  | for [I G] Gs.i
    | C = cell
    | C.type <= Type
    | C.base <= T.base
    | C.mask <= Mask
    | C.tileId <= N+I
    | C.edges <= I2E.K
    | C.gfxId <= G
    | C.gfx <= Frames.G
    | C.mc <= T.mc
    | C.hp <= T.hp
    | C.armor <= T.armor
    | C.rm <= T.rm
    | C.resource <= T.resource
    | C.gold <= T.gold
    | C.wood <= T.wood
    | Ts.(N+I) <= C
    | !N+I
| new_tileset P.url.1 Ts Tr

tile_init Path =
| Data <= Path
| I2E <= "$[Data]/cfg/grid.txt"^cfg.group{3}{?^calcEdges}.i.as_table
| E2I <= E2I{?flip}.as_table
| "[Data]/tiles".paths{}{?^loadTileset}{[?.name ?]}.as_table

export tile_init
