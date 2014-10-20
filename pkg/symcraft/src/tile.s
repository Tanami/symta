use util gfx reader

Data = Void
I2E = Void //index to edge
E2I = Void //edge to index
Tilesets = Void

TTypes = t // tile types
  block   | t base 0       mc 0
  plainL  | t base block   mc [air land plain]
  plainD  | t base plainL  mc [air land plain]
  forest  | t base plainL  mc [air forest]
              rm forestR  hp 1  wood 100  resource wood
  mudL    | t base plainL  mc [air land]
  mudD    | t base mudL    mc [air land]
  waterL  | t base mudL    mc [air water]
  waterD  | t base waterL  mc [air water]
  rock    | t base mudL    mc [air rock] rm rockR  hp 1
  wallH   | t base plainL  mc [air wall] rm wallR  hp 100 armor 20
  wallO   | t base plainL  mc [air wall] rm wallR  hp 100 armor 20
  wallCH  | t base plainL  mc [air wall] rm wallCR hp 100 armor 20
  wallCO  | t base plainL  mc [air wall] rm wallCR hp 100 armor 20
  rockR   | t base plainL  mc [air land] rm 0
  forestR | t base block   mc [air land plain] rm 0
  wallR   | t base block   mc [air land] rm 0
  wallCR  | t base block   mc [air land] rm 0

foldEdges X = X.i.map{[I V]=>V.digits{2}.shl{3*I}}.fold{0 (@ior ? ??)}

calcEdges X =
| T = X.transpose
| [X.0 T.2 X.2 T.0]^foldEdges.ior{X.1.1.shl{12}}

type cell.entity type base rm mask tileId gfxId gfx edges mc hp armor resource gold wood
                 id xy disp neibs content sensors real_xy mm_color 
cell.as_text = "#cell{[$type] [$tileId]}"

type tileset{Name Tiles Trns} name/Name tiles/Tiles trns/Trns

loadTileset P =
| TilesetGfx = "[P]gfx.png"^gfx
| Palette = TilesetGfx.cmap
| Frames = TilesetGfx.frames{32 32}
| TilesetGfx.free
| Ts = dup 4096
| Tr = t
| N = 0
| for [K Type @Gs] "[P]tiles.txt"^cfg
  | Tr."[K]_[Type]" <= N
  | T = TTypes.Type
  | Mask = T.mc{}{MCs.?}.fold{T.hp^($0 0=>MCs.dead) (@ior ? ??)}
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
    | C.mm_color <= Palette.(C.gfx.get{15 15})
    | Ts.(N+I) <= C
  | !N+16
| tileset P.lead.url.1 Ts Tr

main.init_tiles =
| I2E <= "[$data]cfg/grid.txt"^cfg.group{3}{?^calcEdges}.i.table
| E2I <= I2E{?flip}.table
| $tilesets <= "[$data]tiles".paths{}{?^loadTileset}{[?.name ?]}.table

export
