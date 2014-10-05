use gfx macros
ResNames = [good wood oil food mana score]
Dirs = 8{(?.float-2.0)*PI/4.0}{[?.cos ?.sin].round.int}

dirN V = Dirs.locate{V.sign}

type entity
entity.type = \entity
entity.size = [1 1]
entity.owner = 0
entity.hp = 0
entity.armor = 0
entity.rm = Void
entity.resource = Void

type unit.entity type proto_gfx faces

type main{Data} data/Data tilesets/0 es/(m) cache/(m) pf_range/2**14
| $init_tiles
| $init_units

main.gfx File =
| when got!it $cache.File: leave it
| G = gfx "[$data]/[File]"
| $cache.File <= G
| G

main.unitFrames C S File =
| G = $gfx{File}
| W = if S then S.0 else G.w/C
| H = if S then S.1 else W
| G.frames{W H}.group{C}{|[X]=>dup{5 X}; X=>X}{[@? @?.cut{1 3}.flip]}

main.load_unit_hlp Path T =
| say "load_unit [T]"
| unit

main.load_unit Path =
| T = Path.url.1
| when got!it $es.T: leave it
| E = $load_unit_hlp{Path T}
| $es.T <= E
| E

main.init_units =
| for E "[$data]/units".paths: $load_unit{E}
| for [T E] $es
  | E.proto_gfx <= Void
  | E.faces <= Void

cfg File = File.get.utf8.lines{}{?parse}.skip{is.[]}

export main cfg
