use gfx macros
ResNames = [good wood oil food mana score]
Dirs = 8{(?.float-2.0)*PI/4.0}{[?.cos ?.sin].round.int}

dirN V = Dirs.locate{V.sign}

type entity
entity.size = [1 1]
entity.owner = 0
entity.hp = 0
entity.armor = 0
entity.rm = Void
entity.resource = Void


type main{Data} data/Data tilesets/0 es cache/(m) pf_range/2**14
| $init_tiles
| $init_entities

main.gfx File =
| when got!it $cache.File: leave it
| G = gfx "[$data]/[File]"
| $cache.File <= G
| G

main.unitFrames C S File =
| G = $gfx{File}
| W = if S then S.0 else G.w/C
| H = if S then S.1 else W
| G.frames{W H}.group{C}{[X]}

main.load_entity Path =
| T = Path.url.0.split{'/'}.last
| when got!it $es.T: leave it
| E = Me.load_entity_hlp{Path T}
| $es.T <= E
| E

main.init_entities =
| for E "[$data]/units".paths: $load_unit{E}
| for E $es
  | E.protoGfx <= Void
  | E.faces <= Void



cfg File = File.get.utf8.lines{}{?parse}.skip{is.[]}



export main cfg
