use gfx macros
ResNames = [good wood oil food mana score]
Dirs = 8{(?.float-2.0)*PI/4.0}{[?.cos ?.sin].round.int}

//dirN V = pos V,sign Dirs

type main data tilesets es cache pf_range
main Data =
| M = new_main Data 0 ut (m) 2**14
| M.init_tiles
| M.init_entities
| M

main.gfx File =
| when got!it $cache.File: leave it
| G = gfx_load "[$data]/[File]"
| $cache.File <= G
| G

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

type entity
entity.size = [1 1]
entity.owner = 0
entity.hp = 0
entity.armor = 0
entity.rm = Void
entity.resource = Void


cfg File = File.get.utf8.lines{}{?parse}.skip{is.[]}



export main cfg
