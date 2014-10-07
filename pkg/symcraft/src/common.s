use gfx macros reader
ResNames = [good wood oil food mana score]
Dirs = 8{(?.float-2.0)*PI/4.0}{[?.cos ?.sin].round.int}

dirN V = Dirs.locate{V.sign}

cfg File = File.get.utf8.lines{}{?parse}.skip{is.[]}

type entity
entity.type = \entity
entity.size = [1 1]
entity.owner = 0
entity.hp = 0
entity.armor = 0
entity.rm = Void
entity.resource = Void

type resources gold wood oil time
resources.as_text = "#resources{gold([$gold]) wood([$wood]) oil([$oil]) time([$time])}"
resources.list = [[gold $gold] [wood $wood] [oil $oil] [time $time]]

type unit.entity type pud typename movement organic role size/[1 1]
                 hp armor sight damage range cost/(resources)
                 acts anims layer selection shadow proto_gfx faces
unit.as_text = "#unit{[$type]}"

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


normalize_cost C = case C [`=` A B] | [@A @B^normalize_cost]
                          B | B

main.load_unit_hlp Path T =
//| say "load_unit [T]"
| U = Void
| Base = Path.url.0
| Xs = "[Path]/unit.txt".get.utf8.parse{Path}^(|[`|`@Xs]=>Xs; X=>[X]){}{[?1.0 @?2]}
| for X Xs: case X [proto PT]: U <= $load_unit{"[Base]/[PT]"}.copy
| have U: unit
| U.typename <= T.split{_}{?title}.text{' '}
| less T >< daemon: leave U
| for X Xs: case X
  [pud Id] | U.pud <= Id
  [proto PT] |
  [movement @M] | U.movement <= M
  [organic B] | U.organic <= B
  [role R] | U.role <= R
  [size @WH] | U.size <= WH
  [hp V] | U.hp <= V
  [armor V] | U.armor <= V
  [sight V] | U.sight <= V
  [damage @V] | U.damage <= V
  [range V] | U.range <= V
  [cost @V] | for X V^normalize_cost.group{2}: case X
              [gold A] | U.cost.gold <= A
              [wood A] | U.cost.wood <= A
              [oil A] | U.cost.oil <= A
              [time A] | U.cost.time <= A
  [acts @Xs] | U.acts <= Xs
  [anims @Xs] | U.anims <= Xs
  [shadow O] | U.shadow <= O
  [layer O] | U.layer <= O
  [selection @WH] | U.selection <= WH
  Else | bad "cant parse [X] in [Path]"
| U.anims <= U.anims.group{2}{[?0 ?1.1]}.as_map
| U.cost^say
| U

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


export main cfg
