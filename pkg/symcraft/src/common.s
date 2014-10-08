use gfx macros reader
ResNames = [good wood oil food mana score]
Dirs = 8{(?.float-2.0)*PI/4.0}{[?.cos ?.sin].round.int}

MCs = | C = [water land plain air forest wall rock dead invuln 0 1 2 3 4 5 6 7 8 9 10]
      | (C.i){[?1 2**?0]}.table

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

type cost gold wood oil mana time food
cost_from_list Xs =
| C = cost
| for X Xs: case X
  [gold A] | C.gold <= A
  [wood A] | C.wood <= A
  [oil A] | C.oil <= A
  [mana A] | C.mana <= A
  [time A] | C.time <= A
  [food A] | C.food <= A
| C
cost.as_text = "#cost{gold([$gold]) wood([$wood]) oil([$oil]) time([$time]) food([$food])}"
cost.list = [[gold $gold] [wood $wood] [oil $oil] [mana $mana] [time $time]]

DummyGfx = gfx 1 1
DummyIcon = t human DummyGfx orc DummyGfx
DummySprite = Void
StillAnim = [[0 6]]
DeathAnim = [[0 0]]

animSpeed $0 [[_ W _]@Xs] = W+Xs^animSpeed

type unit.entity type pud/Void typename/Void move_class/[] organic undead building role
                 size/[1 1] sprite/Void sounds/Void icon/DummyIcon prodName
                 hp hits mp mana armor sight damage range speed effect
                 cost/(cost) use_cost/(cost) use_cost_player/(cost) research_cost/(cost)
                 acts upgrades upgrade researches deps negs anims layer selection shaded
                 trains builds proto_gfx faces/5 explodes show say shadow
                 dir/Dirs.0 frame mask detector resource resources
                 area shards bounces offset/[0 0] splash impact extends foundation transport
                 move inc cycles ignoresDst nonRMB hotkey targets do forced prio enabled_if
                 fix rmbPrio morphAll morphs hide supply boostsHarvest depot harvests/[] ttl

unit.as_text = "#unit{[$type]}"

type main{Data} data/Data sounds/"[Data]/sounds"
                tilesets/0 types/(t) roles/(t) upgrades/(t) cache/(t)
                pf_range/2**14 ts_names pud/(t)
| $init_tiles
| $ts_names <= $tilesets{}{?0}
| DummySprite <= ($ts_names){[? (dup 5 DummyGfx)]}
| $init_types

main.gfx File =
| when got!it $cache.File: leave it
| G = gfx "[$data]/[File]"
| $cache.File <= G
| G

main.unitFrames C S File =
| G = gfx File
| W = if S then S.0 else G.w/C
| H = if S then S.1 else W
| R = G.frames{W H}.group{C}{|[X]=>dup{5 X}; X=>X}{[@? @?.cut{1 3}.flip]}
| G.free
| R

normalize_cost Xs = Xs{?tail}

main.load_type_hlp Path T =
| say "load_type [T]"
| U = Void
| Base = Path.url.0
| Xs = "[Path]/unit.txt".get.utf8.parse{Path}^(|[`|`@Xs]=>Xs; X=>[X]){}{[?1.0 @?2]}
| for X Xs: case X [proto PT]: U <= $load_type{"[Base]/[PT]"}.copy
| have U: unit
| U.type <= T
| Corpse = 0
| for X Xs: case X
  [pud Id] | U.pud <= Id
  [proto PT] |
  [move_class @Xs] | U.move_class <= Xs
  [role R] | U.role <= R
  [size W H] | U.size <= [W H]
  [hp V] | U.hp <= V
  [hits V] | U.hits <= V
  [mp V] | U.mp <= V
  [mana V] | U.mana <= V
  [armor V] | U.armor <= V
  [sight V] | U.sight <= V
  [damage @V] | U.damage <= V
  [range V] | U.range <= V
  [effect @Xs] | U.effect <= Xs
  [acts @Xs] | U.acts <= Xs
  [anims @Xs] | U.anims <= Xs.group{2}{[?0 ?1.1]}.table
  [shadow O] | U.shadow <= O
  [layer O] | U.layer <= MCs.O
  [faces N] | U.faces <= N
  [selection W H] | U.selection <= [W H]
  [cost @V] | U.cost <= cost_from_list V^normalize_cost
  [use_cost @V] | U.use_cost <= cost_from_list V^normalize_cost
  [research_cost @V] | U.research_cost <= cost_from_list V^normalize_cost
  [use_cost_player @V] | U.use_cost_player <= cost_from_list V^normalize_cost
  [resources @V] | U.resources <= V^normalize_cost.table
  [deps @Xs] | U.deps <= Xs
  [negs @Xs] | U.negs <= Xs
  [trains @Xs] | U.trains <= Xs
  [builds @Xs] | U.builds <= Xs
  [shaded V] | U.shaded <= V
  [upgrades @Xs] | U.upgrades <= Xs
  [upgrade @Xs] | U.upgrade <= Xs
  [researches @Xs] | U.researches <= Xs
  [show Anim] | U.show <= Anim
  [area X Y] | U.area <= [X Y]
  [shards V] | U.shards <= V
  [offset circle] | U.offset <= \circle
  [offset X Y] | U.offset <= [X Y]
  [splash V] | U.splash <= V
  [impact V] | U.impact <= V
  [bounces V] | U.bounces <= V
  [nonRMB V] | U.nonRMB <= V
  [building V] | U.building <= V
  [organic V] | U.organic <= V
  [undead V] | U.undead <= V
  [corpse V] | Corpse <= V
  [move V] | U.move <= V
  [inc V] | U.inc <= V
  [cycles V] | U.cycles <= V
  [ignoresDst V] | U.ignoresDst <= V
  [explodes V] | U.explodes <= V
  [typename V] | U.typename <= V
  [prodName V] | U.prodName <= V
  [targets @Xs] | U.targets <= Xs
  [hotkey V] | U.hotkey <= V
  [do @Xs] | U.do <= Xs
  [forced V] | U.forced <= V
  [prio V] | U.prio <= V
  [rmbPrio V] | U.rmbPrio <= V
  [fix V] | U.fix <= V
  [morphAll V] | U.morphAll <= V
  [morphs @Xs] | U.morphs <= Xs
  [hide V] | U.hide <= V
  [supply V] | U.supply <= V
  [depot @Xs] | U.depot <= Xs
  [boostsHarvest @Xs] | U.boostsHarvest <= Xs
  [say V] | U.say <= V
  [ttl V] | U.ttl <= V
  [detector V] | U.detector <= V
  [resource V] | U.resource <= V
  [extends V] | U.extends <= V
  [foundation @Xs] | U.foundation <= Xs
  [harvests @Xs] | U.harvests <= Xs
  [transport V] | U.transport <= V
  [enabled_if @Xs] | U.enabled_if <= Xs
  Else | bad "load_type{[T]}: cant parse [X] in [Path]"
| have U.typename T.split{_}{?title}.text{' '}
| less U.anims: U.anims <= t
| less U.cost: U.cost <= cost
| SpriteOverride = 0
| when @exists "[Path]/gfxes"
  | Gs = "[Path]/gfxes".paths{}{X=>[X.url.1 $unitFrames{U.faces 0 X}]}.table
  | have Gs.default: DummySprite
  | ($ts_names){(have Gs.?: Gs.default)}
  | SpriteOverride <= 1
  | U.sprite <= Gs
| have U.sprite: DummySprite
| MC = U.move_class{}{|X<1.is_int=>X+5; X=>X}{MCs.?}
| U.mask <= MC.fold{U.layer.shl{5} (@ior ? ??)}
| have U.selection: U.size*2
| when@exists!it "[Path]/icon.png": U.icon.human <= gfx it
| U.icon.orc <= U.icon.human
| when@exists!it "[Path]/icon_orc.png": U.icon.orc <= gfx it
| less got U.sounds: U.sounds <= t
| when@exists!it "[Path]/sounds": U.sounds <= it.paths{}{[?.url.1 ?.paths]}.table
| if U.building and SpriteOverride
  then | Cs = $types.'_construction_site'.sprite
       | Ds = $types.'_destroyed_site'.sprite
       | if U.move_class^is{[plain]}
         then U.sprite <= @table: map [T Gs] U.sprite
                          | N = [Gs.size 2].min
                          | [T [@Gs.take{N} @Cs.T @Ds.T.take{2} @Gs.drop{N}]]
         else U.sprite <= @table: map [T Gs] U.sprite
                          | N = [Gs.size 4].min
                          | [T [@Gs.take{N} @Ds.T.drop{2} @Gs.drop{N}]]
  else | when U.hp: U.cost.food <= 1
       | when Corpse and SpriteOverride:
         | C = $types.'_corpse'
         | G = C.sprite.default
         | O = U.sprite.summer.size
         | [@!U.anims.death @(C.anims.Corpse){[?0+O ?1]}]
         | U.sprite <= @table: map [K V] U.sprite [K [@V @G]]
| have U.anims.still StillAnim
| have U.anims.death DeathAnim
| when got!it U.anims.move: U.speed <= animSpeed it
| have U.sounds.selected "[$sounds]/click.wav"
| less U.hp
  | if U.range
    then | //when!it U.splash: case !U.effect [E V] [E V/(it+1)] // FIXME
         | @fold !U.mask (A B=>@ior A B) 10{MCs.?}
    else @ior MCs.invuln !U.mask
| when got!it U.pud: $pud.it <= T
| when!it U.upgrades: map U it
  | have $upgrades.U []
  | [@!$upgrades.U T]
| when!it U.role:
  | have $roles.it []
  | [@!$roles.it T]
| U

main.load_type Path =
| T = Path.url.1
| when got!it $types.T: leave it
| U = $load_type_hlp{Path T}
| $types.T <= U
| U

main.init_types =
| for E "[$data]/types".paths: $load_type{E}
| for [T E] $types
  | E.proto_gfx <= Void
  | E.faces <= Void


export main cfg MCs
