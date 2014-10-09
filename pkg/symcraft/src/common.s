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
                 id disp

unit.as_text = "#unit{[$type]}"

type main{Data} world data/Data sounds/"[Data]/sounds"
                tilesets/0 types/(t) roles/(t) upgrades/(t) cache/(t)
                pf_range/2**14 ts_names pud/(t) unitSetters
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

ListFields =
| Xs = [move_class effect acts deps negs trains builds upgrades upgrade
        researches target do morphs depot boostsHarvest foundation
        harvests enabled_if]
| Xs{[? 1]}.table

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
  [anims @Xs] | U.anims <= Xs.group{2}{[?0 ?1.1]}.table
  [cost @V] | U.cost <= cost_from_list V^normalize_cost
  [use_cost @V] | U.use_cost <= cost_from_list V^normalize_cost
  [research_cost @V] | U.research_cost <= cost_from_list V^normalize_cost
  [use_cost_player @V] | U.use_cost_player <= cost_from_list V^normalize_cost
  [resources @V] | U.resources <= V^normalize_cost.table
  [corpse V] | Corpse <= V
  [layer O] | U.layer <= MCs.O
  [proto PT] |
  [K @As] | S = $unitSetters.K
          | when no S: bad "load_type{[T]}: uknown field [K] for [Path]"
          | if got ListFields.K then S U As
            else | when As.size <> 1: "load_type{[T]}: bad field [K] for [Path]"
                 | S U As.0
  Else | bad "load_type{[T]}: bad entry [X] for [Path]"
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
| $unitSetters <= (unit)^methods_.keep{?0.0 >< '!'}{[?0.tail ?1]}.table
| for E "[$data]/types".paths: $load_type{E}
| for [T E] $types
  | E.proto_gfx <= Void
  | E.faces <= Void

PudTilesets = [summer winter wasteland swamp]
PudTeams = t nobody(0) neutral(0) capturable(0) computer(1) person(2) rescueable(2)
PudPlayers = [0 0 neutral 0 computer person capturable rescueable]
Critters = t summer\sheep wasteland\boar winter\seal swamp\hellhog

type world w h game_info owned/(dup 32 []) units cycle scheds vs vs_i
           nqs trans orders free_ids new_units del_units margin/10 
           margin_origin/[10 10] max_units/1200 max_w/300 max_h/300 tileset
           world_rect tiles gfxes
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

export main cfg MCs