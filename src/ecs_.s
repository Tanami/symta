use stack bits

ECS =
Systems =
Registered = []
Elements = dup 8 0
None =

RootSize = 128 //size of root index for each component
BlockSize =

ecs_register @Systems = for System Systems: push System Registered

type ecs{max_entities}
     entities alive freed systems/(t) arrays/(t) cycle
     free_blocks/[] root_size/RootSize block_size elements none
| ECS <= Me
| Systems <= $systems
| $none <= =>
| None <= $none
| $block_size <= ($max_entities+$root_size-1)/$root_size
| BlockSize <= $block_size
| $entities <= stack: @flip: dup Id $max_entities Id
| Size = $entities.size
| $alive <= bits Size
| $freed <= stack $entities.size
| I = 0
| $elements <= Elements
| for Constructor Registered.flip:
  | Array = dup $root_size 0
  | ArrayUsage = dup $root_size 0
  | System = Constructor{Me I Array ArrayUsage}
  | Name = System.type
  | $systems.Name <= System
  | $arrays.Name <= Array
  | System.init
  | !I+1
| for I Elements.size: Elements.I <= $systems."n[I]_".array

ecs.new_block =
| less $free_blocks.end: leave: pop $free_blocks
| None = $none
| dup BlockSize None

ecs.free_block Block = push Block $free_blocks

ecs.new @Components =
| Cs = Components
| Id = $entities.pop
| $alive.Id <= 1
| System = 0
| UseDefault = 0
| Value = 0
| while Cs.size
  | Name = pop Cs
  | UseDefault = 0
  | if Name.is_list
    then | Value <= Name.1
         | Name <= Name.0
         | UseDefault <= 0
    else | UseDefault <= 1
  | System <= $systems.Name
  | less got System: bad "ECS: unknown system - [Name]"
  | BlockIdx = Id/BlockSize
  | less System.usage.BlockIdx: 
    | System.array.BlockIdx <= $new_block
  | !System.usage.BlockIdx + 1
  | System.new{Id}
  | less UseDefault:
    | System.Id <= Value
| Id

ecs.free Id = $freed.push{Id}
ecs.array Name = $arrays.Name
ecs.`.` Name = $systems.Name

ecs.clear_freed =
| Freed = $freed.list.uniq
| $freed.clear
| when Freed.size
  | Systems = $systems.list{}{?1}
  | for System Systems:
    | Array = System.array
    | for Id Freed:
      | BlockIdx = Id/BlockSize
      | Block = Array.BlockIdx
      | when Block
        | ItemIdx = Id%BlockSize
        | when Block.ItemIdx <> $none
          | System.free{Id}
          | Block.ItemIdx <= $none
          | !System.usage.BlockIdx - 1
          | less System.usage.BlockIdx
            | $free_block{Array.BlockIdx}
            | Array.BlockIdx <= 0
  | for Id Freed
    | $alive.Id <= 0
    | $entities.push{Id}


ecs.update =
| Systems = $systems.list{}{?1}
| for System Systems: System.update
| $clear_freed
| !$cycle+1

ecs.clear =
| for Id $alive.active: $free{Id}
| $clear_freed
| $cycle <= 0

ecs.text =
| $clear_freed
| @text: map Name,System $systems:
  | "([Name] [System.entities{}{"([?] [System.?.as_text])"}.text])"

ecs_array_get A Id = A.(Id/BlockSize).(Id%BlockSize)
ecs_array_set A Id Value = A.(Id/BlockSize).(Id%BlockSize) <= Value

int.`.` System = Systems.System.Me
int.`!` System Value = Systems.System.Me <= Value

int.enable Component =
| System = Systems.Component
| BlockIdx = Me/BlockSize
| less System.usage.BlockIdx: 
  | System.array.BlockIdx <= ECS.new_block
| !System.usage.BlockIdx + 1
| System.new{Me}

int.disable Component =
| System = Systems.Component
| Array = System.array
| BlockIdx = Me/BlockSize
| Block = Array.BlockIdx
| less Block: leave 0
| ItemIdx = Me%BlockSize
| less Block.ItemIdx <> None: leave 0
| System.free{Me}
| Block.ItemIdx <= None
| !System.usage.BlockIdx - 1
| less System.usage.BlockIdx
  | ECS.free_block{Array.BlockIdx}
  | Array.BlockIdx <= 0
| 1

int.got Component =
| System = Systems.Component
| Array = System.array
| BlockIdx = Me/BlockSize
| Block = Array.BlockIdx
| less Block: leave 0
| ItemIdx = Me%BlockSize
| when Block.ItemIdx <> None: leave 1
| 0

int.components = Systems.list{}{?0}.keep{S => Me.got{S}}


type component_
component_.init =
component_.new Id = Me.Id <= 0
component_.free Id =
component_.update =
component_.`.` Id = ecs_array_get $array Id
component_.`!` Id Value = ecs_array_set $array Id Value
component_.entities =
| None = $ecs.none
| @join: map I,B $array.i.keep{?1}:
  | K = I*BlockSize
  | (dup J BlockSize J).keep{B.?<>None}{?+K}

ComponentDeps = t

component Name @Fields =
| VectorSize = 0
| Vector = 0
| InitValue = 0
| GotInitValue = 0
| Deps = 0
| Args = t
| case Name [`/` N IV]
  | InitValue <= IV
  | GotInitValue <= 1
  | Name <= N
| case Name [`{}` N @Ds]
  | Name <= N
  | Deps <= Ds
| case Name [`.` N Size]
  | VectorSize <= Size
  | Vector <= VectorSize{"n[?]_"}
  | Name <= N
| case Fields [[`[]` @As]@_]:
  | pop Fields
  | for A As: Args.A <= 1
| Component_ = "Component_[Name]_"
| Array_ = "Array_[Name]_"
| BlockSize_ = "BlockSize_[Name]_"
| Elements_ = "Elements_[Name]_"
| Me = \Me
| V = form ~V
| Id = form ~Id
| Id2 = form ~Id2
| when Deps:
  | Deps <= Deps{| [`@` X]=>ComponentDeps.X; D=>[D]}.join
  | InitValue <= form Me.ecs.new{$@Deps}
| ComponentDeps.Name <= Deps
| Xs = form
  | Array_ =
  | Component_ =
  | Elements_ =
  | BlockSize_ =
  | type Name.component_{ecs id array usage}
         type/Name $@Fields
    | Component_ <= Me
    | Array_ <= Me.array
    | BlockSize_ <= Me.ecs.block_size
    | Elements_ <= Me.ecs.elements
  | ecs_register &Name
| Custom = got Args.custom
| when not Vector and not Custom:
  | Fs = @tail: form
    | Name.new Id = Me.Id <= InitValue
  | Xs <= [@Xs @Fs]
| when Vector and not Custom:
  | less GotInitValue: InitValue <= form [$@(dup VectorSize InitValue)]
  | Fs = @tail: form
    | Name.new Id = | ecs_array_set Array_ Id Me.ecs.new{$@Vector}
                    | Me.Id <= InitValue
  | Xs <= [@Xs @Fs]
| when Vector
  | Fs = @tail: form
    | Name.`.` Id = | Id2 = ecs_array_get Array_ Id
                    | [$@(map I VectorSize: form: ecs_array_get Elements_.I Id2)]
    | Name.`!` Id V = | Id2 = ecs_array_get Array_ Id
                      | `|` $@| map I VectorSize:
                                | form: ecs_array_set Elements_.I Id2 V.I
  | Xs <= [@Xs @Fs]
| when Deps and not Vector and not Custom:
  | Fs = @tail: form
    | Name.free Id = Me.ecs.free{(ecs_array_get Me.array Id)}
  | Xs <= [@Xs @Fs]
| when Deps and Vector and not Custom:
  | Fs = @tail: form
    | Name.free Id = | Id2 = ecs_array_get Array_ Id
                     | for ~I VectorSize
                       | Me.ecs.free{(ecs_array_get Elements_.~I Id2)}
                     |  Me.ecs.free{Id2}
  | Xs <= [@Xs @Fs]
| Accessors = @tail: form
  | int.Name = Component_.Me
  | int.$"![Name]" V = Component_.Me <= V
| form @$[@Xs @Accessors]

export ecs ecs_register ecs_array_get ecs_array_set component_ component 'component'