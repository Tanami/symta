use stack bits

ECS =
Systems =
Registered = []

ecs_register @Systems = for System Systems: push System Registered

type ecs{max_entities}
     entities entities_flags freed systems/(t) arrays/(t) cycle
| ECS <= Me
| Systems <= $systems
| $entities <= stack: @flip: dup Id $max_entities Id
| Size = $entities.size
| $entities_flags <= bits Size
| $freed <= stack $entities.size
| I = 0
| for Constructor Registered.flip:
  | Array = dup Size 0
  | Entities = stack Size
  | EntitiesFlags = bits Size
  | System = Constructor{Me I Array Entities EntitiesFlags}
  | Name = typename System
  | $systems.Name <= System
  | $arrays.Name <= Array
  | System.init
  | !I+1

ecs.new @Components =
| Cs = Components
| Id = $entities.pop
| $entities_flags.Id <= 1
| Value = 0
| System = 0
| UseDefault = 0
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
  | when UseDefault: Value <= System.default
  | System.entities.push{Id}
  | System.entities_flags.Id <= 1
  | System.new{Id Value}
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
    | EF = System.entities_flags
    | for Id Freed: when EF.Id:
      | System.entities.remove{Id}
      | EF.Id <= 0
  | for Id Freed
    | $entities_flags.Id <= 0
    | $entities.push{Id}


ecs.update =
| Systems = $systems.list{}{?1}
| for System Systems: System.update
| $clear_freed
| !$cycle+1

ecs.clear =
| for Id $entities_flags.active: $free{Id}
| $clear_freed
| $cycle <= 0

ecs.text =
| $clear_freed
| @text: map Name,System $systems:
  | As = System.array
  | "([Name] [System.entities{}{"([?] [As.?.as_text])"}.text])"


int.`.` System = Systems.System.Me
int.`!` System Value = Systems.System.Me <= Value

type component_
component_.init = for I $array.size: $array.I <= $default
component_.new Id Value = Me.Id <= Value
component_.free Id =
component_.update =
component_.default = 0
component_.`.` Id = $array.Id
component_.`!` Id Value = $array.Id <= Value


component Name @Fields =
| Fast = 0
| Vector = 0
| VectorType = 0
| VectorSize = 0
| DefaultValue = 0
| Deps = 0
| case Name [`/` N DV]
  | DefaultValue <= DV
  | Name <= N
| case Name [`&` N]
  | Fast <= 1
  | Name <= N
| case Name [`&` N]
  | Vector <= 1
  | Name <= N
| case Name [`{}` N @Ds]
  | Name <= N
  | Deps <= Ds
| case DefaultValue [`.` VT Size]
  | Vector <= 1
  | VectorType <= VT
  | DefaultValue <= form [$@(dup Size 0)]
  | VectorSize <= Size
| Component_ = "Component_[Name]_"
| Array_ = "Array_[Name]_"
| Me = \Me
| V = form ~V
| Xs = form
  | Array_ =
  | Component_ =
  | type Name.component_{ecs id array entities entities_flags} $@Fields
    | Component_ <= Me
    | Array_ <= Me.array
  | Name.default = DefaultValue
  | ecs_register &Name
| when Vector
  | Fs = @tail: form
    | Name.`.` ~Id = Me.array.~Id
    | Name.`!` ~Id ~V = Me.array.~Id.init{~V}
  | Xs <= [@Xs @Fs]
| when Deps
  | Fs = @tail: form
    | Name.init =
    | Name.new ~Id ~V = Me.array.~Id <= Me.ecs.new{$@Deps}
    | Name.free ~Id = Me.ecs.free{Me.array.~Id}
  | Xs <= [@Xs @Fs]
| when VectorType
  | Fs = @tail: form
    | Name.new ~Id ~V = for ~I VectorSize
                        | Array_.~Id.~I <= Me.ecs.new{VectorType}
    | Name.free ~Id = for ~I VectorSize
                      | Me.ecs.free{Array_.~Id.~I}
  | Xs <= [@Xs @Fs]
| Accessors = @tail: form
  | int.Name = $if Fast
                then form Array_.Me
                else form Component_.Me
  | int.$"![Name]" V = $@if Fast
                         then form: Array_.Me <= V
                         else form: Component_.Me <= V
| form @$[@Xs @Accessors]

export ecs ecs_register component_ 'component'