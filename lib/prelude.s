non F = X => if F X then 0 else 1
no X = Void >< X
have X = Void <> X

text.end = 1 //FIXME: this is wrong

void.end = 1
void.`.` K = Void
void.find F = Void
void.locate F = Void

_.`<>` B = not Me >< B
_.`<<` B = not B < Me
_.`>` B = B < Me
_.`>>` B = not Me < B

_.is_int = 0
int.is_int = 1

_.is_fn = 0
fn.is_fn = 1

_.is_list = 0
list.is_list = 1

_.is_text = 0
text.is_text = 1

//_.is_hard_list = 0
//list.is_hard_list = 1

text.`<` B =
| unless B.is_text: bad "cant compare string `[Me]` with [B]"
| AS = Me.size
| BS = B.size
| when AS <> BS: leave AS < BS
| times I AS
  | AC = Me.I.code
  | BC = B.I.code
  | when AC <> BC: leave AC < BC
| 0

text.is_upcase =
| times I Me.size
  | C = Me.I.code
  | when C < 'A'.code or 'Z'.code < C: leave 0
  | I !+ 1
| 1

text.is_downcase =
| times I Me.size
  | C = Me.I.code
  | when C < 'a'.code or 'z'.code < C: leave 0
  | I !+ 1
| 1

text.upcase =
| Ys = map Char Me.chars
  | C = Char.code
  | if C < 'a'.code or 'z'.code < C then Char else (C - 'a'.code + 'A'.code).char
| Ys.unchars

text.downcase =
| Ys = map Char Me.chars
  | C = Char.code
  | if C < 'A'.code or 'Z'.code < C then Char else (C - 'A'.code + 'a'.code).char
| Ys.unchars

_.is_keyword = 0
text.is_keyword = Me.size and Me.0.is_upcase

int.i = dup I Me: I //iota operator

list.enum = dup I Me.size: [I Me^pop]

int.`.` F = dup I Me: F I

list.`.` K =
| times I K: Me <= Me.tail
| Me.head

list.size =
| S = 0
| till Me.end
  | Me <= Me.tail
  | S !+ 1
| S

list.`><` B =
| unless B.is_list: leave 0
| till Me.end or B.end: unless Me^pop >< B^pop: leave 0
| 1

hard_list.`><` B =
| unless B.is_list: leave 0 //FIXME: cons_list B will be O(n^2) slow
| N = Me.size
| unless N >< B.size: leave 0
| times I N
  | unless Me.I >< B.I: leave 0
  | I !+ 1
| 1

list.reverse =
| N = Me.size
| Ys = dup I N: 0
| while N > 0
  | N !- 1
  | Ys.N <= pop Me
| Ys

hard_list.reverse =
| N = Me.size
| dup I N
  | N !- 1
  | Me.N

list.map F = dup I Me.size: F Me^pop
hard_list.map F = dup I Me.size: F Me.I

list.each F = till Me.end: F Me^pop
hard_list.each F = times I Me.size: F Me.I

list.sum =
| S = 0
| till Me.end: S !+ Me^pop
| S

hard_list.sum =
| S = 0
| times I Me.size: S !+ Me.I
| S

list.count F =
| C = 0
| till Me.end: when F Me^pop: C !+ 1
| C

hard_list.count F =
| S = 0
| I = 0
| times I Me.size: when F Me.I: S !+ 1
| S

list.countNot F =
| C = 0
| till Me.end: unless Me^pop^F: C !+ 1
| C

hard_list.countNot F = Me.size - Me.count{F}

list.keep F =
| N = Me.count{F}
| Ys = dup I N 0
| J = 0
| while J < N
  | X = pop Me
  | when F X
    | Ys.J <= X
    | J !+ 1
| Ys

hard_list.keep F =
| N = Me.count{F}
| Ys = dup I N 0
| I = 0
| J = 0
| while J < N
  | X = Me.I
  | when F X
    | Ys.J <= X
    | J !+ 1
  | I !+ 1
| Ys

list.skip F =
| N = Me.countNot{F}
| Ys = dup I N 0
| J = 0
| while J < N
  | X = pop Me
  | unless F X
    | Ys.J <= X
    | J !+ 1
| Ys

hard_list.skip F =
| N = Me.countNot{F}
| Ys = dup I N 0
| I = 0
| J = 0
| while J < N
  | X = Me.I
  | unless F X
    | Ys.J <= X
    | J !+ 1
  | I !+ 1
| Ys

list.join =
| Size = Me.map{X=>X.size}.sum
| Rs = dup I Size 0
| I = 0
| for Ys Me: for Y Ys | Rs.I <= Y
                      | I !+ 1
| Rs

_list_.harden = Me

list.harden =
| N = Me.size
| Ys = dup I N 0
| times I N: Ys.I <= pop Me
| Ys

list.unchars = Me.harden.unchars

list.split F =
| Ys = []
| P = Me.locate{F}
| while have P
  | Ys <= [Me.take{P}@Ys]
  | Me <= Me.drop{P+1}
  | P <= Me.locate{F}
| [Me@Ys].reverse

text.split F = Me.chars.split{F}.map{X=>X.unchars}

list.take N = dup I N: Me^pop
hard_list.take N = dup I N: Me.I

list.drop N =
| times I N Me^pop
| Me

hard_list.drop S =
| dup I Me.size-S
  | R = Me.S
  | S !+ 1
  | R

list.last = Me.(Me.size-1)
list.suf X = [@Me X]
list.lead = Me.take{Me.size-1}

list.infix Item =
| N = Me.size*2-1
| if N < 0 then [] else dup I N: if I%2 then Item else Me^pop

list.locate F =
| I = 0
| till Me.end
  | when F Me^pop: leave I
  | I !+ 1

hard_list.locate F = times I Me.size: when F Me.I: leave I

list.find F =
| I = 0
| till Me.end
  | X = Me^pop
  | when F X: leave X
  | I !+ 1

hard_list.find F =
| times I Me.size
  | X = Me.I
  | when F X: leave X

text.chars = dup I Me.size Me.I

list.groupBy N =
| Y = []
| Ys = []
| I = 0
| till Me.end
  | push Me^pop Y
  | I !+ 1
  | when I >< N
    | push Y.reverse Ys
    | Y <= []
    | I <= 0
| when Y.size: push Y.reverse Ys
| Ys.reverse

GGensymCount = 0
gensym Name = "[Name]__[GGensymCount!+1]"

bad @Xs =
| log (map X [bad @Xs] X.as_text).infix{` `}.unchars
| halt

say @Xs =
| log (map X Xs X.as_text).infix{` `}.unchars
| Void

// hashtable
data table buckets
table Size = new_table: dup I Size Void
table.`.` K =
| Bs = Me.buckets
| H = K.hash%Bs.size
| Bs.H.find{X => X.0><K}.1
table.`!` K V =
| Bs = Me.buckets
| H = K.hash%Bs.size
| Xs = Bs.H
| if no Xs then Bs.H <= [[K V]]
  else | Old = Xs.find{X => X.0><K}
       | if no Old then Bs.H <= [[K V]@Xs]
         else Old.1 <= V
| Void


export non say bad no have gensym table
