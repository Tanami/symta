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
| I = 0
| while I < AS
  | AC = Me.I.code
  | BC = B.I.code
  | when AC <> BC: leave AC < BC
| 0

text.is_upcase =
| N = Me.size
| I = 0
| while I < N
  | C = Me.I.code
  | when C < 'A'.code or 'Z'.code < C: leave 0
  | I !+ 1
| 1

text.is_downcase =
| N = Me.size
| I = 0
| while I < N
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

int.i = //iota operator
| Ys = Me x 0
| I = 0
| while I < Me
  | Ys.I <= I
  | I !+ 1
| Ys

list.enum =
| Ys = Me.size x 0
| I = 0
| till Me.end
  | Ys.I <= [I Me^pop]
  | I !+ 1
| Ys

int.`.` F =
| I = 0
| Ys = Me x 0
| while I < Me
  | Ys.I <= F I
  | I! + 1
| Ys

list.`.` K =
| while K > 0
  | Me <= Me.tail
  | K !- 1
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
| I = 0
| while I < N
  | unless Me.I >< B.I: leave 0
  | I !+ 1
| 1

list.reverse =
| N = Me.size
| Ys = N x 0
| while N > 0
  | N !- 1
  | Ys.N <= pop Me
| Ys

hard_list.reverse =
| N = Me.size
| I = 0
| Ys = N x 0
| while I < N
  | N !- 1
  | Ys.I <= Me.N
  | Ys.N <= Me.I
  | I !+ 1
| Ys

list.map F =
| N = Me.size
| Ys = N x 0
| I = 0
| while I < N
  | Ys.I <= F: pop Me
  | I !+ 1
| Ys

hard_list.map F =
| N = Me.size
| I = 0
| Ys = N x 0
| while I < N
  | Ys.I <= Me.I^F
  | I !+ 1
| Ys

list.each F = till Me.end: Me^pop^F

hard_list.each F =
| N = Me.size
| I = 0
| while I < N
  | F Me.I
  | I !+ 1

list.sum =
| S = 0
| till Me.end: S !+ Me^pop
| S

hard_list.sum =
| S = 0
| I = 0
| N = Me.size
| while I < N
  | S !+ Me.I
  | I !+ 1
| S

list.count F =
| C = 0
| till Me.end: when Me^pop^F: C !+ 1
| C

hard_list.count F =
| S = 0
| I = 0
| N = Me.size
| while I < N
  | when F Me.I: S !+ 1
  | I !+ 1
| S

list.countNot F =
| C = 0
| till Me.end: unless Me^pop^F: C !+ 1
| C

hard_list.countNot F = Me.size - Me.count{F}

list.keep F =
| N = Me.count{F}
| Ys = N x 0
| J = 0
| while J < N
  | X = pop Me
  | when F X
    | Ys.J <= X
    | J !+ 1
| Ys

hard_list.keep F =
| N = Me.count{F}
| Ys = N x 0
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
| Ys = N x 0
| J = 0
| while J < N
  | X = pop Me
  | unless F X
    | Ys.J <= X
    | J !+ 1
| Ys

hard_list.skip F =
| N = Me.countNot{F}
| Ys = N x 0
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
| Rs = Size x 0
| I = 0
| Me map: Ys => Ys map: Y => | Rs.I <= Y
                             | I !+ 1
| Rs

_list_.harden = Me

list.harden =
| N = Me.size 
| Ys = N x 0
| I = 0
| till Me.end
  | Ys.I <= pop Me
  | I !+ 1
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

list.take N =
| Ys = N x 0
| I = 0
| while I < N
  | Ys.I <= Me^pop
  | I !+ 1
| Ys

hard_list.take N =
| Ys = N x 0
| I = 0
| while I < N
  | Ys.I <= Me.I
  | I !+ 1
| Ys

list.drop N =
| while N > 0
  | Me^pop
  | N !- 1
| Me

hard_list.drop S =
| N = Me.size
| Ys = N-S x 0
| I = 0
| while S < N
  | Ys.I <= Me.S
  | I !+ 1
  | S !+ 1
| Ys

list.last = Me.(Me.size-1)
list.suf X = [@Me X]
list.lead = Me.take{Me.size-1}

list.infix Item =
| I = 0
| N = Me.size*2-1
| N = if N < 0 then 0 else N
| Ys = N x 0
| while I < N
  | Ys.I <= if I%2 then Item else Me.(I/2)
  | I !+ 1
| Ys

list.locate F =
| I = 0
| till Me.end
  | when F Me^pop: leave I
  | I !+ 1
| Void

hard_list.locate F =
| N = Me.size
| I = 0
| while I < N
  | when F Me.I: leave I
  | I !+ 1
| Void

list.find F =
| N = Me.size
| I = 0
| till Me.end
  | X = Me^pop
  | when F X: leave X
  | I !+ 1
| Void

hard_list.find F =
| N = Me.size
| I = 0
| while I < N
  | X = Me.I
  | when F X: leave X
  | I !+ 1
| Void

text.chars =
| N = Me.size
| I = 0
| R = N x 0
| while I < N
  | R.I <= Me.I
  | I! + 1
| R

list.groupBy N =
| MeSize = Me.size
| YSize = (MeSize+N-1)/N
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
| when Y.size <> 0: push Y.reverse Ys
| Ys.reverse

bad @Xs =
| log (map X [bad @Xs] X.as_text).infix{` `}.unchars
| halt

say @Xs =
| log (map X Xs X.as_text).infix{` `}.unchars
| Void

// hashtable
data table buckets
table Size = new_table: Size x Void
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


export non say bad no have table
