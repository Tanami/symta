not X = if X then 0 else 1
non F = X => if F X then 0 else 1
no X = Void >< X
have X = Void <> X

text.O end = 1
void.O end = 1
void.O `.` K = Void
void.O find F = Void
void.O locate F = Void

int? O = O^tag_of >< int
fn? O = O^tag_of >< fn
list? O = O^tag_of >< list
text? O = O^tag_of >< text

int.N `.` F =
| I = 0
| Ys = N x 0
| while I < N
  | Ys.I <= F I
  | I! + 1
| Ys

hard_list.A `><` B = named `><`
| unless B^list?: leave `><` 0
| N = A.size
| unless N >< B.size: leave `><` 0
| I = 0
| while I < N
  | unless A.I >< B.I: leave `><` 0
  | I !+ 1
| 1

list.As `<>` Bs = if As >< Bs then 0 else 1

hard_list.Xs reverse =
| N = Xs.size
| I = 0
| Ys = N x 0
| while I < N
  | N! - 1
  | Ys.I <= Xs.N
  | Ys.N <= Xs.I
  | I! + 1
| Ys

hard_list.Xs map F =
| N = Xs.size
| I = 0
| Ys = N x 0
| while I < N
  | Ys.I <= Xs.I^F
  | I! + 1
| Ys

hard_list.Xs each F =
| N = Xs.size
| I = 0
| while I < N
  | F Xs.I
  | I! + 1
| Void

hard_list.Xs sum =
| S = 0
| I = 0
| N = Xs.size
| while I < N
  | S !+ Xs.I
  | I !+ 1
| S

hard_list.Xs count F =
| S = 0
| I = 0
| N = Xs.size
| while I < N
  | when F Xs.I: S !+ 1
  | I !+ 1
| S

hard_list.Xs countNot F =
| S = 0
| I = 0
| N = Xs.size
| while I < N
  | unless F Xs.I: S !+ 1
  | I !+ 1
| S

hard_list.Xs keep F =
| N = Xs count F
| Ys = N x 0
| I = 0
| J = 0
| while J < N
  | X = Xs.I
  | when F X
    | Ys.J <= X
    | J !+ 1
  | I !+ 1
| Ys

hard_list.Xs skip F =
| N = Xs countNot F
| Ys = N x 0
| I = 0
| J = 0
| while J < N
  | X = Xs.I
  | unless F X
    | Ys.J <= X
    | J !+ 1
  | I !+ 1
| Ys

list.Xs join =
| Size = Xs.map{X=>X.size}.sum
| Rs = Size x 0
| I = 0
| Xs map: Ys => Ys map: Y => | Rs.I <= Y
                             | I !+ 1
| Rs

list.Xs split F =
| Ys = []
| P = Xs.locate{F}
| while have P
  | Ys <= [Xs.take{P}@Ys]
  | Xs <= Xs.drop{P+1}
  | P <= Xs.locate{F}
| [Xs@Ys].reverse

text.Xs split F = Xs.chars.split{F}.map{X=>X.unchars}

hard_list.Xs take N =
| Ys = N x 0
| I = 0
| while I < N
  | Ys.I <= Xs.I
  | I !+ 1
| Ys

hard_list.Xs drop S =
| N = Xs.size
| Ys = N-S x 0
| I = 0
| while S < N
  | Ys.I <= Xs.S
  | I !+ 1
  | S !+ 1
| Ys

list.Xs last = Xs.(Xs.size-1)
list.Xs suf X = [@Xs X]
list.Xs lead = Xs take Xs.size-1

list.Xs infix Item =
| I = 0
| N = Xs.size*2-1
| N = if N < 0 then 0 else N
| Ys = N x 0
| while I < N
  | Ys.I <= if I%2 then Item else Xs.(I/2)
  | I !+ 1
| Ys

hard_list.Xs locate F = named locate
| N = Xs size
| I = 0
| while I < N
  | when F Xs.I: leave locate I
  | I !+ 1
| Void

hard_list.Xs find F = named find
| N = Xs size
| I = 0
| while I < N
  | X = Xs.I
  | when F X: leave find X
  | I !+ 1
| Void

text.T chars =
| N = T size
| I = 0
| R = N x 0
| while I < N
  | R.I <= T.I
  | I! + 1
| R

bad @Xs =
| log [bad @Xs].map{&text}.infix{` `}.unchars
| halt

say @Xs =
| log Xs.map{&text}.infix{` `}.unchars
| Void

// hashtable
data table buckets
table Size = new_table: Size x Void
table.T `.` K =
| Bs = T.buckets
| H = K.hash%Bs.size
| Bs.H.find{X => X.0><K}.1
table.T `!` K V =
| Bs = T.buckets
| H = K.hash%Bs.size
| Xs = Bs.H
| if no Xs then Bs.H <= [[K V]]
  else | Old = Xs.find{X => X.0><K}
       | if no Old then Bs.H <= [[K V]@Xs]
         else Old.1 <= V
| T

export not non say bad no have table int? fn? list? text?
