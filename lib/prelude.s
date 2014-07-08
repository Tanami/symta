not X = if X then 0 else 1
non F = X => if F X then 0 else 1
no X = Void >< X
have X = Void <> X

text.end O = 1
void.end O = 1
void.`.` O K = Void
void.find O F = Void
void.locate O F = Void

int? O = O^tag_of >< int
fn? O = O^tag_of >< fn
list? O = O^tag_of >< list
text? O = O^tag_of >< text

int.`.` N F =
| I = 0
| Ys = N x 0
| while I < N
  | Ys.I <= F I
  | I! + 1
| Ys

hard_list.`><` A B = named `><`
| unless B^list?: leave `><` 0
| N = A.size
| unless N >< B.size: leave `><` 0
| I = 0
| while I < N
  | unless A.I >< B.I: leave `><` 0
  | I !+ 1
| 1

list.`<>` As Bs = if As >< Bs then 0 else 1

hard_list.reverse Xs =
| N = Xs.size
| I = 0
| Ys = N x 0
| while I < N
  | N! - 1
  | Ys.I <= Xs.N
  | Ys.N <= Xs.I
  | I! + 1
| Ys

hard_list.map Xs F =
| N = Xs.size
| I = 0
| Ys = N x 0
| while I < N
  | Ys.I <= Xs.I^F
  | I! + 1
| Ys

hard_list.each Xs F =
| N = Xs.size
| I = 0
| while I < N
  | F Xs.I
  | I! + 1
| Void

hard_list.sum Xs =
| S = 0
| I = 0
| N = Xs.size
| while I < N
  | S !+ Xs.I
  | I !+ 1
| S

hard_list.count Xs F =
| S = 0
| I = 0
| N = Xs.size
| while I < N
  | when F Xs.I: S !+ 1
  | I !+ 1
| S

hard_list.countNot Xs F =
| S = 0
| I = 0
| N = Xs.size
| while I < N
  | unless F Xs.I: S !+ 1
  | I !+ 1
| S

hard_list.keep Xs F =
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

hard_list.skip Xs F =
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

list.join Xs =
| Size = Xs.map{X=>X.size}.sum
| Rs = Size x 0
| I = 0
| Xs map: Ys => Ys map: Y => | Rs.I <= Y
                             | I !+ 1
| Rs

list.split Xs F =
| Ys = []
| P = Xs.locate{F}
| while have P
  | Ys <= [Xs.take{P}@Ys]
  | Xs <= Xs.drop{P+1}
  | P <= Xs.locate{F}
| [Xs@Ys].reverse

text.split Xs F = Xs.chars.split{F}.map{X=>X.unchars}

hard_list.take Xs N =
| Ys = N x 0
| I = 0
| while I < N
  | Ys.I <= Xs.I
  | I !+ 1
| Ys

hard_list.drop Xs S =
| N = Xs.size
| Ys = N-S x 0
| I = 0
| while S < N
  | Ys.I <= Xs.S
  | I !+ 1
  | S !+ 1
| Ys

list.last Xs = Xs.(Xs.size-1)
list.suf Xs X = [@Xs X]
list.lead Xs = Xs take Xs.size-1

list.infix Xs Item =
| I = 0
| N = Xs.size*2-1
| N = if N < 0 then 0 else N
| Ys = N x 0
| while I < N
  | Ys.I <= if I%2 then Item else Xs.(I/2)
  | I !+ 1
| Ys

hard_list.locate Xs F = named locate
| N = Xs size
| I = 0
| while I < N
  | when F Xs.I: leave locate I
  | I !+ 1
| Void

hard_list.find Xs F = named find
| N = Xs size
| I = 0
| while I < N
  | X = Xs.I
  | when F X: leave find X
  | I !+ 1
| Void

text.chars T =
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
table.`.` T K =
| Bs = T.buckets
| H = K.hash%Bs.size
| Bs.H.find{X => X.0><K}.1
table.`!` T K V =
| Bs = T.buckets
| H = K.hash%Bs.size
| Xs = Bs.H
| if no Xs then Bs.H <= [[K V]]
  else | Old = Xs.find{X => X.0><K}
       | if no Old then Bs.H <= [[K V]@Xs]
         else Old.1 <= V
| T

export not non say bad no have table int? fn? list? text?
