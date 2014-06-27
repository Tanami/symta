not X = if X then 0 else 1

list.Xs reverse =
| N = Xs size
| I = 0
| Ys = N x 0
| while I < N
  | N! - 1
  | Ys{I Xs{N}}
  | Ys{N Xs{I}}
  | I! + 1
| Ys

list.Xs map F =
| N = Xs size
| I = 0
| Ys = N x 0
| while I < N
  | Ys{I Xs{I}^F}
  | I! + 1
| Ys

int.N `{}` F =
| I = 0
| Ys = N x 0
| while I < N
  | Ys{I F.I}
  | I! + 1
| Ys

list.Xs sum =
| S = 0
| I = 0
| N = Xs size
| while I < N
  | S !+ Xs{I}
  | I !+ 1
| S

list.Xs join =
| Size = Xs.map{X=>X.size}.sum
| Rs = Size x 0
| I = 0
| Xs map: Ys => Ys map: Y => | Rs{I Y}
                             | I !+ 1
| Rs

list.Xs infix Item =
| I = 0
| N = Xs.size*2-1
| N = if N < 0 then 0 else N
| Ys = N x 0
| while I < N
  | Ys{I (if I%2 then Item else Xs{I/2})}
  | I !+ 1
| Ys

bad @Xs =
| log [bad @Xs].map{&text}.infix{` `}.join_text
| halt

say @Xs =
| log Xs.map{&text}.infix{` `}.join_text
| Void

export not say bad
