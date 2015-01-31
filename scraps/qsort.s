partition I N Xs =
| K = N-1
| J = K
| M = Xs.K
| while I < J
  | X = Xs.I
  | if X < M
    then | !I+1
    else | !J-1
         | swap Xs.I Xs.J
| swap Xs.I Xs.K
| I

qsort_asc I E Xs = when E-I > 1
| M = partition I E Xs
| say [I E]
| qsort_asc I M Xs
| qsort_asc M+1 E Xs

list.sort =
| Xs = $shuffle
| qsort_asc 0 $size Xs
| Xs



// following qsort despite being inefficient was good enough for some time

sort_asc $[] [H@Zs] =
| Xs = []
| Ys = []
| for Z Zs: if Z < H then push Z Xs else push Z Ys
| [@Xs^sort_asc H @Ys^sort_asc]

list.sort @As =
| F = No
| case As
  [A] | F <= A
  [] | leave: sort_asc $shuffle
  Else | bad "list.sort: invalid number of arguments"
| h $[] [H@Zs] =
  | Xs = []
  | Ys = []
  | for Z Zs: if F Z H then push Z Xs else push Z Ys
  | [@Xs^h H @Ys^h]
| h $shuffle