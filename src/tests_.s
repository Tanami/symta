/*
Ys = [4 5 6]
yoba =
| Xs = [1 2 3]
| times I 100000000: Ys.1 <= Xs
| Ys
yoba
*/

/*Ys = [4 5 6]
yoba =
| Xs = [1 2 3]
| times I 100000000: Ys <= Xs
| Ys
yoba*/

tsay Msg =
| say "  [Msg]"

failed = bad "  test failed"

test_int =
| say 'testing int...'
| A = 123
| B = 456

| tsay "[A].tag >< int and [B].tag >< int"
| unless A.tag >< int and B.tag >< int: failed

| tsay "A >< A"
| unless A >< A: failed

| tsay "A <> B"
| unless A <> B:failed

| tsay "A << A"
| unless A << A: failed

| tsay "A << B"
| unless A << B: failed

| tsay "A >> A"
| unless A >> A: failed

| tsay "B >> A"
| unless B >> A: failed

| tsay "B > A"
| unless B > A: failed

| tsay "A < B"
| unless A < B: failed

| tsay "A+B >< 579"
| unless A+B >< 579: failed

| tsay "A+B >< -333"
| unless A-B >< -333: failed

| tsay "A*B >< 56088"
| unless A*B >< 56088: failed

| tsay "A*B/A >< B"
| unless A*B/A >< B: failed

| tsay "'x'.code.char >< 'x'"
| unless 'x'.code.char >< x: failed

| tsay "B.mask{A} >< 72"
| unless B.mask{A} >< 72: failed
| 1

test_list =
| say 'testing list...'
| Ys = Void
| Validate = (Xs => Xs.0 >< 1 and Xs.1 >< 2 and Xs.2 >< 3)

| tsay '[1 2 3].tag >< list'
| unless [1 2 3].tag >< list: failed

| tsay '[456 1 2 3].tail'
| Ys <= [456 1 2 3].tail
| unless Validate Ys: failed

| tsay '[@[1 2] 3]'
| Ys <= [@[1 2] 3]
| unless Validate Ys: failed

| tsay '[1 @[2 3]]'
| Ys <= [1 @[2 3]]
| unless Validate Ys: failed

| tsay '[1 @[2] 3]]'
| Ys <= [1 @[2] 3]
| unless Validate Ys: failed

| tsay '(A B C => [A B C]) 1 2 3'
| Ys <= (A B C => [A B C]) 1 2 3
| unless Validate Ys: failed

| tsay '(X@Xs => [@Xs X]) 3 1 2'
| Ys <= (X@Xs => [@Xs X]) 3 1 2
| unless Validate Ys: failed

| tsay 'case [x [y 4] 2 [1 3] z] [x [y 4] B [A C] z] [A B C]'
| Ys <= case [x [y 4] 2 [1 3] z] [x [y 4] B [A C] z] [A B C]
| unless Validate Ys: failed

| tsay '(Xs => (Xs.1 <= [1 2 3]; 0)) Xs'
| Xs = [a b c]
| (Xs => (Xs.1 <= [1 2 3]; 0)) Xs
| Ys <= Xs.1
| unless Validate Ys: failed

| tsay '[1 2 3]+[1 2 3]-[1 2 3]'
| Ys <= [1 2 3]+[1 2 3]-[1 2 3]
| unless Validate Ys: failed

| tsay '[1 2 3]*2/2'
| Ys <= [1 2 3]*2/2
| unless Validate Ys: failed

| tsay '[3 1 2].sort{A B => A < B}'
| Ys = [3 1 2].sort{A B => A < B}
| unless Validate Ys: failed
| 1


run_tests =
| test_int
| test_list

export run_tests
