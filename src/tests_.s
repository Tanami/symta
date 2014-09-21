test_int =
| say 'testing int...'
| A = 123
| B = 456
| unless A.tag >< int and B.tag >< int: bad 'A.tag >< int and B.tag >< int'
| unless A >< A: bad 'A >< A'
| unless A <> B: bad 'A <> B'
| unless A << A: bad 'A << A'
| unless A << B: bad 'A << B'
| unless A >> A: bad 'A >> A'
| unless B >> A: bad 'B >> A'
| unless B > A: bad 'B > A'
| unless A < B: bad 'A < B'
| unless A+B >< 579: bad 'A+B >< 579'
| unless A-B >< -333: bad 'A+B >< -333'
| unless A*B >< 56088: bad 'A*B >< 5608'
| unless A*B/A >< B: bad 'A*B/A >< B'
| unless 'x'.code.char >< x: bad '\'x\'.code.char >< \'x\''
| unless B.mask{A} >< 72: bad 'B.mask{A} >< 72'
| say '  test passed'
| 1

test_list =
| say 'testing list...'
| Ys = Void
| unless [1 2 3].tag >< list: bad '[1 2 3].tag >< list'
| Ys <= [456 1 2 3].tail
| unless Ys.0 >< 1 and Ys.1 >< 2 and Ys.2 >< 3: bad '[456 1 2 3].tail'
| Ys <= [@[1 2] 3]
| unless Ys.0 >< 1 and Ys.1 >< 2 and Ys.2 >< 3: bad '[@[1 2] 3]'
| Ys <= [1 @[2 3]]
| unless Ys.0 >< 1 and Ys.1 >< 2 and Ys.2 >< 3: bad '[1 @[2 3]]'
| Ys <= [1 @[2] 3]
| unless Ys.0 >< 1 and Ys.1 >< 2 and Ys.2 >< 3: bad '[1 @[2] 3]]'
| Ys <= (A B C => [A B C]) 1 2 3
| unless Ys.0 >< 1 and Ys.1 >< 2 and Ys.2 >< 3: bad '(A B C => [A B C]) 1 2 3'
| Ys <= (X@Xs => [@Xs X]) 3 1 2
| unless Ys.0 >< 1 and Ys.1 >< 2 and Ys.2 >< 3: bad '(X@Xs => [@Xs X]) 3 1 2'
| Ys <= case [x [y 4] 2 [1 3] z] [x [y 4] B [A C] z] [A B C]
| unless Ys.0 >< 1 and Ys.1 >< 2 and Ys.2 >< 3:
  | bad 'case [x [y 4] 2 [1 3] z] [x [y 4] B [A C] z] [A B C]'
| Ys = [3 1 2].sort{A B => A < B}
| unless Ys.0 >< 1 and Ys.1 >< 2 and Ys.2 >< 3: bad '[3 1 2].sort{A B => A < B}'
| say '  test passed'
| 1


run_tests =
| test_int
| test_list

export run_tests
