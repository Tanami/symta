//f [X@Xs] = [X Xs]
//f [1 2 3]
//map F [X@Xs] = Xs^map{F} add X^F
//[1 2 3]^map{X => X*X}
//sum&0 [X@Xs] = X + sum.Xs
//100000^sum
//(_quote abc).end

point X Y = | x => X
            | y => Y


P = point 123 456
P.x+P.y

/*
As = array 10 Void
As{1 123}
Data = As add 456
[Data{0} Data{1} Data{2}]
*/

/*

point X Y = | x => X
            | y => Y

//Data = array 10 Void
//Data{1 123}
//[Data{0} Data{1} Data{2}]

void X = Void is X

find:Void F [X@Xs] = if F X then X else find F Xs

hash Size =
  | Data = array Size Void
  | `{}` K => Data{K.hash%Size}^find{X => X{0} is K}{1}
  | `{!}` K V => | H = K.hash%Size
                 | Xs = Data{H}
                 | if void Xs then Data{H [t{K V}]}
                   else | Old = Xs^find{X => X{0} is K}
                        | if void Old then Data{H Xs.add{t{K V}}}
                          else Old{1 V}

*/


/*
GError = Void
GInput = Void
GOutput = Void
GTable = Void
GSpecs = Void
GSymbolSource = Void

headed:0 H [X@Xs] = H is X

(define (map F Xs)
  (_if (end Xs)
       (list)
       (rear (F (head Xs))
             (map F (tail Xs)))))

(define (nth N Xs)
  (if (end Xs)
      (error "nth: Xs index is out of range for a list")
      (if (is N 0)
          (head Xs)
          (nth (- N 1) (tail Xs)))))

(define (newInput Text Origin)
  (let ((Row 0)
        (Col 0)
        (Off 0)
        (Last Void)
        (Len (size Text)))
    (fn (Name)
      (match Name
        (peek (fn (O) (when (< Off Len) (get Off Text))))
        (next (fn (O)
                (when (< Off Len)
                  (set Last (get Off Text))
                  (set Col (+ Col 1))
                  (set Off (+ Off 1)))
                (when (is Last Newline)
                  (set Col 0)
                  (set Row (+ Row 1)))
                Last))
        (src (fn (O) (list Row Col Origin)))
        (last (fn (O) Last))
        (error (fn (O) (GError O "error in text reader")))
        ))))

;;(define In (newInput "abcdefg" "test"))
;;(list (c In next) (c In peek) (c In last))


(define (newToken Symbol Value Src)
  (fn (Name)
    (match Name
      (symbol (fn (O) Symbol))
      (value (fn (O) Value))
      (src (fn (O) Src))
      )))
*/
