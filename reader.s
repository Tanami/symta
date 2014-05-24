//f [X@Xs] = [X Xs]
//f [1 2 3]
//map F [X@Xs] = Xs^map{F} grow X^F
//[1 2 3]^map{X => X*X}
sum&0 [X@Xs] = X + sum.Xs

1000000^sum

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
