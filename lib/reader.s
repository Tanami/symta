use prelude

GError = Void
GInput = Void
GOutput = Void
GTable = Void
GSpecs = Void
GSymbolSource = Void
Newline = '\n'

headed&0 H [X@Xs] = H >< X

data reader_input chars origin row col off last len
newInput Text Origin = new_reader_input Text.chars Origin 0 0 0 Void Text.size
reader_input.O `{}` K = (O.chars){K}
reader_input.O peek = when O.off < O.len: O{O.off}
reader_input.O next =
| when O.off < O.len
  | O.last != O{O.off}
  | O.col !+ 1
  | O.off !+ 1
| when O.last >< Newline
  | O.col != 0
  | O.row !+ 1
| O.last
reader_input.O src = [O.row O.col O.origin]
reader_input.O error Msg = bad reader error at O.src Msg

data token symbol value src

/*
add_lexeme Dst Pattern Type
| when no Pattern
  | Dst{type Type}
  | leave add_lexeme Void
| [Cs@Next] = Pattern
| Cs^| [`&` Cs] => Next != \(@Cs $@Next)
| when (headed '+ Next) (! next := `(,cs * ,@(cdr next)))
| when (stringp cs) (! cs := coerce cs 'list)
| e c (if (listp cs) cs (list cs))
   (! unless (gethash c dst)
      (! gethash c dst := if (headed '* next) dst (make-hash-table))
    ! next = (if (headed '* next) (cdr next) next)
    ! /add-lexeme (gethash c dst) next type))
*/

export newInput
