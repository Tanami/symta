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

add_lexeme Dst Pattern Type =
| when Pattern end
  | Dst{type Type}
  | leave add_lexeme Void
| [Cs@Next] = Pattern
| Cs^| [`&` Cs] => Next != \(@Cs $@Next)
| when text? Cs | Cs != Cs.chars
| Cs = if list? Cs then Cs else [Cs]
| Cs each: C =>
  | Kleene = 0
  | C^| [`@` X] => | Kleene != 1
                   | C != X
  | T = if Kleene then Dst else table 256
  | when no Dst{C}: Dst{C T}
  | add_lexeme T Next Type

export newInput
