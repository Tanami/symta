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
reader_input.O error = bad reader error at O.src

data token symbol value src

export newInput
