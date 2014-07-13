data reader_input chars origin row col off last len
newInput Text Origin = new_reader_input Text.chars Origin 0 0 0 Void Text.size
reader_input.`{}` K = Me.chars.K
reader_input.peek = when Me.off < Me.len: Me.chars.(Me.off)
reader_input.next =
| when Me.off < Me.len
  | Me.last <= Me.chars.(Me.off)
  | Me.col !+ 1
  | Me.off !+ 1
  | when Me.last >< '\n'
    | Me.col <= 0
    | Me.row !+ 1
  | Me.last
reader_input.src = [Me.row Me.col Me.origin]
reader_input.error Msg = bad "at [Me.src]: [Msg]"

data token symbol value src parsed
token_is What O = O.is_token and O.symbol >< What

//FIXME: optimize memory usage
add_lexeme Dst Pattern Type =
| when Pattern.end
  | Dst.'type' <= Type
  | leave Void
| [Cs@Next] = Pattern
| Kleene = 0
| on Cs [`&` X] | Cs <= X
                | Next <= \(@$Cs $@Next)
        [`@` X] | Cs <= X
                | Kleene <= 1
| when Cs.is_text | Cs <= Cs.chars
| Cs = if Cs.is_list then Cs else [Cs]
| for C Cs
  | T = Dst.C
  | when no T: 
    | T <= if Kleene then Dst else table 256
    | Dst.C <= T
  | add_lexeme T Next Type
