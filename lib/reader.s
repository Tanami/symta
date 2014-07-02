use prelude

GError = Msg => log Msg
GInput = Void
GOutput = Void
GTable = Void
GSpecs = Void
GSymbolSource = Void
Newline = '\n'

headed&0 H [X@Xs] = H >< X

data reader_input chars origin row col off last len
newInput Text Origin = new_reader_input Text.chars Origin 0 0 0 Void Text.size
reader_input.O `{}` K = O.chars.K
reader_input.O peek = when O.off < O.len: O.(O.off)
reader_input.O next =
| when O.off < O.len
  | O.last != O.(O.off)
  | O.col !+ 1
  | O.off !+ 1
| when O.last >< Newline
  | O.col != 0
  | O.row !+ 1
| O.last
reader_input.O src = [O.row O.col O.origin]
reader_input.O error Msg = bad reader error at O.src Msg

data token symbol value src
token? O = O^tag_of >< token

add_lexeme Dst Pattern Type =
| when Pattern end
  | Dst.type != Type
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
  | when no Dst.C: Dst.C != T
  | add_lexeme T Next Type

init_tokenizer =
| unless no GTable: leave init_tokenizer Void
| Digit = "0123456789"
| HeadChar = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_?"
| TailChar = "[HeadChar][Digit]"
| Ls = \(`+` `-` `*` `/` `%` `^` `.` `->` `~` `|` `;` `,` `:` `=` `=>` `++` `--` `**` `..`
         `><` `<>` `<` `>` `<<` `>>`
         `\\` `$` `@` `&` `!`
         (() end)
         `)` (`(` $(R O => [`()` (read_list R O ')')]))
         `]` (`[` $(R O => [`[]` (read_list R O ']')]))
         `}` (`{` $(R O => [`{}` (read_list R O '}')]))
         (`'` $(R Cs => [text [`\\` @(read_string R 0 `'`)]]))
         (`"` $(R Cs => [text [`\`` @(read_string R 0 '`')]]))
         ($'`' $(R Cs => [symbol (read_string R 0 '`').0]))
         (`//` $&read_comment)
         (`/*` $&read_multi_comment)
         ((&(` ` `\n`)) $(R Cs => read_token R 1))
         ((`#` &`0123456789ABCDEFabcdef`) hex)
         ((&$Digit) integer)
         (($HeadChar @$TailChar) symbol)
         )
| Ss = \((`if` `if`) (`then` `then`) (`else` `else`) (`and` `and`) (`or` `or`) (`Void` `kw`))
| GTable != table 256
| GSpecs != table 256
| Ss each:[A B] => GSpecs.A != B
| Ls each:L =>
  | [Pattern Type] = if list? L then L else [L L]
  | when text? Pattern: Pattern! chars
  | add_lexeme GTable Pattern Type

read_token R LeftSpaced =
| Src = R.src
| Head = R.peek
| Next = R.next
| Cur = Void
| C = Void
| Cs = []
| while 1
  | Cur != Next
  | C != R.peek
  | Next != Next.C
  | when no Next
    | Value = Cs.reverse.unchars
    | Type = GSpecs.Value
    | when no Type: Type != Cur.type
    | when Value >< '-' and LeftSpaced and C <> '\n' and C <> ' ':
      | Type != \negate
    | when Type >< end and C: Type != 0
    | unless Type: R error "unexpected `[Value][C or '']`"
    | when fn? Type
      | Value != Type R Value
      | when token? Value: leave read_token Value
      | Type != Value.0
      | Value != Value.1
    | leave read_token: new_token Type Value Src
  | Cs != [C@Cs]
  | R.next

add_bars Xs =
| Ys = []
| First = 1
| while not Xs.end
  | X = Xs.0
  | Xs != Xs.tail
  | [Row Col Orig] = X.src
  | S = X.symbol
  | when (Col >< 0 or First) and S <> `|` and S <> `then` and S <> `else`:
    | Ys != [(new_token '|' '|' [Row Col-1 Orig]) @Ys]
    | First != 0
| Ys.reverse

tokenize R =
| Ts = []
| while 1
  | Tok = read_token R 0
  | when Tok.symbol >< end: leave tokenize Ts.reverse^add_bars
  | Ts != [Tok@Ts]

read_list R Open Close =
| [Row Col Orig] = R.src
| Xs = []
| while 1
  | X = read_token R 0
  | when X.symbol >< Close: leave read_list Xs.reverse
  | when X.symbol >< end: GError "[Orig]:[Row],[Col]: unclosed `[Open]`"
  | Xs != [X@Xs]

str_empty? X = bad fixme

str_merge Left Middle Right =
| Left = if str_empty? Left then [Middle] else [Left Middle]
| if Right.size >< 1 and Right.1.size >< 0 then Left else [@Left @Right]

read_string R Incut End = /*
| L = []
| while 1
  | C = R.peek
  | unless C >< Incut: R.next
  | case C
     '\\' | case R.next
              'n' | L != ['\n' @L]
              't' | L != ['\t' @L]
              '\\' | L != ['\\' @L]
              C><(Incut or End) | L != [C@L]
              0 | R.error{'EOF in string'}
              Else | R.error{"Invalid escape code: [else]"}
     &End | leave read_string [L.reverse.unchars]
     &Incut | L != L.reverse.unchars
            | M = read_token{R 0}.value
            | E = read_string R Incut End
            | leave read_string: str_merge L M E
     [] | R.error{'EOF in string'}
     Else | L != [C@L]
*/

read_comment R Cs =

read_multi_comment R Cs =

export newInput
