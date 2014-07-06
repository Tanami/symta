use prelude

GTable = Void
GError = Msg => log Msg
GInput = Void
GOutput = Void
GSpecs = Void
Newline = '\n'

headed&0 H [X@Xs] = H >< X

data reader_input chars origin row col off last len
newInput Text Origin = new_reader_input Text.chars Origin 0 0 0 Void Text.size
reader_input.O `{}` K = O.chars.K
reader_input.O peek = when O.off < O.len: O.chars.(O.off)
reader_input.O next =
| when O.off < O.len
  | O.last != O.chars.(O.off)
  | O.col !+ 1
  | O.off !+ 1
  | when O.last >< Newline
    | O.col != 0
    | O.row !+ 1
  | O.last
reader_input.O src = [O.row O.col O.origin]
reader_input.O error Msg = bad "at [O.src]: [Msg]"

data token symbol value src parsed
token? O = O^tag_of >< token
token_is What O = token? O and O.symbol >< What

//FIXME: optimize memory usage
add_lexeme Dst Pattern Type =
| when Pattern end
  | Dst.'type' != Type
  | leave Void
| [Cs@Next] = Pattern
| Kleene = 0
| on Cs [`&` X] | Cs != X
                | Next != \(@$Cs $@Next)
        [`@` X] | Cs != X
                | Kleene != 1
| when text? Cs | Cs != Cs.chars
| Cs = if list? Cs then Cs else [Cs]
| Cs each: C =>
  | T = Dst.C
  | when no T: 
    | T != if Kleene then Dst else table 256
    | Dst.C != T
  | add_lexeme T Next Type

init_tokenizer =
| when have GTable: leave Void
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
| Ss = \((`if` `if`) (`then` `then`) (`else` `else`) (`and` `and`) (`or` `or`) (`Void` `void`))
| GTable != table 256
| GSpecs != table 256
| Ss each:[A B] => GSpecs.A != B
| Ls each:L =>
  | [Pattern Type] = if list? L then L else [L L]
  | when text? Pattern: Pattern! chars
  | add_lexeme GTable Pattern Type

init_tokenizer

read_token R LeftSpaced =
| Src = R.src
| Head = R.peek
| Next = GTable
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
    | when no Type: Type != Cur.'type'
    | when Value >< '-' and LeftSpaced and C <> '\n' and C <> ' ':
      | Type != \negate
    | when Type >< end and have C: Type != 0
    | unless Type: R error "unexpected `[Value][C or '']`"
    | when fn? Type
      | Value != Type R Value
      | when token? Value: leave Value
      | Type != Value.0
      | Value != Value.1
    | leave: new_token Type Value Src 0
  | push C Cs
  | R.next

add_bars Xs =
| Ys = []
| First = 1
| while not Xs.end
  | X = pop Xs
  | [Row Col Orig] = X.src
  | S = X.symbol
  | when (Col >< 0 or First) and S <> `|` and S <> `then` and S <> `else`:
    | push (new_token '|' '|' [Row Col-1 Orig] 0) Ys 
    | First != 0
  | push X Ys
| Ys.reverse

tokenize R =
| Ts = []
| while 1
  | Tok = read_token R 0
  | when Tok^token_is{end}: leave Ts.reverse^add_bars
  | push Tok Ts

read_list R Open Close =
| [Row Col Orig] = R.src
| Xs = []
| while 1
  | X = read_token R 0
  | when X^token_is{Close}: leave Xs.reverse
  | when X^token_is{end}: GError "[Orig]:[Row],[Col]: unclosed `[Open]`"
  | Xs != [X@Xs]

str_empty? X = bad fixme

str_merge Left Middle Right =
| Left = if str_empty? Left then [Middle] else [Left Middle]
| if Right.size >< 1 and Right.1.size >< 0 then Left else [@Left @Right]

read_string R Incut End =
| L = []
| while 1
  | C = R.peek
  | unless C >< Incut: R.next
  | on C
       `\\` | on R.next
                 `n` | L != ['\n' @L]
                 `t` | L != ['\t' @L]
                 `\\` | L != ['\\' @L]
                 C>(in &Incut &End) | L != [C@L]
                 Void | R.error{'EOF in string'}
                 Else | R.error{"Invalid escape code: [Else]"}
       &End | leave [L.reverse.unchars]
       &Incut | L != L.reverse.unchars
              | M = read_token{R 0}.value
              | E = read_string R Incut End
              | leave: str_merge L M E
       [] | R.error{'EOF in string'}
       Else | L != [C@L]

comment_char? C = C and not C >< '\n'
read_comment R Cs =
| while comment_char? R.next
| read_token R 0

read_multi_comment R Cs =
| O = 1
| while O > 0
  | on [R.next R.next]
       [X Void] | R.error{"`/*`: missing `*/`"}
       [`*` `/`] | O!-1
       [`/` `*`] | O!+1
  | R.next
| read_token R 0

parser_error Cause Tok =
| [Row Col Orig] = Tok.src
| bad "at [Orig]:[Row],[Col]: [Cause] [Tok.value or 'eof']"

expect What Head =
| Tok = GInput.0
| unless Tok^token_is{What}: parser_error "expected [What]; got" (Head or Tok)
| pop GInput

parse_if Sym =
| Head = parse_xs
| expect `then` 0
| Then = parse_xs
| expect `else` 0
| Else = parse_xs
| [Sym Head Then Else]

parse_bar H =
| C = H.src.1
| Zs = []
| while not GInput.end
  | Ys = []
  | while not GInput.end and GInput.0.src.1 > C: push GInput^pop Ys
  | push Ys.reverse^parse Zs
  | when GInput.end: leave [H @Zs.reverse]
  | X = GInput.0
  | unless X^token_is{'|'} and X.src.1 >< C: leave [H @Zs.reverse]
  | pop GInput

parse_integer T =
| N = T.size
| I = 0
| Sign = if T.I >< '-'
         then | I !+ 1
              | -1
         else 1
| R = 0
| Base = '0'.code
| while I < N
  | R != R*10 + (T.I.code - Base)
  | I !+ 1
| R*Sign

parse_float T = bad 'parse_float isnt implemented'

parse_negate H =
| A = parse_mul or leave 0
| unless A^token_is{integer} or A^token_is{float}: leave [H A]
| V = "[H.value][A.value]"
| new_token A.symbol V H.src [V^parse_integer]

parse_term =
| when GInput.end: leave 0
| Tok = pop GInput
| when Tok.parsed: parser_error "already parsed token" Tok
| V = Tok.value
| P = on Tok.symbol
         (in escape symbol text) | leave Tok
         integer | parse_integer V
         void | Void
         `()` | parse V
         `[]` | [(new_token symbol `[]` Tok.src 0) @V^parse]
         `|` | leave Tok^parse_bar
         `if` | leave Tok^parse_if
         `-` | leave Tok^parse_negate
         Else | push Tok GInput
              | leave 0
| Tok.parsed != P
| Tok

delim? X = X^token? and on X.symbol (in `:` `=` `=>` `,` `if` `then` `else`) 1

parse_op Ops =
| when GInput.end: leave 0
| V = GInput.0.symbol
| when no Ops.find{O => O><V}: leave 0
| pop GInput

binary_loop Ops Down E =
| O = parse_op Ops or leave E
| when O^token_is{`{}`}
  | As = parse O.value
  | As != if As.find{&delim?} then [As] else As //allows Xs.map{X=>...}
  | O.parsed != [`{}`]
  | leave: binary_loop Ops Down [O E @As]
| B = &Down or parser_error "no right operand for" o
| unless O^token_is{'.'} and E^token_is{integer} and B^token_is{integer}:
  | leave: binary_loop Ops Down [O E B]
| V = "[E.value].[B.value]"
| F = new_token float V E.src [V^parse_float]
| leave: binary_loop Ops Down F

parse_binary Down Ops = binary_loop Ops Down: &Down or leave 0
suffix_loop E = suffix_loop [(parse_op [`!`] or leave E) E]
parse_suffix = suffix_loop: parse_binary &parse_term [`.` `^` `->` `~` `{}`] or leave 0
parse_prefix =
| O = parse_op [negate `\\` `$` `@` `&`] or leave (parse_suffix)
| when O^token_is{negate}: leave O^parse_negate
| [O (parse_prefix or parser_error "no operand for" O)]
parse_mul = parse_binary &parse_prefix [`*` `/` `%`]
parse_add = parse_binary &parse_mul [`+` `-`]
parse_dots = parse_binary &parse_add [`..`]
parse_bool = parse_binary &parse_dots [`><` `<>` `<` `>` `<<` `>>`]

parse_logic =
| O = parse_op [`and` `or`] or leave (parse_bool)
| GOutput != GOutput.reverse
| P = GInput.locate{&delim?}
| Tok = have P and GInput{P}
| when not P or have [`if` `then` `else` ].locate{X = Tok^token_is{X}}:
  | GOutput != [(parse_xs) GOutput O]
  | leave 0
| R = GInput.drop{P}
| GInput != GInput.take{P}
| GOutput != if Tok^token_is{`:`}
             then [[O GOutput.tail R^parse] GOutput.head]
             else [[O GOutput R^parse]]
| Void

parse_delim =
| O = parse_op [`:` `=` `=>` `,`] or leave (parse_logic)
| Pref = if GOutput.size > 0 then GOutput.reverse else [void]
| unless O^token_is{`,`}
  | GOutput != [(parse_xs) Pref O]
  | leave Void
| Pref = Pref.map{X => new_token escape X^parse_strip O.src 0}
| R = GInput.split{X => X^token_is{`,`}}
| R = R.reverse.map{X => [@X (new_token `:` `:` O.src 0)]}
| GInput != [@R Pref].join
| GOutput != (parse_xs).reverse
| Void

parse_semicolon =
| P = GInput.locate{X => X^token_is{`|`} or X^token_is{`;`}}
| M = when have P: GInput.P
| when no P or M^token_is{`|`}: leave 0
| L = parse GInput.take{P}
| R = parse GInput.drop{P+1}
| GInput != []
| GOutput != if R.0^token_is{`};`} then [@R.tail.reverse L M] else [R L M]
| Void

parse_xs =
| shade (GOutput [])
  | parse_semicolon
  | named loop // FIXME: implement unwind_protect
    | while 1
      | X = parse_delim or leave loop GOutput.reverse
      | when have X: push X GOutput

parse Input =
| shade (GInput Input)
  | Xs = parse_xs
  | unless GInput.end: parser_error "unexpected" GInput.0
  | Xs

parse_strip X =
| if token? X
  then | P = X.parsed
       | R = if P then parse_strip P.0 else X.value
       //| when text? R: R != new_meta R X.src
       | leave R
  else if list? X
  then | for V X
         | when (on V [U@Us] | X^token_is{`!`}) and not (on X [Z@Zs] X^token_is{`!`}):
           | leave [`!!`] 
       | X map:V => parse_strip V
  else X

read Chars =
| R = parse_strip: parse: tokenize: newInput Chars test
| R != R.0
| on R [X S] S
       R R

normalize Expr = on Expr [`|` @As] Expr
                         X [`|` X]

export read parse_strip parse tokenize newInput
