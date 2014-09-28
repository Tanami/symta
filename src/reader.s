GTable = Void
GSpecs = Void //kludge to recognize if/then/else
GError = Msg => bad Msg
GInput = Void
GOutput = Void

data text_stream chars origin row col off last len
text_stream.`{}` K = Me.chars.K
text_stream.peek = when Me.off < Me.len: Me.chars.(Me.off)
text_stream.next =
| when Me.off < Me.len
  | Me.last <= Me.chars.(Me.off)
  | Me.col !+ 1
  | Me.off !+ 1
  | when Me.last >< '\n'
    | Me.col <= 0
    | Me.row !+ 1
  | Me.last
text_stream.src = [Me.row Me.col Me.origin]
text_stream.error Msg = bad "at [Me.src]: [Msg]"

makeTextStream Text Origin = new_text_stream Text.list Origin 0 0 0 Void Text.size

data token symbol value src parsed
token_is What O = O.is_token and O.symbol >< What

//FIXME: optimize memory usage
add_lexeme Dst Pattern Type =
| when Pattern.end
  | Dst.'type' <= Type
  | leave Void
| [Cs@Next] = Pattern
| Kleene = 0
| case Cs [`&` X] | Cs <= X
                  | Next <= \(@$Cs $@Next)
          [`@` X] | Cs <= X
                  | Kleene <= 1
| when Cs.is_text: Cs <= Cs.list
| Cs = if Cs.is_list then Cs else [Cs]
| for C Cs
  | T = Dst.C
  | when no T: 
    | T <= if Kleene then Dst else m
    | Dst.C <= T
  | add_lexeme T Next Type

init_tokenizer =
| when got GTable: leave Void
| Digit = "0123456789"
| HexDigit = "0123456789ABCDEF"
| HeadChar = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_?~"
| TailChar = "[HeadChar][Digit]"
| Ls = \(`+` `-` `*` `/` `%` `^` `.` `->` `|` `;` `,` `:` `=` `=>` `<=`
         `++` `--` `**` `..`
         `><` `<>` `<` `>` `<<` `>>`
         `\\` `$` `@` `&` `!`
         (() end)
         `)` (`(` $(R O => [`()` (read_list R O ')')]))
         `]` (`[` $(R O => [`[]` (read_list R O ']')]))
         `}` (`{` $(R O => [`{}` (read_list R O '}')]))
         (`'` $(R Cs => [text [`\\` @(read_string R 0 `'`)]]))
         (`"` $(R Cs => [splice (read_string R '[' '"')]))
         ($'`' $(R Cs => [symbol (read_string R 0 '`').0]))
         (`//` $&read_comment)
         ((`/` `*`) $&read_multi_comment)
         ((&(` ` `\n`)) $(R Cs => read_token R 1))
         ((`#` &$HexDigit) hex)
         ((&$Digit) integer)
         (($HeadChar @$TailChar) symbol)
         )
| Ss = \((`if` `if`) (`then` `then`) (`else` `else`) (`and` `and`) (`or` `or`) (`Void` `void`))
| GTable <= m
| GSpecs <= m
| for [A B] Ss: GSpecs.A <= B
| for L Ls
  | [Pattern Type] = if L.is_list then L else [L L]
  | when Pattern.is_text: Pattern <= Pattern.list
  | add_lexeme GTable Pattern Type

read_token R LeftSpaced =
| Src = R.src
| Head = R.peek
| Next = GTable
| Cur = Void
| C = Void
| Cs = []
| while 1
  | Cur <= Next
  | C <= R.peek
  | Next <= Next.C
  | when no Next
    | Value = Cs.flip.text
    | Type = GSpecs.Value
    | when no Type: Type <= Cur.'type'
    | when Value >< '-' and LeftSpaced and C <> '\n' and C <> ' ':
      | Type <= \negate
    | when Type >< end and got C: Type <= 0
    | less Type: R error "unexpected `[Value][C or '']`"
    | when Type.is_fn
      | Value <= Type R Value
      | when Value.is_token: leave Value
      | Type <= Value.0
      | Value <= Value.1
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
    | First <= 0
  | push X Ys
| Ys.flip

tokenize R =
| Ts = []
| while 1
  | Tok = read_token R 0
  | when Tok^token_is{end}: leave Ts.flip^add_bars
  | push Tok Ts

read_list R Open Close =
| [Row Col Orig] = R.src
| Xs = []
| while 1
  | X = read_token R 0
  | when X^token_is{Close}: leave Xs.flip
  | when X^token_is{end}: GError "[Orig]:[Row],[Col]: unclosed `[Open]`"
  | Xs <= [X@Xs]

str_is_empty X = bad fixme

spliced_string_normalize Xs =
| Ys = Xs.skip{X => '' >< X}
| map Y Ys: if Y.is_text then new_token symbol Y [0 0 none] 0
            else if Y.is_token then Y
            else new_token '()' Y [0 0 none] 0

read_string R Incut End =
| L = []
| while 1
  | C = R.peek
  | less C >< Incut: R.next
  | case C
     `\\` | case R.next
             `n` | L <= ['\n' @L]
             `t` | L <= ['\t' @L]
             `\\` | L <= ['\\' @L]
             C<&Incut+&End | L <= [C@L]
             Void | R.error{'EOF in string'}
             Else | R.error{"Invalid escape code: [Else]"}
     &End | Ys = [L.flip.text]
          | when End >< '"': Ys <= spliced_string_normalize Ys //"
          | leave Ys
     &Incut | L <= L.flip.text
            | M = (read_token R 0).value
            | E = read_string R Incut End
            | leave: spliced_string_normalize [L M @E]
     Void | R.error{'EOF in string'}
     Else | L <= [C@L]

is_comment_char C = got C and C <> '\n'
read_comment R Cs =
| while R.next^is_comment_char:
| read_token R 0

read_multi_comment R Cs =
| O = 1
| while O > 0
  | case [R.next R.peek]
      [X Void] | R.error{"`/*`: missing `*/`"}
      [`*` `/`] | O !- 1
                | R.next
      [`/` `*`] | O !+ 1
                | R.next
| read_token R 0

parser_error Cause Tok =
| [Row Col Orig] = Tok.src
| bad "[Orig]:[Row],[Col]: [Cause] [Tok.value or 'eof']"

expect What Head =
| Tok = GInput.0
| less Tok^token_is{What}: parser_error "expected [What]; got" (Head or Tok)
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
  | push Ys.flip^parse_tokens Zs
  | when GInput.end: leave [H @Zs.flip]
  | X = GInput.0
  | less X^token_is{'|'} and X.src.1 >< C: leave [H @Zs.flip]
  | pop GInput

parse_negate H =
| A = parse_mul or leave 0
| less A^token_is{integer} or A^token_is{hex} or A^token_is{float}: leave [H A]
| new_token A.symbol "-[A.value]" H.src [-A.parsed.0]

parse_term =
| when GInput.end: leave 0
| Tok = pop GInput
| when Tok.parsed: parser_error "already parsed token" Tok
| V = Tok.value
| P = case Tok.symbol
         escape+symbol+text | leave Tok
         splice | [(new_token symbol `"` Tok.src 0) @V^parse_tokens] //"
         integer | V.int{10}
         hex | V.tail.int{16}
         void | Void
         `()` | parse_tokens V
         `[]` | [(new_token symbol `[]` Tok.src 0) @V^parse_tokens]
         `|` | leave Tok^parse_bar
         `if` | leave Tok^parse_if
         `-` | leave Tok^parse_negate
         `,` | new_token symbol `,` Tok.src 0
         Else | push Tok GInput
              | leave 0
| Tok.parsed <= [P]
| Tok

is_delim X = X.is_token and case X.symbol `:`+`=`+`=>`+`<=`+`,`+`if`+`then`+`else` 1

parse_op Ops =
| when GInput.end: leave 0
| V = GInput.0.symbol
| when no Ops.find{O => O><V}: leave 0
| pop GInput

binary_loop Ops Down E =
| O = parse_op Ops or leave E
| when O^token_is{`{}`}
  | As = parse_tokens O.value
  | As <= if got As.find{&is_delim} then [As] else As //allows Xs.map{X=>...}
  | O.parsed <= [`{}`]
  | leave: binary_loop Ops Down [O E @As]
| when O^token_is{`!`}: leave: binary_loop Ops Down [O E]
| B = &Down or parser_error "no right operand for" O
| less O^token_is{'.'} and E^token_is{integer} and B^token_is{integer}:
  | leave: binary_loop Ops Down [O E B]
| V = "[E.value].[B.value]"
| F = new_token float V E.src [V^parse_float]
| leave: binary_loop Ops Down F

parse_binary Down Ops = binary_loop Ops Down: &Down or leave 0
parse_suffix = parse_binary &parse_term [`.` `^` `->` `{}` `!`]
parse_pow = parse_binary &parse_suffix [`**`]
parse_prefix =
| O = parse_op [negate `\\` `$` `@` `&`] or leave (parse_pow)
| when O^token_is{negate}: leave O^parse_negate
| [O (parse_prefix or parser_error "no operand for" O)]
parse_mul = parse_binary &parse_prefix [`*` `/` `%`]
parse_add = parse_binary &parse_mul [`+` `-`]
parse_dots = parse_binary &parse_add [`..`]
parse_bool = parse_binary &parse_dots [`><` `<>` `<` `>` `<<` `>>`]

parse_logic =
| O = parse_op [`and` `or`] or leave (parse_bool)
| GOutput <= GOutput.flip
| P = GInput.locate{&is_delim} //hack LL(1) to speed-up parsing
| Tok = got P and GInput.P
| when no P or got [`if` `then` `else` ].locate{X => Tok^token_is{X}}:
  | GOutput <= [(parse_xs) GOutput O]
  | leave 0
| R = GInput.take{P}
| GInput <= GInput.drop{P}
| GOutput <= if Tok^token_is{`:`}
             then [[O GOutput.tail R^parse_tokens] GOutput.head]
             else [[O GOutput R^parse_tokens]]
| Void

parse_delim =
| O = parse_op [`:` `=` `=>` `<=`] or leave (parse_logic)
| Pref = if GOutput.size > 0 then GOutput.flip else []
| GOutput <= [(parse_xs) Pref O]
| Void

parse_semicolon =
| P = GInput.locate{X => X^token_is{`|`} or X^token_is{`;`}}
| M = when got P: GInput.P
| when no P or M^token_is{`|`}: leave 0
| L = parse_tokens GInput.take{P}
| R = parse_tokens GInput.drop{P+1}
| GInput <= []
| GOutput <= if R.0^token_is{`};`} then [@R.tail.flip L M] else [R L M]
| Void

parse_xs =
| let GOutput []
  | parse_semicolon
  | named loop // FIXME: implement unwind_protect
    | while 1
      | X = parse_delim or leave loop GOutput.flip
      | when got X: push X GOutput

parse_tokens Input =
| let GInput Input
  | Xs = parse_xs
  | less GInput.end: parser_error "unexpected" GInput.0
  | Xs

parse_strip X =
| if X.is_token
  then | P = X.parsed
       | R = if P then parse_strip P.0 else X.value
       | R
  else if X.is_list
  then | less X.size: leave X
       | Head = X.head
       | Meta = when Head.is_token: Head.src
       | when got X.locate{?^token_is{`,`}}: X <= [',' @X.split{?^token_is{`,`}}]
       | Ys = map V X: parse_strip V
       | for V X
         | when case V [U@Us] U^token_is{`!`} and not case X [Z@Zs] Z^token_is{`!`}:
           | leave [`!!` @Ys]
       | when got Meta and Meta.2 <> '<none>': Ys <= new_meta Ys Meta
       | Ys
  else X

text.parse src/'<none>' =
| init_tokenizer
| R = parse_strip: parse_tokens: tokenize: makeTextStream Me Src
| less R.end: R <= R.0
| case R [X S] S
         R R

export
