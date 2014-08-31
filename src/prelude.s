non F = X => if F X then 0 else 1
no X = Void >< X
have X = Void <> X

_.`<>` B = not Me >< B
_.`<<` B = not B < Me
_.`>` B = B < Me
_.`>>` B = not Me < B

_.is_int = 0
int.is_int = 1

_.is_fn = 0
fn.is_fn = 1

_.is_list = 0
list.is_list = 1

_.is_text = 0
text.is_text = 1

//_.is_hard_list = 0
//list.is_hard_list = 1

text.`<` B =
| unless B.is_text: bad "cant compare string `[Me]` with [B]"
| AS = Me.size
| BS = B.size
| when AS <> BS: leave AS < BS
| times I AS
  | AC = Me.I.code
  | BC = B.I.code
  | when AC <> BC: leave AC < BC
| 0

text.is_upcase =
| times I Me.size
  | C = Me.I.code
  | when C < 'A'.code or 'Z'.code < C: leave 0
  | I !+ 1
| 1

text.is_downcase =
| times I Me.size
  | C = Me.I.code
  | when C < 'a'.code or 'z'.code < C: leave 0
  | I !+ 1
| 1

text.upcase =
| Ys = map Char Me.chars
  | C = Char.code
  | if C < 'a'.code or 'z'.code < C then Char else (C - 'a'.code + 'A'.code).char
| Ys.unchars

text.downcase =
| Ys = map Char Me.chars
  | C = Char.code
  | if C < 'A'.code or 'Z'.code < C then Char else (C - 'A'.code + 'a'.code).char
| Ys.unchars

_.is_keyword = 0
text.is_keyword = not: Me.size and Me.0.is_upcase

int.i = dup I Me: I //iota operator

list.enum = dup I Me.size: [I Me^pop]

int.`.` F = dup I Me: F I

list.`.` K =
| times I K: Me <= Me.tail
| Me.head

list.size =
| S = 0
| till Me.end
  | Me <= Me.tail
  | S !+ 1
| S

list.`><` B =
| unless B.is_list: leave 0
| till Me.end or B.end: unless Me^pop >< B^pop: leave 0
| 1

hard_list.`><` B =
| unless B.is_list: leave 0 //FIXME: cons_list B will be O(n^2) slow
| N = Me.size
| unless N >< B.size: leave 0
| times I N
  | unless Me.I >< B.I: leave 0
  | I !+ 1
| 1

list.reverse =
| N = Me.size
| Ys = dup N
| while N > 0
  | N !- 1
  | Ys.N <= pop Me
| Ys

hard_list.reverse =
| N = Me.size
| dup N
  | N !- 1
  | Me.N

list.map F = dup Me.size: F Me^pop
hard_list.map F = dup I Me.size: F Me.I
text.map F = Me.chars.map{F}

list.each F = till Me.end: F Me^pop
hard_list.each F = times I Me.size: F Me.I

list.sum =
| S = 0
| till Me.end: S !+ Me^pop
| S

hard_list.sum =
| S = 0
| times I Me.size: S !+ Me.I
| S

list.count F =
| C = 0
| till Me.end: when F Me^pop: C !+ 1
| C

hard_list.count F =
| S = 0
| I = 0
| times I Me.size: when F Me.I: S !+ 1
| S

list.keep F =
| Ys = []
| for X Me: when F X: Ys <= [X@Ys]
| Ys.reverse

list.skip F =
| Ys = []
| for X Me: unless F X: Ys <= [X@Ys]
| Ys.reverse

list.join =
| Size = Me.map{X=>X.size}.sum
| Rs = dup Size
| I = 0
| for Ys Me: for Y Ys | Rs.I <= Y
                      | I !+ 1
| Rs

_list_.harden = Me

list.harden =
| N = Me.size
| Ys = dup N
| times I N: Ys.I <= pop Me
| Ys

text.harden = Me.chars

list.apply F = Me.harden.apply{F}

list.unchars = Me.harden.unchars

list.split F =
| Ys = []
| P = Me.locate{F}
| while have P
  | Ys <= [Me.take{P}@Ys]
  | Me <= Me.drop{P+1}
  | P <= Me.locate{F}
| [Me@Ys].reverse

text.split F = Me.chars.split{F}.map{X=>X.unchars}

list.take N = dup N: Me^pop
hard_list.take N = dup I N: Me.I

list.drop N =
| times I N Me^pop
| Me

hard_list.drop S =
| dup Me.size-S
  | R = Me.S
  | S !+ 1
  | R

text.drop S = Me.chars.drop{S}.unchars
text.take S = Me.chars.take{S}.unchars
text.last S = Me.(Me.size-1)
text.head = Me.0
text.tail = Me.drop{1}
text.lead = Me.take{Me.size-1}

list.last = Me.(Me.size-1)
list.suf X = [@Me X]
list.lead = Me.take{Me.size-1}

list.infix Item = // intersperse from Haskell
| N = Me.size*2-1
| if N < 0 then [] else dup I N: if I%2 then Item else Me^pop

list.locate F =
| I = 0
| till Me.end
  | when F Me^pop: leave I
  | I !+ 1

hard_list.locate F = times I Me.size: when F Me.I: leave I

list.find F =
| I = 0
| till Me.end
  | X = Me^pop
  | when F X: leave X
  | I !+ 1

hard_list.find F =
| times I Me.size
  | X = Me.I
  | when F X: leave X

text.chars = dup I Me.size Me.I

list.groupBy N =
| Y = []
| Ys = []
| I = 0
| till Me.end
  | push Me^pop Y
  | I !+ 1
  | when I >< N
    | push Y.reverse Ys
    | Y <= []
    | I <= 0
| when Y.size: push Y.reverse Ys
| Ys.reverse

list.all F =
| for X Me: unless F X: leave 0
| 1

list.any F =
| for X Me: when F X: leave 1
| 0

HexChars = '0123456789ABCDEF'

int.x =
| unless Me: leave '0'
| Cs = []
| S = ''
| when Me < 0
  | S <= '-'
  | Me <= -Me
| while Me > 0
  | Cs <= [HexChars.(Me%16) @Cs]
  | Me !/ 16
| [S@Cs].unchars

_.as_text = ['#:' Me^address.x].unchars

void.as_text = 'Void'

int.as_text =
| unless Me: leave '0'
| Cs = []
| S = ''
| when Me < 0
  | S <= '-'
  | Me <= -Me
| while Me > 0
  | Cs <= [HexChars.(Me%10) @Cs]
  | Me !/ 10
| [S@Cs].unchars

plain_char C =
| N = C.code
| if   ('a'.code << N and N << 'z'.code)
    or ('A'.code << N and N << 'Z'.code)
    or ('0'.code << N and N << '9'.code)
    or '_'.code >< N
  then 1
  else 0

text.as_text =
| Cs = []
| Q = 0
| for C Me
  | unless plain_char C: Q <= 1
  | when C >< '`': C <= '\\`'
  | push C Cs
| if Q then ['`' @['`' @Cs].reverse].unchars else Me

list.as_text = "([(map X Me X.as_text).infix{' '}.unchars])"

_.textify_ = Me.as_text
text.textify_ = Me

GGensymCount = 0
gensym Name = "[Name]__[GGensymCount!+1]"

bad @Xs =
| say_ (map X ['error:' @Xs '\n'] "[X]").infix{` `}.unchars
| halt

say @Xs =
| say_ (map X [@Xs '\n'] "[X]").infix{` `}.unchars
| Void

// hashtable
data table buckets
table Size = new_table: dup Size Void
table.`.` K =
| Bs = Me.buckets
| H = K.hash%Bs.size
| Xs = Bs.H
| when no Xs: leave Void
| X = Xs.find{X => X.0><K}
| when no X: leave Void
| X.1
table.`!` K V =
| Bs = Me.buckets
| H = K.hash%Bs.size
| Xs = Bs.H
| if no Xs then Bs.H <= [[K V]]
  else | Old = Xs.find{X => X.0><K}
       | if no Old then Bs.H <= [[K V]@Xs]
         else Old.1 <= V
| Void

table.size = Me.buckets.map{X=>X.size}.sum
table.harden = Me.buckets.skip{X => X >< Void}.join
table.as_text = "#table[Me.harden.as_text]"

list.as_table =
| T = table Me.size*2
| for [K V] Me: T.K <= V
| T

list.uniq =
| Seen = table Me.size*2
| Me.skip{X => have Seen.X or (Seen.X <= 1) and 0}

text.pad Count Item =
| C = Item.as_text
| when C.size > 1: bad "pad item: [C]"
| N = Count - Me.size
| when N < 0: bad "text is larger than [Count]: '[Me]'"
| "[(dup N C).unchars][Me]"


data macro name expander

data meta object_ info_
meta._ Name =
| M = _this_method
| Me.0 <= Me.0.object_
| Me.apply_method{M}

meta.is_list = Me.object_.is_list
meta.is_text = Me.object_.is_text
meta.as_text = Me.object_.as_text

export non say bad no have gensym table new_macro new_meta
