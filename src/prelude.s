non F = X => if F X then 0 else 1
no X = Void >< X
got X = Void <> X

_.`<>` B = not Me >< B
_.`<<` B = not B < Me
_.`>` B = B < Me
_.`>>` B = not Me < B

_.is_int = 0
int.is_int = 1

_.is_float = 0
float.is_float = 1

_.is_fn = 0
fn.is_fn = 1

_.is_list = 0
list.is_list = 1

_.is_text = 0
text.is_text = 1

//_.is_hard_list = 0
//list.is_hard_list = 1

_.`{}` F = Me.map{F}
fn.`{}` @As = As.apply{Me}


int.sign = if Me < 0 then -1
           else if Me > 0 then 1
           else 0

float.sign = if Me < 0.0 then -1.0
             else if Me > 0.0 then 1.0
             else 0.0

int.abs = if Me < 0 then -Me else Me

float.abs = if Me < 0.0 then -Me else Me

list.neg = dup I Me.size -Me.I
list.`+` Ys = dup I Me.size Me.I+Ys.I
list.`-` Ys = dup I Me.size Me.I-Ys.I
list.`*` A = map X Me: X*A
list.`/` A = map X Me: X/A
list.`%` A = map X Me: X%A
list.float = map X Me: X.float
list.int = map X Me: X.int

list.length =
| R = 0.0
| map X Me: R !+ (X*X).float
| R.sqrt

list.normalize = Me / Me.length

text.`<` B =
| less B.is_text: bad "cant compare string `[Me]` with [B]"
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

text.is_digit =
| times I Me.size
  | C = Me.I.code
  | when C < '0'.code or '9'.code < C: leave 0
  | I !+ 1
| 1

text.upcase =
| Ys = map Char Me.list
  | C = Char.code
  | if C < 'a'.code or 'z'.code < C then Char else (C - 'a'.code + 'A'.code).char
| Ys.text

text.downcase =
| Ys = map Char Me.list
  | C = Char.code
  | if C < 'A'.code or 'Z'.code < C then Char else (C - 'A'.code + 'a'.code).char
| Ys.text

text.title =
| less Me.size: leave Me
| if Me.0.is_upcase then Me else "[Me.0.upcase][Me.tail]"

_.is_keyword = 0
text.is_keyword = not: Me.size and Me.0.is_upcase

int.list = dup I Me: I //iota operator

int.map F = dup I Me: F I

list.i = dup I Me.size: [I Me^pop]

list.`.` K =
| times I K: Me <= Me.tail
| Me.head

list.`$` K = [@Me.take{K} @Me.drop{K+1}]

list.x = Me.0
list.y = Me.1
list.z = Me.2

list.size =
| S = 0
| till Me.end
  | Me <= Me.tail
  | S !+ 1
| S

list.end = not Me.size

text.end = not Me.size

_bytes_.list = dup I Me.size Me.I

bytes.bytes = Me

list.bytes =
| N = Me.size
| Ys = N.bytes
| times I N: Ys.I <= pop Me
| Ys

list.utf8 = Me.bytes.utf8

list.head = Me.0

list.tail = Me.list.tail

list.`><` B =
| less B.is_list: leave 0
| till Me.end or B.end: less Me^pop >< B^pop: leave 0
| 1

hard_list.`><` B =
| less B.is_list: leave 0 //FIXME: cons_list B will be O(n^2) slow
| N = Me.size
| less N >< B.size: leave 0
| times I N
  | less Me.I >< B.I: leave 0
  | I !+ 1
| 1

list.flip =
| N = Me.size
| Ys = dup N
| while N > 0
  | N !- 1
  | Ys.N <= pop Me
| Ys

hard_list.flip =
| N = Me.size
| dup N
  | N !- 1
  | Me.N

text.flip = Me.list.flip.text

list.map F = dup Me.size: F Me^pop
hard_list.map F = dup I Me.size: F Me.I
text.map F = Me.list.map{F}

list.e F = till Me.end: F Me^pop
hard_list.e F = times I Me.size: F Me.I

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
| if F.is_fn
  then for X Me: when F X: Ys <= [X@Ys]
  else for X Me: when F >< X: Ys <= [X@Ys]
| Ys.flip

list.skip F =
| Ys = []
| if F.is_fn
  then for X Me: less F X: Ys <= [X@Ys]
  else for X Me: less F >< X: Ys <= [X@Ys]
| Ys.flip

list.join =
| Size = Me.map{X=>X.size}.sum
| Rs = dup Size
| I = 0
| for Ys Me: for Y Ys | Rs.I <= Y
                      | I !+ 1
| Rs

_list_.list = Me

list.list =
| N = Me.size
| Ys = dup N
| times I N: Ys.I <= pop Me
| Ys

list.apply F = Me.list.apply{F}
list.apply_method F = Me.list.apply_method{F}

list.text @As =
| R = Me.list
| if As.size then R.text{As.0} else R.text

list.split S =
| F = if S.is_fn then S else X => S >< X
| Ys = []
| P = Me.locate{F}
| while got P
  | Ys <= [Me.take{P}@Ys]
  | Me <= Me.drop{P+1}
  | P <= Me.locate{F}
| [Me@Ys].flip

text.split F = Me.list.split{F}.map{X=>X.text}

text.lines = Me.split{'\n'}

text.get = get_file_ Me
text.get_text = get_text_file_ Me
text.set Value =
| if Value.is_text then set_text_file_ Me Value else set_file_ Me Value.bytes
| 0
text.exists = file_exists_ Me
text.time = file_time_ Me

text.paths @As =
| Path = if Me.last >< '/' then Me else "[Me]/"
| Xs = if As.size then Me.items{all} else Me.items
| map X Xs "[Path][X]"

text.url =
| Name = ""
| Ext = ""
| Xs = Me.list.flip
| Sep = Xs.locate{?><'/'}
| Dot = Xs.locate{?><'.'}
| when got Dot and (no Sep or Dot < Sep):
  | Ext <= Xs.take{Dot}.flip.text
  | Xs <= Xs.drop{Dot+1}
  | when got Sep: Sep !- (Dot+1)
| Folder = Void
| Name = Void
| if got Sep
  then | Folder <= Xs.drop{Sep+1}.flip.text
       | Name <= Xs.take{Sep}.flip.text
       | when Folder >< '': Folder <= '/'
  else | Folder <= ''
       | Name <= Xs.flip.text
| [Folder Name Ext]

list.unurl =
| Folder = Me.0
| Name = Me.1
| Ext = Me.2
| when Folder <> '/': Folder <= "[Folder]/"
| when Ext <> '': Ext <= ".[Ext]"
| "[Folder][Name][Ext]"

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

text.drop S = Me.list.drop{S}.text
text.take S = Me.list.take{S}.text
text.last = Me.(Me.size-1)
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
| less F.is_fn: F <= (X => F >< X)
| I = 0
| till Me.end
  | when F Me^pop: leave I
  | I !+ 1

hard_list.locate F =
| if F.is_fn then times I Me.size: when F Me.I: leave I
  else times I Me.size: when F >< Me.I: leave I

text.locate F =
| if F.is_fn then times I Me.size: when F Me.I: leave I
  else times I Me.size: when F >< Me.I: leave I

list.find F =
| less F.is_fn: F <= (X => F >< X)
| I = 0
| till Me.end
  | X = Me^pop
  | when F X: leave X
  | I !+ 1

hard_list.find F =
| less F.is_fn: F <= (X => F >< X)
| times I Me.size
  | X = Me.I
  | when F X: leave X

text.list = dup I Me.size Me.I

list.group N =
| Y = []
| Ys = []
| I = 0
| till Me.end
  | push Me^pop Y
  | I !+ 1
  | when I >< N
    | push Y.flip Ys
    | Y <= []
    | I <= 0
| when Y.size: push Y.flip Ys
| Ys.flip

list.all F =
| less F.is_fn: F <= (X => F >< X)
| for X Me: less F X: leave 0
| 1

list.any F =
| less F.is_fn: F <= (X => F >< X)
| for X Me: when F X: leave 1
| 0

list.max =
| when Me.end: leave Void
| M = Me.head
| for X Me: when X > M: M <= X
| M

list.min =
| when Me.end: leave Void
| M = Me.head
| for X Me: when X < M: M <= X
| M

HexChars = '0123456789ABCDEF'

int.x =
| less Me: leave '0'
| Cs = []
| S = ''
| when Me < 0
  | S <= '-'
  | Me <= -Me
| while Me > 0
  | Cs <= [HexChars.(Me%16) @Cs]
  | Me !/ 16
| [S@Cs].text

_.as_text = ['#:' Me^address.x].text

void.as_text = 'Void'

int.as_text =
| less Me: leave '0'
| Cs = []
| S = ''
| when Me < 0
  | S <= '-'
  | Me <= -Me
| while Me > 0
  | Cs <= [HexChars.(Me%10) @Cs]
  | Me !/ 10
| [S@Cs].text

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
  | less plain_char C: Q <= 1
  | when C >< '`': C <= '\\`'
  | push C Cs
| if Q then ['`' @['`' @Cs].flip].text else Me

list.as_text = "([(map X Me X.as_text).text{' '}])"

_.textify_ = Me.as_text
text.textify_ = Me

say Text = say_ "[Text]\n"
bad Text =
| say_ "bad: [Text]\n"
| halt

// hashtable
data map buckets
map_ Size = new_map: dup Size Void
map.`.` K =
| Bs = Me.buckets
| H = K.hash%Bs.size
| Xs = Bs.H
| when no Xs: leave Void
| for X Xs: when X.0 >< K: leave X.1
| Void
map.`!` K V =
| Bs = Me.buckets
| H = K.hash%Bs.size
| Xs = Bs.H
| if no Xs then Bs.H <= [[K V]]
  else | Old = Xs.find{X => X.0><K}
       | if no Old then Bs.H <= [[K V]@Xs]
         else Old.1 <= V
| Void
map.`$` K =
| Bs = Me.buckets
| H = K.hash%Bs.size
| Xs = Bs.H
| when no Xs: leave Void
| L = Xs.locate{X => X.0><K}
| when got L: Bs.H <= $Xs.L
| Me
map._ Name =
| if Me.size > 1 then Me.0.(Name.drop{4}) <= Me.1 else Me.0.Name


map.size = Me.buckets.map{X => if got X then X.size else 0}.sum
map.list = Me.buckets.skip{X => X >< Void}.join
map.as_text = "#m{[Me.list{}{?0}]}"

list.as_map =
| T = m size/(Me.size*2)
| for [K V] Me: T.K <= V
| T

list.uniq =
| Seen = m size/(Me.size*2)
| Me.skip{X => got Seen.X or (Seen.X <= 1) and 0}

text.pad Count Item =
| X = "[Item]"
| when X.size > 1: bad "pad item: [X]"
| N = Count.abs - Me.size
| when N < 0: bad "text is larger than [Count.abs]: '[Me]'"
| Pad = @text: dup N X
| if Count < 0 then "[Pad][Me]" else "[Me][Pad]"

list.pad Count Item =
| N = Count.abs - Me.size
| when N < 0: bad "list is larger than [Count.abs]: '[Me]'"
| Pad = dup N Item
| if Count < 0 then [@Pad @Me] else [@Me @Pad]

data macro name expander

data meta object_ info_
meta._ Name =
| M = _this_method
| Me.0 <= Me.0.object_
| Me.apply_method{M}

meta.is_list = Me.object_.is_list
meta.is_text = Me.object_.is_text
meta.as_text = Me.object_.as_text

LCG_Seed = Void
LCG_M = 2147483647
LCG_M_F = LCG_M.float
LCG_A = 16807
LCG_B = 0

lcg_init Seed =
| LCG_Seed <= Seed
| 10.rand
| Void

int.rand =
| LCG_Seed <= (LCG_Seed*LCG_A + LCG_B) % LCG_M
| @int: @round: LCG_Seed.float*Me.float/LCG_M_F

float.rand =
| LCG_Seed <= (LCG_Seed*LCG_A + LCG_B) % LCG_M
| LCG_Seed.float/LCG_M_F*Me

list.rand = Me.(@rand Me.size-1)

GGensymCount = 0
text.rand = "[Me]__[GGensymCount!+1]"

lcg_init: time

list.shuffle =
| Xs = Me.list
| N = Xs.size
| while N > 1
  | N !- 1
  | R = N.rand
  | X = Xs.R
  | Xs.R <= Xs.N
  | Xs.N <= X
| Xs

sort_asc &[] [H@Zs] =
| Xs = []
| Ys = []
| for Z Zs: if Z < H then push Z Xs else push Z Ys
| [@Xs^sort_asc H @Ys^sort_asc]

list.sort @As =
| F = Void
| case As
  [A] | F <= A
  [] | leave: sort_asc Me.shuffle
  Else | bad "list.sort: invalid number of arguments"
| h &[] [H@Zs] =
  | Xs = []
  | Ys = []
  | for Z Zs: if F Z H then push Z Xs else push Z Ys
  | [@Xs^h H @Ys^h]
| h Me.shuffle

text.int Radix =
| N = Me.size
| I = 0
| Sign = if Me.I >< '-'
         then | I !+ 1
              | -1
         else 1
| R = 0
| Base = '0'.code
| AlphaBase = 'A'.code - 10
| while I < N
  | C = Me.I.code
  | V = if '0'.code << C and C << '9'.code then C - Base else C - AlphaBase
  | R <= R*Radix + V
  | I !+ 1
| R*Sign

list.u4 = Me.3*#1000000 + Me.2*#10000 + Me.1*#100 + Me.0
list.u4b = Me.0*#1000000 + Me.1*#10000 + Me.2*#100 + Me.3
list.s4 = as R Me.u4: when R.mask{#80000000}: R!-#100000000
list.s4b = as R Me.u4b: when R.mask{#80000000}: R!-#100000000

list.u2 = Me.1*#100 + Me.0
list.u2b = Me.0*#100 + Me.1
list.s2 = as R Me.u2: when R.mask{#8000}: R!-#10000
list.s2b = as R Me.u2b: when R.mask{#8000}: R!-#10000

int.u4 = [Me/#1000000%256 Me/#10000%256 Me/#100%256 Me%256]
int.u4b = [Me%256 Me/#100%256 Me/#10000%256 Me/#1000000%256]
int.s4 = 
| when Me < 0: #100000000+Me!
| [Me%256 Me/#100%256 Me/#10000%256 Me/#1000000%256]
int.s4b =
| when Me < 0: #100000000+Me!
| [Me/#1000000%256 Me/#10000%256 Me/#100%256 Me%256]

export non say bad no got map_ new_macro new_meta
