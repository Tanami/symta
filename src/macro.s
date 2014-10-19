GExpansionDepth = Void
GExpansionDepthLimit = 1000
GMacros = Void
GDefaultLeave = Void
GModuleCompiler = Void
GModuleFolders = Void
GSrc = [0 0 unknown]
GTypes = Void
GVarsTypes = []

mex_error Message =
| [Row Col Orig] = GSrc
| bad "[Orig]:[Row],[Col]: [Message]"

source_ = [_quote GSrc]
destination_ = [_quote GModuleFolders{}.2]
compiler_ = [_quote GModuleFolders{}.0]

is_var_sym X = X.is_text and not X.is_keyword

load_symbol Library Name =
| Module = GModuleCompiler Library
| when no Module: mex_error "couldn't compile [Library]"
| Found = Module^load_library.find{X => X.0 >< Name}
| less got Found: mex_error "couldn't load `[Name]` from `[Library]`"
| Found.1

expand_list_hole_advanced H Hs Key Hit Miss =
| [Again Took Rest Xs I N Else] = form: ~Again ~Took ~Rest ~Xs ~I ~N ~Else
| Fail = form: if I < N
               then | !I + 1
                    | _goto Again
               else Miss
| form | Xs = Key.list // ensure it is simple list
       | I = 0
       | N = Xs.size
       | _label Again
       | Took = Xs.take{I}
       | Rest = Xs.drop{I}
       | case Took
         H | case Rest
               [$@Hs] Hit
               Else Fail
         Else | Fail

expand_list_hole Key Hole Hit Miss = case Hole
  [] | [_if [_mcall Key end] Hit Miss]
  [[`@` Zs]] | expand_hole Key Zs Hit Miss
  [[`@` Zs] @More] | expand_list_hole_advanced Zs More Key Hit Miss
  [[`*` A B] @Xs] | [G Else] = form: ~G ~Else
                  | Hole = form: @A @(`<` G [-B @_]+[])
                  | Hit = form: case G [$@Xs] Hit Else Miss
                  | expand_list_hole Key Hole Hit Miss
  [[`%` A B] @Xs] | expand_list_hole Key (form: (`<` [B@_] A)*B $@Xs) Hit Miss
  [[`/` Size Sub] @Xs]
    | Sz = @rand 'Sz'
    | Ys = @rand 'Ys'
    | Zs = @rand 'Zs'
    | Hit = expand_list_hole Zs Xs Hit Miss
    | form | Sz = Size
           | _if Key.size < Sz
                 Miss
                 (`|` (Ys = Key.take{Sz})
                      (Zs = Key.drop{Sz})
                      $(expand_hole Ys Sub Hit Miss))
  [X@Xs] | H = @rand 'X'
         | Hs = @rand 'Xs'
         | Hit <= expand_list_hole Hs Xs Hit Miss
         | [`if` [_mcall Key end]
                 Miss
                 [let_ [[H [_mcall Key head]]
                        [Hs [_mcall Key tail]]]
                   (expand_hole H X Hit Miss)]]

expand_hole_keywords Key Hit Xs =
| [I As Size] = form: ~I ~As ~Size
| form: `|` $@Xs{[`=` [?.1.title] 0]}
            (As = Key)
            (Size = As.size)
            $@(map [O K V] Xs
               | L = @rand 'l'
               | form: named L
                 | times I Size: less I%2
                   | when K >< As.I
                     | $K.title <= As.(I+1)
                     | leave L 0
                 | (`<=` ($K.title) V))
            Hit

expand_hole_term Key Hole Hit Miss =
| when Hole >< '_': leave Hit
| when Hole >< '~': leave: form: if Key >< Void then Miss else Hit
| when Hole.is_keyword
  | when Hole.size and Hole.last >< '?':
    | leave: form: _if (@$"is_[Hole.lead]" Key) Hit Miss
  | Hole <= [_quote Hole]
| leave: if Hole.is_text then [let_ [[Hole Key]] Hit]
         else [_if ['><' Hole Key] Hit Miss]

expand_hole Key Hole Hit Miss =
| less Hole^is{[X@Xs]}: leave: expand_hole_term Key Hole Hit Miss
| case Hole
  [`[]` @Xs] | P = Xs.locate{$0[`/` K V]=>K.is_keyword}
             | when got P: Xs <= [@Xs.take{P} [`@` [`//` @Xs.drop{P}]]]
             | [_if [_mcall Key is_list]
                    (expand_list_hole Key Xs Hit Miss)
                    Miss]
  [`<` A B] | expand_hole Key B (expand_hole Key A Hit Miss) Miss
  [`+` @Xs] | [_if (expand_match Key (map X Xs [X 1]) 0 Void) Hit Miss]
  [`-` @Xs] | [_if (expand_match Key (map X Xs [X 1]) 0 Void) Miss Hit]
  [O<`+`+`-` [&O @Xs] @Ys] | expand_hole Key [O @Xs @Ys] Hit Miss
  [X<`.`+`^` [Y<`.`+`^` A B] @As]
    | G = @rand 'G'
    | Hit = expand_hole G [X G @As] Hit Miss
    | expand_hole Key [Y [`<` G A] B] Hit Miss
  [X<`.`+`^` [`{}` [Y A B] @As] @Bs] | expand_hole Key [X [Y A B @As] @Bs] Hit Miss
  [`.` A B @As] | G = @rand 'G'
                | [let_ [[G (if B.is_keyword
                             then [_mcall Key B @As]
                             else [_mcall Key "." B @As])]]
                    (expand_hole G A Hit Miss)]
  [`^` A B @As] | G = @rand 'G'
                | [let_ [[G [B @As]]]
                    (expand_hole G A Hit Miss)]
  [`{}` [Op A B] @As] | expand_hole Key [Op A B @As] Hit Miss
  [`=>` A B] | [let_ [[A.0 Key]]
                 [_if ['|' @B] Hit Miss]]
  [`&` X] | form: _if X >< Key Hit Miss
  [`//` @Xs] | expand_hole_keywords Key Hit Xs
  [`\\` X] | form: _if Hole >< Key Hit Miss
  [`"` @Xs] /*"*/ //FIXME: use special matcher for text
    | Vs = []
    | Cs = map X Xs: if X.is_text then map C X.list [`\\` C]
                     else | V = form ~G
                          | N = X.0
                          | less N >< _: push [X.0 V] Vs
                          | [[`@` V]]
    | Hit = form: `|` $@(map [A B] Vs: form (A = _mcall B text))
                      Hit
    | form: _if (_mcall Key is_text)
                $(expand_list_hole Key Cs.join Hit Miss)
                Miss
  [[X@Xs]] | expand_hole Key [X@Xs] Hit Miss
  Else | mex_error "bad match case: [Hole]"

expand_match Keyform Cases Default Key =
| when no Key: Key <= @rand 'Key'
| E = @rand end
| D = @rand default
| R = @rand 'R'
| Ys = []
| for Case Cases.flip
  | Name = @rand c
  | NextLabel = if Ys.size > 0 then Ys.0.1 else D
  | Miss = [_goto NextLabel]
  | Hit = [_progn [_set R [_progn @Case.tail]]
                  [_goto E]]
  | Ys <= [[_label Name] (expand_hole Key Case.head Hit Miss) @Ys]
| [let_ [[Key Keyform]
         [R 0]]
    @Ys.tail
    [_label D]
    [_set R Default]
    [_label E]
    R]

case @Xs = expand_match Xs.0 Xs.tail.group{2} 0 Void

is @As =
| case As
   [A] | form: $['$' 0] A => 1
   [A B] | [case B A 1]
   Else | mex_error "invalid number of args to `is`: [As]"

min A B = form | ~A = A
               | ~B = B
               | if ~A < ~B then ~A else ~B
max A B = form | ~A = A
               | ~B = B
               | if ~A > ~B then ~A else ~B

swap A B = form | ~T = A
                | A <= B
                | B <= ~T

`if` A B C = [_if A B C]
not @Xs = [_if Xs 0 1]
`and` A B = [_if A B 0]
`or` A B = form: let_ ((~V A)) (_if ~V ~V B)
when @Xs = [_if Xs.lead Xs.last Void]
less @Xs = [_if Xs.lead Void Xs.last]

expand_while Head Body =
| L = @rand l
| [_progn [_label L]
          [_if Head
               [_progn Body [_goto L]]
               Void]]

while @As = expand_while As.lead As.last
till @As = expand_while [not As.lead] As.last

times Var Count Body =
| I = if got Var then Var else @rand 'I'
| N = @rand 'N'
| ['|' ['=' [N] Count]
       ['=' [I] [0]]
       [less [`and` [_eq [_tag N] 0]
                      [_gte N 0]]
         [_fatal 'dup: bad loop count']]
       [while [_lt I N]
         ['|' Body
              [_set I [_add I 1]]]]]

expand_dup Var Count Body =
| I = if got Var then Var else @rand 'I'
| N = @rand 'N'
| Ys = @rand 'Ys'
| ['|' ['=' [N] Count]
       ['=' [I] [0]]
       [less [`and` [_eq [_tag N] 0]
                      [_gte N 0]]
         [_fatal 'dup: bad loop count']]
       ['=' [Ys] [_alloc N]]
       [while [_lt I N]
         ['|' [_store Ys I Body]
              [_set I [_add I 1]]]]
       [_tagged [_quote 'T_LIST'] Ys]]

dup @As = case As
  [X Xs Body] | expand_dup X Xs Body
  [Xs Body] | expand_dup Void Xs Body
  [Xs] | expand_dup Void Xs 0
  Else | mex_error "bad dup [As]"

expand_map_for Type Item Items Body =
| Xs = @rand 'Xs'
| I = @rand 'I'
| N = @rand 'N'
| ['|' ['=' [Xs] [_mcall Items list]]
       [Type I [_mcall Xs size]
          ['|' ['=' [Item] [_mcall Xs '.' I]]
               Body]]]

map Item Items Body = expand_map_for dup Item Items Body
for Item Items Body = expand_map_for times Item Items Body

expand_quoted_list Xs =
| Ys = map X Xs: if X.is_list then expand_quoted_list X else [_quote X]
| ['_list' @Ys]

expand_quasiquote O =
| less O.is_list: leave [_quote O]
| case O
  [`$` X] | X
  Else | ['[]' @(map X O: expand_quasiquote X)]

`\\` O = expand_quasiquote O

expand_form O AGT =
| less O.is_list: leave
  if O.is_text and not O.is_keyword then O
  else if O.is_text and O.size > 1 and O.0 >< '~' then
    | AG = AGT.O
    | when no AG
      | AG <= O.tail.rand
      | AGT.O <= AG
    | AG
  else [_quote O]
| case O
  [`$` X] | X
  Else | ['[]' @(map X O: expand_form X AGT)]

form O =
| AGT = t
| R = expand_form O AGT
| when AGT.size > 0: R <= [let_ (map [K V] AGT [V [_mcall [_quote K.tail] rand]]) R]
| R

expand_text_splice Xs =
| case Xs
   [X] | when X.is_text: leave [_quote X]
   [] | leave [_quote '']
| As = map X Xs: if X.is_text then [_quote X] else [_mcall X textify_]
| [_mcall [_list @As] text]

`"` @Xs /*"*/ = expand_text_splice Xs

pop O = form: as O.head: O <= O.tail

push Item O = form: O <= [Item @O]

let @As =
| when As.size < 2: mex_error "bad let @As"
| Bs = As.lead.group{2}
| Body = As.last
| Gs = map B Bs ['G'.rand @B]
| R = @rand 'R'
| [let_ [[R 0] @(map G Gs [G.0 G.1])]
    @(map G Gs [_set G.1 G.2])
    [_set R Body]
    @(map G Gs [_set G.1 G.0])
    R]

`+` A B = [_mcall A '+' B]
`-` @As = case As
  [A] | [_mcall A neg]
  [A B] | [_mcall A '-' B]
  Else | mex_error "`-` got wrong number of args: [As]"
`*` A B = [_mcall A '*' B]
`/` A B = [_mcall A '/' B]
`%` A B = [_mcall A '%' B]
`**` A B = [_mcall A '**' B]
`<` A B = [_mcall A '<' B]
`>` A B = [_mcall A '>' B]
`<<` A B = [_mcall A '<<' B]
`>>` A B = [_mcall A '>>' B]
`><` A B = [_mcall A '><' B]
`<>` A B = [_mcall A '<>' B]
`^` A B = [B A]
`.` A B = if A.is_keyword then [A B]
          else if B.is_keyword
               then | when A^is_var_sym: case GVarsTypes.find{?0><A} [Var Type]
                      | Fields = GTypes.Type
                      | P = got Fields and Fields.locate{B}
                      | when got P: leave [_dget A P]
                    | ['{}' ['.' A B]]
          else [_mcall A '.' B]

expand_colon_r E Found =
| less E.is_list: leave E
| P = E.locate{$0["!" Y] => Y.is_keyword}
| less got P: leave: map X E: expand_colon_r X Found
| Name = E.P.1
| Expr = E.drop{P+1}
| G = 'G'.rand
| Found Name G
| [@E.take{P}.tail ['|' ['<=' [G] Expr] G]]

`:` A B =
| Name = 0
| G = 0
| E = expand_colon_r A: X Y => | Name <= X; G <= Y
| less Name: leave [@A B]
| B = B.rmap{if Name >< ? then G else ?} //FIXME: preserve metainfo
| [let_ [[G 0]] [@E B]]

`,` @As = case As
  [[X@Xs] @Ys] | [X Xs @Ys]
  Else | mex_error "invalid arglist to `,`"

expand_self_ref O = case O
  [H<`.`+`{}`+`^` X @Xs] | [H (expand_self_ref X) @Xs]
  Else | [`.` 'Me' O]

`$` Expr = expand_self_ref Expr

have Var Default = form | when (no Var) (`<=` (Var) Default)
                        | Var

supply Surrogate What =
| form: `|` (`=` (~W) What)
            (_if Void >< ~W Surrogate ~W)

expand_method_arg_r A FX FY =
| when A.is_text
  | when A >< '?': leave: FX A
  | when A >< '??': leave: FY A
  | when A.size > 1 and A.0 >< '?':
    | M = A.tail
    | V = '?'
    | when M.0 >< '?'
      | V <= '??'
      | M <= M.tail
    | when M.is_digit: M <= M.int{10}
    | leave: expand_method_arg_r ['.' V M] FX FY
| less A.is_list: leave A
| case A
   [`{}` X @Xs] | [A.0 (expand_method_arg_r X FX FY) @Xs]
   [`{}` @Xs] | A
   [`\\` @Xs] | A
   [_quote @Xs] | A
   Else | map X A: expand_method_arg_r X FX FY

expand_method_arg Expr =
| X = Void
| Y = Void
| R = expand_method_arg_r Expr (N => have X: form ~X) (N => have Y: form ~Y)
| As = [X Y].skip{Void}
| when As.size: Expr <= form: _fn As R
| Expr

`{}` H @As =
| As = map X As: expand_method_arg X
| case H
  [`.` A B] | [_mcall A B @As]
  [`^` A B] | [B @As A]
  Else | if H.is_keyword then [H @As] else [_mcall H '{}' @As]

`!!` @As =
| Ys = map A As A
| V = Void
| P = As.locate{$0[`!` X] =>| V<=X; 1}
| when no P: mex_error "invalid !! - no ! in [As]"
| Ys.P <= V
| expand_assign V Ys

is_incut X = case X [`@` Xs] 1

`[]` @Xs =
| Save = 0
| As = map A Xs: case A
                 ['!!' '@' ['!' X]] | Save <= X
                                    | ['@' X]
                 Else | A
| when Save: leave: form: Save <= `[]` $@As
| IncutCount = As.count{&is_incut}
| when IncutCount >< 0: leave [_list @As]
| when IncutCount >< 1
  | case As.last
    [`@` Xs] | As = As.flip.tail
             | till As.end: Xs <= [_mcall Xs pre As^pop]
             | leave Xs
| As = map A As: if A^is_incut then A.1 else [_list A]
| [_mcall [_list @As] join]

t @As_ =
| As = As_
| Size = 0
| case As [[`/` size S] @Xs]
  | Size <= S
  | As <= Xs
| T = form ~T
| As <= As.group{2}
| if As.size
  then | less Size: Size <= 2*As.size
       | form: `|` (T = table_ Size)
                   $@(map [K V] As
                     | when K.is_text: K <= form \K
                     | when V.is_text: V <= form \V
                     | form: T.K <= V)
                 T
  else | less Size: Size <= 256
       | form: table_ Size

//FIXME: move it to compiler.s
mangle_name Name =
| Rs = map C Name
  | N = C.code
  | if   ('a'.code << N and N << 'z'.code)
      or ('A'.code << N and N << 'Z'.code)
      or ('0'.code << N and N << '9'.code)
    then C
    else "_[N.x.pad{-2 0}]"
| [_ @Rs].text

result_and_label Name =
| Mangled = mangle_name Name
| ["ReturnOf[Mangled]_" "end_of[Mangled]_"]

expand_named Name Body =
| [R End] = result_and_label Name
| [let_ [[R 0]]
    [_set R Body]
    [_label End]
    R]

named @As = expand_named As.head [_progn @As.tail]

expand_leave Name Value =
| [R End] = result_and_label Name
| [_progn [_set R Value] [_goto End]]

add_pattern_matcher Args Body =
| G = @rand 'As'
| Default = case Args
    [[`$` '_'] @Tail]
      | Args <= Tail
      | form G.0
    [[`$` D] @Tail]
      | Args <= Tail
      | D
    Else | form: _fatal 'couldnt match args list'
| case Args
   [[`@` All]] | Args <= All
   Else | Body <= expand_match G [[['[]' @Args] Body]] Default Void
        | Args <= G
| [Args Body]

pattern_arg X = not X.is_text or X.is_keyword

`=>` As Body =
| Name = 0
| case As [[`@` N] @Zs]: when N.is_keyword
  | Name <= N
  | As <= Zs
| Body <= [`|` Body]
| [A B] = if no As.find{&pattern_arg} then [As Body] else add_pattern_matcher As Body
| R = [_fn A B]
| when Name: R <= [let_ [[Name 0]] [`|` [_set Name R] [`&` Name]]]
| R

expand_block_item_fn Name Args Body =
| KName = "_k_[Name]"
| [A B] = if no Args.find{&pattern_arg} then [Args Body] else add_pattern_matcher Args Body
| B <= [default_leave_ Name (expand_named Name B)]
| [Name [_fn A [_progn [_mark Name] B]]]

expand_destructuring Value Bs Body =
| O = @rand 'O'
| Ys = map [I B] Bs.i: [B [_mcall O '.' I]]
| [let_ [[O Value]] [let_ Ys Body]]

expand_assign Place Value =
| case Place
  [`.` A B] | if B.is_keyword
              then | when A^is_var_sym: case GVarsTypes.find{?0><A} [Var Type]
                     | Fields = GTypes.Type
                     | P = got Fields and Fields.locate{B}
                     | when got P: leave [_dset A P Value]
                   | [_mcall A "![B]" Value]
              else [_mcall A "!" B Value]
  [`$` [`.` A B]] | expand_assign [`.` [`$` A] B] Value
  [`$` X] | expand_assign [`.` 'Me' X] Value
  Else | [_set Place Value]

`<=` Place Value = expand_assign Place.0 Value

type Name @Fields =
| CtorName = 0
| CtorArgs = []
| CtorBody = 0
| Super = [_]
| ProvideCopy = 1
| while Name.is_list: case Name
  ['{}' N @As]
    | when case As [A@_] A.is_keyword: CtorName <= pop As
    | CtorArgs <= As
    | Name <= N
  ['.' A B] | Name <= A
            | if B >< ~ then Super <= Super.skip{_}
              else if B >< no_copy then ProvideCopy <= 0
              else push B Super
  Else | mex_error "data: bad declarator [Name]"
| less CtorName: CtorName <= Name
| Vs = []
| Fs = map F Fields: case F
       [`/` Name Value] | push Value Vs
                        | Name
       [`|` @Body] | CtorBody <= F
                   | Void
       Else | push 0 Vs
            | F
| Fs = Fs.skip{Void}
| Vs = Vs.flip
| GTypes.Name <= Fs
| Ctor = if CtorBody
         then [`=` [CtorName @CtorArgs]
                   [`|` [`=` ['Me'] [_data Name @Vs]]
                        [_type Name 'Me' CtorBody]
                        'Me']]
         else [`=` [CtorName @CtorArgs] [_data Name @Vs]]
| V = @rand 'V'
| Copy = if ProvideCopy
         then [[`=` [[`.` Name "copy"]] [_data Name @(map F Fs [`$` F])]]
               [`=` [[`.` Name "deep_copy"]] [_data Name @(map F Fs [`$` [`.` F deep_copy]])]]]
         else []
| ['@' ['|' Ctor
            @Copy
            @(map S Super [_subtype S Name])
            [`=` [[`.` Name "fields_"]] ['[]' @Fs]]
            [`=` [[`.` Name "is_[Name]"]] 1]
            [`=` [[`.` '_' "is_[Name]"]] 0]
            @(map [I F] Fs.i [`=` [[`.` Name F]]  [_dget 'Me' I]])
            @(map [I F] Fs.i [`=` [[`.` Name "![F]"] V]  [_dset 'Me' I V]])
            ]]

heir Child Parent =
| form @| Child._ ~Method ~Args =
          | ~Args.0 <= Parent
          | ~Args.apply_method{~Method}

expand_block_item_method Type Name Args Body =
| less Name >< _
  | [\Me @!Args]
  | when got GTypes.Type: Body <= form: _type Type $\Me Body
| when Name >< _
  | case Args
    [Method As] | Args <= [['@' As]]
                | Body <= form: `|` (Method = _this_method)
                                    ($\Me = As.0)
                                    (_type Type $\Me Body)
    Else | mex_error "bad arglist for _; should be: Method Args"
| Body <= form: default_leave_ Name $(expand_named Name Body)
| [Void [_dmet Name Type [`=>` Args [_progn [_mark "[Type].[Name]"] Body]]]]

expand_block_item Expr =
| Y = case Expr
  [`=` [`!!` [`!` Place]] Value] | [Void (expand_assign Place Value)]
  [`=` [[`.` Type Method] @Args] Body] | expand_block_item_method Type Method Args Body
  [`=` [Name @Args] Value]
    | if Name.is_keyword then expand_block_item_fn Name Args Value
      else | when Args.size: mex_error "`=`: left side has too many expressions"
           | [Name Value]
  Else
    | Z = mex Expr
    | case Z [`=` [] [`|` @Xs]]
      | Ys = map X Xs: expand_block_item X
      | leave Ys.join
    | [Void [_nomex Z]]
| [Y]

make_multimethod Xs =
| when case Xs [[`=>` As Expr]] (As.size >< 0 or As.0^is_var_sym)
  | leave Xs.0
| All = @rand 'A'
| Default = [_fatal "couldn't match lambda"]
| Name = []
| Xs = map X Xs: case X
    [`=>` As Expr]
      | case As [[`@` N] @Zs]: when N.is_keywrod:
        | Name <= [[`@` N]]
        | As <= Zs
      | case As [['&' D] @Zs] | Default <= D; As <= Zs
      | [['[]' @As] Expr]
| ['=>' [@Name ['@' All]] (expand_match All Xs Default Void)]

expand_block_helper R A B =
| if no A then [B @R]
  else if A.is_keyword then [[_set A B] @R]
  else | R = if R.size then [_progn @R] else Void
       | if A^is_var_sym then [[let_ [[A B]] R]]
         else if case A [`[]` @Bs] Bs.all{?^is_var_sym} then
            [(expand_destructuring B A.tail R)]
         else [(expand_match B [[A R]] [_fatal "couldnt match [B] to [A]"] Void)]

expand_block Xs =
| when Xs.size >< 1 and not case Xs.0 [`=` @Zs] 1: leave Xs.0
| Ms = []
| Ys = []
| for X Xs: case X
  [`=>` A B] | push X Ms
  Else | push X Ys
| less Ms.end: push Ms.flip^make_multimethod Ys
| Xs <= Ys.flip
| Xs <= map X Xs: expand_block_item X
| Xs <= Xs.join
| R = []
| for [A B] Xs.flip: R <= expand_block_helper R A B
| R <= [_progn @R]
| Bs = Xs.keep{X => X.0.is_keyword}
| when Bs.size: R <= [let_ (map B Bs [B.0 Void]) R]
| R

`|` @Xs = expand_block Xs
`;` @Xs = expand_block Xs
`@` X = [`=` [] [_nomex X]]

let_ @As = [_call [_fn (map B As.0 B.0) [_progn @As.tail]]
                  @(map B As.0 B.1)]

default_leave_ Name Body = let GDefaultLeave Name [_nomex Body^mex]

leave @As = case As
  [Name Value] | expand_leave Name Value
  [Value] | when no GDefaultLeave: mex_error "missing default leave"
          | expand_leave GDefaultLeave Value
  Else | mex_error "errorneous leave syntax"

as @As = case As
  [Value Expr] | form: as ~Name Value Expr
  [Name Value Expr] | form | Name = Value
                           | Expr
                           | Name

callcc F =
| K = @rand 'K'
| R = @rand 'R'
| [`|` [`=` [K] [_setjmp]]
       [`if` [_mcall K is_int]
             [F [_fn [R] [_longjmp K [_list R]]]]
             [_mcall K '.' 0]]]

fin Finalizer Body =
| B = @rand b
| F = @rand f
| R = @rand 'R'
| [[_fn [B] [B [_fn [] Finalizer]]]
   [_fn [F]
     ['|' [_set_unwind_handler ['&' F]]
          ['=' [R] Body]
          [F]
          [_remove_unwind_handler]
          R]]]

compile_when @Conds Body =
| Xs = map C Conds: case C
       [`-` X] | not get_rt_flag_ X
       Else  | get_rt_flag_ C
| if Xs.all{1} then form @(`|` Body) else 0

FFI_Lib = Void

copy_file A B =
| if get_rt_flag_ windows
  then | A <= A.replace{'/' '\\'}
       | B <= B.replace{'/' '\\'}
       | unix "copy /y \"[A]\" \"[B]\""
  else unix "cp -f '[A]' '[B]'"

copy_ffi S D =
| D.mkpath
| for X S.items
  | SF = "[S][X]"
  | DF = "[D][X]"
  | when DF.url.2 <> o and (not DF.exists or DF.time < SF.time):
    | copy_file SF DF

ffi_begin Name =
| [Root Srcs Dst] = GModuleFolders{}
| RootFFI = "[Root]ffi/[Name]/lib/"
| DstFFI = "[Dst]ffi/[Name]/"
| less RootFFI.exists: mex_error "Missing [RootFFI]"
| copy_ffi RootFFI DstFFI
| FFI_Lib <= form | ~L = \$"[DstFFI]main"
                  | if ~L.exists then ~L
                    else "[main_lib]/ffi/[\Name]/main"
| 0

expand_ffi Name Result Symbol Args =
| FFI_Package = GSrc.2.url.1 // determine package from current filename
| F = "FFI_[FFI_Package]_[Name]_"
| ATs = map A Args A.2 // argument types
| ANs = map A Args A.1 // argument names
| Extern = "[FFI_Package]?[F]"
| R = form @| F = ffi_load FFI_Lib \Symbol
            | Name $@ANs = | ~X = \Extern
                           | form (_ffi_call \(Result $@ATs) ~X $@ANs)
            | export_hidden F \Name
| R

ffi @Xs = case Xs
  [[`.` Symbol Result] @Args] | expand_ffi Symbol Result Symbol Args
  [Name [[`.` Symbol Result] @Args]] | expand_ffi Name Result Symbol Args
  Else | mex_error "ffi: bad arglist = [Xs]"


GExports = Void

exports_preprocess Xs = 
| map X Xs: case X
    [`\\` N] | V = if N.is_keyword then [`&` N] else N
             | [_list [_quote N] [new_macro [_quote N] V] ]
    Else | V = if X.is_keyword then [`&` X] else X
         | [_list [_quote X] V]

export_hidden @Xs =
| GExports <= [@GExports @Xs]
| 0

export @Xs =
| GExports <= [@Xs @GExports]
| [_list @GExports^exports_preprocess]

handle_extern X =
| P = X.locate{'?'}
| L = X.size
| less got P and P > 0 and P < L-1: leave X
| Pkg = X.take{P}
| Sym = X.drop{P+1}
| [Pkg Sym]

mex_extern Pkg Name =
| Sym = load_symbol Pkg Name
| when Sym.is_macro
  | when Name.is_keyword: mex_error "cant reference macro's value in [Pkg]?[Name]"
  | leave: mex Sym.expander
| leave: mex: form: let_ ((~R (_import (_quote Pkg) (_quote Name)))) ~R

normalize_arg X =
| if X.is_keyword
  then case X^handle_extern
       [Pkg Name] | mex_extern Pkg Name
       Else | [_quote X]
  else mex X

mex_normal X Xs =
| when GExpansionDepth > GExpansionDepthLimit: mex_error "macroexpansion depth exceed at [[X@Xs]]"
| Macro = when X.is_keyword: GMacros.X
| when X.is_text: case X^handle_extern [Pkg Sym]: when Sym.is_keyword:
  | M = load_symbol Pkg Sym
  | if M.is_macro
    then Macro <= M
    else | S = Sym.rand
         | leave: mex [let_ [[S [_import [_quote Pkg] [_quote Sym]]]] [S @Xs]]
| when no Macro
  | case X [`@` Z]: leave: mex [_mcall Xs.last Z @Xs.lead]
  | when got Xs.locate{$0[`@` X]=>1}
    | when X >< _mcall: leave: mex: form: _mcall [$Xs.0 $@Xs.drop{2}] apply_method (_method $Xs.1)
    | when X.is_keyword: X <= form &X
    | leave: mex: form [$@Xs].apply{X}
  | Ks = []
  | NewXs = []
  | for X Xs: if case X [`/` A B] A.is_keyword then push [X.1 X.2] Ks else push X NewXs
  | when Ks.size: Xs <= [@NewXs.flip @Ks.flip.join]
  | Y = X^mex
  | if (X.is_list and not Y.is_list) or (X.is_text and X <> Y)
    then leave: mex [Y@Xs]
    else leave [Y @(map X Xs: normalize_arg X)]
| Expander = Macro.expander
| NArgs = Expander.nargs
| when NArgs >> 0 and NArgs <> Xs.size:
  | [Row Col Orig] = GSrc
  | mex_error "bad number of args to macro [Macro.name]"
| mex Xs.apply{Expander}

normalize_nesting O =
| case O [X] | if X.is_keyword then O else normalize_nesting X
         X | X

mex Expr =
| when no GMacros: mex_error 'lib_path got clobbered again'
| Expr <= normalize_nesting Expr
| when Expr.is_text
  | case Expr^handle_extern [Pkg Name]: leave: mex_extern Pkg Name
  | when not Expr.is_keyword and got GMacros.Expr: Expr <= GMacros.Expr.expander
| less Expr.is_list: leave Expr
| let GExpansionDepth GExpansionDepth+1: case Expr
  [_fn As Body] | [_fn As Body^mex]
  [_set Place Value] | [_set Place (if Value.is_keyword then [_quote Value] else mex Value)]
  [_label Name] | Expr
  [_goto Name] | Expr
  [_quote X] | if X.is_list then expand_quoted_list X else Expr
  [_nomex X] | X // no macroexpand
  [_type Type Var Body] | let GVarsTypes [[Var Type]@GVarsTypes]: mex Body
  [`&` O] | if O.is_keyword then O else [O^mex]
  [] | Expr
  [X@Xs] | Src = when Expr.is_meta: Expr.meta_ 
         | let GSrc (if got Src then Src else GSrc)
           | Result = mex_normal X Xs
           | when got Src and Result.is_list: Result <= new_meta Result Src
           | Result

macroexpand Expr Macros ModuleCompiler ModuleFolders =
| let GMacros Macros
      GExpansionDepth 0
      GExports []
      GModuleCompiler ModuleCompiler
      GModuleFolders ModuleFolders
      GTypes (t)
  | R = mex Expr
  | R

list @Xs = form [$@Xs]
mtx Xs = form [$@Xs.tail{}{[`[]` @?]}]

export macroexpand 'let_' 'let' 'default_leave_' 'leave' 'case' 'is' 'if' '@' '[]' 't' '\\' 'form'
       'mtx' 'list' 'not' 'and' 'or' 'when' 'less' 'while' 'till' 'dup' 'times' 'map' 'for' 'type'
       'heir' 'named' 'export_hidden' 'export' 'pop' 'push' 'as' 'callcc' 'fin' '|' ';' ',' '$'
       '+' '-' '*' '/' '%' '**' '<' '>' '<<' '>>' '><' '<>' '^' '.' ':' '{}' '<=' '=>' '!!'
       'ffi_begin' 'ffi' 'min' 'max' 'swap' 'supply' 'have' 'source_' 'compile_when' '"'
