GExpansionDepth = Void
GExpansionDepthLimit = 1000
GMacros = Void
GDefaultLeave = Void
GModuleCompiler = Void
GSrc = [0 0 unknown]

mex_error Message =
| [Row Col Orig] = GSrc
| bad "[Orig]:[Row],[Col]: [Message]"

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
               then | I !+ 1
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
  [X@Xs] | H = @rand 'X'
         | Hit <= expand_list_hole Key Xs Hit Miss
         | [`if` [_mcall Key end]
                 Miss
                 [let_ [[H [_mcall Key head]]
                        [Key [_mcall Key tail]]]
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

expand_hole Key Hole Hit Miss =
| less case Hole [X@Xs]
  | when Hole >< '_': leave Hit
  | when Hole.is_keyword: Hole <= [_quote Hole]
  | leave: if Hole.is_text then [let_ [[Hole Key]] Hit]
           else [_if ['><' Hole Key] Hit Miss]
| case Hole
  [`>` A B] | expand_hole Key A (expand_hole Key B Hit Miss) Miss
  [in @Xs] | [_if (expand_match Key (map X Xs [X 1]) 0 Void) Hit Miss]
  [not @Xs] | [_if (expand_match Key (map X Xs [X 1]) 0 Void) Miss Hit]
  [bind A B] | G = @rand 'G'
             | [let_ [[G [A Key]]] (expand_hole G B Hit Miss)]
  [`=>` A B] | [let_ [[A.0 Key]]
                 [_if ['|' @B] Hit Miss]]
  [`&` X] | form: _if X >< Key Hit Miss
  [`[]` @Xs] | P = Xs.locate{&0[`/`@_]=>1}
             | when got P: Xs <= [@Xs.take{P} [`@` [`/` @Xs.drop{P}]]]
             | [_if [_mcall Key is_list]
                    (expand_list_hole Key Xs Hit Miss)
                    Miss]
  [`/` @Xs] | expand_hole_keywords Key Hit Xs
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

min A B = form | ~A = A
               | ~B = B
               | if ~A < ~B then ~A else ~B
max A B = form | ~A = A
               | ~B = B
               | if ~A > ~B then ~A else ~B

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
| AGT = table
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
          else if B.is_keyword then ['{}' ['.' A B]]
          else [_mcall A '.' B]
`:` A B = [@A B]
`,` @As = case As
  [[X@Xs] @Ys] | [X Xs @Ys]
  Else | mex_error "invalid arglist to `,`"

init Var Default = form | when (no Var) (`<=` (Var) Default)
                        | Var

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
   [`{}` X Y] | [A.0 (expand_method_arg_r X FX FY) Y]
   [`{}` @Xs] | A
   [`\\` @Xs] | A
   [_quote @Xs] | A
   Else | map X A: expand_method_arg_r X FX FY

expand_method_arg Expr =
| X = Void
| Y = Void
| R = expand_method_arg_r Expr (N => init X: form ~X) (N => init Y: form ~Y)
| As = [X Y].skip{Void}
| when As.size: Expr <= form: _fn As R
| Expr

`{}` H @As =
| As = map X As: expand_method_arg X
| case H
  [`.` A B] | [_mcall A B @As]
  [`^` A B] | [B @As A]
  Else | if H.is_keyword then [H @As] else [_mcall H '{}' @As]

`!!` @As = expand_assign_result As

is_incut X = case X [`@` Xs] 1

`[]` @As =
| IncutCount = As.count{&is_incut}
| when IncutCount >< 0: leave [_list @As]
| when IncutCount >< 1
  | case As.last
    [`@` Xs] | As = As.flip.tail
             | till As.end: Xs <= [_mcall Xs pre As^pop]
             | leave Xs
| As = map A As: if A^is_incut then A.1 else [_list A]
| [_mcall [_list @As] join]

table @As_ =
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
| Default = case Args
    [[`&` D] @Tail]
      | Args <= Tail
      | D
    Else | form: _fatal 'couldnt match args list'
| case Args
   [[`@` All]] | Args <= All
   Else | G = @rand 'As'
        | Body <= expand_match G [[['[]' @Args] Body]] Default Void
        | Args <= G
| [Args Body]

pattern_arg X = not X.is_text or X.is_keyword

`=>` As Body =
| Body <= [`|` Body]
| [A B] = if no As.find{&pattern_arg} then [As Body] else add_pattern_matcher As Body
| [_fn A B]

expand_block_item_fn Name Args Body =
| KName = "_k_[Name]"
| [A B] = if no Args.find{&pattern_arg} then [Args Body] else add_pattern_matcher Args Body
| B <= [default_leave_ Name (expand_named Name B)]
| [Name [_fn A [_progn [_mark Name] B]]]

expand_destructuring Value Bs =
| XsVar = Void
| when Bs.size: case Bs.last [`@` X]
  | XsVar <= X
  | Bs <= Bs.lead
| O = @rand 'O'
| Ys = map [I B] Bs.i: [B [_mcall O '.' I]]
| when got XsVar: Ys <= [[XsVar [_mcall O drop Bs.size]] @Ys]
| [[O Value] @Ys]

expand_assign Place Value =
| case Place
  [`.` Object Field] | if Field.is_keyword
                       then [_mcall Object "set_[Field]" Value]
                       else [_mcall Object "!" Field Value]
  Else | [_set Place Value]

`<=` Place Value = expand_assign Place.0 Value

expand_assign_result As =
| Ys = map A As A
| V = Void
| P = As.locate{X => case X [`!` X]
                     | V <= X
                     | 1}
| when no P: mex_error 'invalid !! - no ! in [As]'
| Ys.P <= V
| expand_assign V Ys

expand_block_item_data Name Fields =
| Gs = map F Fields: @rand 'A'
| O = @rand 'O'
| V = @rand 'V'
| [[`=` ["new_[Name]" @Gs] [_data Name @Gs]]
   [`=` [[`.` Name "is_[Name]"]] 1]
   [`=` [[`.` '_' "is_[Name]"]] 0]
   @(map [I F] Fields.i [`=` [[`.` Name F]]  [_dget 'Me' I]])
   @(map [I F] Fields.i [`=` [[`.` Name "set_[F]"] V]  [_dset 'Me' I V]])]

expand_block_item_method Type Name Args Body =
| Body <= form: default_leave_ Name $(expand_named Name Body)
| [Void [_dmet Name Type [`=>` [\Me @Args] [_progn [_mark "[Type].[Name]"] Body]]]]

expand_block_item Expr =
| Y = case Expr
  [data Name @Fields]
    | Ys = map X (expand_block_item_data Name Fields): expand_block_item X
    | leave Ys.join
  [`=` [`!!` [`!` Place]] Value] | [Void (expand_assign Place Value)]
  [`=` [[`.` Type Method] @Args] Body] | expand_block_item_method Type Method Args Body
  [`=` [[`[]` @Bs]] Value] | leave: expand_destructuring Value Bs
  [`=` [Name @Args] Value]
    | less Name.is_text: mex_error "[Name] is not text in `=`"
    | if Name.is_keyword then expand_block_item_fn Name Args Value else [Name Value]
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
| Dummy = @rand 'D'
| All = @rand 'A'
| Key = @rand 'K'
| Cases = map X Xs: case X
    [`=>` As Expr] 
      | when As.size >< 0: mex_error 'prototype doesnt support no args multimethods'
      | [As.0 [_fn (if As.0^is_var_sym then As else [Dummy As.tail]) Expr]]
| Sel = expand_match [_mcall All '.' 1] Cases [no_method_ Key] Key
| [_fn All [_mcall All apply Sel]]

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
| for X Xs.flip
  | R <= case X [A B]
         | if got A
           then if A.is_keyword
                then [[_set A B] @R]
                else [[let_ [[A B]] (if R.size then [_progn @R] else Void)]]
           else [B @R]
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

FFI_Package = Void
FFI_Lib = Void

ffi_begin Package Lib =
| FFI_Package <= Package
| FFI_Lib <= Lib
| 0

expand_ffi Name Result Symbol Args =
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
  | when got Xs.locate{&0[`@` X]=>1}
    | when X >< _mcall: leave: mex: form: _mcall [$Xs.0 $@Xs.drop{2}] apply_method (_method $Xs.1)
    | when X.is_keyword: X <= form &X
    | leave: mex: form [$@Xs].apply{X}
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
  [_quote X] | Expr
  [_nomex X] | X // no macroexpand
  [`&` O] | if O.is_keyword then O else [O^mex]
  [] | Expr
  [X@Xs] | Src = when Expr.is_meta: Expr.info_ 
         | let GSrc (if got Src then Src else GSrc)
           | Result = mex_normal X Xs
           | when got Src and Result.is_list: Result <= new_meta Result Src
           | Result

macroexpand Expr Macros ModuleCompiler =
| let GMacros Macros
      GExpansionDepth 0
      GExports []
      GModuleCompiler ModuleCompiler
  | R = mex Expr
  | R

export macroexpand 'let_' 'let' 'default_leave_' 'leave' 'case' 'if' '@' '[]' 'table' '\\' 'form'
       'not' 'and' 'or' 'when' 'less' 'while' 'till' 'dup' 'times' 'map' 'for'
       'named' 'export_hidden' 'export' 'pop' 'push' 'as' 'callcc' 'fin' '|' ';' ',' 'init'
       '+' '-' '*' '/' '%' '**' '<' '>' '<<' '>>' '><' '<>' '^' '.' ':' '{}' '<=' '=>' '!!'
       'ffi_begin' 'ffi' 'min' 'max' '"'
