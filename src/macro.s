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
| when no Module: bad "couldn't compile [Library]"
| Found = Module^load_library.find{X => X.0 >< Name}
| unless have Found: mex_error "couldn't load `[Name]` from `[Library]`"
| Found.1

expand_list_hole Key Hole Hit Miss = case Hole
  [] | [_if [_mcall Key end] Hit Miss]
  [[`@` Zs]] | expand_hole Key Zs Hit Miss
  [[`@` Zs] @More] | "case: @ in the middle isn't supported"
  [X@Xs] | H = gensym 'X'
         | Hit <= expand_list_hole Key Xs Hit Miss
         | [`if` [_mcall Key end]
                 Miss
                 [let_ [[H [_mcall Key head]]
                        [Key [_mcall Key tail]]]
                   (expand_hole H X Hit Miss)]]

expand_hole Key Hole Hit Miss =
| unless case Hole [X@Xs]
  | when Hole.is_keyword: Hole <= [_quote Hole]
  | leave: if Hole >< '_' then Hit
           else if Hole.is_text then [let_ [[Hole Key]] Hit]
           else [_if ['><' Hole Key] Hit Miss]
| case Hole
  [`>` A B] | expand_hole Key A (expand_hole Key B Hit Miss) Miss
  [in @Xs] | [_if (expand_match Key (map X Xs [X 1]) 0 Void) Hit Miss]
  [not @Xs] | [_if (expand_match Key (map X Xs [X 1]) 0 Void) Miss Hit]
  [bind A B] | G = gensym 'G'
             | [let_ [[G [A Key]]] (expand_hole G B Hit Miss)]
  [`=>` A B] | [let_ [[A.0 Key]]
                 [_if ['|' @B] Hit Miss]]
  [`&` X] | [_if [`><` X Key] Hit Miss]
  [`[]` @Xs] | [_if [_mcall Key is_list] 
                    (expand_list_hole Key Xs Hit Miss)
                    Miss]
  Else | mex_error "bad match case: [Hole]"

expand_match Keyform Cases Default Key =
| when no Key: Key <= gensym 'Key'
| E = gensym end
| D = gensym default
| R = gensym 'R'
| Ys = []
| for Case Cases.reverse
  | Name = gensym c
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

case @Xs = expand_match Xs.0 Xs.tail.groupBy{2} 0 Void

`if` A B C = [_if A B C]
not @Xs = [_if Xs 0 1]
`and` A B = [_if A B 0]
`or` A B =
| V = gensym 'V'
| [let_ [[V A]] [_if V V B]]
when @Xs = [_if Xs.lead Xs.last Void]
unless @Xs = [_if Xs.lead Void Xs.last]

expand_while Head Body =
| L = gensym l
| [_progn [_label L]
          [_if Head
               [_progn Body [_goto L]]
               Void]]

while @As = expand_while As.lead As.last
till @As = expand_while [not As.lead] As.last

times Var Count Body =
| I = if have Var then Var else gensym 'I'
| N = gensym 'N'
| ['|' ['=' [N] Count]
       ['=' [I] [0]]
       [unless [`and` [_eq [_tag N] 0]
                      [_gte N 0]]
         [_fatal 'dup: bad loop count']]
       [while [_lt I N]
         ['|' Body
              [_set I [_add I 1]]]]]

expand_dup Var Count Body =
| I = if have Var then Var else gensym 'I'
| N = gensym 'N'
| Ys = gensym 'Ys'
| ['|' ['=' [N] Count]
       ['=' [I] [0]]
       [unless [`and` [_eq [_tag N] 0]
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
| Xs = gensym 'Xs'
| I = gensym 'I'
| N = gensym 'N'
| ['|' ['=' [Xs] [_mcall Items harden]]
       [Type I [_mcall Xs size]
          ['|' ['=' [Item] [_mcall Xs '.' I]]
               Body]]]

map Item Items Body = expand_map_for dup Item Items Body
for Item Items Body = expand_map_for times Item Items Body

expand_quasiquote O =
| unless O.is_list: leave [_quote O]
| case O
  [`$` X] | X
  Else | ['[]' @(map X O: expand_quasiquote X)]

`\\` O = expand_quasiquote O

expand_form O AGT =
| unless O.is_list: leave
  if O.is_text and not O.is_keyword then O
  else if O.is_text and O.size > 1 and O.0 >< '?' then
    | AG = AGT.O
    | when no AG
      | AG <= gensym O.tail
      | AGT.O <= AG
    | AG
  else [_quote O]
| case O
  [`$` X] | X
  Else | ['[]' @(map X O: expand_form X AGT)]

form O =
| AGT = table
| R = expand_form O AGT
| when AGT.size > 0: R <= [let_ (map [K V] AGT [V [gensym [_quote K.tail]]]) R]
| R

expand_text_splice Xs =
| case Xs
   [X] | when X.is_text: leave [_quote X]
   [] | leave [_quote '']
| As = map X Xs: if X.is_text then [_quote X] else [_mcall X textify_]
| [_mcall [_list @As] text]

`"` @Xs /*"*/ = expand_text_splice Xs

pop O =
| R = gensym 'R'
| [`|` [`=` [R] [_mcall O head]]
       [_set O [_mcall O tail]]
       R]

push Item O = [_set O [_mcall O pre Item]]

let @As =
| when As.size < 2: mex_error "bad let @As"
| Bs = As.lead.groupBy{2}
| Body = As.last
| Gs = map B Bs [(gensym 'G') @B]
| R = gensym 'R'
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
`<` A B = [_mcall A '<' B]
`>` A B = [_mcall A '>' B]
`<<` A B = [_mcall A '<<' B]
`>>` A B = [_mcall A '>>' B]
`><` A B = [_mcall A '><' B]
`<>` A B = [_mcall A '<>' B]
`^` A B = [B A]
`.` A B = if A.is_keyword then
            | Sym = load_symbol A B
            | when Sym.is_macro
              | when B.is_keyword: mex_error "cant reference macro's value in [A].[B]"
              | leave Sym.expander
            | form: let_ ((?R (_import (_quote A) (_quote B)))) ?R
          else if B.is_keyword then ['{}' ['.' A B]]
          else [_mcall A '.' B]
`:` A B = [@A B]
`,` @As = case As
  [[X@Xs] @Ys] | [X Xs @Ys]
  Else | mex_error "invalid arglist to `,`"

expand_method_arg_r A ArgName =
| when A >< '?': leave: ArgName A
| unless A.is_list: leave A
| case A
   [`{}` X Y] | [A.0 (expand_method_arg_r X ArgName) Y]
   [`{}` @Xs] | A
   [`\\` @Xs] | A
   [_quote @Xs] | A
   Else | map X A: expand_method_arg_r X ArgName

expand_method_arg A =
| G = Void
| R = expand_method_arg_r A: X =>
      | when no G: G <= form ?G
      | G
| when have G: A <= form: _fn (G) R
| A

`{}` @Xs = case Xs
  [[`.` A B] @As] | [_mcall A B @(map X As: expand_method_arg X)]
  [[`^` A B] @As] | [B @As A]
  [H @As] | [_mcall H '{}' @(map X As: expand_method_arg X)]
  Else | mex_error "invalid `{}`"

`!!` @As = expand_assign_result As


is_incut X = case X [`@` Xs] 1

`[]` @As =
| IncutCount = As.count{&is_incut}
| when IncutCount >< 0: leave [_list @As]
| when IncutCount >< 1
  | case As.last
    [`@` Xs] | As = As.reverse.tail
             | till As.end: Xs <= [_mcall Xs pre As^pop]
             | leave Xs
| As = map A As: if A^is_incut then A.1 else [_list A]
| [_mcall [_list @As] join]

table @As_ =
| As = As_
| Size = 256
| case As [[`/` size S] @Xs]
  | Size <= S
  | As <= Xs
| T = form ?T
| As <= As.groupBy{2}
| if As.size
  then form: `|` (T = table_ Size)
                 $@(map [K V] As
                   | when K.is_text: K <= form \K
                   | form: T.K <= V)
                 T
  else form: table_ Size

pattern_arg X = not X.is_text or X.is_keyword

`=>` As Body =
| Body <= [`|` Body]
| [A B] = if no As.find{&pattern_arg} then [As Body] else add_pattern_matcher As Body
| [_fn As Body]

//FIXME: move it to compiler.s
mangle_name Name =
| Cs = Name.chars
| Rs = map C Cs
  | N = C.code
  | if   ('a'.code << N and N << 'z'.code)
      or ('A'.code << N and N << 'Z'.code)
      or ('0'.code << N and N << '9'.code)
    then C
    else "_[N.x.pad{2 0}]"
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
    Else | [_list]
| case Args
   [[`@` All]] | Args <= All
   Else | Gs = map A Args: gensym 'A'
        | // FIXME: value gets duplicated - potentially exponential code growth
        | for G Gs: Body <= expand_match G [[Args^pop Body]] Default Void
        | Args <= Gs
| [Args Body]

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
| O = gensym 'O'
| Ys = map [I B] Bs.enum: [B [_mcall O '.' I]]
| when have XsVar: Ys <= [[XsVar [_mcall O drop Bs.size]] @Ys]
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
| Gs = map F Fields: gensym 'A'
| O = gensym 'O'
| V = gensym 'V'
| [[`=` ["new_[Name]" @Gs] [_data Name @Gs]]
   [`=` [[`.` Name "is_[Name]"]] 1]
   [`=` [[`.` '_' "is_[Name]"]] 0]
   @(map [I F] Fields.enum [`=` [[`.` Name F]]  [_dget 'Me' I]])
   @(map [I F] Fields.enum [`=` [[`.` Name "set_[F]"] V]  [_dset 'Me' I V]])]

expand_block_item_method Type Name Args Body =
| case Args
  [[`@` As]]
    | G = form ?As
    | Body <= form: let_ (($\Me (_mcall G head))
                          (As (_mcall G tail)))
                      Body
    | Args <= G
  Else | Args <= [\Me @Args]
| Body <= form: default_leave_ Name $(expand_named Name Body)
| [Void [_dmet Name Type [_fn Args [_progn [_mark "[Type].[Name]"] Body]]]]

expand_block_item Expr =
| Y = case Expr
  [data Name @Fields]
    | Ys = map X (expand_block_item_data Name Fields): expand_block_item X
    | leave Ys.join
  [`=` [`!!` [`!` Place]] Value] | [Void (expand_assign Place Value)]
  [`=` [[`.` Type Method] @Args] Body] | expand_block_item_method Type Method Args Body
  [`=` [[`[]` @Bs]] Value] | leave: expand_destructuring Value Bs
  [`=` [Name @Args] Value]
    | unless Name.is_text: mex_error "[Name] is not text in `=`"
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
| Dummy = gensym 'D'
| All = gensym 'A'
| Key = gensym 'K'
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
| unless Ms.end: push Ms.reverse^make_multimethod Ys
| Xs <= Ys.reverse
| Xs <= map X Xs: expand_block_item X
| Xs <= Xs.join
| R = []
| for X Xs.reverse
  | R <= case X [A B]
         | if have A
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

callcc F =
| K = gensym 'K'
| R = gensym 'R'
| [`|` [`=` [K] [_setjmp]]
       [`if` [_mcall K is_int]
             [F [_fn [R] [_longjmp K [_list R]]]]
             [_mcall K '.' 0]]]

fin Finalizer Body =
| B = gensym b
| F = gensym f
| R = gensym 'R'
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
| R = form @| F = ffi_load FFI_Lib \Symbol
            | Name $@ANs = | ?X = \F
                           | form (_ffi_call \(Result $@ATs) FFI_Package.?X $@ANs)
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

normalize_nesting O =
| case O [X] | if X.is_keyword then O else normalize_nesting X
         X | X

mex_normal X Xs =
| when GExpansionDepth > GExpansionDepthLimit: mex_error "macroexpansion depth exceed at [[X@Xs]]"
| Macro = when X.is_keyword: GMacros.X
| case X [`.` Library Name]: when Library.is_keyword and Name.is_keyword:
  | Sym = load_symbol Library Name
  | when Sym.is_macro: Macro <= Sym
| when no Macro
  | Y = X^mex
  | if (X.is_list and not Y.is_list) or (X.is_text and X <> Y)
    then leave: mex [Y@Xs]
    else leave [Y @(map X Xs: if X.is_keyword then [_quote X] else mex X)]
| Expander = Macro.expander
| NArgs = Expander.nargs
| when NArgs >> 0 and NArgs <> Xs.size:
  | [Row Col Orig] = GSrc
  | mex_error "bad number of args to macro [Macro.name]"
| mex Xs.apply{Expander}

mex Expr =
| when no GMacros: mex_error 'lib_path got clobbered again'
| Expr <= normalize_nesting Expr
| when Expr.is_text and not Expr.is_keyword and have GMacros.Expr: Expr <= GMacros.Expr.expander
| unless Expr.is_list: leave Expr
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
         | let GSrc (if have Src then Src else GSrc)
           | Result = mex_normal X Xs
           | when have Src and Result.is_list: Result <= new_meta Result Src
           | Result

macroexpand Expr Macros ModuleCompiler =
| let GMacros Macros
      GExpansionDepth 0
      GExports []
      GModuleCompiler ModuleCompiler
  | R = mex Expr
  | R

export macroexpand 'let_' 'let' 'default_leave_' 'leave' 'case' 'if' '@' '[]' 'table' '\\' 'form'
       'not' 'and' 'or' 'when' 'unless' 'while' 'till' 'dup' 'times' 'map' 'for'
       'named' 'export_hidden' 'export' 'pop' 'push' 'callcc' 'fin' '|' ';' ','
       '+' '-' '*' '/' '%' '<' '>' '<<' '>>' '><' '<>' '^' '.' ':' '{}' '<=' '=>' '!!' '"'
       'ffi_begin' 'ffi'
