use prelude

GMacros = Void
GDefaultLeave = Void


is_var_sym X = X.is_text and not X.is_keyword

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
  Else | bad "hole: [Hole]"

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

expand_quasiquote O =
| unless O.is_list: leave [_quote O]
| case O
  [`$` X] | X
  Else | ['[]' @(map X O: expand_quasiquote X)]

`\\` O = expand_quasiquote O

pop O =
| R = gensym 'R'
| [`|` [`=` [R] [_mcall O head]]
       [_set O [_mcall O tail]]
       R]

push Item O = [_set O [_mcall O pre Item]]

let Bs Body =
| Gs = map B Bs [(gensym 'G') @B]
| R = gensym 'R'
| [let_ [[R 0] @(map G Gs [G.0 G.1])]
    @(map G Gs [_set G.1 G.2])
    [_set R Body]
    @(map G Gs [_set G.1 G.0])
    R]

`+` A B = [_mcall A '+' B]
`-` @As = case As
  [`-` A] | [_mcall A neg]
  [`-` A B] | [_mcall A '-' B]
`*` A B = [_mcall A '*' B]
`/` A B = [_mcall A '/' B]
`%` A B = [_mcall A '%' B]
`<` A B = [_mcall A '<' B]
`>` A B = [_mcall A '>' B]
`<<` A B = [_mcall A '<<' B]
`>>` A B = [_mcall A '>>' B]
`><` A B = [_mcall A '><' B]
`<>` A B = [_mcall A '<>' B]

pattern_arg X = not X.is_text

`=>` As Body =
| Body <= [`|` Body]
| [A B] = if As.find{&pattern_arg} then add_pattern_matcher As Body else [As Body]
| [_fn As Body]

//FIXME: move it to compiler.s
mangle_name Name =
| Cs = Name.chars
| Rs = map C Cs
  | N = C.code
  | if   'a'.code << N or N << 'z'.code
      or 'A'.code << N or N << 'Z'.code
      or '0'.code << N or N << '9'.code
    then C
    else "_[N.x.pad{2 0}]"
| [_ @Rs].unchars

result_and_label Name =
| Mangled = mangle_name Name
| ["ReturnOf[Mangled]_" "end_of[Mangled]_"]

expand_named Name Body =
| [R End] = result_and_label Name
| [let_ [[R 0]]
    [_set R Body]
    [_label End]
    R]

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
| [A B] = if Args.find{&pattern_arg} then add_pattern_matcher Args Body else [Args Body]
| B <= [default_leave_ Name (expand_named Name B)]
| [Name [_fn A [_progn [_mark Name] B]]]

expand_destructuring Value Bs =
| XsVar = Void
| when Bs.size: case Bs.last.1 [`@` X]
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

expand_assign_result As =
| Ys = map A As A
| V = Void
| P = As.locate{X => case X [`!` X]
                     | V <= X
                     | 1}
| when no P: bad '!!: no ! in [As]'
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
| Body <= [default_leave_ Name (expand_named Name Body)]
| [Void [_dmet Name Type [_fn [\Me @Args] [_progn [_mark "[Type].[Name]"] Body]]]]

expand_block_item X =
| Y = case X
  [data Name @Fields]
    | Ys = map X (expand_block_item_data Name Fields): expand_block_item X
    | leave Ys.join
  [`=` [`!!` [`!` Place]] Value] | [Void (expand_assign Place Value)]
  [`=` [[`.` Type Method] @Args] Body] | expand_block_item_method Type Method Args Body
  [`=` [[`[]` @Bs]] Value] | leave: expand_destructuring Value Bs
  [`=` [Name @Args] Value]
    | unless Name.is_text: bad "`=`: [Name] is not text"
    | if Name.is_keyword then expand_block_item_fn Name Args Value else [Name Value]
  Else | [Void X]
| [Y]

make_multimethod Xs =
| when case Xs [[`=>` As Expr]] (As.size >< 0 or As.0^is_var_sym)
  | leave Xs.0
| Dummy = gensym 'D'
| All = gensym 'A'
| Key = gensym 'K'
| Cases = map X Xs: case X
    [`=>` As Expr] 
      | when As.size >< 0: bad 'prototype doesnt support no args multimethods'
      | [As.0 [_fn (if As.0^is_var_sym then As else [Dummy As.tail]) Expr]]
| Sel = expand_match [_mcall All '.' 1] Cases [no_method_ Key] Key
| [_fn All [_mcall All apply Sel]]

expand_block Xs =
| leave [_progn @Xs]
| when Xs.size >< 1 or not case Xs.0 [`=` @Zs] 1: leave Xs.0
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

let_ @As = [_call [_fn (map B As.0 B.0) [_progn @As.tail]]
                  @(map B As.0 B.1)]

default_leave_ Name Body = let GDefaultLeave Name [_nomex Body^mex]

leave @As = case As
  [Name Value] | expand_leave Name Value
  [Value] | when no GDefaultLeave: bad "missing default leave"
          | expand_leave GDefaultLeave Value
  Else | bad "leave syntax"

//load_macros Library = Library^load_library.keep{[K V]=>V.is_macro}.as_table




normalize_matryoshka O =
| case O [X] | if X.is_keyword then O else normalize_matryoshka X
         X | X

mex Expr =
| Expr <= normalize_matryoshka Expr
| unless Expr.is_list: leave Expr
| case Expr
  [_fn As Body] | [_fn As Body^mex]
  [_set Place Value] | [_set Place (if Value.is_keyword then [_quote Value] else mex Value)]
  [_label Name] | Expr
  [_goto Name] | Expr
  [_quote X] | Expr
  [_nomex X] | X // no macroexpand
  [`&` O] | if O.is_keyword then O else [O^mex]
  [] | Expr
  [X@Xs]
    | Macro = when X.is_keyword: GMacros.X
    | if have Macro
      then mex Xs.apply{Macro.expander}
      else [X^mex @(map X Xs: if X.is_keyword then [_quote X] else mex X)]

macroexpand Expr Macros =
| let GMacros Macros
  | mex Expr


export macroexpand 'let_' 'let' 'default_leave_' 'leave' 'case' 'if' '|' '+' '*' '/' '%'
