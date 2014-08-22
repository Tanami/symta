use prelude reader

GExpansionDepth = Void
GExpansionDepthLimit = 100
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
  Else | bad "dup [As]"

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

expand_text_splice Text =
| Xs = Text.chars
| As = []
| S = Xs.locate{X => X >< '['}
| when no S: leave [_quote Text]
| while have S
  | push [_quote Xs.take{S}.unchars] As
  | Xs <= Xs.drop{S+1}
  | S <= Xs.locate{X => X >< ']'}
  | when no S: bad 'unterminated ['
  | push [_mcall Xs.take{S}.unchars^parse textify_] As
  | Xs <= Xs.drop{S+1}
  | S <= Xs.locate{X => X >< '['}
| when Xs.size > 0: push [_quote Xs.unchars] As
| [_mcall [_list @As.reverse] unchars]

`"` Text /*"*/ = expand_text_splice Text

pop O =
| R = gensym 'R'
| [`|` [`=` [R] [_mcall O head]]
       [_set O [_mcall O tail]]
       R]

push Item O = [_set O [_mcall O pre Item]]

let @As =
| when As.size < 2: bad 'let @As'
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
  Else | bad "`-` got wrong number of args: [As]"
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
`.` A B = if B.is_keyword then ['{}' ['.' A B]] else [_mcall A '.' B]
`:` A B = [@A B]
`{}` @Xs = case Xs
  [[`.` A B] @As] | [_mcall A B @As]
  [[`^` A B] @As] | [B @As A]
  [H @As] | [_mcall H '{}' @As]
  Else | bad "`{}` [Xs]"

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

let_ @As = [_call [_fn (map B As.0 B.0) [_progn @As.tail]]
                  @(map B As.0 B.1)]

default_leave_ Name Body = let GDefaultLeave Name [_nomex Body^mex]

leave @As = case As
  [Name Value] | expand_leave Name Value
  [Value] | when no GDefaultLeave: bad "missing default leave"
          | expand_leave GDefaultLeave Value
  Else | bad "leave syntax"

export @Xs =
| Xs = map X Xs: case X
        [`\\` N] | [_list [_quote N] [new_macro [_quote N] [`&` N]] ]
        Else | [_list [_quote X] [`&` X]]
| [_list @Xs]

//load_macros Library = Library^load_library.keep{[K V]=>V.is_macro}.as_table

normalize_nesting O =
| case O [X] | if X.is_keyword then O else normalize_nesting X
         X | X

GSrc = [0 0 unknown]

mex Expr =
| when no GMacros: bad 'lib_path got clobbered again'
| Expr <= normalize_nesting Expr
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
  [X@Xs]
    | Src = when Expr.is_meta: Expr.info_ 
    | let GSrc (if have Src then Src else GSrc)
      | Macro = when X.is_keyword: GMacros.X
      | Result = if no Macro
        then [X^mex @(map X Xs: if X.is_keyword then [_quote X] else mex X)]
        else | Expander = Macro.expander
             | NArgs = Expander.nargs
             | when NArgs >> 0 and NArgs <> Xs.size:
               | [Row Col Orig] = GSrc
               | bad "[Orig]:[Row],[Col]: bad number of args to macro [Expr]"
             | mex Xs.apply{Expander}
      | when have Src and Result.is_list: Result <= new_meta Result Src
      | Result
macroexpand Expr Macros =
| let GMacros Macros
      GExpansionDepth 0
  | R = mex Expr
  | R

export macroexpand 'let_' 'let' 'default_leave_' 'leave' 'case' 'if' '[]' '\\'
       'not' 'and' 'or' 'when' 'unless' 'while' 'till' 'dup' 'times' 'map' 'for'
       'named' 'export' 'pop' 'push'
       '|' '+' '-' '*' '/' '%' '<' '>' '<<' '>>' '><' '<>' '^' '.' ':' '{}' '<=' '=>' '!!' '"'
