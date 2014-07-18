use prelude

`+` A B = [_mcall A '+' B]
`*` A B = [_mcall A '*' B]
`/` A B = [_mcall A '/' B]
`%` A B = [_mcall A '%' B]

load_macros Library = Library^load_library.keep{[K V]=>V.is_macro}.as_table

GMacros = Void
GDefaultLeave = Void

normalize_matryoshka O =
| on O [X] | if X.is_keyword then O else normalize_matryoshka X
     X | X

mex Expr =
| Expr <= normalize_matryoshka Expr
| unless Expr.is_list: leave Expr
| on Expr
  [_fn As Body] | [_fn As Body^mex]
  [_set Place Value] | [_set Place (if Value.is_keyword then [_quote Value] else mex Value)]
  [_label Name] | Expr
  [_goto Name] | Expr
  [_quote X] | Expr
  [_nomex X] | X // no macroexpand
  [`&` O] | if O.is_keyword then O else [O^mex]
  [] | Expr
  [X@Xs]
    | Macro = GMacros.X
    | if have Macro
      then mex Xs.apply{Macro.expander}
      else [X^mex @(map X Xs: if X.is_keyword then [_quote X] else mex X)]

macroexpand Expr Macros =
| let GMacros Macros
  | mex Expr

export macroexpand //'+' '*' '/' '%'
