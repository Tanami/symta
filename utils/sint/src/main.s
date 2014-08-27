use reader eval

Env = table 100
Env.'Env_' <= Env
Env.'Uses_' <= [core prelude]
Env.'Last_' <= Void
Env.'X' <= 123

Done = 0

till Done
| say_ '> '
| case (get_line)^parse
  [exit] | Done <= 1
  [use @Xs] | Env.'Uses_' <= [@Xs @Env.'Uses_'].uniq
  [`=` [Var] Expr] | Env.Var <= eval Expr Env
  Expr | say: eval Expr Env
