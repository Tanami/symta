use prelude

GEnv = Void
GOut = Void // where resulting assembly code is stored
GNs = Void // unique name of current function
GInits = Void
GRawInits = Void
GFns = Void
GClosure = Void // other lambdas, this lambda references
GBases = Void

GGensymCount = 0
gensym Name = "[Name]__[GGensymCount!+1]"

GAll = gensym all

ssa @As = | push As GOut
          | Void

get_parent_index Parent =
| P = GClosure.0.locate{E => Parent >< E}
| when have P: leave P
| Parents = GClosure.0
| GClosure.0 <= [@Parents Parent]
| Parents.size

path_to_sym X Es =
| when Es.end: leave Void
| [Head@Tail] = Es
| when Head.0 >< GAll // reference to the whole arglist?
  | Head <= Head.2
  | unless Head.0 >< X: leave (path_to_sym X Tail)
  | when Es^address >< GEnv^address: leave [GAll Void] // argument of the current function
  | leave [GAll (get_parent_index Head.1)]
| P = Head.locate{V => X >< V.0}
| when no P: leave (path_to_sym X Tail)
| when Es^address >< GEnv^address: leave [P Void] // argument of the current function
| [P (get_parent_index Head.P.1)]

ssa_symbol K X Value =
| on (path_to_sym X GEnv)
     [Pos Parent]
       | Base = if have Parent then gensym "B" else \E
       | when have Parent
         | ssa var Base
         | ssa load Base \P Parent
       | when Pos >< GAll
         | when have Value: bad "cant set [X]"
         | ssa add_tag K Base \T_LIST
         | leave Void
       | when no Value: leave (ssa arg_load K Base Pos)
       | if Base >< \E and GBases.size >< 1
         then ssa arg_store Base Pos Value
         else ssa lift Base Pos Value // must be copied into parent environment
     Else
       | bad "undefined variable: [X]"

ssa_quote_list_rec Xs =
| [list @(Xs map: X => if X.is_list then ssa_quote_list_rec X else [_quote X])]

ssa_quote_list K Xs =
| Name = gensym list
| ssa move K Name
| push [Name (ssa_quote_list_rec)] GInits

cstring S = [@S.chars.map{C => C.code} 0]

ssa_cstring Src =
| Name = gensym b
| ssa bytes Name Src^cstring
| Name

ssa_var Name =
| V = gensym Name
| ssa var V
| V

ssa_global Name =
| V = gensym Name
| ssa global V
| V

ssa_text K S =
| BytesName = ssa_cstring S
| Name = ssa_global "s"
| push [text Name BytesName] GRawInits
| ssa move K Name

ssa_quote K X = if X.is_text then ssa_text K X
                else if X.is_list then ssa_quote_list K X
                else ssa_expr K X

ssa_resolve Name = [Name GNs]

ssa_fn_body K F Args Body O Prologue Epilogue =
| LocalEnv = if Args.is_text
             then [[GAll [Args F]] @GEnv]
             else [Args.map{A=>[A F]} @GEnv]
| let GBases   [[]]
      GOut     []
      GNs      F
      GEnv     LocalEnv
      GClosure [[]@GClosure]
  | when have Prologue: ssa label GNs
  | SizeVar = "[F]_size"
  | when Prologue
    | if Args.is_text
      then ssa check_varargs SizeVar Void /*(get_meta O)*/
      else ssa check_nargs Args.size SizeVar Void /*(get_meta O)*/
  | when no K: K <= ssa_var "result"
  | ssa_expr K Body
  | when Epilogue: ssa return K
  | [GOut GClosure.0]

// FIXME:
// check if we really need new closure here, because in some cases we can reuse parent's closure
// a single argument to a function could be passed in register, while a closure would be created if required
// a single reference closure could be itself held in a register
// for now we just capture required parent's closure
ssa_fn Name K Args Body O =
| F = gensym f
| [Body Cs] = ssa_fn_body Void F Args Body O 1 1
| push Body GFns
| NParents = Cs.size
| ssa closure K F NParents
| I = 0
| for C Cs
  | if C^address >< GNs^address //self?
    then ssa store K I \E
    else ssa copy K I \P C^get_parent_index
  | I !+ 1

ssa_if K Cnd Then Else =
| ThenLabel = gensym `then`
| EndLabel = gensym endif
| C = ssa_var cnd
| ssa_expr C Cnd
| ssa branch C ThenLabel
| ssa_expr K Else
| ssa jmp EndLabel
| ssa local_label ThenLabel
| ssa_expr K Then
| ssa local_label EndLabel

ssa_hoist_decls Expr Hoist = // C/C++ style declaration hoisting
| unless Expr.is_list: leave Expr
| on Expr
     [_fn @Xs] | Expr
     [[fn As@Xs] @Vs]
       | if As.is_text
         then | Hoist [As]
              | [_progn [_set As [_list @Vs]]
                        @Xs.map{X => ssa_hoist_decls X Hoist}]
         else | Hoist As
              | [_progn @As.map{A => [_set A Vs^pop]}
                        @Xs.map{X => ssa_hoist_decls X Hoist}]
     Xs | Xs.map{X => ssa_hoist_decls X Hoist}

ssa_let K Args Vals Xs =
| Body = ssa_hoist_decls [_progn @Xs]: Hs =>
         | Args <= [@Args @Hs]
         | Vals <= [@Vals @Hs.map{H => 0}]
| when Args.size >< 0
  | ssa_expr K Body
  | leave Void
| F = gensym f
| [SsaBody Cs] = ssa_fn_body K F Args Body [] 0 0
| NParents = Cs.size
| P = ssa_var p // parent environment
| ssa local_closure P NParents
| I = 0
| for C Cs
  | if C^address >< GNs^address // self?
    then ssa store P I \E
    else ssa copy P I \P C^get_parent_index
  | I !+ 1
| E = ssa_var env
| ssa arglist E Args.size
| I = 0
| for V Vals
  | Tmp = ssa_var tmp
  | ssa_expr Tmp V
  | ssa arg_store E I Tmp
  | I !+ 1
| SaveP = ssa_var save_p
| SaveE = ssa_var save_e
| ssa mave SaveP \P
| ssa move SaveE \E
| ssa move \E E
| ssa move \P P
| for S SsaBody.reverse: push S GOut
| ssa move \P SaveP
| ssa move \E SaveE

is_fn_sym T = T.is_text and T.size > 0 and T.0.is_upcase

ssa_apply K F As IsMethod =
| unless IsMethod: on F [_fn Bs @Body]: leave: ssa_let K Bs As Body
| ssa push_base
| let GBases [[] @GBases]: named block
  | H = ssa_var head
  | ssa_expr H F
  | Vs = As.map{A => | V = ssa_var a
                     | ssa_expr V A
                     | V}
  | E = ssa_var env
  | NArgs = As.size
  | ssa arglist E NArgs
  | I = 0
  | for V Vs
    | ssa arg_store E I V
    | I !+ 1
  | when F^is_fn_sym: leave block: ssa call K H
  | MethodName = NArgs > 0
             and on As [[_quote X @Renamed] @Xs] (X.is_text and X)
  | unless MethodName: leave block: ssa call_tagged_dynamic K H
  | MethodNameBytes = ssa_cstring MethodName
  | M = ssa_global m
  | push [resolve_method M MethodNameBytes] GRawInits
  | if IsMethod
    then ssa call_method K H M
    else ssa call_tagged K H M

ssa_set K Place Value =
| R = ssa_var r
| ssa_expr R Value
| ssa_symbol Void Place R
| ssa move K R

/*
// FIXME: _label should be allowed only inside of _progn
ssa_progn K Xs =
| when Xs.size >< 0: Xs <= [[]]
| D = ssa_var dummy
| for X Xs: on X [_label Name] | GBases <= [[Name @GBases.head] @GBases.tail]
| till Xs.end
  | X = pop Xs
  | when Xs.end: D <= K
  | ssa_expr D X
  | when Xs.end and on X [_label@Zs] 1: ssa move D 'Void'
*/
ssa_expr K X =

ctest = 

export ctest gensym
