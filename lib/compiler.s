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
       | ssa arg_store Base Pos Value
       | if Base >< \E
         then ssa arg_store Base Pos Value
         else ssa lift Base Pos Value // must be copied into parent environment
     Else
       | bad "undefined variable: [X]"

ssa_quote_list_rec Xs =
| [list @(Xs map: X => if list? X then ssa_quote_list_rec X else [_quote X])]

ssa_quote_list K Xs =
| Name = gensym list
| ssa move K Name
| push [Name (ssa_quote_list_rec)] GInits

cstring S = [@S.chars.map{C => C.code} 0]

ssa_cstring Src =
| Name = gensym "b"
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

ssa_quote K X = if text? X then ssa_text K X
                else if list? X then ssa_quote_list K X
                else ssa_expr K X

ssa_resolve Name = [Name GNs]

ssa_fn_body K F Args Body O Prologue Epilogue =
| LocalEnv = if text? Args
             then [[GAll [Args F]] @GEnv]
             else [Args.map{A=>[A F]} @GEnv]
| shade (GBases [[]])
        (GOut [])
        (GNs F)
        (GEnv LocalEnv)
        (GClosure [[]@GClosure])
  | when have Prologue: ssa label GNs
  | SizeVar = "[F]_size"
  | when Prologue
    | if text? Args
      then ssa check_varargs SizeVar Void /*(get_meta O)*/
      else ssa check_nargs Args.size SizeVar Void /*(get_meta O)*/
  | when no K: K <= ssa_var "result"
  | ssa_expr K Body
  | when Epilogue: ssa return K
  | [GOut GClosure.0]

ssa_expr K X =

ctest = 

export ctest gensym
