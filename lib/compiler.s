use prelude

GEnv = Void
GOut = Void // where resulting assembly code is stored
GNs = Void // unique name of current function
GInits = Void
GRawInits = Void
GFns = Void
GClosure = Void // other lambdas, this lambda references
GBases = Void

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
| [list @(map X Xs: if X.is_list then ssa_quote_list_rec X else [_quote X])]

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
| Name = ssa_global s
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
  | when no K: K <= ssa_var result
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
| for [I C] Cs.enum: if C^address >< GNs^address // self?
                     then ssa store K I \E
                     else ssa copy K I \P C^get_parent_index

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
| for [I C] Cs.enum: if C^address >< GNs^address // self?
                     then ssa store P I \E
                     else ssa copy P I \P C^get_parent_index
| E = ssa_var env
| ssa arglist E Args.size
| for [I V] Vals.enum
  | Tmp = ssa_var tmp
  | ssa_expr Tmp V
  | ssa arg_store E I Tmp
| SaveP = ssa_var save_p
| SaveE = ssa_var save_e
| ssa mave SaveP \P
| ssa move SaveE \E
| ssa move \E E
| ssa move \P P
| for S SsaBody.reverse: push S GOut
| ssa move \P SaveP
| ssa move \E SaveE

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
  | for [I V] Vs.enum: ssa arg_store E I V
  | when F.is_keyword: leave block: ssa call K H
  | MethodName = NArgs and on As [[_quote X @Renamed] @Xs] (X.is_text and X)
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

// FIXME: _label should be allowed only inside of _progn
ssa_progn K Xs =
| when Xs.end: Xs <= [[]]
| D = ssa_var dummy
| for X Xs: on X [_label Name] | GBases <= [[Name @GBases.head] @GBases.tail]
| till Xs.end
  | X = pop Xs
  | when Xs.end: D <= K
  | ssa_expr D X
  | when Xs.end and on X [_label@Zs] 1: ssa move D 'Void'

GUniquifyStack = Void

uniquify_form Expr =
| on Expr
  [_fn As @Body]
    | Bs = if As.is_text then [As] else As
    | Rs = Bs.map{B => [B B^gensym]}
    | let GUniquifyStack [Rs @GUniquifyStack]
      | Bs = Rs.map{R => R.1}
      | Bs = if As.is_text then Bs.0 else Bs
      | [_fn Bs @Body.map{&uniquify_expr}]
  [_quote X] Expr
  [_label X] Expr
  [_goto X] Expr
  [_call @Xs] Xs^uniquify_form
  Xs | on Xs [[_fn As @Body] @Vs]
             | when not As.is_text and As.size <> Vs.size:
               | bad "invalid number of arguments in [Expr]"
     | Xs.map{&uniquify_expr}

is_special_sym X = X.is

uniquify_name S = for Closure GUniquifyStack: for X Closure: when X.0 >< S: leave X.1

uniquify_atom Expr =
| unless Expr.is_text: leave Expr
| unless Expr.size and Expr.0 >< _: leave Expr
| Renamed = uniquify_name Expr
| when no Renamed: bad "undefined variable: [Expr]"
| Renamed

uniquify_expr Expr = if Expr.is_list
                     then uniquify_form Expr
                     else uniquify_atom Expr

uniquify Expr = let GUniquifyStack []: uniquify_expr Expr

ssa_fixed K Op A B =
| AV = ssa_var a
| BV = ssa_var b
| ssa_expr AV A
| ssa_expr BV B
| ssa Op K AV BV

ssa_list K Xs =
| unless Xs.size: leave: ssa move K 'Empty'
| L = ssa_var l
| ssa arglist L Xs.size
| for [I X] Xs.enum
  | R = ssa_var r
  | ssa_expr R X
  | ssa arg_store L I R
| ssa add_tag K L \T_LIST

ssa_data K Type Xs =
| Size = Xs.size
| BytesName = ssa_cstring Type.1
| TypeVar = ssa_global t
| let GOut []
  | ssa type TypeVar BytesName BytesName Size
  | for X GOut.reverse: push X GRawInits
| ssa data K TypeVar Size
| Tmp = ssa_var v
| for [I X] Xs.enum
  | ssa_expr Tmp X
  | ssa dinit K I Tmp

ssa_dget K Src Off =
| unless Off.is_int: bad "dget: offset must be integer"
| S = ssa_var s
| ssa_expr S Src
| ssa dget K S Off

ssa_dset K Dst Off Value =
| unless Off.is_int: bad "dset: offset must be integer"
| D = ssa_var d
| ssa_expr D Dst
| ssa_expr K Value
| ssa dset D Off K

ssa_dmet K MethodName TypeName Handler =
| MethodNameBytes = ssa_cstring MethodName.1
| MethodVar = ssa_global m
| push [resolve_method MethodVar MethodNameBytes] GRawInits
| TypeNameBytes = ssa_cstring TypeName.1
| TypeVar = ssa_global t
| push [resolve_type TypeVar TypeNameBytes] GRawInits
| H = ssa_var h
| ssa_expr H Handler
| ssa dmet MethodVar TypeVar H
| ssa move K 0

ssa_import K Lib Symbol =
| Lib <= Lib.1
| Symbol <= Symbol.1
| G = ssa_global i
| push [import G Lib Symbol Lib^ssa_cstring Symbol^ssa_cstring] GRawInits
| ssa move K G

ssa_label Name = ssa local_label Name

ssa_goto Name =
| N = GBases.locate{B => have B.locate{X => X><Name}}
| when no N: bad "cant find label [Name]"
| for I N.i
  | ssa gc 0 // FIXME: have to GC, simple pop_base wont LIFT
  | ssa pop_base
| ssa jmp Name

ssa_mark Name =
| V = ssa_var x
| ssa_text V Name.1
| ssa mark V

ssa_form K Xs = on Xs
  [_fn As Body] | ssa_fn n^gensym K As Body Xs
  [_if Cnd Then Else] | ssa_if K Cnd Then Else
  [_quote X @Xs] | ssa_quote K X
  [_set Place Value] | ssa_set K Place Value
  [_progn @Xs] | ssa_progn K Xs
  [_label Name] | ssa_label Name
  [_goto Name] | ssa_goto Name
  [_mark Name] | ssa_mark Name
  [_data Type @Xs] | ssa_data K Type Xs
  [_dget Src Index] | ssa_dget K Src Index
  [_dset Dst Index Value] | ssa_dset K Dst Index Value
  [_dmet Method Type Handler] | ssa_dmet K Method Type Handler
  [_mcall F @As] | ssa_apply K F As 1
  [_list @Xs] | ssa_list K Xs
  [_import Lib Symbol] | ssa_import K Lib Symbol
  //[_add A B] | ssa_fixed K fixed_add A B
  [F @As] | ssa_apply K F As 0
  [] | ssa_atom K Void
  Else | bad "special form: [Xs]"

ssa_atom K X =
| if X.is_int then ssa fixnum K X
  else if X.is_text then ssa_symbol K X Void
  else if X >< Void then ssa move K 'Void'
  else if X.size >< 0 then ssa move K "Empty"
  else bad "atom: [X]"

ssa_expr K X = if X.is_list then ssa_form K X else ssa_atom K X

GLibFolder = "/Users/nikita/Documents/git/symta/lib/"

// FIXME: do caching
get_lib_exports LibName =
| LibFile = "[GLibFolder][LibName].s"
| Text = //load_text_file LibFile
| Expr = // normalize: read: text
| on Expr.last [export @Xs] | Xs.skip{X => on X [`\\` X]}
               Else | Void

produce_ssa Entry Expr =
| let GOut []
      GFns []
      GInits []
      GRawInits []
      GClosure []
  | ssa entry Entry
  | R = ssa_var result
  | uniquify Expr!
  | Ssa = ssa_expr R Expr
  | ssa return R
  | InitLabels = []
  | GInits <= map [Name Expr] GInits:
    let GInits []
        GClosure []
    | L = "init_[Name]"
    | push L InitLabels
    | ssa label L
    | uniquify Expr!
    | ssa_expr Name Expr
    | ssa return_no_gc Name
    | ssa global Name
  | ssa entry setup
  | for X GRawInits.reverse: push X GOut
  | for L InitLabels: ssa gosub L
  | ssa return_no_gc 0
  | Rs = [GOut@GFns].reverse.join.reverse
  //| Rs <= peephole_optimize Rs
  | Rs


GCompiled = Void

c Statement = push Statement GCompiled
cnorm [X@Xs] = c "  [X.upcase]([(map X Xs X.as_text).infix{','}.unchars]);"

ssa_to_c Xs = let GCompiled []
| Statics = []
| Decls = []
| Imports = table 256
| c 'BEGIN_CODE'
| for X Xs: on X
  [entry Name] | c "ENTRY([Name])"
  [label Name] | push "DECL_LABEL([Name])" Decls
               | c "LABEL([Name])"
  [global Name] | push "static void *[Name];" Decls
  [alloc_closure Place Name Size] | push "#define [Name]_size [Size]" Decls
                                  | cnorm X
  [type Place Name TagName Size]
    | TName = gensym n
    | c "  RESOLVE_TYPE([Place],[Name]);"
    | c "  VAR([TName]);"
    | c "  TEXT([TName],[TagName]);"
    | c "  SET_TYPE_SIZE_AND_NAME((intptr_t)[Place],[Size],[TName]);"
  [import Dst Lib Symbol LibCStr SymbolCStr]
    | Key = "[Lib]::[Symbol]"
    | Import = Imports.Key
    | LibExports = Imports.Lib
    | if have Import
      then c "  MOVE([Dst], [Import]);"
      else | when no LibExports
             | LibExports <= gensym n
             | c "  void *[LibExports] = api->load_lib(api,(char*)([LibCStr]));"
             | Imports.Lib <= LibExports
           | SymbolText = gensym s
           | c "  VAR([SymbolText]);"
           | c "  TEXT([SymbolText],[SymbolCStr]);"
           | c "  [Dst] = api->find_export(api,[SymbolText],[LibExports]);"
           | Imports.Key <= Dst
  [bytes Name Xs]
    | Brackets = '[]'
    | Values = (map X Xs X.as_text).infix{','}.unchars
    | push "static uint8_t [Name][Brackets] = {[Values]};" Decls
  Else | cnorm X //FIXME: check if it is known and has correct argnum
| c 'END_CODE'
| GCompiled <= GCompiled.reverse
| for D Decls: push D GCompiled
| push '#include "runtime.h"' GCompiled
| GCompiled.infix{'\n'}.unchars

ssa_produce_file File Src =
| Ssa = produce_ssa entry Src
| Text = ssa_to_c Ssa
//| save_text_file File Text

ctest = 

export ctest gensym
