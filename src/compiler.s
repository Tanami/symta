GEnv = No
GOut = No // where resulting assembly code is stored
GNs = No // unique name of current function
GRawInits = No
GFns = No
GClosure = No // other lambdas, this lambda references
GBases = No
GUniquifyStack = No
GHoistedTexts = No
GResolvedMethods = No
GImportLibs = No
GSrc = [0 0 unknown]
GAll = @rand all

ssa @As = | push As GOut
          | No

get_parent_index Parent =
| P = GClosure.0.locate{E => Parent >< E}
| when got P: leave P
| Parents = GClosure.head
| GClosure <= [[@Parents Parent] @GClosure.tail]
| Parents.size

path_to_sym X Es =
| when Es.end: leave No
| [Head@Tail] = Es
| when case Head [U@Us] U >< GAll // reference to the whole arglist?
  | Head <= Head.1
  | less Head.0 >< X: leave (path_to_sym X Tail)
  | when Es^address >< GEnv^address: leave [GAll No] // argument of the current function
  | leave [GAll (get_parent_index Head.1)]
| P = Head.locate{V => X >< V.0}
| when no P: leave (path_to_sym X Tail)
| when Es^address >< GEnv^address: leave [P No] // argument of the current function
| [P (get_parent_index Head.P.1)]

ssa_symbol K X Value =
| case (path_to_sym X GEnv)
     [Pos Parent]
       | Base = if got Parent then "B".rand else \E
       | when got Parent
         | ssa var Base
         | ssa load Base \P Parent
       | when Pos >< GAll
         | when got Value: bad "cant set [X]"
         | ssa tagged K Base \T_LIST
         | leave No
       | when no Value: leave (ssa arg_load K Base Pos)
       | if Base >< \E and GBases.size >< 1
         then ssa arg_store Base Pos Value
         else ssa lift Base Pos Value // must be copied into parent environment
     Else
       | bad "unknown symbol: [X]"

cstring_bytes S = [@S.list.map{C => C.code} 0]

ssa_cstring Src = as Name 'b'.rand: ssa bytes Name Src^cstring_bytes

ssa_var Name = as V Name.rand: ssa var V

ssa_global Name = as V Name.rand: ssa global V

ev X = as R 'r'^ssa_var: ssa_expr R X

ssa_quote K X = if X.is_text then ssa_expr K GHoistedTexts.X
                else if X.is_list then compiler_error "ssa_quote: got list [X]"
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
  | when Prologue: ssa label GNs
  | SizeVar = "[F]_size"
  | when Prologue
    | if Args.is_text
      then ssa check_varargs SizeVar 'Empty' /*(get_meta O)*/
      else ssa check_nargs Args.size SizeVar 'Empty' /*(get_meta O)*/
  | when no K: K <= ssa_var result
  | ssa_expr K Body
  | when Epilogue: ssa return K
  | [GOut GClosure.0]

// FIXME:
// check if we really need new closure here, because in some cases we can reuse parent's closure
// a single argument to a function could be passed in register, while a closure would be created if required
// a single reference closure could be itself held in a register
// for now we just capture required parent's closure
ssa_fn Name K Args Expr O =
| F = @rand f
| [Body Cs] = ssa_fn_body No F Args Expr O 1 1
| push Body GFns
| NParents = Cs.size
| ssa alloc_closure K F NParents
| for [I C] Cs.i: if C^address >< GNs^address // self?
                  then ssa store K I \E
                  else ssa copy K I \P C^get_parent_index

ssa_if K Cnd Then Else =
| ThenLabel = @rand `then`
| EndLabel = @rand endif
| ssa branch Cnd^ev ThenLabel
| ssa_expr K Else
| ssa jmp EndLabel
| ssa local_label ThenLabel
| ssa_expr K Then
| ssa local_label EndLabel

//FIXME: currently hoisting may clobber sime toplevel syms;
//       make new syms valid only downstream
ssa_hoist_decls Expr Hoist = // C/C++ style declaration hoisting
| less Expr.is_list: leave Expr
| case Expr
     [_fn @Xs] | Expr
     [[_fn As @Xs] @Vs]
       | Vs <= Vs.map{V => ssa_hoist_decls V Hoist}
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
  | leave No
| F = @rand f
| [SsaBody Cs] = ssa_fn_body K F Args Body [] 0 0
| NParents = Cs.size
| P = ssa_var p // parent environment
| ssa local_closure P NParents
| for [I C] Cs.i: if C^address >< GNs^address // self?
                  then ssa store P I \E
                  else ssa copy P I \P C^get_parent_index
| E = ssa_var env
| ssa arglist E Args.size
| for [I V] Vals.i: ssa arg_store E I V^ev
| SaveP = ssa_var save_p
| SaveE = ssa_var save_e
| ssa move SaveP \P
| ssa move SaveE \E
| ssa move \E E
| ssa move \P P
| for S SsaBody.flip: push S GOut
| ssa move \P SaveP
| ssa move \E SaveE

ssa_apply K F As =
| case F [_fn Bs @Body]: leave: ssa_let K Bs As Body
| ssa push_base
| let GBases [[] @GBases]
  | H = ev F
  | Vs = map A As: ev A
  | E = ssa_var env
  | ssa arglist E As.size
  | for [I V] Vs.i: ssa arg_store E I V
  | if F.is_keyword then ssa call K H else ssa call_tagged K H

resolve_type Name =
| TypeNameBytes = ssa_cstring Name
| TypeVar = ssa_global t
| push [resolve_type TypeVar TypeNameBytes] GRawInits
| TypeVar

resolve_method Name =
| M = GResolvedMethods.Name
| when got M: leave M
| M <= ssa_global m
| GResolvedMethods.Name <= M
| push [resolve_method M Name^ssa_cstring] GRawInits
| M

ssa_apply_method K Name O As =
| ssa push_base
| let GBases [[] @GBases]: named block
  | As <= [O@As]
  | Vs = map A As: ev A
  | E = ssa_var env
  | ssa arglist E As.size
  | for [I V] Vs.i: ssa arg_store E I V
  | ssa call_method K Vs.0 Name.1^resolve_method

ssa_set K Place Value =
| R = ev Value
| ssa_symbol No Place R
| ssa move K R

// FIXME: _label should be allowed only inside of _progn
ssa_progn K Xs =
| when Xs.end: Xs <= [[]]
| D = ssa_var dummy
| for X Xs: case X [_label Name] | GBases <= [[Name @GBases.head] @GBases.tail]
| till Xs.end
  | X = pop Xs
  | when Xs.end: D <= K
  | ssa_expr D X
  | when Xs.end and case X [_label@Zs] 1: ssa move D 'No'

compiler_error Msg =
| [Row Col Orig] = GSrc
| say "[Orig]:[Row],[Col]: [Msg]"
| halt

expr_symbols_sub Expr Syms =
  if Expr.is_text then Syms.Expr <= 1
  else when Expr.is_list: map X Expr: expr_symbols_sub X Syms 

expr_symbols Expr =
| Syms = t size/1000
| expr_symbols_sub Expr Syms
| Syms

uniquify_let Xs =
| case Xs [[_fn As @Body] @Vs]
  | when As.is_text: leave Xs
  | when As.size <> Vs.size: compiler_error "bad number of arguments in [Xs]"
  | when no Vs.find{V => case V [_import X Y] 1}: leave Xs
  | Used = expr_symbols Body
  | NewAs = []
  | NewVs = []
  | till As.end
    | A = pop As
    | V = pop Vs
    | case V
        [_import [_quote X] [_quote Y]]
          | when no GImportLibs.X: GImportLibs.X <= @rand lib
          | when got Used.A
            | push A NewAs
            | push V NewVs
        Else
          | push A NewAs
          | push V NewVs
  | As <= NewAs.flip
  | Vs <= NewVs.flip
  | Xs <= [[_fn As @Body] @Vs]
| Xs

uniquify_form Expr =
| Src = Expr.meta_
| let GSrc (if got Src then Src else GSrc)
  | case Expr
    [_fn As @Body]
      | Bs = if As.is_text then [As] else As
      | BadArg = Bs.find{?.is_text^not}
      | when got BadArg: compiler_error "invalid argument [BadArg]"
      | when Bs.size <> Bs.uniq.size: compiler_error "duplicate args in [Bs]"
      | Rs = Bs.map{[? ?.rand]}
      | let GUniquifyStack [Rs @GUniquifyStack]
        | Bs = Rs.map{?.1}
        | Bs = if As.is_text then Bs.0 else Bs
        | [_fn Bs @Body.map{&uniquify_expr}]
    [_quote X] | when X.is_text: GHoistedTexts.X <= @rand 'T'
               | Expr
    [_label X] Expr
    [_goto X] Expr
    [_call @Xs] Xs^uniquify_form
    Xs | Xs <= uniquify_let Xs
       | Xs.map{&uniquify_expr}

uniquify_name S = for Closure GUniquifyStack: for X Closure: when X.0 >< S: leave X.1

uniquify_atom Expr =
| less Expr.is_text: leave Expr
| when Expr.size and Expr.0 >< _: leave Expr
| Renamed = uniquify_name Expr
| when no Renamed: compiler_error "undefined variable `[Expr]`"
| Renamed

uniquify_expr Expr = if Expr.is_list
                     then uniquify_form Expr
                     else uniquify_atom Expr

uniquify Expr =
| let GUniquifyStack []
  | R = uniquify_expr Expr
  | [[_fn (map [K V] GHoistedTexts V) R] @(map [K V] GHoistedTexts [_text K])]

ssa_list K Xs =
| less Xs.size: leave: ssa move K 'Empty'
| L = ssa_var l
| ssa arglist L Xs.size
| for [I X] Xs.i: ssa arg_store L I X^ev
| ssa tagged K L \T_LIST

ssa_data K Type Xs =
| Size = Xs.size
| BytesName = ssa_cstring Type.1
| TypeVar = ssa_global t
| let GOut []
  | ssa type TypeVar BytesName BytesName Size
  | for X GOut.flip: push X GRawInits
| ssa alloc_data K TypeVar Size
| for [I X] Xs.i: ssa dinit K I X^ev

ssa_subtype K Super Sub =
| ssa subtype Super.1^resolve_type Sub.1^resolve_type
| ssa move K 0

ssa_dget K Src Off =
| less Off.is_int: bad "dget: offset must be integer"
| ssa dget K Src^ev Off

ssa_dset K Dst Off Value =
| less Off.is_int: bad "dset: offset must be integer"
| D = ev Dst
| ssa_expr K Value
| ssa dset D Off K

ssa_dmet K MethodName TypeName Handler =
| MethodVar = MethodName.1^resolve_method
| TypeVar = resolve_type TypeName.1
| ssa dmet MethodVar TypeVar Handler^ev
| ssa move K 0

ssa_import K Lib Symbol =
| Lib <= Lib.1
| Symbol <= Symbol.1
| G = ssa_global i
| push [import G Lib Symbol GImportLibs.Lib Symbol^ssa_cstring] GRawInits
| ssa move K G

ssa_label Name = ssa local_label Name

ssa_goto Name =
| N = GBases.locate{B => got B.locate{?><Name}}
| when no N: bad "cant find label [Name]"
| times I N
  | ssa gc (ssa_var d) 0 // FIXME: have to GC, simple pop_base wont LIFT
  | ssa pop_base
| ssa jmp Name

ssa_mark Name =
| V = ssa_var m
| ssa_text V Name.1
| ssa mark V

ssa_fixed1 K Op X = ssa Op K X^ev
ssa_fixed2 K Op A B = ssa Op K A^ev B^ev

ssa_alloc K N =
| X = ssa_var x
| ssa_fixed1 X fixnum_unfixnum N
| ssa arglist K X

ssa_store Base Off Value = ssa untagged_store Base^ev Off^ev Value^ev
ssa_tagged K Tag X = ssa tagged K X^ev Tag.1

ssa_text K S = ssa text K S^ssa_cstring

ssa_ffi_var Type Name =
| V = @rand v
| ssa ffi_var Type V
| V

ssa_ffi_call K Type F As =
| F <= ev F
| As <= map A As: ev A
| Type <= map X Type.tail
          | case X.1
            text | 'text_'
            ptr | 'voidp_'
            T | T
| [ResultType @AsTypes] = Type
| less As.size >< AsTypes.size: bad "argument number doesn't match signature"
| R = less ResultType >< void: ssa_ffi_var ResultType r
| ATs = AsTypes
| Vs = map A As | AType = pop ATs
                | V = ssa_ffi_var AType v
                | ssa "ffi_to_[AType]" V A
                | V
| ssa ffi_call ResultType R F AsTypes Vs
| if ResultType >< void then ssa move K 0 else ssa "ffi_from_[ResultType]" K R

ssa_form K Xs = case Xs
  [_fn As Body] | ssa_fn 'n'.rand K As Body Xs
  [_if Cnd Then Else] | ssa_if K Cnd Then Else
  [_quote X @Xs] | ssa_quote K X
  [_set Place Value] | ssa_set K Place Value
  [_progn @Xs] | ssa_progn K Xs
  [_label Name] | ssa_label Name
  [_goto Name] | ssa_goto Name
  [_mark Name] | ssa_mark Name
  [_data Type @Xs] | ssa_data K Type Xs
  [_subtype Super Sub] | ssa_subtype K Super Sub
  [_dget Src Index] | ssa_dget K Src Index
  [_dset Dst Index Value] | ssa_dset K Dst Index Value
  [_dmet Method Type Handler] | ssa_dmet K Method Type Handler
  [_mcall O Method @As] | ssa_apply_method K Method O As
  [_list @Xs] | ssa_list K Xs
  [_text X] | ssa_text K X
  [_alloc N] | ssa_alloc K N
  [_store Base Off Value] | ssa_store Base Off Value
  [_tagged Tag X] | ssa_tagged K Tag X
  [_import Lib Symbol] | ssa_import K Lib Symbol
  [_add A B] | ssa_fixed2 K fixnum_add A B
  [_eq A B] | ssa_fixed2 K fixnum_eq A B
  [_lt A B] | ssa_fixed2 K fixnum_lt A B
  [_gte A B] | ssa_fixed2 K fixnum_gte A B
  [_tag X] | ssa_fixed1 K fixnum_tag X
  [_fatal Msg] | ssa fatal Msg^ev
  [_this_method] | ssa this_method K
  [_method_name Method] | ssa method_name K Method^ev
  [_method Name] | ssa move K: resolve_method Name.1
  [_type_id O] | ssa type_id K O^ev
  [_setjmp] | ssa setjmp K
  [_longjmp State Value] | ssa longjmp State^ev Value^ev
  [_set_unwind_handler H] | ssa set_unwind_handler K H^ev
  [_remove_unwind_handler] | ssa set_unwind_handler K
  [_ffi_call Type F @As] | ssa_ffi_call K Type F As
  [_ffi_get Type Ptr Off] | ssa ffi_get K Type.1 Ptr^ev Off^ev
  [_ffi_set Type Ptr Off Val] | ssa ffi_set Type.1 Ptr^ev Off^ev Val^ev
                              | ssa move K 0
  [F @As] | ssa_apply K F As
  [] | ssa_atom K No
  Else | bad "special form: [Xs]"

ssa_atom K X =
| if X.is_int then
    | when X > #7FFFFFFF or X < -#7FFFFFFF: X <= "[X]LL" //FIXME: kludge
    | ssa load_fixnum K X
  else if X.is_text then ssa_symbol K X No
  else if X >< No then ssa move K 'No'
  else if X.is_float then ssa load_float K X
  else bad "bad atom: [X]"

ssa_expr K X = if X.is_list then ssa_form K X else ssa_atom K X

ssa_load_lib Dst Name =
| ssa var Dst
| ssa load_lib Dst Name^ssa_cstring

produce_ssa Entry Expr =
| let GEnv []
      GOut []
      GFns []
      GRawInits []
      GClosure []
      GBases [[]]
      GHoistedTexts (t size/1000)
      GResolvedMethods (t size/500)
      GImportLibs (t)
  | ssa entry Entry
  | R = ssa_var result
  | uniquify !Expr
  | Ssa = ssa_expr R Expr
  | ssa return R
  | ssa entry setup
  | for [Name Dst] GImportLibs: ssa_load_lib Dst Name
  | for X GRawInits.flip: push X GOut
  | ssa return_no_gc 0
  | Rs = [GOut@GFns].flip.join.flip
  //| Rs <= peephole_optimize Rs
  | Rs

GCompiled = No

c Statement = push Statement GCompiled
cnorm [X@Xs] = c "  [X.upcase]([(map X Xs X.as_text).text{','}]);"

ssa_to_c Xs = let GCompiled []
| Statics = []
| Decls = []
| Imports = t
| c 'BEGIN_CODE'
| for X Xs: case X
  [entry Name] | c "ENTRY([Name])"
  [label Name] | push "DECL_LABEL([Name])" Decls
               | c "LABEL([Name])"
  [global Name] | push "static void *[Name];" Decls
  [alloc_closure Place Name Size] | push "#define [Name]_size [Size]" Decls
                                  | cnorm X
  [type Place Name TagName Size]
    | TName = @rand n
    | c "  RESOLVE_TYPE([Place],[Name]);"
    | c "  VAR([TName]);"
    | c "  TEXT([TName],[TagName]);"
    | c "  SET_TYPE_SIZE_AND_NAME((intptr_t)[Place],[Size],[TName]);"
  [load_lib Dst LibCStr] | c "  LOAD_LIB([Dst],[LibCStr]);"
  [import Dst Lib Symbol LibExports SymbolCStr]
    | Key = "[Lib]::[Symbol]"
    | Import = Imports.Key
    | if got Import
      then c "  MOVE([Dst], [Import]);"
      else | SymbolText = @rand s
           | c "  VAR([SymbolText]);"
           | c "  TEXT([SymbolText],[SymbolCStr]);"
           | c "  FIND_EXPORT([Dst],[SymbolText],[LibExports]);"
           | Imports.Key <= Dst
  [bytes Name Xs]
    | Brackets = '[]'
    | Values = (map X Xs X.as_text).text{','}
    | push "static uint8_t [Name][Brackets] = {[Values]};" Decls
  [ffi_call ResultType Dst F ArgsTypes Args]
    | ArgsText = Args.text{', '}
    | ArgsTypesText = ArgsTypes.text{', '}
    | Call = "(([ResultType](*)([ArgsTypesText]))[F])([ArgsText]);"
    | when got Dst: Call <= "[Dst] = [Call]"
    | c "  [Call]"
  Else | cnorm X //FIXME: check if it is known and has correct argnum
| c 'END_CODE'
| GCompiled <= GCompiled.flip
| for D Decls: push D GCompiled
| push '#include "runtime.h"' GCompiled
| GCompiled.text{'\n'}

ssa_produce_file Src =
| Ssa = produce_ssa entry Src
| Text = ssa_to_c Ssa
| Text

export produce_ssa ssa_to_c ssa_produce_file
