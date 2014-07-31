use prelude compiler reader macro

GRootFolder = Void
GSrcFolders = Void
GDstFolder = Void
GHeaderTimestamp = Void
GLibFolder = Void
GMacros = Void

read_normalized Text =
| Expr = read Text
| on Expr [`|` @As] Expr
          X [`|` X]

skip_macros Xs = Xs.skip{X => X.1.is_macro}

// FIXME: do caching
get_lib_exports LibName =
| LibFile = "[GLibFolder][LibName].s"
| Text = load_text LibFile
| Expr = read_normalized Text
| on Expr.last [export @Xs] | skip_macros Xs
               Else | Void

c_runtime_compiler Dst Src =
| RtFolder = "[GRootFolder]runtime"
| unix "gcc -O1 -Wno-return-type -Wno-pointer-sign -I '[RtFolder]' -g -o '[Dst]' '[Src]'"

c_compiler Dst Src =
| RtFolder = "[GRootFolder]runtime"
| unix "gcc -O1 -Wno-return-type -Wno-pointer-sign -I '[RtFolder]' -g -fpic -shared -o '[Dst]' '[Src]'"

file_older Src Dst =
| DstDate = if file_exists Dst then file_time Dst else 0
| Src^file_time << DstDate and GHeaderTimestamp << DstDate

compile_runtime Src Dst =
| when file_older Src Dst: leave Void
| say "compiling runtime..."
| Result = c_runtime_compiler Dst Src
| when Result <> "": bad Result

add_imports Expr Deps =
| unless Deps.size > 0: leave Expr
| [[_fn (map D Deps D.1) Expr]
   (map D Deps [_import [_quote D.0] [_quote D.1]])]

symta_compile_expr Name Dst Expr =
| Uses = [core]
| Expr = on Expr
           [`|` [use @Us] @Xs]
              | Uses <= [@Uses @Us]
              | [`|` @Xs]
           Else | Expr
| Deps = Uses.tail
| for D Deps: unless compile_module D: bad "module [Name].s"
| say "compiling [Name]..."
| Imports = (map U Uses: map E U^get_lib_exports: [U E]).join
| ExprWithDeps = add_imports Expr Imports
| ExpandedExpr = macroexpand ExprWithDeps GMacros
| CFile = "[Dst].c"
| ssa_produce_file CFile ExpandedExpr
| Result = c_compiler Dst CFile
| when Result <> "": bad Result
| Deps

symta_read_file Filename =
| Text = load_text Filename
| read_normalized Text

compile_module Name =
| for Folder GSrcFolders
  | SrcFile = "[Folder][Name]"
  | when file_exists SrcFile
    | DstFile = "[GDstFolder][Name]"
    | DepFile = "[DstFile].dep"
    | when file_exists DepFile
      | Deps DepFile^symta_read_file.1
      | CompiledDeps = map D Deps: compile_module D
      | when file_older SrcFile DstFile and CompiledDeps.all{X => X <> Void and file_older X DstFile}:
        | leave DstFile
    | Expr = symta_read_file SrcFile
    | Deps = symta_compile_expr Name DstFile Expr
    | DepsText = Deps.infix{' '}.unchars
    | save_text DepFile DepsText
| Void

symta BaseDir =
| MacrosLib = '/Users/nikita/Documents/git/symta/cache/lib/macro'
| let GMacros MacrosLib^load_library.keep{X => X.1.is_macro}.as_table
      GRootFolder "/Users/nikita/Documents/git/symta/"
      GSrcFolders ["[BaseDir]src/" "[GRootFolder]lib/"]
      GDstFolder "[BaseDir]lib/"
      GLibFolder "[GRootFolder]lib/"
      GHeaderTimestamp (file_time "[GRootFolder]/runtime/runtime.c")
  | say GHeaderTimestamp
  | bad 123
  | RuntimeSrc = "[GRootFolder]/runtime/runtime.c"
  | RuntimePath = "[BaseDir]run"
  | compile_runtime RuntimeSrc RuntimePath
  | DstFile = compile_module main
  | when DstFile >< Void: bad "no main.s"
  | Result = unix "[RuntimePath] '[GDstFolder]'"
  | say Result

export symta
