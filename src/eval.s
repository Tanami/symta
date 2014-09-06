use prelude compiler reader macro

GRootFolder = '/Users/nikita/Documents/git/symta/'
GSrcFolders = Void
GDstFolder = Void
GHeaderTimestamp = Void
GMacros = Void
GShowInfo = 0
GCompiledModules = Void

read_normalized Text Filename =
| Expr = parse Text Filename
| case Expr [`|` @As] Expr
            X [`|` X]

load_macros Library = Library^load_library.keep{X => X.1.is_macro}

// FIXME: do caching
get_lib_exports LibName =
| for Folder GSrcFolders
  | LibFile = "[Folder][LibName].s"
  | when file_exists LibFile
    | Text = load_text LibFile
    | Expr = read_normalized Text LibFile
    | leave: case Expr.last [export @Xs] | [@Xs (gensym 'Dummy')]
                            Else | Void
| bad "no [LibName].s"

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
| unless Deps.size: leave Expr
| [[_fn (map D Deps D.1) Expr]
   @(map D Deps [_import [_quote D.0] [_quote D.1]])]

compile_expr Name Dst Expr =
| Uses = [core prelude]
| Expr <= case Expr
            [`|` [use @Us] @Xs]
               | Uses <= [@Uses @Us]
               | [`|` @Xs]
            Else | Expr
| Uses <= Uses.skip{X => Name >< X}.uniq
| Deps = Uses.tail
| for D Deps: unless compile_module D: bad "cant compile [D].s"
| when GShowInfo: say "compiling [Name]..."
| Imports = (map U Uses: map E U^get_lib_exports: [U E]).join
| Macros = Imports.skip{X => X.1.is_text}.map{X => X.0}.uniq.skip{X => X><macro} // keep macros
| Imports = Imports.keep{X => X.1.is_text} // skip macros
| ExprWithDeps = add_imports Expr Imports
| Ms = [GMacros @(map M Macros "[GDstFolder][M]"^load_macros)].join
| ExpandedExpr = macroexpand ExprWithDeps Ms.as_table &compile_module
| CFile = "[Dst].c"
| ssa_produce_file CFile ExpandedExpr
| Result = c_compiler Dst CFile
| unless file_exists Dst: bad "[CFile]: [Result]"
| Deps

load_symta_file Filename =
| Text = load_text Filename
| read_normalized Text Filename

compile_module_sub Name =
| for Folder GSrcFolders
  | SrcFile = "[Folder][Name].s"
  | when file_exists SrcFile
    | DstFile = "[GDstFolder][Name]"
    | GCompiledModules.Name <= DstFile
    | DepFile = "[DstFile].dep"
    | when file_exists DepFile and file_older SrcFile DepFile:
      | Deps = DepFile^load_symta_file.1
      | CompiledDeps = map D Deps: compile_module D
      | when file_older SrcFile DstFile and CompiledDeps.all{X => have X and file_older X DstFile}:
        | leave DstFile
    | Expr = load_symta_file SrcFile
    | Deps = compile_expr Name DstFile Expr
    | DepsText = Deps.text{' '}
    | save_text DepFile DepsText
    | leave DstFile
| Void

compile_module Name =
| DstFile = GCompiledModules.Name
| when no DstFile: DstFile <= compile_module_sub Name
| DstFile

build_entry Entry =
| let GMacros 'macro'^load_macros
  | DstFile = compile_module Entry
  | when no DstFile: bad "cant compile [Entry]"
  | DstFile

build @As =
| SrcFolder = Void
| DstFolder = Void
| case As
   [S D] | SrcFolder <= S
         | DstFolder <= D
   [S] | SrcFolder <= S
       | DstFolder <= S
   Else | bad "build: bad arglist = [As]"
| let GDstFolder "[DstFolder]lib/"
      GSrcFolders ["[SrcFolder]src/" "[GRootFolder]src/"]
      GHeaderTimestamp (file_time "[GRootFolder]/runtime/runtime.h")
      GShowInfo 1
      GCompiledModules (table 100)
  | RuntimeSrc = "[GRootFolder]runtime/runtime.c"
  | RuntimePath = "[DstFolder]run"
  | compile_runtime RuntimeSrc RuntimePath
  | build_entry main
  | unix "[RuntimePath] ':[GDstFolder]'"

eval Expr Env =
| BuildFolder = "[GRootFolder]build/tmp/"
| let GMacros 'macro'^load_macros
      GSrcFolders ["[GRootFolder]src/"]
      GHeaderTimestamp (file_time "[GRootFolder]/runtime/runtime.h")
      GDstFolder "[BuildFolder]lib/"
      GCompiledModules (table 100)
  | Entry = gensym tmp
  | DstFile = "[BuildFolder]lib/[Entry]"
  | Vars = map [K V] Env K
  | Values = map [K V] Env V
  | Expr <= [_fn Vars
              ['|' ['<=' ['Last_'] Expr]
                   @(map V Vars ['<=' [['.' 'Env_' ['\\' V]]] V])
                   0]]
  | Expr <= ['|' [use @Env.'Uses_'] Expr]
  | Deps = compile_expr Entry DstFile Expr
  | unless file_exists DstFile: bad "cant compile [DstFile]"
  | Values.apply{DstFile^load_library}
  | Env.'Last_'
export build eval
