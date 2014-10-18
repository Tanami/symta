use compiler reader macro

GRootFolder = Void
GSrcFolders = Void
GDstFolder = Void
GHeaderTimestamp = Void
GMacros = Void
GShowInfo = 0
GCompiledModules = Void

read_normalized Text Filename =
| Expr = Text.parse{src Filename}
| case Expr [`|` @As] Expr
            X [`|` X]

load_macros Library = Library^load_library.keep{X => X.1.is_macro}

// FIXME: do caching
get_lib_exports LibName =
| for Folder GSrcFolders
  | LibFile = "[Folder][LibName].s"
  | when LibFile.exists
    | Text = LibFile.get_text
    | Expr = read_normalized Text LibFile
    | leave: case Expr.last [export @Xs] | [@Xs 'Dummy'.rand]
                            Else | ['Dummy'.rand]
| bad "no [LibName].s"

c_runtime_compiler Dst Src =
| RtFolder = "[GRootFolder]runtime"
| unix "gcc -O1 -Wno-return-type -Wno-pointer-sign -I '[RtFolder]' -g -o '[Dst]' '[Src]'"

c_compiler Dst Src =
| RtFolder = "[GRootFolder]runtime"
| unix "gcc -O1 -Wno-return-type -Wno-pointer-sign -I '[RtFolder]' -g -fpic -shared -o '[Dst]' '[Src]'"

// check if Dependent file is up to date with Source file
newerThan Source Dependent =
| DependentTime = Dependent.time
| Source.time << DependentTime and GHeaderTimestamp << DependentTime

compile_runtime Src Dst =
| when Dst^newerThan{Src}: leave Void
| say "compiling runtime..."
| Result = c_runtime_compiler Dst Src
| when Result <> "": bad Result

add_imports Expr Deps =
| less Deps.size: leave Expr
| [[_fn (map D Deps D.1) Expr]
   @(map D Deps [_import [_quote D.0] [_quote D.1]])]

module_folders = [GRootFolder GSrcFolders GDstFolder]

compile_expr Name Dst Expr =
| Uses = [rt_ core_]
| Expr <= case Expr
            [`|` [use @Us] @Xs]
               | Uses <= [@Uses @Us]
               | [`|` @Xs]
            Else | Expr
| Uses <= Uses.skip{X => Name >< X}.uniq
| Deps = Uses.tail
| for D Deps: less compile_module D: bad "cant compile [D].s"
| when GShowInfo: say "compiling [Name]..."
| Imports = (map U Uses: map E U^get_lib_exports: [U E]).join
| Macros = Imports.skip{X => X.1.is_text}.map{X => X.0}.uniq.skip{X => X><macro} // keep macros
| Imports = Imports.keep{X => X.1.is_text} // skip macros
| ExprWithDeps = add_imports Expr Imports
| Ms = [GMacros @(map M Macros "[GDstFolder][M]"^load_macros)].join
| ExpandedExpr = macroexpand ExprWithDeps Ms.table &compile_module &module_folders
| Text = ssa_produce_file ExpandedExpr
| CFile = "[Dst].c"
| CFile.set{Text}
| Result = c_compiler Dst CFile
| less Dst.exists: bad "[CFile]: [Result]"
| Deps

load_symta_file Filename =
| Text = Filename.get_text
| read_normalized Text Filename

compile_module_sub Name =
| for Folder GSrcFolders
  | SrcFile = "[Folder][Name].s"
  | when SrcFile.exists
    | DstFile = "[GDstFolder][Name]"
    | GCompiledModules.Name <= DstFile
    | DepFile = "[DstFile].dep"
    | when DepFile.exists and DepFile^newerThan{SrcFile}:
      | Deps = DepFile^load_symta_file.1
      | CompiledDeps = map D Deps: compile_module D
      | when DstFile^newerThan{SrcFile} and CompiledDeps.all{X => got X and DstFile^newerThan{X}}:
        | leave DstFile
    | Expr = load_symta_file SrcFile
    | Deps = compile_expr Name DstFile Expr
    | DepFile.set{Deps.text{' '}}
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

build RootFolder SrcFolder dst/0 =
| DstFolder = Dst or SrcFolder
| less RootFolder.last >< '/': RootFolder <= "[RootFolder]/"
| let GRootFolder RootFolder
      GDstFolder "[DstFolder]lib/"
      GSrcFolders ["[SrcFolder]src/" "[GRootFolder]src/"]
      GHeaderTimestamp "[GRootFolder]/runtime/symta.h".time
      GShowInfo 1
      GCompiledModules (t)
  | "[GDstFolder]/ffi".mkpath
  | register_library_folder GDstFolder
  | RuntimeSrc = "[GRootFolder]runtime/runtime.c"
  | RuntimePath = "[DstFolder]run"
  | compile_runtime RuntimeSrc RuntimePath
  | build_entry main
  | RuntimePath //unix RuntimePath //"[RuntimePath] ':[GDstFolder]'"

eval RootFolder Expr Env =
| BuildFolder = "[GRootFolder]build/tmp/"
| let GRootFolder RootFolder
      GMacros 'macro'^load_macros
      GSrcFolders ["[GRootFolder]src/"]
      GHeaderTimestamp "[GRootFolder]/runtime/symta.h".time
      GDstFolder "[BuildFolder]lib/"
      GCompiledModules (t)
  | Entry = @rand tmp
  | DstFile = "[BuildFolder]lib/[Entry]"
  | Vars = map [K V] Env K
  | Values = map [K V] Env V
  | Expr <= [_fn Vars
              ['|' ['<=' ['Last_'] Expr]
                   @(map V Vars ['<=' [['.' 'Env_' ['\\' V]]] V])
                   0]]
  | Expr <= ['|' [use @Env.'Uses_'] Expr]
  | Deps = compile_expr Entry DstFile Expr
  | less DstFile.exists: bad "cant compile [DstFile]"
  | Values.apply{DstFile^load_library}
  | Env.'Last_'
export build eval
