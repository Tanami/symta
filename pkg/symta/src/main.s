use eval

Root = main_root
Args = main_args

print_usage =
| say 'Usage: symta [OPTION] <SRC_DIR> [DST_DIR]'
| say 'Produce DST_DIR/lib and DST_DIR/run from SRC_DIR/src'
| say 'If DST_DIR is unspecified, SRC_DIR is used instead'
| say ''
| say 'OPTIONS:'
| say '       -r <root_dir>   search root_dir for compiler files'
| say '       -v              print version'
| say '       -h              print this help'
| halt

case Args [@_ '-h'+'--help' @_]: print_usage

case Args [@_ '-v'+'--version' @_]
| say "Symta v[get_symta_version]"
| say "Copyright (C) 2014 Nikita Sadkov"
| say "See LICENSE for copying conditions"
| halt

case Args ['-r' UserRoot @Xs]
| Root <= UserRoot
| Args <= Xs

SrcDir = Void
DstDir = Void

case Args
  [Src Dst] | SrcDir <= Src; DstDir <= Dst
  [Src] | SrcDir <= Src; DstDir <= Src
  Else | print_usage

less "[Root]src/rt_.s".exists: bad "Missing [Root]src/rt_.s"

say: build Root SrcDir dst/DstDir
