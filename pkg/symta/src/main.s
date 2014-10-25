use eval

Root = main_root
Args = main_args

print_usage =
| say 'Usage: symta [OPTION] <SRC_DIR> [DST_DIR]'
| say '       symta [OPTION] <SRC_DIR/main.s> [DST_DIR]'
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

SrcDir = No
DstDir = No

case Args
  [Src Dst] | SrcDir <= Src; DstDir <= Dst
  [Src] | SrcDir <= Src; DstDir <= Src
  Else | print_usage

Root <= Root.replace{'\\' '/'}
SrcDir <= SrcDir.replace{'\\' '/'}
DstDir <= DstDir.replace{'\\' '/'}

less Root.last >< '/': Root <= "[Root]/"

less "[Root]src/rt_.s".exists: bad "Missing [Root]src/rt_.s"
less "[Root]runtime/symta.h".exists: bad "Missing [Root]runtime/symta.h"

build Root SrcDir dst/DstDir
