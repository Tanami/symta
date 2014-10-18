use eval

Root = main_root
Args = main_args

print_usage =
| say 'Usage: symta [-r <root_dir>] <src_dir> [dst_dir]'
| halt

case Args [@_ '-h'+'--help' @_]: print_usage

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
