use eval

Src = Void
Dst = Void

case (main_args)
  [A B] | Src <= A; Dst <= B
  [A] | Src <= A; Dst <= A
  Else | say 'Usage: symta <src_dir> <build_dir>'

build Src Dst
