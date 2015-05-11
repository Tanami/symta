type stack{Init} xs used
| if Init.is_int then $xs <= dup{Init}
  else | $xs <= dup{Init.size}
       | for X Init: $push{X}

stack.push X =
| $xs.$used <= X
| !$used+1

stack.pop =
| !$used-1
| $xs.$used

stack.alloc @Xs =
| Item = $pop
| Item.init{@Xs}
| Item

stack.size = $xs.size

stack.clear = $used <= 0

stack.`.` Index = $xs.($used - Index)

stack.list = dup I $used: $xs.I

export stack