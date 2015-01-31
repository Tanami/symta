// Randomized Meldable Heap
type rmh_node key value parent left right
rmh_node.as_text = "#(rmh_node [$key] [$value])"

rmh_merge H1 H2 =
| less H1: leave H2
| less H2: leave H1
| when H2.key < H1.key: leave: rmh_merge H2 H1
| if 1.rand
  then | H1.left <= rmh_merge H1.left H2
       | when H1.left: H1.left.parent <= H1
  else | H1.right <= rmh_merge H1.right H2
       | when H1.right: H1.right.parent <= H1
| H1

type heap root

heap.push Key Value =
| U = rmh_node
| U.key <= Key
| U.value <= Value
| $root <= rmh_merge U $root
| $root.parent <= 0
| Me

heap.pop =
| less $root: leave 0
| R = $root
| $root <= rmh_merge R.left R.right
| R

heap.list =
| Xs = $deep_copy
| Ys = []
| while!it Xs.pop: push it Ys
| Ys.flip

export heap
