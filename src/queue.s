type queue{Size} xs a b
| $xs <= dup Size 0

queue.end = $a >< $b

queue.push Item =
| $xs.$b <= Item
| !$b+1
| when $b >< $xs.size: $b <= 0

queue.pop =
| Item = $xs.$a
| !$a+1
| when $a >< $xs.size: $a <= 0
| Item

queue.clear =
| $xs.clear{0}
| $b <= $a

export queue