type bits{size} xs
| $xs <= (($size+7)/8).bytes
| $clear{0}

bits.clear Bit =
| Byte = if Bit then #FF else 0
| for I $xs.size: $xs.I <= Byte

bits.`.`  N = $xs.(N/>3).get{N^^7}

bits.`!`  N V =
| I = N/>3
| $xs.I <= $xs.I.set{N^^7 V}

bits.list = map I $size $I

bits.active = $list.i.keep{?1}{?0}

export bits