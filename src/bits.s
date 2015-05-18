type bits{size} xs
| $xs <= (($size+7)/8).bytes

bits.clear Bit =
| Byte = if Bit then #FF else 0
| for I $xs.size: $xs.I <= Byte

bits.`.`  N = $xs.(N/>3).get{N^^7}

bits.`!`  N V =
| I = N/>3
| $xs.I <= $xs.I.set{N^^7 V}

bits.list = map I $size $I

export bits