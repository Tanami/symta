use heap octree_m

/*
Octree Neighbor Discovery Algorithm (ONDA).

ONDA is used to get cubes adjacent to the specified cube, inside of an octree.

Algorithm steps:
 1. on a given cube's side (square): push random sub-square onto stack
 2. pop top square, then add corresponding to it cube to Neighbors list and for
    each segment of each side of this square, push all corresponding neigbor
    square onto stack, if they are part of original cube's side
 3. if stack isnt empty, goto 2.
 4. return Neighbors list

Note: hash popped quads by their XYZ
*/

rects_overlap [AX AY AW AH] [BX BY BW BH] =
| AX<BX+BW and AY<BY+BH and BX<AX+AW and BY<AY+AH

type octree{Size} size/Size root

octree.clear Value = $root <= Value

ot_phash X,Y,Z = Z*#10000+Y*#100+X //point's hash
ot_p2i X,Y,Z = Z*4 + Y*2 + X
ot_merge$_ [X<1.is_int @1.all{(same ? X)}] = X

ot_get O L P T =
| if T.is_int then T,O,L // Value,Origin,Dimension
  else | !L / 2
       | PD = P / L
       | I = ot_p2i PD
       | PM = P % L
       | ot_get O+PD*L L PM T.I

ot_set L P V T =
| if T.is_int then
     if L < 2 or same V T then V
     else ot_set L P V: dup I 8 T // create new cube node
  else | !L / 2
       | PD = P / L
       | I = ot_p2i PD
       | PM = P % L
       | R = ot_set L PM V T.I
       | T.I <= ot_merge R
       | T

Origin = 0,0,0

// gets cube at P in the form of [Value OriginXYZ EdgeLength]
octree.get P = ot_get Origin $size P $root

octree.set P V = $root <= ot_merge: ot_set $size P V $root

// optimized accessor
octree.at P =
| O = Origin
| L = $size
| T = $root
| till T.is_int
  | !L / 2
  | PD = P / L
  | I = ot_p2i PD
  | PM = P % L
  | !O + PD*L
  | P <= PM
  | T <= T.I
| T

genOctNeibFn x_neibs 0 X
genOctNeibFn y_neibs 1 Y
genOctNeibFn z_neibs 2 Z

octree.neibs P =
| [S O L] = $get{P}
| [X Y Z] = O
| West  = $x_neibs{[Y Z L L] [X-1 Y   Z  ]}
| East  = $x_neibs{[Y Z L L] [X+L Y   Z  ]}
| North = $y_neibs{[X Z L L] [X   Y-1 Z  ]}
| South = $y_neibs{[Y Z L L] [X   Y+L Z  ]}
| Down  = $z_neibs{[X Y L L] [X   Y   Z-1]}
| Up    = $z_neibs{[X Y L L] [X   Y   Z+L]}
| [@West @East @North @South @Down @Up]

// get column of voxels at X,Y in the form of [@_[Count Id]@_]
octree.getPilar X Y =
| Size = $size
| Xs = []
| Z = 0
| while Z < Size
  | S,O,L = $get{X,Y,Z}
  | Xs <= [L,S @Xs]
  | !Z + L
| on Xs.flip | @r$_ [@H A,V B,&V @T] => [@H @[A+B,V @T]^r]

octree.height X Y = $size - $getPilar{X Y}.last.0

export octree
