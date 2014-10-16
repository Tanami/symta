
cfg File =
| less File.exists: bad "cant open [File]"
| File.get.utf8.lines{}{?parse}.skip{is.[]}

rects_intersect [AX AY AW AH] [BX BY BW BH] = AX<BX+BW and AY<BY+BH and BX<AX+AW and BY<AY+AH

export cfg rects_intersect
