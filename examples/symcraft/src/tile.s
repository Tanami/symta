MCs = | C = [water land plain air forest wall rock dead invuln 0 1 2 3 4 5 6 7 8 9 10]
      | (C.i){[?1 2**?0]}.as_map

TTypes = m // tile types
  block   | m base 0       mc 0
  plainL  | m base block   mc [air land plain]
  plainD  | m base plainL  mc [air land plain]
  forest  | m base plainL  mc [air forest]
              rm forestR  hp 1  wood 100  resource wood
  mudL    | m base plainL  mc [air land]
  mudD    | m base mudL    mc [air land]
  waterL  | m base mudL    mc [air water]
  waterD  | m base waterL  mc [air water]
  rock    | m base mudL    mc [air rock] rm rockR  hp 1
  wallH   | m base plainL  mc [air wall] rm wallR  hp 100 armor 20
  wallO   | m base plainL  mc [air wall] rm wallR  hp 100 armor 20
  wallCH  | m base plainL  mc [air wall] rm wallCR hp 100 armor 20
  wallCO  | m base plainL  mc [air wall] rm wallCR hp 100 armor 20
  rockR   | m base plainL  mc [air land] rm 0
  forestR | m base block   mc [air land plain] rm 0
  wallR   | m base block   mc [air land] rm 0
  wallCR  | m base block   mc [air land] rm 0


