MCs = | C = [water land plain air forest wall rock dead invuln 0 1 2 3 4 5 6 7 8 9 10]
      | (map [I M] C.enum: 2**I).as_table

TTypes = table // tile types
  block   | table base 0       mc 0
  plainL  | table base block   mc [air land plain]
  plainD  | table base plainL  mc [air land plain]
  forest  | table base plainL  mc [air forest]
                  rm forestR  hp 1  wood 100  resource wood
  mudL    | table base plainL  mc [air land]
  mudD    | table base mudL    mc [air land]
  waterL  | table base mudL    mc [air water]
  waterD  | table base waterL  mc [air water]
  rock    | table base mudL    mc [air rock] rm rockR  hp 1
  wallH   | table base plainL  mc [air wall] rm wallR  hp 100 armor 20
  wallO   | table base plainL  mc [air wall] rm wallR  hp 100 armor 20
  wallCH  | table base plainL  mc [air wall] rm wallCR hp 100 armor 20
  wallCO  | table base plainL  mc [air wall] rm wallCR hp 100 armor 20
  rockR   | table base plainL  mc [air land] rm 0
  forestR | table base block   mc [air land plain] rm 0
  wallR   | table base block   mc [air land] rm 0
  wallCR  | table base block   mc [air land] rm 0


