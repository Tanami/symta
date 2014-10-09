use common tile macros world

M = main (main_path).url.0
M.load_pud{'/Users/nikita/Documents/git/symta/build/symcraft/maps/test.pud'}

\done

/*
Effects = t
  bloodlust | U Amount => U.effects.bloodlust <= Amount
  haste | U Amount => U.effects.haste <= Amount
  slow | U Amount => U.effects.slow <= Amount
  invisibility | U Amount => U.effects.invisibility <= Amount
  unholy_armor | U Amount => U.effects.unholy_armor <= Amount
  spawn | T M Type => say "spawn [Type]" //spawnUnit M.owner Type T.tile T
  morph | X M Type => say "spawn [Type]" //morph Type X

Targets = t
  organic | U A T => T.organic
  undead | U A T => T.undead
  non_buidling | U A T => not T.building
  can_move | U A T Type => bad "fix can_move" //ruleMove UTs.Type A T
  true | U A T = 1
  carries_resources | U A T => T.has_resources

Results = t //act results
  die | U => | U.die; U.act{remove}
  store | U => bad 'fix store' //doStore U
  unload | U => bad 'fix unload' //doStore U
*/
