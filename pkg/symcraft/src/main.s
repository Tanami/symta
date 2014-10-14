use common tile macros world view gui gfx

M = main (main_path).url.0
//W = world M
//W.load_pud{'/Users/nikita/Documents/git/symta/build/symcraft/maps/test.pud'}

//when (main_args).size

MainMenu = dlg: mtx
  |   0   0 0 | gfx "[M.data]/ui/default/image/menu.png"
  |  60 460 1 | txt 'SymCraft v0.1 by Nikita Sadkov'
  | 208 240 1 | lay v 8: list
                button{'New Campaign'    on/0 (=>)}
                button{'Custom Scenario' (=>)}
                button{'Multi Player'    on/0 (=>)}
                button{'Load Game'       on/0 (=>)}
                button{'Map Editor'      on/0 (=>)}
                button{'Exit Program'    (=>get_gui{}.exit)}


gui MainMenu

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
