use common tile macros world gfx gui widgets view

M = main (main_path).url.0
World = Void

set_skin "[M.data]ui/orc"

/*World <= world M
World.load_pud{'/Users/nikita/Documents/git/symta/build/symcraft/maps/test.pud'}
View = view 640 480 M
gui View cursor/(skin_cursor point)*/

//when (main_args).size

Tabs = Void
MenuBG = gfx "[M.data]/ui/default/image/menu.png"

pud_desc Path =
| Cs = Path.get^(@r$[] [4/M.utf8 4/L.u4 L/D @Xs] => [[M D] @Xs^r])
| less Cs^is{[[\TYPE _]@_]}: bad "Invalid PUD file: [Path]"
| for [T D] Cs: when T >< 'DESC': leave D.take{D.locate{0}^supply{32}}.utf8
| ''

View = view 640 480 M
Ingame = View

ScenarioMenu =
| Desc = txt ''
| File = Void
| Start = button 'Start Game' state/disabled: =>
          | World <= world M
          | World.load_pud{File}
          | Tabs.pick{ingame}
| dlg: mtx
  |   0   0 | MenuBG
  | 230 220 | txt size/medium 'Custom Game Setup'
  |  40 256 | txt 'Your Race:'
  |  40 272 | droplist ['Map Default' 'Orc' 'Human']
  | 220 256 | txt 'Resources:'
  | 220 272 | droplist ['Low' 'Medium' 'High']
  | 400 256 | txt 'Units:'
  | 400 272 | droplist w/160 ['Map Default' 'One Peasant only']
  |  40 306 | txt 'Game Type:'
  |  40 322 | droplist ['Map Default']
  | 220 306 | txt 'Game Type:'
  | 220 322 | droplist ['Map Default' 'Forest' 'Winter' 'Wasteland' 'Swamp']
  |  14 400 | txt 'Description:'
  |  32 416 | Desc
  | 320   0 | folder_widget "[M.data]/maps": P =>
              | if P.url.2 >< pud
                then | Start.state <= \normal
                     | File <= P
                     | Desc.value <= pud_desc P
                else | Start.state <= \disabled
                     | Desc.value <= ''
  | 400 370 | lay v 8 [Start (button 'Cancel Game': => Tabs.pick{main})]

MainMenu = dlg: mtx
  |   0   0 | MenuBG
  |  60 460 | txt 'SymCraft v0.1 by Nikita Sadkov'
  | 208 240 | lay v 8: list
              button{'New Campaign'    state/disabled (=>)}
              button{'Custom Scenario' (=>Tabs.pick{scenario})}
              button{'Multi Player'    state/disabled (=>)}
              button{'Load Game'       state/disabled (=>)}
              button{'Map Editor'      state/disabled (=>)}
              button{'Exit Program'    (=>get_gui{}.exit)}

Tabs <= tabs scenario: t main(MainMenu) scenario(ScenarioMenu) ingame(Ingame)

gui Tabs cursor/(skin_cursor point)

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
