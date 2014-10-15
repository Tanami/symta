use common tile macros world view gui gfx

M = main (main_path).url.0
//W = world M
//W.load_pud{'/Users/nikita/Documents/git/symta/build/symcraft/maps/test.pud'}

//when (main_args).size

Tabs = Void

MenuBG = gfx "[M.data]/ui/default/image/menu.png"

type litems.~{Xs w/300 lines/5 f/(V=>)} f/F ih/Void lines/Lines xs/Xs box picked o/Void
| $box <= lay v 0: dup $lines: litem '' w/W
| $offset <= 0
litems._ Method Args =
| Args.0 <= Args.0.box
| Args.apply_method{Method}
litems.offset = $o
litems.`!offset` NO =
| when NO >< $o: leave 0
| $o <= max 0: @clip 0 $xs.size-1 NO
| times K $lines
  | I = $o + K
  | Item = $box.items.K
  | if I < $xs.size
    then | Item.text <= $xs.I
         | Item.state <= if I >< $picked then \picked else \normal
    else | Item.text <= ''
         | Item.state <= \disabled
litems.value = $xs.($picked)
litems.data = $xs
litems.`!data` Ys =
| $xs <= Ys
| $picked <= 0
| $o <= Void
| $offset <= 0
litems.input @In = case In
  [mice left 1 P] | have $ih: $box.items.0.render.h
                  | NP = @clip 0 $xs.size-1 P.1/$ih+$o
                  | when NP <> $picked
                    | K = $picked - $o
                    | when K >> 0 and K < $lines:
                      | $box.items.K.state <= \normal
                    | $picked <= NP
                    | $f $xs.NP
                    | $box.items.(NP-$o).state <= \picked
litems.itemAt Point XY WH = [Me XY WH] //override lay's method

type folder_litems.~{Root f/(V=>)} root/Root f/F sub
| less $root.last >< '/': $root <= "[$root]/"
| $sub <= litems lines/9 f/(N => F "[Root][N]") Root.items
folder_litems._ Method Args =
| Args.0 <= Args.0.sub
| Args.apply_method{Method}
folder_litems.input @In = case In
  [mice double_left 1 P] | say hello
  Else | $sub.input{@In}
folder_litems.itemAt Point XY WH = [Me XY WH]

ScenarioMenu =
| Desc = txt ''
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
  | 320   0 | folder_litems "[M.data]/maps" //litems ['Hello World' 'Second Line']
  | 400 370 | button 'Cancel Game': => Tabs.pick{main}

MainMenu = dlg: mtx
  |   0   0 | MenuBG
  |  60 460 | txt 'SymCraft v0.1 by Nikita Sadkov'
  | 208 240 | lay v 8: list
              button{'New Campaign'    on/0 (=>)}
              button{'Custom Scenario' (=>Tabs.pick{scenario})}
              button{'Multi Player'    on/0 (=>)}
              button{'Load Game'       on/0 (=>)}
              button{'Map Editor'      on/0 (=>)}
              button{'Exit Program'    (=>get_gui{}.exit)}

Tabs <= tabs scenario: t main(MainMenu) scenario(ScenarioMenu) ingame(MenuBG)

gui Tabs

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
