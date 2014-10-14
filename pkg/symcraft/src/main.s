use common tile macros world view gui gfx

M = main (main_path).url.0
//W = world M
//W.load_pud{'/Users/nikita/Documents/git/symta/build/symcraft/maps/test.pud'}

//when (main_args).size

Tabs = Void

MenuBG = gfx "[M.data]/ui/default/image/menu.png"

type litem.widget{Text w/140 on/1} text_/Text w/W h state font fw fh init
| $state <= if On then \normal else \disabled
litem.render =
| less $init
  | $h <= "litem/normal"^skin.h
  | $font <= font small
  | $fw <= $font.width{$text_}
  | $fh <= $font.height
  | $init <= 1
| Me
litem.text = $text_
litem.`!text` Text =
| $init <= 0
| $text_ <= Text
litem.draw G P =
| BG = "litem/[$state]"^skin
| G.blit{P BG rect/[0 0 $w BG.h]}
| Tint = case $state picked(\white) disabled(\gray) _(\yellow)
| X = 2
| Y = BG.h/2-$fh/2
| $font.draw{G P.0+X P.1+Y Tint $text_}
litem.input @In = case In
  [mice left 1 P] | $state <= case $state normal(\picked) picked(\normal) X(X)

type droplist.widget{Xs w/140} w/W h/1 xs/[] drop rs picked value over parent
| less Xs.size: Xs <= [' ']
| $xs <= Xs{(litem ? w/$w)}
droplist.render =
| $rs <= map X $xs X.render
| case $rs [R@_]: $h <= R.h
| Me
droplist.draw G P =
| Y = 0
| N = 0
| for R $rs
  | Visible = N >< $picked or $drop
  | when Visible: G.blit{P+[0 Y] R}
  | !Y + R.h
  | !N + 1
| less $drop
  | A = skin "arrow/down-normal"
  | G.blit{P+[$w-A.w 0] A}
| Void
droplist.input @In = case In
  [mice over S P] | $over <= S
  [mice left 1 P] | $drop <= 1
  [mice left 0 P] | $drop <= 0

ScenarioMenu = dlg: mtx
  |   0   0 0 | MenuBG
  | 230 220 1 | txt size/medium 'Custom Game Setup'
  |  40 256 1 | txt 'Your Race:'
  //|  40 272 1 | litem 'Map Default'
  |  40 272 1 | droplist ['Map Default' 'Orc' 'Human']
  |  400 370 1 | button 'Cancel Game': => Tabs.pick{main}

MainMenu = dlg: mtx
  |   0   0 0 | MenuBG
  |  60 460 1 | txt 'SymCraft v0.1 by Nikita Sadkov'
  | 208 240 1 | lay v 8: list
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
