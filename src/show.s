use show_

show F =
| Input = []
| G = F Input
| Result = show_gfx G.handle
| when Result <> '': bad "show: [Result]"
| show_close

export show
