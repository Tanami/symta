genOctNeibFn Name N V =
| Basis = \(X Y Z)
| Basis.N <= '_'
| Basis2d = @del N \(X Y Z)
| [Qs] = form: ~Qs
| L = \L
| form @
  | octree.Name ~R ~E =
    | V = ~E.N
    | ~Q = $get{~E}
    | ~Ns = t ~Q.1^ot_phash(~Q)
    | edge_neibs Qs L ~F =
      | ~N = 0
      | while ~N < L
        | ~Q = $get{~F{N}}
        | [~S ~O<[$@Basis] L] = ~Q
        | ~H = ~O^ot_phash
        | less got ~Ns.~H
          | when rects_overlap [$@Basis2d L L] ~R
            | ~Ns.~H <= ~Q
            | push ~Q Qs
        | !~N + ~Q.2
      | Qs
    | [~Q]^| @r $$0
             [~Q<[~S [$@Basis] L] @Qs] => `|`
               $@| [X Y] = Basis2d
                 | map [A B C D] \((`+`N `+`L) (`+`N `-`1) (`+`L `+`N) (`-`1 `+`N))
                   | Xs = [[A X B] [C Y D]].insert{N V}
                   | form: Qs <= edge_neibs Qs L: $\N => [$@Xs]
               (r Qs)
    | ~Ns{?1}



export 'genOctNeibFn'
