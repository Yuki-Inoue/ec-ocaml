

let nth i =
  let moves = possible_moves initial in
  play initial (List.nth moves i)



let rec dumb_play node =
  if terminal node then node
  else
    dumb_play (play node (List.hd (possible_moves node)))
