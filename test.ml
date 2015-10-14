

let nth i =
  let moves = possible_moves initial in
  play initial (List.nth moves i)



let rec dumb_play node =
  if terminal node then node
  else
    dumb_play (play node (List.hd (possible_moves node)))


let test_node = ref initial
let test_moves = ref (possible_moves initial)

let test_play i =
  test_node := play !test_node (List.nth !test_moves i);
  test_moves := possible_moves !test_node;
  !test_node
