let moves = possible_moves initial

let first_move_test = List.iteri
  (fun i move ->
    Printf.printf "moving %ith move\n" i;
    ignore (play initial move))
  moves
