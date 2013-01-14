
module type GAME =
sig
  type node
  type action
  val play : node -> action -> node
  val random_move : node -> action
  val possible_moves : node -> action list
  val terminal : node -> bool
  val score : node -> int
  val init : node
end

exception InvalidMove

module MonteCarlo ( Game : GAME ) =
struct

  open Game

  let rec random_play ?(moves = []) node =
    if terminal node then score node, moves
    else
      let action = random_move node in
      random_play ~moves:(action :: moves) (play node action)


  let rec nestedmc ?(moves = []) node lv =
    if lv = 0 then
      random_play node
    else
      let try_move move =
	nestedmc ~moves:[move::moves] (play node move) (lv-1)
      in
      List.fold_left max (-1, [])
	(List.rev_map try_move (possible_moves node))

end

    
