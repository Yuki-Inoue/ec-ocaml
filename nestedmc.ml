

exception InvalidMove

module MonteCarlo ( Game : AIGame.S ) =
struct

  open Game

  let rec random_play ?(moves = []) node =
    if terminal node then score node, moves
    else
      let action = random_move node in
      random_play ~moves:(action :: moves) (play node action)

  type score = int

  let rec nestedmc ?(moves:Game.action list = []) node lv : score * Game.action list =
    if lv = 0 then
      random_play ~moves node
    else
      let try_move (move : Game.action) : int * Game.action list=
	nestedmc ~moves:(move::moves) (play node move) (lv-1)
      in
      List.fold_left max (min_int, [])
	(List.rev_map try_move (possible_moves node))

end

    
