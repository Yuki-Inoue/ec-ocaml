
module type CORDINATE =
sig
  val make : (int * int) -> (int * int)
  val unmake : (int * int) -> (int * int)
  val unit : int
end

module FirstCordinate =
struct
  let make = id
  let unmake = id
  let unit = 1
end

module SecondCordinate =
struct
  let make (x,y) = (x-y, x+y)
  let unmake (x,y) = (x+y)/2, (y-x)/2
  let unit = 2
end

module ThirdCordinate =
struct
  let make (x,y) = (y,x)
  let unmake = make
  let unit = 1
end

module ForthCordinate =
struct
  let make (x,y) = (x+y, x-y)
  let unmake (x,y) = (x+y)/2, (x-y)/2
  let unit = 2
end


module type VIEW =
sig
  type t
  val make : (module CORDINATE) -> t
  val add_node : (int * int) -> t -> t
  val add_line : (int * int) -> t -> t
  val possible_moves : t -> int * (int * int) list
  val playable : (int * int) -> t -> bool
  val move_exist : t -> bool
  val node_size : t -> int
end





module DirView : VIEW =
struct

  module CordinateSet
    = Set.Make(
      struct type t = int * int let compare = compare end
    )

  type line_set = CordinateSet.t 
  type node_set = CordinateSet.t 
  type t = (module CORDINATE) * node_set * line_set

  let make cordinate =
    cordinate, CordinateSet.empty, CordinateSet.empty

  let add_node (cordinate, node_set, line_set) cor =
    let module Cordinate = (val cordinate) in
    let newnodes = CordinateSet.add (Cordinate.make cor) node_set in
    (cordinate, newnodes, line_set)

  let add_line (cordinate, node_set, line_set) cor =
    let module Cordinate = (val cordinate) in
    let newlines = CordinateSet.add (Cordinate.make cor) line_set in
    (cordinate, node_set, newlines)



  let possible_moves (cordinate, node_set, line_set) =
    let module Cordinate = (val cordinate) in
    let encode is_line set =
      CordinateSet.fold
	(fun elm ret -> (elm, is_line)::ret)
	set
	[]
    in
    let node_codes = encode false node_set in
    let line_codes = encode true line_set in
    let rec merge ans node_codes line_codes =
      match node_codes, line_codes with
	  _, [] -> List.rev_append node_codes ans
	| [], _ -> List.rev_append line_codes ans
	| node_hd::_, line_hd::line_tl
	  when node_hd < line_hd ->
	  merge (line_hd::ans) node_codes line_tl
	| node_hd::node_tl, _ ->
	  merge (node_hd::ans) node_tl line_codes
    in
    let codes = merge [] node_codes line_codes in
    let module CalcState = struct
      type t =
	  NextAvailable of int * int
	| ComparableNodes of int * int list
      type node_type = Valid | Virtual
      type lines_automaton = (node_type * int) Queue.t * int ref
      let push (node_type, _ as node) (queue, valid_size) =
	if node_type = Valid then incr valid_size;
	Queue.add node queue;
	if Queue.length queue < Param.connect then None
	else
	  let vs = !valid_size in
	  let popped = Queue.pop queue in
	  if fst popped = Valid then decr valid_size;
	  if vs = Param.connect - 1 then
	    Some (snd popped)
	  else
	    None
    end in
    let open CalcState in
    let lines_of_ys x (y::ys) =
      (* ys are in decrease order *)
      (* ys contain at least 1 elem *)
      let inserted =
	List.fold_left
	  (* always y0 is the last valid *)
	  (fun ((_, y0)::inserted_tl as inserted) ys -> 
	    match ys with
		[] -> (Virtual, y0 - Cordinate.unit)::inserted
	      | y::tl ->
		let upper_virtual = y + Cordinate.unit in
		let inserted = 
		  (if upper_virtual = y0 then inserted
		   else (Virtual, upper_virtual)::innserted)
		in
		(Valid, y)::inserted)
	  [Valid, y; Virtual, y + Cordinate.unit] 
	  ys
      in
      let inserted = (Virtual, snd (hd inserted) - Cordinate.unit)::inserted in
      let lines_of_inserted ans atm inserted =
	match inserted with
	    [] -> ans
	  | hd::tl -> match push 
	  
	  

		    
		    
		
		
	let add_moves state moves =
	  match state with
	      NextAvailable _ -> moves
	    | ComparableNodes (x,ys) ->
	      
	      
      List.fold_left
	(fun (state, moves) (((x1,y1) as c1), c1_is_line) ->
	  match state with
	    | _ when c1_is_line
		->
	      (NextAvailable(x1,y1 + Param.margin * Cordinate.unit),
	       add_moves state moves)
	    | NextAvailable c0 when c0 > c1
		-> (state, moves)
	    | NextAvailable _ -> (ComparableNodes (x1,[y1]), moves)
	    | ComparableNodes(x0, _) when x0 < x1
		-> (ComparableNodes (x1, [y1]), add_moves state moves)
	    | ComparableNodes(x0, y0s) -> (ComparableNodes (x0, y1::y0s), moves)
	  if is_line then
	    NextAvailable (x, y + Margin.value * Cordinate.unit)
	  else
	    
	  
end


module Morpoin : GAME =
struct
  type dir = int
  type cordinate = int * int
  type node = DirView.t list
  type action = dir * cordinate
  let play node (dir,cordinate) =
    if not (playable (List.nth node dir) cordinate) then
      raise InvalidMove
    else
      List.mapi
	(fun i view ->
	  let view = DirView.add_node cordinate view in
	  if dir != i then view
	  else DirView.add_line cordinate view)
	node
  let random_move node =
    let dir_moves = List.map DirView.possible_moves node in
    let totalmoves =
      List.fold_left (fun sum (size, _) -> sum + size)
	0 dir_moves
    in
    let rec choose_nth dir n ((size, moves)::tl) =
      if n >= size then choose_nth (succ dir) (n-size) tl
      else (dir, List.nth n moves)
    in
    choose_nth 0 (Random.int totalmoves) dir_moves
  let possible_moves node =
    List.fold_left
      (fun moves (dir, (_, dir_moves)) ->
	List.fold_left
	  (fun moves dir_move -> (dir,dir_move)::moves)
	  moves
	  dir_moves)
      moves
      (List.mapi
	 (fun dir e -> dir,e)
	 (List.map DirView.possible_moves node))
  let terminal node =
    List.exists DirView.move_exist node
  let score (view::views) =
    node_size view
end
