
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

  type cordinate_type = Node | Line

  module CordinateSet
    = Set.Make(
      struct type t = (int * int) * cordinate_type let compare = compare end
    )

  type t = (module CORDINATE) * CordinateSet.t

  let make cordinate =
    cordinate, CordinateSet.empty


  let add_base (cordinate, cordinate_set) cor cor_type =
    let module Cordinate = (val cordinate) in
    cordinate,
    CordinateSet.add
      ((Cordinate.make cor), cor_type)
      cordinate_set


  let add_node view node_cor =
    add_base view node_cor Node

  let add_line view line_cor =
    add_base view line_cor Line



  let possible_moves (cordinate, cordinate_set) =
    let module Cordinate = (val cordinate) in
    let unit = Cordinate.unit in
    let module CalcState = struct
      type t =
	  NextAvailable of int * int
	| ComparableNodes of (int * int) * (int * int) list
      type node_type = Valid | Virtual
    end in

    let add_moves ((x,y as top), revnodes) moves =
      let (top, rest) =
	let rec insert_virtuals ret (x0,y0 as valid) revnodes =
	  match revnodes with
	      [] -> ((x0,y0-unit), Virtual), (valid, Valid)::ret
	    | (_,y1 as newvalid)::tl when y1 + unit = y0 ->
	        insert_virtuals ((valid,Valid)::ret) newvalid tl
	    | newvalid::tl ->
	        insert_virtuals
		  (((x0,y0-unit),Virtual)
		   ::(valid,Valid)::ret)
		  newvalid tl
	in
	insert_virtuals [x,y+unit] top revnodes
      in (* the inserted is in the increasing order *)

      let buffer = Array.make Param.connects top in
      let valid_size = ref 0 in (* top is always virtual *)
      let index = ref 1 in
      let ret = ref [] in
      let ret_size = ref 0 in
      let pop_flag = ref false in

      List.iter
	(fun node ->
	  buffer.(!index) <- node;
	  incr index;
	  if snd node = Valid then incr valid_size;
	  if !index = Param.connects then begin
	    pop_flag := true;
	    index := 0
	  end;
	  if !pop_flag then begin
	    let (cor, validity) = buffer.(!index) in
	    if !valid_size = pred Param.connects
	    then begin
	      ret := Cordinate.unmake cor::!ret;
	      incr ret_size
	    end;
	    if validity = Valid then decr valid_size
	  end)
	rest;
      !ret_size, !ret
    in

    CordinateSet.fold
      (fun elm (state, moves) ->
	match state, elm with
	    NextAvailable _, (_, Line) ->
	      (* by constraint state must be comparables *)
	      raise Failure
	  | ComparableNodes nodes, ((x,y), Line) ->
	      NextAvailable (x,y + Param.margin*unit),
	      add_moves nodes moves
	  | NextAvailable c0, (c1, Node) when c0 > c1 ->
	      state, moves
	  | NextAvailable _, ((x,y as c), Node) -> ComparableNodes [c], moves
	  | ComparableNodes ((x0,y0), _ as nodes), ((x1,y1 as c1), Node)
	      when x0 < x1 || y0 + 2*unit < y1 ->
	      ComparableNodes (c1, []),
		add_moves nodes moves
	  | ComparableNodes (c,nodes), (node, Node) ->
	      ComparableNodes (node,c::nodes), moves)
      cordinate_set
      (NextAvailable(min_int, min_int), [])


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
