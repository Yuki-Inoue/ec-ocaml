
module type CORDINATE =
sig
  val make : (int * int) -> (int * int)
  val unmake : (int * int) -> (int * int)
  val unit : int
end

module FirstCordinate : CORDINATE =
struct
  let make x = x
  let unmake x = x
  let unit = 1
end

module SecondCordinate : CORDINATE=
struct
  let make (x,y) = (x-y, x+y)
  let unmake (x,y) = (x+y)/2, (y-x)/2
  let unit = 2
end

module ThirdCordinate : CORDINATE =
struct
  let make (x,y) = (y,x)
  let unmake = make
  let unit = 1
end

module ForthCordinate : CORDINATE =
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


module type PARAM =
sig
  val connects : int
  val margin : int
end

type cordinate_type = Node | Line

module CordinateSet
  = Set.Make( struct
    type t = (int * int) * cordinate_type
    let compare = compare
  end )

module DirView (Param : PARAM) : VIEW =
struct

  type t = (module CORDINATE) * CordinateSet.t

  let make cordinate =
    cordinate, CordinateSet.empty


  let add_base cor (cordinate, cordinate_set) cor_type =
    let module Cordinate = (val cordinate : CORDINATE) in
    cordinate,
    CordinateSet.add
      ((Cordinate.make cor), cor_type)
      cordinate_set


  let add_node node_cor view =
    add_base node_cor view Node

  let add_line line_cor view =
    add_base line_cor view Line



  let possible_moves (cordinate, cordinate_set) =
    let module Cordinate = (val cordinate : CORDINATE) in
    let unit = Cordinate.unit in
    let module CalcState = struct
      type t =
	  NextAvailable of int * int
	| ComparableNodes of (int * int) * (int * int) list
      type node_type = Valid | Virtual
    end in
    let open CalcState in

    let add_moves (x,y as top) revnodes (move_size, move_list) =
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
	insert_virtuals [(x,y+unit),Virtual] top revnodes
      in (* the inserted is in the increasing order *)

      let buffer = Array.make Param.connects top in
      let valid_size = ref 0 in (* top is always virtual *)
      let index = ref 1 in
      let ret = ref move_list in
      let ret_size = ref move_size in
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
    let (>>) calc_state moves =
      match calc_state with
	  NextAvailable _ -> moves
	| ComparableNodes (c, revnodes) ->
	    add_moves c revnodes moves
    in

    let (last_state, moves) =
      CordinateSet.fold
	(fun elm (state, moves as current) ->
	  match state, elm with
	      NextAvailable _, (_, Line) ->
	      (* by constraint state must be comparables *)
		raise (Failure "Programming Bug in possible_moves")
	    | ComparableNodes _ , ((x,y), Line) ->
	        NextAvailable (x,y + Param.margin*unit), state >> moves
	    | NextAvailable (x0,y0), (c1, Node) when (x0,y0) > c1 ->
	        current
	    | NextAvailable _, ((x,y as c), Node) -> ComparableNodes (c,[]), moves
	    | ComparableNodes ((x0,y0), _), ((x1,y1 as c1), Node)
	        when x0 < x1 || y0 + 2*unit < y1 ->
	        ComparableNodes (c1, []), state >> moves
	    | ComparableNodes (c,nodes), (node, Node) ->
	        ComparableNodes (node,c::nodes), moves)
	cordinate_set
	(NextAvailable(min_int, min_int), (0,[]))
    in
    last_state >> moves


  (* cor is considered to designate the front of line *)
  let playable cor (cordinate, cordinate_set) =
    let module Cordinate = (val cordinate : CORDINATE) in
    let (x,y) = Cordinate.make cor in
    let count = ref 0 in
    for i = 0 to pred Param.connects do
      if CordinateSet.mem ((x, y+i*Cordinate.unit),Node) cordinate_set
      then incr count
    done;
    !count = pred Param.connects


  (* naive easy implementation *)
  let move_exist t =
    fst (possible_moves t) = 0


  let node_size (_,t) =
    CordinateSet.fold
      (fun (_,cord_type) total ->
	match cord_type with
	    Node -> succ total
	  | Line -> total)
      t 0

end

module type S =
sig
  type node
  type action
  val play : node -> action -> node
  val random_move : node -> action
  val possible_moves : node -> action list
  val terminal : node -> bool
  val score : node -> int
  val initial : node
end

(* satisfied AI.GAME *)
module Make (Param: PARAM)
  : S =
struct
  module DirView = DirView(Param)
  type dir = int
  type cordinate = int * int
  type node = DirView.t list
  type action = dir * cordinate

  let add_node cordinate views =
    List.mapi
      (fun i view ->
	DirView.add_node cordinate view)
      views

  let add_line (dir,cordinate) views =
    List.mapi (fun i view ->
      if i != dir then view
      else
	DirView.add_line
	  cordinate
	  (List.nth views dir)) views

  let play node ((dir,cordinate) as action :action) =
    if not (DirView.playable cordinate (List.nth node dir)) then
      raise AI.InvalidAction
    else
      add_line action (add_node cordinate node)

  let random_move (node:node) =
    let (dir_moves : (int * (int * int) list) list) = List.map DirView.possible_moves node in
    let totalmoves =
      List.fold_left (fun sum (size, _) -> sum + size)
	0 dir_moves
    in
    let rec choose_nth dir (n:int) size_moves_list =
      match size_moves_list with
	  [] -> raise (Failure "Program error in random_move")
	| (size,moves)::tl ->
	  if n >= size then choose_nth (succ dir) (n-size) tl
	  else dir, List.nth moves n
    in
    choose_nth 0 (Random.int totalmoves) dir_moves
  let possible_moves node =
    List.fold_left
      (fun moves (dir, (_, dir_moves)) ->
	List.fold_left
	  (fun moves dir_move -> (dir,dir_move)::moves)
	  moves
	  dir_moves)
      []
      (List.mapi
	 (fun dir e -> dir,e)
	 (List.map DirView.possible_moves node))
  let terminal node =
    List.exists DirView.move_exist node
  let score views =
    DirView.node_size (List.hd views)

  let empty =
    [DirView.make (module FirstCordinate);
     DirView.make (module SecondCordinate);
     DirView.make (module ThirdCordinate);
     DirView.make (module ForthCordinate)]

  let initial =
    let o = true in
    let x = false in
    let arr =
      [|[|x;x;x;o;o;o;o;x;x;x|];
	[|x;x;x;o;x;x;o;x;x;x|];
	[|x;x;x;o;x;x;o;x;x;x|];
	[|o;o;o;o;x;x;o;o;o;o|];
	[|o;x;x;x;x;x;x;x;x;o|];
	[|o;x;x;x;x;x;x;x;x;o|];
	[|o;o;o;o;x;x;o;o;o;o|];
	[|x;x;x;o;x;x;o;x;x;x|];
	[|x;x;x;o;x;x;o;x;x;x|];
	[|x;x;x;o;o;o;o;x;x;x|]|]
    in
    let ans = ref empty in
    Array.iteri (fun i ->
      Array.iteri (fun j e ->
	if e then
	  ans := add_node (i,j) !ans))
      arr;
    !ans

end

module AttachedParam : PARAM =
struct
  let connects = 5
  let margin = 4
end

module DetachedParam : PARAM =
struct
  let connects = 5
  let margin = 5
end



module T : S = Make(AttachedParam)
module D : S = Make(DetachedParam)
