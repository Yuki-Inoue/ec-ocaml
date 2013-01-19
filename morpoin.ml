
type cordinate_system = {
  of_global : (int * int) -> (int * int);
  to_global : (int * int) -> (int * int);
  unit : int
}

let unit_of_cordinate_system cordinatesystem =
  cordinatesystem.unit



let first_cordinate =
  let id x = x in {
    of_global = id;
    to_global = id;
    unit = 1
  }

let second_cordinate =
  let of_global (x,y) = x-y, x+y in
  let to_global (x,y) = (x+y)/2, (y-x)/2 in
  { of_global; to_global; unit = 2 }


let third_cordinate =
  let switch (x,y) = y,x in
  { of_global = switch; to_global = switch; unit = 1 }

let forth_cordinate =
  let of_global (x,y) = x+y, x-y in
  let to_global (x,y) = (x+y)/2, (x-y)/2 in
  { of_global; to_global; unit = 2}


type entity_type = Node | Line

module EntitySet
  = Set.Make( struct
    type t = (int * int) * entity_type
    let compare = compare
  end )

module type VIEW =
sig
  type t
  val make : cordinate_system -> t
  val add_node : (int * int) -> t -> t
  val add_line : (int * int) -> t -> t
  val possible_moves : t -> int * (int * int) list
  val playable : (int * int) -> t -> bool
  val move_exist : t -> bool
  val node_size : t -> int
  val first_range : t -> int * int
  val node_exist : (int * int) -> t -> bool
end


module type PARAM =
sig
  val connects : int
  val margin : int
end




(*
  the 
*)
module DirView (Param : PARAM) : VIEW =
struct

  type t = {
    cordinate_system : cordinate_system;
    entities : EntitySet.t
  }

  let make cordinate_system = {
    cordinate_system;
    entities = EntitySet.empty
  }

  let add_base local_cordinate view cor_type = {
    cordinate_system = view.cordinate_system;
    entities =
      EntitySet.add
	(local_cordinate, cor_type)
	view.entities
  }

  let add_node node_cor view =
    add_base node_cor view Node
  let add_line line_cor view =
    add_base line_cor view Line


  type calc_state =
	NextAvailable of int * int
      | ComparableNodes of (int * int) * (int * int) list
  type node_type = Valid | Virtual

  let possible_moves view =
    let unit = view.cordinate_system.unit in
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
	      ret := cor::!ret;
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
      EntitySet.fold
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
	view.entities
	(NextAvailable(min_int, min_int), (0,[]))
    in
    last_state >> moves


  (* cor is considered to designate the front of line *)
  let playable (x,y) view =
    let unit = view.cordinate_system.unit in
    let count = ref 0 in
    for i = 0 to pred Param.connects do
      if EntitySet.mem ((x, y+i*unit),Node) view.entities
      then incr count
    done;
    !count = pred Param.connects


  (* naive easy implementation *)
  let move_exist t =
    fst (possible_moves t) = 0


  let node_size view =
    EntitySet.fold
      (fun (_,cord_type) total ->
	match cord_type with
	    Node -> succ total
	  | Line -> total)
      view.entities 0

  let first_range view =
    fst (fst (EntitySet.min_elt view.entities)),
    fst (fst (EntitySet.max_elt view.entities))

  let node_exist cor view =
    EntitySet.mem (cor,Node) view.entities

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
  val print_node : Format.formatter -> node -> unit
  val print_action : Format.formatter -> action -> unit
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

  let print_action formatter (dir, (x,y) : action) =
    Format.pp_print_string
      formatter
      (Printf.sprintf "(dir%i:(%i,%i))" dir x y)

  let add_node cordinate views =
    List.map
      (fun view ->
	DirView.add_node (view.cordinate_system.of_global cordinate) view)
      views

  let add_line (dir,cordinate : action) views =
    List.mapi (fun i view ->
      if i != dir then view
      else
	DirView.add_line
	  cordinate
	  (List.nth views dir)) views

  let play node ((dir,cordinate) as action :action) =
    let dirview = List.nth node dir in
    if not (DirView.playable cordinate dirview) then
      raise AIGame.InvalidAction
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
    [DirView.make first_cordinate;
     DirView.make second_cordinate;
     DirView.make third_cordinate;
     DirView.make forth_cordinate]

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

  let print_node formatter view_list =
    let hd_view = List.hd view_list in
    let (first_begin, first_end) =
      DirView.first_range hd_view
    in
    let (second_begin, second_end) =
      DirView.first_range (List.nth view_list 2)
    in
    let ox_of_bool b = match b with
	true -> 'o'
      | false -> 'x'
    in
    Format.pp_open_vbox formatter 0;
    for i = first_begin to first_end do
      for j = second_begin to second_end do
	Format.pp_print_char formatter
	  (ox_of_bool
	     (DirView.node_exist (i,j) hd_view))
      done;
      Format.pp_print_cut formatter ()
    done;
    Format.pp_close_box formatter ()

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
