module type CORDINATE_SYSTEM =
sig
  type 'a cordinate constraint 'a = [< `Global | `Local ]
  type t = {
    of_global : [`Global] cordinate -> [`Local] cordinate;
    to_global : [`Local] cordinate -> [`Global] cordinate;
    unit : int
  }
  val compare : 'a cordinate -> 'a cordinate -> int
  val local_cordinate : (int * int) -> [`Local] cordinate
  val global_cordinate : (int * int) -> [`Global] cordinate
  val values : 'a cordinate -> (int * int)
  val cordinate_systems : t array
  val up : 'a cordinate -> t -> 'a cordinate
  val print_cordinate : Format.formatter -> 'a cordinate -> unit
end

module CordinateSystem : CORDINATE_SYSTEM =
struct

  type 'a cordinate = int * int
  constraint 'a = [< `Global | `Local ]

  type t = {
    of_global : [`Global] cordinate -> [`Local] cordinate;
    to_global : [`Local] cordinate -> [`Global] cordinate;
    unit : int
  }


  let print_cordinate formatter (x,y) =
    Format.fprintf formatter "(%d,%d)" x y


  let local_cordinate x = x
  let global_cordinate x = x
  let values x = x
  let compare = compare

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


  let cordinate_systems =
    [|first_cordinate;
      second_cordinate;
      third_cordinate;
      forth_cordinate|]

  let up (x,y) cordinate_system = (x, y+cordinate_system.unit)

end


type entity_type = Node | Line
type 'a cordinate = 'a CordinateSystem.cordinate
type size = int
type entity = [`Local] cordinate * entity_type

module EntitySet
  = Set.Make( struct
    open CordinateSystem
    type t = entity
    let compare (c1, t1) (c2, t2) =
      let result1 = CordinateSystem.compare c1 c2 in
      if result1 != 0 then result1
      else Pervasives.compare t1 t2
  end )


module GlobalCordinateSet =
  Set.Make (struct
    type t = [`Global] CordinateSystem.cordinate
    let compare = CordinateSystem.compare
  end)

module type VIEW =
sig
  type t
  val make : CordinateSystem.t -> t
  val cordinate_system_of_view : t -> CordinateSystem.t
  val add_node : [`Local] cordinate -> t -> t
  val add_line : [`Local] cordinate -> t -> t
  val possible_moves : t -> size * [`Local] cordinate list
  val playable : [`Local] cordinate -> t -> bool
  val move_exist : t -> bool
  val num_nodes : t -> int
  val num_lines : t -> int
  val first_range : t -> int * int
  val node_exist : [`Local] cordinate -> t -> bool
  val nodes : t -> GlobalCordinateSet.t
  val insertion_node_of_line : [`Local] cordinate -> t -> [`Local] cordinate
  val print_view : Format.formatter -> t -> unit
end


module type PARAM =
sig
  val connects : int
  val margin : int
end




module DirView (Param : PARAM) : VIEW =
struct

  type t = {
    cordinate_system : CordinateSystem.t;
    entities : EntitySet.t
  }


  let count_entities_by_type typ {entities; _} =
    EntitySet.fold
      (fun (_, elmtyp) count ->
	if typ = elmtyp then succ count
	else count)
      entities 0

  let num_lines view =
    count_entities_by_type Line view

  let num_nodes view =
    count_entities_by_type Node view


  let nodes { entities; cordinate_system } =
    EntitySet.fold
      (fun elt global_cords ->
	if snd elt = Line then
	  global_cords
	else
	  GlobalCordinateSet.add
	    (cordinate_system.CordinateSystem.to_global
	       (fst elt))
	    global_cords)
      entities
      GlobalCordinateSet.empty


  type dirview_t = t

  let make cordinate_system = {
    cordinate_system;
    entities = EntitySet.empty
  }

  let cordinate_system_of_view { cordinate_system; _ } =
    cordinate_system

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


  module PossibleMoves =
  struct

    type state =
	NextAvailable of int * int
      | ConnectableNodes of (int * int) * (int * int) list


    (* type for calculating connectable nodes *)
    type token = {
      cordinate : int * int;
      node_exist : bool
    }


    (*

      by folding of set, we get something like
      (an, an-1), ... (a4, a3), (a2, a1, a0)
      like list of list.

      for each list in the outer list.

      ai+2, ai+1, ai  ==> tokens of ai-1, ai, ai+1, ai+2, ai+3
      tokens to lines would add this to ans list.
      ans list should be the proper order.
      eager adding is performed to do this.
    *)

    type t = {
      size : int;
      content : [`Local] cordinate list
    }

    type fold_argument = {
      state : state;
      ans_list : t
    }



    let rec tokens_of_rev_position_list
	~unit
	(ans: token list)
	(x1,y1 as c1 : int * int)
	(position_rest: (int * int) list) =
      match position_rest with
	  [] ->
	    { cordinate = x1,y1-unit; node_exist=false }
	    ::{ cordinate = c1; node_exist=true }
	    ::ans
	| (_,y0 as c0)::tl when y0 + unit = y1 ->
	  tokens_of_rev_position_list ~unit
	    ({ cordinate = c1; node_exist = true } ::ans)
	    c0 tl
	| (x0,y0 as c0)::tl ->
	  tokens_of_rev_position_list ~unit
	    ({ cordinate = x0, y0+unit; node_exist = false }::
		{ cordinate = c1; node_exist = true }::ans)
	    c0 tl



    (* tokens returned in order *)
    let tokens_of_state ~unit state =
      match state with
	  NextAvailable _ -> []
	| ConnectableNodes (x,y as c, cs) ->
	  tokens_of_rev_position_list ~unit
	    [{cordinate = x, y+unit; node_exist = false}]
	    c cs

    let eligible_for_line token_list =
      List.fold_left
	(fun count token ->
	  if token.node_exist
	  then succ count
	  else count)
	0 token_list = pred Param.connects

    let popped_and_queue_eligible popped queue =
      eligible_for_line
	(popped ::
	   (List.rev
	      (Queue.fold
		 (fun obj elm -> elm :: obj)
		 [] queue)))


    (* queue: sized less than Param.connects *)
    let rec add_tokens_base node_count queue tokens sized_list =
      match tokens with
	  [] -> sized_list
	| ({cordinate; node_exist} as token)::rest_of_tokens ->
	  let node_count = ref node_count in
	  let newlist = ref sized_list in
	  Queue.add token queue;
	  if node_exist then incr node_count;
	  if Queue.length queue = Param.connects then begin
	    let popped = Queue.pop queue in
	    if !node_count = Param.connects - 1 then begin
	      assert (popped_and_queue_eligible popped queue);
	      newlist := {
		size =
		  succ sized_list.size;
		content =
		  CordinateSystem.local_cordinate
		    popped.cordinate :: sized_list.content
	      }
	    end;
	    if popped.node_exist then decr node_count
	  end;
	  add_tokens_base !node_count queue rest_of_tokens !newlist


    (* tokens are in order *)
    let add_tokens tokens list =
      add_tokens_base 0 (Queue.create ()) tokens list


    let tokens_adjacent ~unit smaller bigger =
      let (x0,y0) = smaller.cordinate in
      let (x1,y1) = bigger.cordinate in
      x0 = x1 && y0 + unit = y1


    let rec valid_tokens_base ~unit top_token rest_tokens =
      match rest_tokens with
	  [] -> true
	| hd_token::tail_tokens ->
	  (top_token.node_exist || hd_token.node_exist)
	  && tokens_adjacent ~unit top_token hd_token
	  && valid_tokens_base ~unit hd_token tail_tokens

    let valid_tokens ~unit tokens =
      match tokens with
	  [] -> true
	| hd::tl -> valid_tokens_base ~unit hd tl


    (* the state is in rev order *)
    let cons_ans ~unit state ans_list =
      let tokens = tokens_of_state ~unit state in
      assert( valid_tokens ~unit tokens );
      add_tokens tokens ans_list

    let folding_function unit entity ({state; ans_list } as current_folding) =
      match state, entity with
	  _, (cordinate, Line) ->
	    let (x,y) = CordinateSystem.values cordinate in {
	    state = NextAvailable (x, y + Param.margin * unit);
	    ans_list = cons_ans ~unit state ans_list
	  }
	| NextAvailable(x0,y0), (c1, Node) ->
	  let (x1,y1) = CordinateSystem.values c1 in
	  if (x0,y0) > (x1,y1) then current_folding
	  else {
	    state = ConnectableNodes ((x1,y1), []);
	    ans_list
	  }
	| ConnectableNodes((x0,y0 as c0), cs), (c1, Node) ->
	  let (x1,y1 as c1) = CordinateSystem.values c1 in
	  if x0 < x1 || y0 + 2*unit < y1 then {
	    state = ConnectableNodes (c1, []);
	    ans_list = cons_ans ~unit state ans_list
	  }
	  else {
	    state = ConnectableNodes(c1, c0::cs);
	    ans_list
	  }


    let calculate view =
      let folded_obj =
	EntitySet.fold
	  (folding_function view.cordinate_system.CordinateSystem.unit)
	  view.entities
	  {state = NextAvailable(min_int, min_int);
	   ans_list = {size = 0; content = []}}
      in
      cons_ans
	view.cordinate_system.CordinateSystem.unit
	folded_obj.state
	folded_obj.ans_list

  end

  (* cor is considered to designate the front of line *)
  let playable local_cordinate view =
    let (x,y) = CordinateSystem.values local_cordinate in
    let unit = view.cordinate_system.CordinateSystem.unit in
    let count = ref 0 in
    for i = 0 to pred Param.connects do
      if EntitySet.mem
	(CordinateSystem.local_cordinate
	   (x, y+i*unit),Node) view.entities
      then incr count
    done;
    !count = pred Param.connects


  let possible_moves view =
    let ans = PossibleMoves.calculate view in
    let anslist = ans.PossibleMoves.content in
    assert ( List.for_all (fun c -> playable c view) anslist );
    ans.PossibleMoves.size, anslist


  let rec insertion_node_of_line_base local_cordinate view =
    if not (EntitySet.mem (local_cordinate, Node) view.entities) then
      local_cordinate
    else
      insertion_node_of_line_base
	(CordinateSystem.up local_cordinate view.cordinate_system)
	view

  (* assumes the line to be valid *)
  let insertion_node_of_line local_cordinate view =
    assert (playable local_cordinate view);
    insertion_node_of_line_base local_cordinate view


  (* naive easy implementation *)
  let move_exist t =
    fst (possible_moves t) > 0


  let node_size view =
    EntitySet.fold
      (fun (_,cord_type) total ->
	match cord_type with
	    Node -> succ total
	  | Line -> total)
      view.entities 0

  let first_range view =
    let first_value_of_entity c =
      fst (CordinateSystem.values (fst c))
    in
    first_value_of_entity (EntitySet.min_elt view.entities),
    first_value_of_entity (EntitySet.max_elt view.entities)


  let second_range view =
    let update_range value (bottom, top) =
      (min bottom value, max top value)
    in
    EntitySet.fold
      (fun (local_cordinate, _) ->
	update_range
	  (snd (CordinateSystem.values local_cordinate)))
      view.entities
      (max_int, min_int)


  let node_exist cor view =
    EntitySet.mem (cor,Node) view.entities

  let ox_of_bool bool =
    match bool with
	true -> 'O'
      | false -> 'X'

  let print_view formatter view =
    let (first_begin, first_end) = first_range view in
    let (second_begin, second_end) = second_range view in
    Format.pp_open_vbox formatter 0;
    for i = first_begin to first_end do
      for j = second_begin to second_end do
	let local_cordinate =
	  CordinateSystem.local_cordinate (i,j)
	in
	Format.pp_print_char formatter
	  (ox_of_bool
	     (node_exist local_cordinate view))
      done;
      Format.pp_print_cut formatter ()
    done;
    Format.pp_close_box formatter ()


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
  val string_of_action : action -> string
end

(* satisfied AI.GAME *)
module Make (Param: PARAM)
  : S =
struct
  module DirView = DirView(Param)
  type dir = int
  type cordinate = [`Local] CordinateSystem.cordinate
  type node = DirView.t array
  type action = dir * cordinate


  let view_local_cordinate global_cordinate view =
    (DirView.cordinate_system_of_view view).
      CordinateSystem.of_global global_cordinate

  let global_cordinate_of_view_cordinate local_cordinate view =
    (DirView.cordinate_system_of_view view).
      CordinateSystem.to_global local_cordinate

  let string_of_action (dir, cord : action) =
    let cordinate_system =
      CordinateSystem.cordinate_systems.(dir)
    in
    let global_cordinate =
      cordinate_system.CordinateSystem.to_global cord
    in
    let (x,y) = CordinateSystem.values global_cordinate in
    Printf.sprintf "(dir%i:(%i,%i))" dir x y



  let print_action formatter action =
    Format.pp_print_string
      formatter
      (string_of_action action)

  let add_node global_cordinate views =
    Array.map
      (fun view ->
	let local_cordinate =
	  view_local_cordinate global_cordinate view in
	DirView.add_node local_cordinate view) views

  let add_line (dir,cordinate : action) views =
    Array.mapi (fun i view ->
      if i != dir then view
      else
	DirView.add_line
	  cordinate
	  views.(dir)) views

  let num_nodes (views : node) =
    DirView.num_nodes views.(0)

  let num_lines (views : node) =
    Array.fold_left
      (fun (total:int) view ->
	total + DirView.num_lines view)
      0 views

  let rec elements_equal prev_hd l =
    match l with
	[] -> true
      | hd::tl when hd = prev_hd -> elements_equal hd tl
      | _ -> false


  (* asserts that the arg node is sound *)
  let verify_node (views : node) : bool =
    let view_nodes = Array.map DirView.nodes views in
    let all_same = ref true in
    let index = ref 0 in
    while !all_same && !index < 3 do
      if not (GlobalCordinateSet.equal
	    view_nodes.(!index)
	    view_nodes.(!index+1)) then
	all_same := false;
      incr index
    done;
    !all_same



  let play node ((dir,cordinate) as action :action) =
    let dirview = node.(dir) in
    if not (DirView.playable cordinate dirview) then
      raise AIGame.InvalidAction
    else
      let insertion_cordinate =
	DirView.insertion_node_of_line cordinate dirview
      in
      let insertion_global_cordinate =
	global_cordinate_of_view_cordinate
	  insertion_cordinate
	  dirview
      in
      let ret = add_line action (add_node insertion_global_cordinate node) in
      assert( verify_node node );
      assert( succ (num_nodes node) = num_nodes ret );
      assert( succ (num_lines node) = num_lines ret );
      ret

  let random_move (node:node) =
    let dir_moves =
      Array.map DirView.possible_moves node in
    let totalmoves =
      Array.fold_left (fun sum (size, _) -> sum + size)
	0 dir_moves
    in
    let r = ref (Random.int totalmoves) in
    let index = ref 0 in
    while fst dir_moves.(!index) <= !r do
      r := !r - fst dir_moves.(!index);
      incr index;
    done;
    !index, List.nth (snd dir_moves.(!index)) !r

  let possible_moves (node : node) =
    let dir_possible_moves =
      Array.fold_right
	(fun dirview anslist ->
	  (DirView.possible_moves dirview)::anslist)
	node []
    in
    List.fold_left
      (fun moves (dir, (_, dir_moves)) ->
	List.fold_left
	  (fun moves dir_move -> (dir,dir_move)::moves)
	  moves
	  dir_moves)
      []
      (List.mapi
	 (fun dir e -> dir,e)
	 dir_possible_moves)

  let terminal node =
    not (List.exists DirView.move_exist (Array.to_list node))

  let score views =
    DirView.num_nodes views.(0)

  let empty =
    Array.map
      DirView.make
      CordinateSystem.cordinate_systems

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
	  ans :=
	    add_node
	    (CordinateSystem.global_cordinate (i,j)) !ans))
      arr;
    !ans

  let print_node formatter views =
    let hd_view = views.(0) in
    let (first_begin, first_end) =
      DirView.first_range hd_view
    in
    let (second_begin, second_end) =
      DirView.first_range views.(2)
    in
    let ox_of_bool b = match b with
	true -> 'o'
      | false -> 'x'
    in
    Format.pp_open_vbox formatter 0;
    for i = first_begin to first_end do
      for j = second_begin to second_end do
	let global_cordinate =
	  CordinateSystem.global_cordinate (i,j)
	in
	let local_cordinate =
	  view_local_cordinate global_cordinate hd_view
	in
	Format.pp_print_char formatter
	  (ox_of_bool
	     (DirView.node_exist local_cordinate hd_view))
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
