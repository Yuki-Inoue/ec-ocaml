
module type PARAM =
sig
  val connects : int
  val margin : int
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

module Make : functor (Param : PARAM) -> S

(* attached morpoin *)
module T : S

(* detached morpoin *)
module D : S
