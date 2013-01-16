
exception InvalidAction

module type GAME =
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
