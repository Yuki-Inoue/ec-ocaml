open Nestedmc

module MorpoinMonteCarlo = MonteCarlo ( Morpoin.D )

let play lv =
  MorpoinMonteCarlo.nestedmc Morpoin.D.initial lv

let _ =
  let (score, action_list) = play 1 in
  print_int score;
  List.iter
    (fun action ->
      print_string " ";
      print_string (Morpoin.D.string_of_action action))
    action_list
