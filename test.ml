open OUnit2
open GameState
open Serial

let card = "testing white card"
let card' = make_card card

let select =
  "{ \"msg_type\": \"SELECT\", \"card\": \"" ^ card ^ "\" }"

let json = Yojson.Basic.from_file "GAMESTATE_TEST.json"
let gs_json = Yojson.Basic.prettify (Yojson.Basic.to_string json)
let player = make_player "Lauren" 10 [card']
let gs = make_game_state 5 [player] player card' [("Lauren",card')] [card'] [card']
let win_msg = "Sean won the last round!"

let gamestate_tests =
[
  "check card translation" >:: (fun _ -> assert_equal "testing white card" (string_of_card card'));
]

let serial_tests =
[
  "check SELECT translation" >:: (fun _ -> assert_equal card' (translate_SELECT select));
  "check SELECT serialization" >:: (fun _ -> assert_equal select (serialize_SELECT card'));
  "check GAMESTATE serialization" >:: (fun _ -> assert_equal gs_json (serialize_GAMESTATE gs win_msg));
]

let suite =
  "Camels Against Humanity test suite"
  >::: gamestate_tests @ serial_tests

let _ = run_test_tt_main suite
