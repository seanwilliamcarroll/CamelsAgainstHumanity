open GameState

(******************************************************************************)
(** Types and Exceptions ******************************************************)
(******************************************************************************)

exception ParseFailure of string

(******************************************************************************)
(** Helper Functions **********************************************************)
(******************************************************************************)

(* Get a string [member] from [json]. *)
let get_string member json =
  Yojson.Basic.Util.to_string (Yojson.Basic.Util.member member json)

(* Get a integer [member] from [json]. *)
let get_int member json =
  Yojson.Basic.Util.to_int (Yojson.Basic.Util.member member json)

(* Get a list [member] from [json]. Translate each element of the list according
 * to [converter]. *)
let get_list converter member json =
  Yojson.Basic.Util.convert_each converter (Yojson.Basic.Util.member member json)

(* Get a list [member] from [json]. Translate each element of the list to a
 * string. *)
let get_string_list member json =
  get_list Yojson.Basic.Util.to_string member json

(* Translates a Yojson object to a player object *)
let json_to_player json =
  let name = get_string "name" json in 
  let points = get_int "points" json in
  let hand = GameState.make_cards (get_string_list "hand" json) in
  GameState.make_player name points hand

(* Translates a Yojson object to a (string * card). Used for the white_table *)
let json_to_white_table json =
  let name = get_string "name" json in
  let card = GameState.make_card (get_string "card" json) in
  (name,card)

(* Create a string JSON out of a player's [hand]. *)
let rec serialize_hand hand =
  match hand with
  | [] -> ""
  | h::[] -> "\"" ^ GameState.string_of_card h ^ "\""
  | h::t -> "\"" ^ GameState.string_of_card h ^ "\"," ^ (serialize_hand t)

(* Create a string JSON out of a [player]. *)
let serialize_player player =
  "{" ^
        "\"name\": \"" ^ player.name ^ "\"," ^
        "\"points\": " ^ string_of_int player.points ^ "," ^
        "\"hand\": [" ^ serialize_hand player.hand ^ "]" ^
  "}"

(* Create a string JSON out of a list of [players]. *)
let rec serialize_players players = 
  match players with
  | [] -> ""
  | h::[] -> serialize_player h
  | h::t -> serialize_player h ^ "," ^ (serialize_players t)

(* Create a string JSON out of a card on a white_table. *)
let serialize_card (name,card) =
  "{" ^
        "\"name\": \"" ^ name ^ "\"," ^
        "\"card\": \"" ^ GameState.string_of_card card ^ "\"" ^
  "}"

(* Create a string JSON out of a [white_table]. *)
let rec serialize_white_table white_table =
  match white_table with
  | [] -> ""
  | h::[] -> serialize_card h
  | h::t -> serialize_card h ^ "," ^ (serialize_white_table t)

(* Create a string JSON out of a list of cards in a [deck]. *)
let rec serialize_deck deck =
  match deck with
  | [] -> ""
  | h::[] -> "\"" ^ GameState.string_of_card h ^ "\""
  | h::t -> "\"" ^ GameState.string_of_card h ^ "\"," ^ (serialize_deck t)

(******************************************************************************)
(** .mli Functions ************************************************************)
(******************************************************************************)

let load_deck file game_state =
  let json = Yojson.Basic.from_file file in
  let deck_type = get_string "deck_type" json in
  let deck = get_string_list "deck" json in
  match deck_type with
  | "white" -> 
     { hand_size = game_state.hand_size;
       players = game_state.players;
       turn = game_state.turn;
       black_table = game_state.black_table;
       white_table = game_state.white_table;
       white_deck = GameState.make_deck deck;
       black_deck = game_state.black_deck; }
  | "black" ->
     { hand_size = game_state.hand_size;
       players = game_state.players;
       turn = game_state.turn;
       black_table = game_state.black_table;
       white_table = game_state.white_table;
       white_deck = game_state.white_deck;
       black_deck = GameState.make_deck deck; }
  | _ -> raise (ParseFailure "You have an incorrectly formatted deck")

let translate_SELECT message =
  let json = Yojson.Basic.from_string message in
  let msg_type = get_string "msg_type" json in
  if msg_type <> "SELECT" then
    raise (ParseFailure "You cannot translate a game state into a card")
  else
    GameState.make_card (get_string "card" json)

let translate_GAMESTATE message = 
  let json = Yojson.Basic.from_string message in
  let msg_type = get_string "msg_type" json in
  if msg_type <> "GAMESTATE" then
    raise (ParseFailure "You cannot translate a card into a game state")
  else
    let hs = get_int "hand_size" json in
    let players = get_list json_to_player "players" json in
    let turn = match get_list json_to_player "turn" json with
      | [x] -> x
      | _ -> raise (ParseFailure "Incorrectly formatted GAMESTATE message")
    in
    let bt = GameState.make_card (get_string "black_table" json) in
    let wt = get_list json_to_white_table "white_table" json in
    let wd = GameState.make_deck (get_string_list "white_deck" json) in
    let bd = GameState.make_deck (get_string_list "black_deck" json) in
    let win_msg = get_string "win_msg" json in
    let game_state = GameState.make_game_state hs players turn bt wt wd bd in
    (game_state, win_msg)

let serialize_SELECT c =
  let card = GameState.string_of_card c in
  "{" ^
        "\"msg_type\": \"SELECT\"," ^
        "\"card\": \"" ^ card ^ "\"" ^
  "}" |> Yojson.Basic.prettify

let serialize_GAMESTATE gs win_msg =
  let hs = string_of_int gs.hand_size in
  let bt = GameState.string_of_card gs.black_table in
  let turn = serialize_player gs.turn in
  let players = serialize_players gs.players in
  let wt = serialize_white_table gs.white_table in
  let wd = serialize_deck gs.white_deck in
  let bd = serialize_deck gs.black_deck in
  "{" ^
        "\"msg_type\": \"GAMESTATE\"," ^
        "\"hand_size\": " ^ hs ^ "," ^
        "\"players\": [" ^ players ^ "]," ^
        "\"turn\": [" ^ turn ^ "]," ^
        "\"black_table\": \"" ^ bt ^ "\"," ^
        "\"white_table\": [" ^ wt ^ "]," ^
        "\"white_deck\": [" ^ wd ^ "]," ^
        "\"black_deck\": [" ^ bd ^ "]," ^
        "\"win_msg\": \"" ^ win_msg ^ "\"" ^
  "}" |> Yojson.Basic.prettify
