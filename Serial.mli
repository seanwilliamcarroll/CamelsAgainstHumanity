exception ParseFailure of string

(* Load a deck into play from a JSON file. The JSON file must specify whether
 * the cards are Black or White *)
val load_deck : string -> GameState.game_state -> GameState.game_state

(* Translate a string JSON message to a card *)
val translate_SELECT : string -> GameState.card

(* Translate a string JSON message to a game_state with a message of who won *)
val translate_GAMESTATE : string -> (GameState.game_state * string)

(* Create a string JSON message out of a card *)
val serialize_SELECT : GameState.card -> string

(* Create a string JSON message out of a game_state *)
val serialize_GAMESTATE : GameState.game_state -> string -> string
