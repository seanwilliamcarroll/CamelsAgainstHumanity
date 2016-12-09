(* [GameOver] is raised when the game has ended. Currenty, this is when
 * game_state.white_deck or game_state.black_deck is empty *)
exception GameOver
(* [Illegal] is raised when a player attempts to call an unspecified command *)
exception Illegal

(* This will represent a single Cards Against Humanity card. *)
type card

(* [deck] represents a full deck of cards, Black or White. *)
type deck = card list

(* [cards] represents a set of cards, such as a player's hand, or the current
 * cards on a table. *)
type cards = card list

(* [player] represents a player in the game. Fields are a player’s name (string),
 * points (int), and hand (cards). *)
type player = {
  name : string;
  points : int;
  hand : cards;
}

(* [game_state] represents the current state of the game *)
type game_state = {
  (* maximum hand size for a game. *)
  hand_size:int;
  (* players in the game *)
  players : player list;
  (* whose turn is it? *)
  turn : player;
  (* which black card is currently on the table? *)
  black_table : card;
  (* which white cards are currently on the table? who played them? *)
  white_table : (string * card) list;
  (* which white cards are left in the deck? *)
  white_deck : deck;
  (* which black cards are left in the deck? *)
  black_deck : deck;
}

(* Type constructors for the above types *)
val make_card : string -> card
val make_deck : string list -> deck
val make_cards : string list -> cards
val make_player : string -> int -> cards -> player
val make_game_state : int -> player list -> player -> card -> (string * card) list
  -> deck -> deck -> game_state

(* string conversion for the above types *)
val string_of_card : card -> string

(* Initialize an "empty" game_state before anyone has joined *)
val init_game_state : unit -> game_state

(* Shuffle a given deck. *)
val shuffle : deck -> deck

(* Get the maximum score in the game_state *)
val get_max_score : game_state -> int

(* Get the name of the player with the maximum score in the game_state *)
val get_max_player : game_state -> string

(* Populate players hands with cards from the white deck *)
val populate_hands : game_state -> game_state

(* Used to draw a white card into everyone's hand from the current
 * game_state.white_deck. *)
val draw_white_cards : game_state -> game_state

(* Used to draw a white card into a player’s hand from the current
 * game_state.white_deck. *)
val draw_white_card : player -> game_state -> game_state

(* Used to draw a black card onto the table from the current
 * game_state.black_deck. *)
val draw_black_card : game_state -> game_state

(* Used by a player to play a white card onto game_state.white_table. *)
val play_white_card : player -> card -> game_state -> game_state

(* Choose a white card from game_state.white_table as the winning card.
 * Increment the corresponding player's points by +1 *)
val choose_white_card : card -> game_state -> (game_state * string)

(* Return as a string the cards on the white table in the given game_state *)
val check_white_table : game_state -> string

(* Return as a string the given black card on the black table *)
val check_black_table_pretty : card -> string

(* Return as a pretty string the card's in a given hand *)
val check_hand_pretty : cards -> int ref -> string

(* Return as a string the card's in a given hand *)
val check_hand : cards -> string

(* Return as a string the relevant game information for the player *)
val check_game_state_player : player -> game_state -> string

(* Return as a string the relevant game information for the judge *)
val check_game_state_judge : game_state -> string
