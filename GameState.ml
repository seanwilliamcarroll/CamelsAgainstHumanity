(******************************************************************************)
(** Types and Exceptions ******************************************************)
(******************************************************************************)

exception GameOver
exception Illegal

type card = string
type deck = card list
type cards = card list

type player = {
  name : string;
  points : int;
  hand : cards;
}

type game_state = {
  hand_size:int;
  players : player list;
  turn : player;
  black_table : card;
  white_table : (string * card) list;
  white_deck : deck;
  black_deck : deck;
}

(******************************************************************************)
(** Type Constructors *********************************************************)
(******************************************************************************)

let make_card str : card =
  str

let string_of_card card : string = 
  card

let make_deck strs : deck =
  strs

let make_cards strs : cards =
  strs

let make_player name points hand : player =
  { name = name;
    points = points;
    hand = hand; }

let make_game_state hs players turn bt wt wd bd : game_state =
  { hand_size = hs;
    players = players;
    turn = turn;
    black_table = bt;
    white_table = wt;
    white_deck = wd;
    black_deck = bd; }

(******************************************************************************)
(** Helper Functions **********************************************************)
(******************************************************************************)

(* Replace a player in [player_list] with the same name as [new_player] with
 * [new_player]. *)
let rec replace_player new_player player_list acc =
  match player_list with
  | [] -> raise Illegal
  | h::t ->
     if h.name = new_player.name then [new_player] @ acc @ t
     else replace_player new_player t (h::acc)

(* Remove [card] from [player]'s hand. [acc] acts as an accumulator as you loop
 * through [player]'s hand. *)
let rec pull_card hand card acc =
  match hand with
  | [] -> raise Illegal
  | h::t ->
     if h = card then t @ acc
     else pull_card t card (h::acc)

(* Identify the name of the player who played [card] on [white_table] *)
let rec winner_card card white_table =
  match white_table with
  | [] -> raise Illegal
  | (n,c)::t ->
     if c = card then n
     else winner_card card t

(* Find the player in [players] with the same name as [winner] and increment
 * their points by +1 *)
let rec winner_points winner players acc =
  match players with
  | [] -> raise Illegal
  | h::t ->
     if h.name = winner then
       let new_player = { name = h.name;
                          points = h.points + 1;
                          hand = h.hand } in
       [new_player] @ t @ acc
     else winner_points winner t (h::acc)

(* Return as a string the points of every player in [players]. Increments the
 * [winner]'s points by 1. *)
let rec check_points players winner =
  match players with
  | [] -> ""
  | h::t -> 
     if h.name = winner then 
       h.name ^ ": " ^ string_of_int (h.points + 1) ^ "\n" ^ check_points t winner
     else
       h.name ^ ": " ^ string_of_int h.points ^ "\n" ^ check_points t winner

(* Get the cards from the [white_table] and put them in a list *)
let rec get_white_table_cards white_table =
  match white_table with
  | [] -> []
  | (_,c)::t -> c::(get_white_table_cards t)

(* Return as a string the cards in [white_table] *)
let rec check_white_table' white_table =
  match white_table with
  | [] -> ""
  | (_,c)::t -> c ^ "\n" ^ check_white_table' t

(* Populate [player] with [hs] number of cards from [deck] *)
let rec populate_player player deck hs = 
  match deck with
  | [] -> raise GameOver
  | h::t ->
     let new_player = { name = player.name;
                        points = player.points;
                        hand = h::player.hand } in
     if (List.length new_player.hand) = hs then (new_player,t)
     else populate_player new_player t hs

(* Populate [players] with [hs] number of cards from [deck].
 * [acc] acts as an accumulator *)
let rec populate_players players hs deck acc =
  match players with
  | [] -> (acc,deck)
  | h::t -> 
     let (new_player,new_deck) = populate_player h deck hs in
     populate_players t hs new_deck (new_player::acc)

(* Gets the maximum score and winner out of [players]. Uses [max] to
 * keep track *)
let rec get_max_score' players (max,player) =
  match players with
  | [] -> (max,player)
  | h::t ->
     if h.points > max then get_max_score' t (h.points,h.name)
     else get_max_score' t (max,player)

(******************************************************************************)
(** Pretty Printing ***********************************************************)
(******************************************************************************)

(* MAX WHITE CARD LENGTH = 16 * 7 = 112 chars *)
(* MAX BLACK CARD LENGTH = 40 * 7 = 280 chars *)
let white_card_width = 16
let black_card_width = 40
let card_height = 7

(* Increment a [counter] and return it as a string *)
let inc_and_return (counter : int ref) : string =
  let () = counter := !counter + 1 in
  string_of_int !counter

(* Replace the last character in [str] with the value of [counter] *)
let add_index str counter =
  let str_trunc = String.sub str 0 (String.length str - 1) in
  str_trunc ^ (inc_and_return counter)

(* [pad_space str length] pads [str] to with spaces to [length] *)
let rec pad_spaces str length =
 if String.length str < length then
   pad_spaces (str ^ " ") length
 else str

(* [truncate_word word length] truncates [word] to [length] and returns a tuple
 * of the first half of the word and the second. Note that the first half is 
 * padded with a space on either side *)
let truncate_word word length =
  let first = Str.first_chars word (length-2) in
  let second = Str.last_chars word ((String.length word) - length + 2) in
  (" " ^ first ^ " ",second)

(* [prettify_line word_list acc] creates a line of length [width] out of
 * the words in [word_list]. It returns a tuple of the line and the rest of the
 * unused words. [acc] is an accumulator for the line. *)
let rec prettify_line word_list acc width =
  if String.length acc < width then
    match word_list with
    | h::t -> 
       if String.length (acc ^ " " ^ h) > width then
         (* if this is the first word, you gotta wrap it *)
         if String.length acc = 0 then
           let (first_half,sec_half) = truncate_word h width in
           (first_half,sec_half::t) (* add sec_half back to word pool *)
         else
           (pad_spaces acc width,word_list)
       else prettify_line t (acc ^ " " ^ h) width
    | [] -> (pad_spaces acc width,[])
  else (acc,word_list)

(* [prettify_words word_list acc] breaks word list into [card_height] number of
 * lines, returned in a list representation. [acc] is an accumulator for this
 * list of lines. *)
let rec prettify_words word_list acc width =
  if List.length acc < card_height then
    let (line,new_word_list) = prettify_line word_list "" width in
    prettify_words new_word_list (acc @ [line]) width
  else acc

(* [prettify_card c] breaks a card [c] into a list of [card_height] number of
 * strings. If the card is short, the list is populated with spaces *)
let prettify_card c width =
  let word_list = Str.split (Str.regexp "[ \t]+") c in
  match prettify_words word_list [] width with
  | [a;b;c;d;e;f;g] -> (a,b,c,d,e,f,g)
  | _ -> ("","","","","","","") (* shouldn't happen *)

(******************************************************************************)
(** .mli Functions ************************************************************)
(******************************************************************************)

let init_game_state () = 
  let player =
    { name = "";
      points = 0;
      hand = []; } in
  { hand_size = 0;
    players = [];
    turn = player;
    black_table = make_card "";
    white_table = [];
    white_deck = make_deck [];
    black_deck = make_deck []; }

(* Citation:
 * http://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml *)
let shuffle deck =
  let () = Random.init (int_of_float (Unix.time ())) in
  let num_deck = List.map (fun c -> (Random.bits (), c)) deck in
  let sorted = List.sort compare num_deck in
  List.map snd sorted

let get_max_score game_state = fst (get_max_score' game_state.players (0,""))
let get_max_player game_state = snd (get_max_score' game_state.players (0,""))

let populate_hands game_state =
  let (new_players,new_deck) = populate_players game_state.players game_state.hand_size game_state.white_deck [] in
    { hand_size = game_state.hand_size;
      players = new_players;
      turn = game_state.turn;
      black_table = game_state.black_table;
      white_table = game_state.white_table;
      white_deck = new_deck;
      black_deck = game_state.black_deck; }

let draw_white_card player game_state =
  match game_state.white_deck with
  | [] -> raise GameOver
  | h::t ->
     let new_player = { name = player.name;
                        points = player.points;
                        hand = h::player.hand } in
     { hand_size = game_state.hand_size;
       players = replace_player new_player game_state.players [];
       turn = game_state.turn;
       black_table = game_state.black_table;
       white_table = game_state.white_table;
       white_deck = t;
       black_deck = game_state.black_deck; }

(* Draws white cards into every [players] hand in [game_state] except the person
 * whose turn it is *)
let rec draw_white_cards' players game_state =
  match players with
  | [] -> game_state
  | h::t -> 
     if h.name = game_state.turn.name then
       draw_white_cards' t game_state
     else
       draw_white_cards' t (draw_white_card h game_state)

let draw_white_cards game_state = draw_white_cards' game_state.players game_state

let draw_black_card game_state =
  match game_state.black_deck with
  | [] -> raise GameOver
  | h::t ->
     { hand_size = game_state.hand_size;
       players = game_state.players;
       turn = game_state.turn;
       black_table = h;
       white_table = game_state.white_table;
       white_deck = game_state.white_deck;
       black_deck = t; }

let play_white_card player card game_state =
  let new_player = 
       { name = player.name;
         points = player.points;
         hand = pull_card player.hand card [] } in
  { hand_size = game_state.hand_size;
    players = replace_player new_player game_state.players [];
    turn = game_state.turn;
    black_table = game_state.black_table;
    white_table = (player.name,card)::game_state.white_table;
    white_deck = game_state.white_deck;
    black_deck = game_state.black_deck; }

let rec check_hand_pretty hand counter =
  match hand with
  | [] -> ""
  | c1::c2::c3::c4::t -> 
     let (c1_a,c1_b,c1_c,c1_d,c1_e,c1_f,c1_g) = prettify_card c1 white_card_width in
     let (c2_a,c2_b,c2_c,c2_d,c2_e,c2_f,c2_g) = prettify_card c2 white_card_width in
     let (c3_a,c3_b,c3_c,c3_d,c3_e,c3_f,c3_g) = prettify_card c3 white_card_width in
     let (c4_a,c4_b,c4_c,c4_d,c4_e,c4_f,c4_g) = prettify_card c4 white_card_width in
     let c1_g_ind = add_index c1_g counter in
     let c2_g_ind = add_index c2_g counter in
     let c3_g_ind = add_index c3_g counter in
     let c4_g_ind = add_index c4_g counter in
     "┌────────────────┐┌────────────────┐┌────────────────┐┌────────────────┐" ^ "\n" ^
     "│" ^   c1_a   ^ "││" ^   c2_a   ^ "││" ^   c3_a   ^ "││" ^   c4_a   ^ "│" ^ "\n" ^
     "│" ^   c1_b   ^ "││" ^   c2_b   ^ "││" ^   c3_b   ^ "││" ^   c4_b   ^ "│" ^ "\n" ^
     "│" ^   c1_c   ^ "││" ^   c2_c   ^ "││" ^   c3_c   ^ "││" ^   c4_c   ^ "│" ^ "\n" ^
     "│" ^   c1_d   ^ "││" ^   c2_d   ^ "││" ^   c3_d   ^ "││" ^   c4_d   ^ "│" ^ "\n" ^
     "│" ^   c1_e   ^ "││" ^   c2_e   ^ "││" ^   c3_e   ^ "││" ^   c4_e   ^ "│" ^ "\n" ^
     "│" ^   c1_f   ^ "││" ^   c2_f   ^ "││" ^   c3_f   ^ "││" ^   c4_f   ^ "│" ^ "\n" ^
     "│" ^ c1_g_ind ^ "││" ^ c2_g_ind ^ "││" ^ c3_g_ind ^ "││" ^ c4_g_ind ^ "│" ^ "\n" ^
     "└────────────────┘└────────────────┘└────────────────┘└────────────────┘" ^ "\n" ^
     check_hand_pretty t counter
  | c1::c2::c3::t -> 
     let (c1_a,c1_b,c1_c,c1_d,c1_e,c1_f,c1_g) = prettify_card c1 white_card_width in
     let (c2_a,c2_b,c2_c,c2_d,c2_e,c2_f,c2_g) = prettify_card c2 white_card_width in
     let (c3_a,c3_b,c3_c,c3_d,c3_e,c3_f,c3_g) = prettify_card c3 white_card_width in
     let c1_g_ind = add_index c1_g counter in
     let c2_g_ind = add_index c2_g counter in
     let c3_g_ind = add_index c3_g counter in
     "┌────────────────┐┌────────────────┐┌────────────────┐" ^ "\n" ^
     "│" ^   c1_a   ^ "││" ^   c2_a   ^ "││" ^   c3_a   ^ "│" ^ "\n" ^
     "│" ^   c1_b   ^ "││" ^   c2_b   ^ "││" ^   c3_b   ^ "│" ^ "\n" ^
     "│" ^   c1_c   ^ "││" ^   c2_c   ^ "││" ^   c3_c   ^ "│" ^ "\n" ^
     "│" ^   c1_d   ^ "││" ^   c2_d   ^ "││" ^   c3_d   ^ "│" ^ "\n" ^
     "│" ^   c1_e   ^ "││" ^   c2_e   ^ "││" ^   c3_e   ^ "│" ^ "\n" ^
     "│" ^   c1_f   ^ "││" ^   c2_f   ^ "││" ^   c3_f   ^ "│" ^ "\n" ^
     "│" ^ c1_g_ind ^ "││" ^ c2_g_ind ^ "││" ^ c3_g_ind ^ "│" ^ "\n" ^
     "└────────────────┘└────────────────┘└────────────────┘" ^ "\n" ^
     check_hand_pretty t counter
  | c1::c2::t -> 
     let (c1_a,c1_b,c1_c,c1_d,c1_e,c1_f,c1_g) = prettify_card c1 white_card_width in
     let (c2_a,c2_b,c2_c,c2_d,c2_e,c2_f,c2_g) = prettify_card c2 white_card_width in
     let c1_g_ind = add_index c1_g counter in
     let c2_g_ind = add_index c2_g counter in
     "┌────────────────┐┌────────────────┐" ^ "\n" ^
     "│" ^   c1_a   ^ "││" ^   c2_a   ^ "│" ^ "\n" ^
     "│" ^   c1_b   ^ "││" ^   c2_b   ^ "│" ^ "\n" ^
     "│" ^   c1_c   ^ "││" ^   c2_c   ^ "│" ^ "\n" ^
     "│" ^   c1_d   ^ "││" ^   c2_d   ^ "│" ^ "\n" ^
     "│" ^   c1_e   ^ "││" ^   c2_e   ^ "│" ^ "\n" ^
     "│" ^   c1_f   ^ "││" ^   c2_f   ^ "│" ^ "\n" ^
     "│" ^ c1_g_ind ^ "││" ^ c2_g_ind ^ "│" ^ "\n" ^
     "└────────────────┘└────────────────┘" ^ "\n" ^
     check_hand_pretty t counter
  | [c1] -> 
     let (c1_a,c1_b,c1_c,c1_d,c1_e,c1_f,c1_g) = prettify_card c1 white_card_width in
     let c1_g_ind = add_index c1_g counter in
     "┌────────────────┐" ^ "\n" ^
     "│" ^   c1_a   ^ "│" ^ "\n" ^
     "│" ^   c1_b   ^ "│" ^ "\n" ^
     "│" ^   c1_c   ^ "│" ^ "\n" ^
     "│" ^   c1_d   ^ "│" ^ "\n" ^
     "│" ^   c1_e   ^ "│" ^ "\n" ^
     "│" ^   c1_f   ^ "│" ^ "\n" ^
     "│" ^ c1_g_ind ^ "│" ^ "\n" ^
     "└────────────────┘" ^ "\n"

let check_black_table_pretty card =
  let (a,b,c,d,e,f,g) = prettify_card card black_card_width in
  "┌────────────────────────────────────────┐" ^ "\n" ^
  "│" ^                 a                ^ "│" ^ "\n" ^
  "│" ^                 b                ^ "│" ^ "\n" ^
  "│" ^                 c                ^ "│" ^ "\n" ^
  "│" ^                 d                ^ "│" ^ "\n" ^
  "│" ^                 e                ^ "│" ^ "\n" ^
  "│" ^                 f                ^ "│" ^ "\n" ^
  "│" ^                 g                ^ "│" ^ "\n" ^
  "└────────────────────────────────────────┘" ^ "\n"

let choose_white_card card game_state =
  let winner = winner_card card game_state.white_table in
  let new_players = winner_points winner game_state.players [] in
  let new_gs =
  { hand_size = game_state.hand_size;
    players = new_players;
    turn = game_state.turn;
    black_table = game_state.black_table;
    white_table = [];
    white_deck = game_state.white_deck;
    black_deck = game_state.black_deck; } in
  let win_msg = 
    "------------------------------------------------------------------------\n" ^
    winner ^ " won!\n" ^
    "------------------------------------------------------------------------\n" ^
    "The black card was: " ^ "\n" ^
    check_black_table_pretty game_state.black_table ^ "\n" ^
    "The white card was: " ^ "\n" ^
    check_hand_pretty [card] (ref (-1)) ^
    "------------------------------------------------------------------------\n" ^
    "PLAYER'S POINTS:" ^ "\n" ^
    "------------------------------------------------------------------------\n" ^
    check_points game_state.players winner in
  (new_gs, win_msg)

let check_white_table gs = check_white_table' gs.white_table

let rec check_hand hand =
  match hand with
  | [] -> ""
  | h::t -> h ^ "\n" ^ check_hand t

let check_game_state_player player game_state =
  "------------------------------------------------------------------------\n" ^
  "THE CARDS IN YOUR HAND:" ^ "\n" ^
  "------------------------------------------------------------------------\n" ^
  check_hand_pretty player.hand (ref (-1)) ^
  "------------------------------------------------------------------------\n" ^
  "It is currently " ^ game_state.turn.name ^ "'s turn" ^ "\n" ^
  "They are judging the following black card:" ^ "\n" ^
  "------------------------------------------------------------------------\n" ^
  check_black_table_pretty game_state.black_table ^ "\n"

let check_game_state_judge game_state =
  "------------------------------------------------------------------------\n" ^
  "It is currently your turn" ^ "\n" ^
  "You are judging the following black card:" ^ "\n" ^
  "------------------------------------------------------------------------\n" ^
  check_black_table_pretty game_state.black_table ^ "\n" ^
  "------------------------------------------------------------------------\n" ^
  "The current white cards on the table are:" ^ "\n" ^
  "------------------------------------------------------------------------\n" ^
  check_hand_pretty (get_white_table_cards game_state.white_table) (ref (-1)) ^ "\n"
