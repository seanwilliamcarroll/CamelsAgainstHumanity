open Async.Std
open Cohttp_async
open GameState
open Serial

(******************************************************************************)
(** Types, Exceptions, Globals ************************************************)
(******************************************************************************)

(* [Illegal_args] is thrown when a unexpectedly illegal arguments are supplied
 * to a function. This generally should not happen. *)
exception Illegal_args

(* [current_game_state] is a global ref to the current game state *)
let current_game_state = 
  ref (init_game_state ())

(* [num_players] is a global ref to the number of players in this game *)
let num_players = 
  ref 0

(* [hand_size] is a global ref to the hand size of this game *)
let hand_size = 
  ref 0

(* [white_deck_file] is a global ref to the white deck JSON file used in this
 * game *)
let white_deck_file = 
  ref ""

(* [black_deck_file] is a global ref to the black deck JSON file used in this
 * game *)
let black_deck_file = 
  ref ""

(* [players] is a (string * string) global ref mapping player names to their IP
 * addresses *)
let players : (string * string) list ref = 
  ref []

(* [win_msg] is the most recent message stating who won with what black and
 * white card *)
let win_msg = 
  ref ""

(* [max_points] is a global ref to the number of points a user must obtain to
 * win the game *)
let max_points =
  ref 0

(******************************************************************************)
(** GameState Manipulation Helper Functions ***********************************)
(******************************************************************************)

(* [print str] prints [str] to the server log *)
let print str : unit = Log.Global.info "%s" str

(* [find_player player_name] returns the player record beloning to [player_name]
 * from a list of players *)
let rec find_player player_name = function
  | [] -> raise Illegal_args (* Shouldn't happen *)
  | h::t -> 
    if String.equal h.name player_name then h
    else find_player player_name t

(* [next_player player_name in_list acc] is in charge of fairly cycling whose
 * turn it is to judge a black card. [acc] is used as an accumulator. 
 * [player_name] is the player whose turn it was last *)
let rec next_player player_name in_list acc =
  match in_list, acc with
  | h1::h2::t, _ ->
    if String.equal h1 player_name then h2
    else next_player player_name (h2::t) (acc@[h1])
  | h::[], h_acc::_ -> 
    if String.equal h player_name then h_acc
    else raise Illegal_args (* Shouldn't happen? *)
  | _, _ -> raise Illegal_args (* Shouldn't happen? *)

(* [get_ip_from_ip_port s] extracts an IP address from [s] where [s] is of
 * format <IP address>:<port> *)
let get_ip_from_ip_port s = 
  let colon_idx = String.index s ':' in
  String.sub s 0 colon_idx

(* [populate_game_state name] populates the empty game state to start the first
 * round of the game. [name] is the player's whose turn it is first *)
let populate_game_state name =
  let white_deck_gs = 
    load_deck !white_deck_file !current_game_state in
  let white_deck = 
    white_deck_gs.white_deck |> shuffle in
  let black_deck_gs = 
    load_deck !black_deck_file !current_game_state in
  let black_deck = 
    black_deck_gs.black_deck |> shuffle in
  let my_players = 
    List.map (fun (_,_name) -> make_player _name 0 [] ) !players in
  let cur_player = make_player name 0 [] in
  let black_table = make_card "" in
  current_game_state := 
    make_game_state !hand_size my_players cur_player black_table []
    white_deck black_deck;
  (* Populate players hands with white cards *)
  current_game_state := populate_hands !current_game_state;
  (* Place a black card on the table *)
  current_game_state := draw_black_card !current_game_state;
  (* Alert this player that the game has started *)
  print "Begin game"

(* [submit_white_card player white_card] submits [white_card] from
 * [player] into the game_state on the white_table *)
let submit_white_card player white_card =
  let new_gs = play_white_card player white_card !current_game_state in
  current_game_state := new_gs

(* [win_white_card player white_card] submits [white_card] from
 * [player] into the game_state as the winning white card, changes the
 * game_state to a new person's turn *)
let win_white_card player white_card =
  let player_name_list = 
    List.map (fun (_,a) -> a) !players in 
  let new_turn_name = 
    next_player player.name player_name_list [] in
  let new_turn = 
    find_player new_turn_name !current_game_state.players in
  let (new_gs,new_win_msg) = 
    choose_white_card white_card !current_game_state in
  current_game_state := new_gs;
  if ((get_max_score !current_game_state) >= !max_points) then
    raise GameOver
  else
    win_msg := new_win_msg;
  (* Give everyone a new white card *)
  current_game_state := 
    draw_white_cards !current_game_state;
  (* Change whose turn it is *)
  current_game_state :=
    make_game_state
    !current_game_state.hand_size 
    !current_game_state.players
    new_turn
    !current_game_state.black_table
    !current_game_state.white_table
    !current_game_state.white_deck
    !current_game_state.black_deck;
  (* Change the black card *)
  current_game_state :=
    draw_black_card !current_game_state;
  print "Black card selected"

(* [card_from_player card player_ip] handles when [player_ip] submits [card]. *)
let card_from_player card player_ip =
  (* Valid player *)
  let player_name =
    List.assoc player_ip !players in
  let cur_player =
    find_player player_name !current_game_state.players in
  let white_card =
    translate_SELECT card in
  let () = print ("Card sent: " ^ GameState.string_of_card white_card) in
  if ((List.length !current_game_state.white_table) < (!num_players-1)) then
    (* Player is submitting a white card for judgement *)
    let () = print "Still waiting for players" in
    submit_white_card cur_player white_card
  else
    (* Judge has selected a winning card! *)
    win_white_card cur_player white_card

(******************************************************************************)
(** Server Helper Functions ***************************************************)
(******************************************************************************)

(* [connect_player body conn] connects a player identified by name in request
 * [body] on connection [conn]. Note that [body] must be of format
 * #NAME#<player's name> *)
let connect_player body conn =
  if (String.length body <= 6) then (* Needs to start with "#NAME#" *)
    Server.respond_with_string "Invalid connection"
  else if not ((String.sub body 0 6) |> (String.equal "#NAME#")) then
    Server.respond_with_string "Invalid connection"
  else 
    let name = String.sub body 6 ((String.length body) - 6) in
    let ip_addr = (conn |> Socket.Address.to_string |> get_ip_from_ip_port) in
    if (List.mem_assoc ip_addr !players) then (* Player already connected *)
      let () = print "This player is already connected!" in
      Server.respond_with_string "Already connected"
    else (* Update players list *)
      let _ = players := (ip_addr, name)::(!players) in  
      if (List.length !players) <> !num_players then (* Keep waiting *)
        Server.respond_with_string "Still waiting"
      else (* Last new connection, set up game_state *)
        let () = populate_game_state name in
        serialize_GAMESTATE !current_game_state !win_msg
        |> Server.respond_with_string

(* [do_post_response ~body conn] handles a POST with body [~body] over
 * connection [conn]. This is either a player connecting or a player sending
 * a card. *)
let do_post_response ~body conn =
  (Body.to_string body) >>= fun b ->
  let () = print ("POST Message from" ^
    (conn |> Socket.Address.to_string |> get_ip_from_ip_port)) in
  let () = print b in
  if ((List.length !players) < !num_players) then 
    (* This is a player trying to connect *)
    connect_player b conn
  else 
    (* This is a player sending a card *)
    let () = print "All Players connected" in
    let player_ip = (conn |> Socket.Address.to_string |> get_ip_from_ip_port) in
    let () = print player_ip in
    if (not (List.mem_assoc player_ip !players)) then
      Server.respond_with_string "Invalid player"
    else
      try 
        card_from_player b player_ip;
        serialize_GAMESTATE !current_game_state !win_msg
        |> Server.respond_with_string
      with 
      | _ ->
         win_msg := "GAMEOVER";
         serialize_GAMESTATE !current_game_state !win_msg
         |> Server.respond_with_string

(* [rec_handler ~body conn req] handles requests of type [req] with body [~body]
 * over connection [conn]. *)
let rec_handler ~body conn req = 
  match req |> Cohttp.Request.meth with
  | `POST ->
    begin
      try do_post_response ~body conn
      with
      | _ -> (* Game ends, set final win msg *)
         win_msg := "GAMEOVER";
         serialize_GAMESTATE !current_game_state !win_msg
         |> Server.respond_with_string
    end
  | `GET -> (* Send serialized game_state *)
    let _ = print ("GET Message from " ^ 
      (conn |> Socket.Address.to_string |> get_ip_from_ip_port)) in
    if List.length !players< !num_players then 
      (* Still waiting forconnections *)
      Server.respond_with_string "Game not initialized"
    else
      (* All players connected, Send serialized game_state *)
      serialize_GAMESTATE !current_game_state !win_msg
      |> Server.respond_with_string
  | _ -> Server.respond `Method_not_allowed

(******************************************************************************)
(** Run Server ****************************************************************)
(******************************************************************************)

let start_server port hs np wd bd mp () =
  eprintf "Listening for game_state requests and player information\n";
  eprintf "Game started on http://localhost:%d\n" port;
  try
    if hs <= 2 then raise Illegal_args
    else if np <= 2 then raise Illegal_args
    else if mp <= 0 then raise Illegal_args
    else 
      hand_size := hs;
      num_players := np;
      white_deck_file := wd;
      black_deck_file := bd;
      max_points := mp;
    Cohttp_async.Server.create ~on_handler_error:`Raise
      (Tcp.on_port port) rec_handler
    >>= fun _ -> Deferred.never ()
  with
  | Illegal_args -> print ("Your arguments are illegal. " ^
       "Number players must be greater than 2, hand size must be greater " ^
       "than 2, and max points must be greater than 0."); exit 0

let () =
  Command.async
    ~summary:"Game server for CamelsAgainstHumanity"
    Command.Spec.(empty +>
                  flag "-p" (optional_with_default 8080 int)
                    ~doc:"int Source port to listen on. Default 8080."
                  +> 
                  flag "-hs" (optional_with_default 5 int)
                    ~doc:("int Number of Cards a player starts with in their"
                    ^ " hands. Default 5.")
                  +> 
                  flag "-np" (optional_with_default 3 int)
                    ~doc:"int Number of players in this game. Default 3."
                  +> 
                  flag "-wd" (optional_with_default "WhiteDeck.json" string)
                    ~doc:("string Filename containing white deck representation. "
                    ^ "Default WhiteDeck.json.")
                  +> 
                  flag "-bd" (optional_with_default "BlackDeck.json" string)
                    ~doc:("string Filename containing black deck representation. "
                    ^ "Default BlackDeck.json.")
                  +> 
                  flag "-mp" (optional_with_default 7 int)
                    ~doc:"int Max points a player reaches to win. Default 7."
    ) start_server
  |> Command.run
