(* TODO:
 * when it's your turn replace Your Name with "your" in the prints 
 * CATCH GAME OVER!!!!! might want to do this in GameState.ml
 * if trying to put it in my_host.ml is too tricky *)
open Serial
open GameState

open String
open Core.Std
open Async.Std
open Cohttp_async

(******************************************************************************)
(** Types, Exceptions, Globals ************************************************)
(******************************************************************************)

(* [Connection_error] with the host *)
exception Connection_error

(* [phase] represents different phases of action for the player. *)
type phase =  SelectBlack | SelectWhite| Waiting

(* [my_name] is the name of the player *)
let my_name = ref "NONAME"

(* [host_code] is the IP addr of the host *)
let host_code = ref (Uri.of_string "")

(* [current_gamestate] is the current gamestate as passed by the host *)
let current_gamestate = ref None

let last_win_msg = ref ""

(******************************************************************************)
(** Helper Functions **********************************************************)
(******************************************************************************)

(* Unwraps a string option and returns a string, or an error if None. *)
let string_of_string_option = function
  | Some s -> s
  | None -> failwith "Found None in string Option where Some was expected."

(* Unwraps a game_state option and returns a game_state, or an error if None. *)
let unwrap_gs gs = 
  match gs with
  | None -> failwith "Found None in game_state Option where Some was expected."
  | Some g -> g

(* Helper method for printing with async. *)
let print_async s = 
  Core.Std.Out_channel.output_string stdout s;
  Core.Std.Out_channel.flush stdout

(* Helper method for getting user input with async. *)
let read_async () = 
  Core.Std.In_channel.input_line stdin |> string_of_string_option

(* [get_me gs] returns the player's record of themself in the gamestate [gs] *)
let get_me gs = 
  match List.find gs.players ~f:(fun p -> p.name = !my_name) with
    | Some me -> me
    | None -> raise Connection_error

(* [get_phase gs] infers the current phase of the game from the player's 
 * perspective given game state [gs], and returns a phase variant. *)
let get_phase gs = 
  (* if it's my turn *)
  if gs.turn.name = !my_name then 
    if (List.length gs.white_table) = ((List.length gs.players) - 1) then 
      (* everyone has submitted their white card *)
      SelectBlack
    else
      (* waiting for everyone to submit their card *)
      let () = print_async "You're the judge! Waiting for all players to submit...\n" in
      Time.pause (Time.Span.of_sec 5.0);
      Waiting
  (* if it's not my turn *)
  else
    let me = get_me gs in
    if List.length me.hand = gs.hand_size then 
      (* I need to submit a white card *)
      SelectWhite
    else 
      (* waiting for everyone to submit their card, or for judge to pick a card *)
      let () = print_async "Waiting for the judge to select a winner...\n" in
      Time.pause (Time.Span.of_sec 5.0);
      Waiting

(* [get_valid_selection cards] shows the player the white [cards] on the table
 * and allows them to pick a winning card. Invalid cards are not accepted. *)
let rec get_valid_selection cards =
  let str = read_async () in
  try
    (* TODO: check what happens if user inputs out of bounds index *)
    let idx = int_of_string str in
    match List.nth cards idx with
    | Some c -> GameState.string_of_card c
    | None ->
      print_async "That's not a valid index, stupid.\n";
      get_valid_selection cards
  with
    Failure _ ->
      print_async "Type a number, silly.\n";
      get_valid_selection cards

(* [get_body (_,body)] converts the body of a response to a string deferred *)
let get_body (_,body) = body |> Body.to_string

(* [connect_to_game hc] globally sets host_code (IP addr) to [hc] *)
let connect_to_game hc = 
  host_code := (Uri.of_string ("http://" ^ hc));
  print_async ("Connecting to game at " ^ hc ^ "\n\n")

(* [send_select card_name] Sends a POST message to the connected host with the 
name of the card selected. *)
let rec send_card card =
  let card_serial = Serial.serialize_SELECT card in
  Deferred.upon (!host_code|>Client.post ~body:(Body.of_string (card_serial)))
  (fun _ -> Deferred.upon ((!host_code |> Client.get) >>= get_body) update_gs)

(* Helper for game phase where the player selects a white card *)
(* It's not my turn and I need to submit a white card *)
and select_white gs =
  let me = get_me gs in
  print_async (check_game_state_player me gs);
  print_async "Select a white card to play by typing it's index.\n";
  print_async ">>> ";
  let card_name = get_valid_selection me.hand in
  let () = print_async ("Selected card: " ^ card_name ^ "\n") in
  send_card (GameState.make_card card_name)

(* Helper for game phase where the player selects a black card *)
(* It's my turn and everyone has submitted their white card *)
and select_black gs =
  print_async (check_game_state_judge gs);
  print_async ("Select a white card to win by typing it's index: \n");
  print_async ">>> ";
  let white_cards = List.map ~f:(fun pair -> snd pair) gs.white_table in
  let card_name = get_valid_selection white_cards in
  send_card (GameState.make_card card_name)

(* Game loop that handles the following commands:
 *   [quit]: exits the client 
 *   [card name]: selects and sends a card to the host *)
and game_loop gs : unit =
  (* Process current command *)
  begin
   match get_phase gs with
   | SelectBlack -> select_black gs
   | SelectWhite -> select_white gs
   | Waiting -> wait ""
  end

(* [update_gs str] updates the game_state global var and continues to wait if
 * the game is not initialized, meaning all players haven't joined yet.
 * Note: The host infers phase when not all players have joined ("Game not
 * initialized"). The player infers phase when all players have joined but
 * the player must wait anyways. *)
and update_gs str =
  match str with
  (* Not all players have joined as inferenced by the host, so we must wait *)
  | "Game not initialized" ->
     current_gamestate := None;
     print_async "Waiting for all players to join...\n";
     Time.pause (Time.Span.of_sec 5.0);
     wait ""
  (* All players have joined so we got a valid gs message, but we might still
   * have to wait: get game_loop to inference this *)
  | gs ->
    Time.pause (Time.Span.of_sec 1.0);
    let (gamestate,win_msg) = Serial.translate_GAMESTATE gs in
    current_gamestate := Some gamestate;
    let () = begin
      match win_msg with
      | "GAMEOVER" -> 
         print_async ("Game over: " 
                      ^ (get_max_player (unwrap_gs !current_gamestate)) 
                      ^ " wins!\n\n");
         ignore (exit 0); () (* hacky way to exit cleanly *)
      | "" -> ()
      | s -> (* this is the win message! *)
        if s = !last_win_msg then () (* we've printed this already *)
        else 
          print_async s;
          last_win_msg := s;
          (* Pause to let user read win message *)
          Time.pause (Time.Span.of_sec 5.0)
    end in
    game_loop (unwrap_gs !current_gamestate)

(* [wait str] GETS the game_state from the host and updates the global copy. *)
and wait str =
  (* This match is with the initial connection initialization POST *)
  match str with
  | "Already connected" -> 
     print_async "You're already connected to this game!\n";
     ignore (exit 0); () (* hacky way to exit cleanly *)
  | "Invalid connection" -> 
     print_async "Invalid connection request. Exiting.\n";
     ignore (exit 0); () (* hacky way to exit cleanly *)
  | _ ->
     Deferred.upon ((!host_code |> Client.get) >>= get_body) update_gs

(* [send_name name] Sends a POST message to the connected host identifying this
 * player's name, prepended with #NAME# to signal to the server that it is 
 * receiving a name. Upon completion of the POST, this calls wait. *)
let send_name name =
  Deferred.upon (!host_code |> Client.post 
    ~body:(Body.of_string ("#NAME#" ^ name)) >>= get_body) wait

(******************************************************************************)
(** Run Game ******************************************************************)
(******************************************************************************)

let () =
  print_async
  ("\n------------------------------------------------------------------------\n" ^
    "Welcome to Camels Against Humanity!\n" ^
  "------------------------------------------------------------------------\n" ^
    "Please enter \"[IP address]:[port]\" of the game you wish to join.\n\n" ^
    ">>> ");
  (* host code = IP addr *)
  let host_code = (read_async ()) in
  connect_to_game host_code;
  (* entering name kicks off connection and game_loop *)
  print_async "Enter the name you wish to use for this game to connect.\n\n>>> ";
  let name = (read_async ()) in
  my_name := name;
  send_name !my_name

let _ = Scheduler.go()
