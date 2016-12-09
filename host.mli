open GameState
open Yojson

(* A type to represent requests from various connected players. Probably
 * implemented using Yojson *)
type request

(* A type to represent the responses to various connected players after
 * recieving a request. Probably implemented using Yojson. *)
type response

(* A type to represent the connection state information with connected 
 * players.*)
type connection

(* Maps player ids to the connection *) 
type connection_mapping

(* Main loop that waits for requests from clients and handles them*)
val main: unit -> unit

(* Takes a client's request and generate an appropriate response *)
val handle_request: request -> response

(* Returns a player's id from their connection from a connection mapping*)
val player_id_of_connection: connection_mapping -> connection -> name


