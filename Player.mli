(* open GameState *)
open Yojson

(* (* the host ip address entered by the player used to connect to a host *)
val host_ip : string

(* [open_connection ip] establishes a connection with the host with address
[ip]*)
val open_connection : string -> unit

(* [put_card h wc] takes white card [wc] from the player's hand [h] and sends a
message to the host that the card was selected. *) 
val put_card : hand -> white_card -> unit

(* [choose_card wc] takes a white card [wc] from the table and sends a message 
to the host that the selected card wins the active black card. *)
val choose_card : table -> white_card -> unit

(* [check_points] requests information from the host about all of the players 
current scores. *)
val check_points : unit -> int list

(* [check_hand] requests information from the host about the players hand *)
val check_hand : unit -> hand

(* [check_table] requests information from the host about the cards on the 
table but only when this player has the black card *)
val check_table : unit -> table

(* [gameloop] is a REPL that runs until the player exits the game. *)
val gameloop : unit -> unit
 *)