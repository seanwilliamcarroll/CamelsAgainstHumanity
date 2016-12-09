# CamelsAgainstHumanity

To play Camels Against Humanity:
(all commands should be run in directory containing Makefile)

Player 1: run make host, run ./Host.native with the following optional flags

=== flags ===

  [-bd string]   Filename containing black deck representation. Default
                 BlackDeck.json.
  [-hs int]      Number of Cards a player starts with in their hands. Default 5.
  [-mp int]      Max points a player reaches to win. Default 7.
  [-np int]      Number of players in this game. Default 3.
  [-p int]       Source port to listen on. Default 8080.
  [-wd string]   Filename containing white deck representation. Default
                 WhiteDeck.json.
  [-build-info]  print info about this build and exit
  [-version]     print the version of this build and exit
  [-help]        print this help text and exit

For example, to play with 5 players run:

./Host.native -np 5

All other players: run make play, type in the (IP address:port) of the host, 
and your name to connect


Requires cohttp and async
