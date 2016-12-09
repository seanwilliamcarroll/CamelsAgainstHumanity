play:
	corebuild -pkgs cohttp.async,yojson,str,ANSITerminal,cohttp,core,async Serial.native Player.native && ./Player.native
host: 
	corebuild -pkgs yojson,str,ANSITerminal Serial.byte && \
	ocamlbuild -pkgs yojson,str,ANSITerminal GameState.byte && \
	corebuild -pkgs cohttp.async,yojson,str,ANSITerminal,cohttp,core,async Host.native
clean:
	ocamlbuild -clean
	rm -f checktypes.ml
