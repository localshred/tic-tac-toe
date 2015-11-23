ENTRY=TicTacToe.elm

elm.js: $(ENTRY)
	elm-make $(ENTRY) --output elm.js

clean:
	rm -rf elm.js
