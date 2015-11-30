ENTRY=src/Main.elm
OUT_FILE=elm.js

$(OUT_FILE): $(ENTRY)
	elm-make --warn --output $(OUT_FILE) $(ENTRY)

watch:
	watchman-make -p 'src/**/*.Elm' -t elm.js

clean:
	rm -rf $(OUT_FILE)

.PHONY: clean watch $(OUT_FILE)
