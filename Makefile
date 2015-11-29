ENTRY=Main.elm
OUT_FILE=elm.js

$(OUT_FILE): $(ENTRY)
	elm-make $(ENTRY) --output $(OUT_FILE)

watch:
	watchman-make -p '*.Elm' -t elm.js

clean:
	rm -rf $(OUT_FILE)

.PHONY: clean watch $(OUT_FILE)
