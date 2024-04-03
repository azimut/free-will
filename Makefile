DATASETS = $(wildcard data/*.txt)

.PHONY: dev deps clean

public/elm.js: src/Main.elm src/Themes.elm
	elm make src/Main.elm --output=public/elm.js --optimize

src/Themes.elm: $(DATASETS)
	echo 'module Themes exposing (all)' >  $@
	echo -n 'all = '                    >> $@
	jq --raw-input . $(DATASETS) | jq --slurp --compact-output >> $@

dev:  ; elm-live src/Main.elm --dir=public/ --open -- --output=public/elm.js --debug
deps: ; npm install -g elm-live
clean:; rm -f src/Themes.elm public/elm.js
