public/elm.js: src/Main.elm
	elm make src/Main.elm --output=public/elm.js

.PHONY: dev
dev:
	elm-live src/Main.elm --port=8080 --dir=public/ --open -- --output=public/elm.js --debug

.PHONY: deps
deps:
	npm install -g elm-live
