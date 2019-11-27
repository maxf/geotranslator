elm.js: src/*.elm
	elm make src/Main.elm --output=js/elm.js --debug

install:
	elm make src/Main.elm --output=js/elm.js --optimize
	uglifyjs js/elm.js > js/elmu.js
	mv js/elmu.js js/elm.js
	@echo "Ready to commit"
