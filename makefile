elm.js: src/*.elm
	elm make src/Main.elm --output=elm.js --debug

install:
	elm make src/Main.elm --output=elm.js --optimize
	uglifyjs elm.js > elmu.js
	mv elmu.js elm.js
	@echo "Ready to commit"
