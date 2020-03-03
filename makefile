public/elm.js: src/*.elm
	elm make src/Main.elm --output=elm.js --debug || (osascript -e 'display notification "Elm compilation error"')



build:
	elm make src/Main.elm --output=elm.js --optimize
	uglifyjs elm.js > elm2.js
	mv elm2.js elm.js
