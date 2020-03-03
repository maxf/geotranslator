public/elm.js: src/*.elm
	elm make src/Main.elm --output=elm.js --debug || (osascript -e 'display notification "Elm compilation error"')



build:
	elm make src/Main.elm --output=elm.js --optimize
	uglifyjs elm.js > server/public/elm.js
	rm elm.js
