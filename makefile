public/elm.js: src/*.elm
	elm make src/Main.elm --output=public/elm.js --debug

install:
	elm make src/Main.elm --output=elm.js --optimize
	uglifyjs elm.js > public/elm.js
	rm elm.js
ifndef DEST
$(error DEST not set)
endif
	rsync -avzr --delete public $(DEST)
