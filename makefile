public/elm.js: src/*.elm
	elm make src/Main.elm --output=public/javascripts/elm.js --debug

install:
	elm make src/Main.elm --output=elm.js --optimize
	uglifyjs elm.js > public/javascripts/elm.js
	rm elm.js
	@if [ -z $DEST ]; then\
		rsync -avzr --exclude .git --exclude node_modules --exclude elm-stuff --delete . $(DEST);\
	else\
		echo "DEST variable not set";\
	fi
	@echo "Make sure you've bumped the PWA version"
