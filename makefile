public/elm.js: src/*.elm
	elm make src/Main.elm --output=public/javascripts/elm.js --debug


build:
	elm make src/Main.elm --output=elm.js --optimize
	uglifyjs elm.js > elm.js
	rm elm.js

install: build
	mkdir -p letsencrypt
	cp ../letsencrypt/live/e98.me/fullchain.pem ./secrets/e98-fullchain.pem
	cp ../letsencrypt/live/e98.me/privkey.pem ./secrets/e98-privkey.pem
	cp ../letsencrypt/live/e98.me-0001/fullchain.pem ./secrets/e98-wildcard-fullchain.pem
	cp ../letsencrypt/live/e98.me-0001/privkey.pem ./secrets/e98-wildcard-privkey.pem

	@if [ -z $DEST ]; then\
		rsync -avzr --exclude .git --exclude secrets --exclude node_modules --exclude elm-stuff --delete . $(DEST);\
	else\
		echo "DEST variable not set";\
	fi
	@echo "Make sure you've bumped the PWA version"
	@echo "install docker on the target: sudo ./install-docker.sh"
	@echo "run: docker build -t geotranslator ."
	@echo "run: docker run -p443:443 geotranslator"
