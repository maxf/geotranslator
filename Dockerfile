FROM alpine:latest

RUN apk update
RUN apk add nodejs npm git curl make
WORKDIR /app
RUN git clone https://github.com/maxf/geotranslator.git

# compile elm code
WORKDIR geotranslator
RUN npm install
RUN node_modules/.bin/elm make src/Main.elm --output=elm.js --optimize
RUN node_modules/.bin/uglifyjs elm.js > server/public/elm.js
RUN rm elm.js


# build server
WORKDIR /app/geotranslator/server
RUN npm install

ARG w3w_api_key=missing
ENV W3WAPIKEY=$w3w_api_key

ARG runtime_env=0
ENV RUNTIME_ENV=$dev


# start server
CMD npm start
