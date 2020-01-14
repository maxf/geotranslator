FROM alpine:latest

RUN apk update
RUN apk add nodejs npm
COPY server /app

# build server
WORKDIR /app
RUN npm install

ARG w3w_api_key=missing
ENV W3WAPIKEY=$w3w_api_key

ARG runtime_env=0
ENV RUNTIME_ENV=$dev


# start server
CMD npm start
