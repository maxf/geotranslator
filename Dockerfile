FROM alpine:latest

EXPOSE 443

RUN apk update
RUN apk add nginx nodejs npm
RUN apk add openrc --no-cache

COPY . /app
RUN rm -rf /app/server/node_modules
COPY nginx-config /etc/nginx/sites-available/default

ARG w3w_api_key=missing
ENV W3WAPIKEY=$w3w_api_key

WORKDIR /app/server
RUN npm install
CMD npm start
