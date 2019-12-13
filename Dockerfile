FROM alpine:latest

EXPOSE 443

RUN apk update
RUN apk add nginx nodejs npm

COPY letsencrypt /
COPY . /app
COPY nginx-config /etc/nginx/sites-available/default
COPY /app/public /var/www/html/assets
RUN mv /var/www/html/assets/index-prod.html /var/www/html/assets/index.html

WORKDIR /app
RUN npm install
CMD npm start
