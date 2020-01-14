# How to run

Easiest way, from the CLI (requires docker installed, but no need to clone this repo):

    docker pull maxf/geotranslate
    docker run -p [port]:3000 --env W3WAPIKEY=[api-key] --env RUNTIME_ENV=[dev|live] maxf/geotranslate


Alternatively, if you want to build images from your local clone of this repo:

    docker build .
    docker run -p [port]:3000 --env W3WAPIKEY=[api-key] --env RUNTIME_ENV=[dev|live] geotranslator



# How to develop

First time:

- clone this repo
- install elm: `npm install elm`
- build the server: `cd server && npm install`


Always:

- compile elm code on change: `make`
- Start the server: `cd server && W3WAPIKEY=[YOUR-API-KEY] RUNTIME_ENV=[dev|live] npm start`

# How to deploy

- compile elm in production mode: `make build`


- deploy your own way (eg, push the docker image: `docker push maxf/geotranslate`)
