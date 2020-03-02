# Geotranslator

Quickly communicate your position in case of emergency [PROOF OF CONCEPT - NO GUARANTEES]

## TL;DR

It's sometimes difficult to be able to communicate your location when you're talking to someone on the phone (999 in particular). Your phone often knows where you are but it's difficult to find your GPS coordinates on it, and to communicate them over the phone.

Various solutions are being developed and used by emergency services. This is a prototype for one solution. It's fast, open source, compatible, works offline, and is as accurate as your phone's GPS.

Hopefully it'll inspire others to build better emergency service tools, until those tools (including this one) become redundant when [Advanced Mobile Location](https://en.wikipedia.org/wiki/Advanced_Mobile_Location) is supported across the board.



- [Background](#background)
- [How it works](#how)
- [How to run/develop/deploy](#run)


## <a id="background"></a>Background

Suppose you're lost in the woods, and you need urgent help. Fortunately you have your phone and are able to dial 999. The operators asks where you are? You can't give an address, have no idea what your GPS coordinates are, let alone eastings/northings which happens to be the only geocode that your local 999 call center can type in their system to send your position to the people out to rescue you.

This happens more often than you think. That's why more and more emergency services are now using various geolocation services. In the UK [What3Words](https://what3words.com) is becoming the most commonly used.

Here's a typical 999 exchange:

- You: help, I'm lost and in danger
- 999: what's your address?
- You: I don't know, I'm in the woods.
- 999: OK. You need to download the What3Words app and give me the 3 words
[more or less time passes depending on how familiar you are with downloading apps]
- You: OK, downloading.
[ your phone downloads 61MB ]
- You: OK, running the app
[you start the app, select "English", tap to accept the T&Cs, tap multiple times to skip the tutorial, agree to give geolocation permissions]
- You: here are my 3 words: ...
[999 operator goes to what3words.com, enters the 3 words in their system (which was recently upgraded to support W3W), and the response team is on its way.

You can imagine it can take a long while to be found.

Here's the exchange again, using this webapp:

- You: help, I'm lost and in danger
- 999: what's your address?
- You: I don't know, I'm in the woods
- 999: OK, go to "geo.e98.me" in your browser, click "share your location", and tell me the numbers in the first blue box.
[ Your phone downloads 156kB ]
[ the first blue box shows your eastings/northings, which is what this 999 call centre understands]
- You: 529457, 233942
[999 enters numbers in their CAD, response team on its way]


With what3words you need to download a 61MB app, you need to tap about 10 times to get your position. 999 needs to know how to find a location from 3 words. With this solution: you only need to download 156kB, and tap once before you're able to communicate your position, which is simultaneously displayed using 6 different codes.


Also:
- this webapp is a PWA (progressive web app), so it can be installed beforehand, like a native app, and works offline
- it supports: eastings/northings, british national grid coordinates, lat/lon (decimal and degrees/minutes/seconds), Open Location Code (aka "plus codes", used in Google Maps), and what3words
- The app can also be used to find a different location by typing coordinates (any scheme above), and clicking on "show on map".

### Important notes:

- This is a proof of concept. It's untested in real situations and so there's no guarantee it's working everywhere - don't rely on it in a critical situations
- This isn't a dig at what3words or other similar tools. This is just a way to show that there's a much faster way to convey your location to 999 in emergency situations.

## <a id="how"></a>How it works

- Since this is a PWA. Everything runs in your browser, which will cache resources and offer you to install the app to work offline (but you don't have to).

- It's written in Elm. Not the simplest of languages to learn, but it creates rock-solid code, guarantees no runtime exceptions and makes you feel much better about how robust and reliable your app is.

- The downloaded code is minimised, compressed by the server, heavily cached, so that very little needs to be downloaded before the app is usable.

- The only reason why there's a server (as opposed to just static files) is to hide the What3words API key in Ajax calls. What3words are working on a solution. Of course, removing w3w from the app would also remove the need for a server.

## <a id="run"></a>Run/develop/deploy

### How to run

Easiest way, from the CLI (requires docker installed, but no need to clone this repo):

    docker pull maxf/geotranslate
    docker run -p [port]:3000 --env W3WAPIKEY=[api-key] --env RUNTIME_ENV=[dev|live] --restart always maxf/geotranslate


Alternatively, if you want to build images from your local clone of this repo:

    docker build .
    docker run -p [port]:3000 --env W3WAPIKEY=[api-key] --env RUNTIME_ENV=[dev|live] --restart always geotranslator



## How to develop

First time:

- clone this repo
- install elm: `npm install elm`
- build the server: `cd server && npm install`


Every time:

- compile elm code on change: `make`
- Start the server: `cd server && W3WAPIKEY=[YOUR-API-KEY] RUNTIME_ENV=[dev|live] npm start`

## How to deploy

- compile elm in production mode: `make build`


- deploy your own way (eg, push the docker image: `docker push maxf/geotranslate`)
