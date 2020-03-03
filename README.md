# Geotranslator

Quickly communicate your position in case of emergency [PROOF OF CONCEPT - NO GUARANTEES]

See the README file in the master branch for full details

## Static version

This branch has a version of the app that you can install on a server that only hosts static files (like github pages).

In principle, this should be the default setup: no server logic is required, everything gets downloaded by the browser and all the logic happens there.

However we made a server version (using Express) because of what3words: if you want to support what3words in the app, you need to make XHR calls to the what3words API from your browser. In order to do that you need to pass a secret API key in the URL (or in a header). That key is therefore visible if you look at XHR calls in your browser. In order to avoid that, we need to set up a proxy server which hides the API key. This is all the ExpressJS server does, on top of service the static files.

If w3w had a system to avoid exposing the API key (using tokens, for instance), we wouldn't have to do this at all. Until that happens, this branch has a version of the app that doesn't support what3words.

The other reason why the default version doesn't run on github pages or some other static server is that we wanted full control of the static server in order to fine tune the caching strategy, as well as setting up compression, etc. The nginx configuration file can be found in the master branch.

The third reason is that I wanted to run this on my own domain, with a very short name (e98.me), and not `github.com/geotranslator`. So I had to take care of the TLS cert for the domain and everything. Again, completely optional.
