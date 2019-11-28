//Import the http module
var http = require('http');

//Create the server listening on port 8888
http.createServer(function(request, response) {
    //Log the URL for debugging etc.
    console.log(request.url);

    //Create a new http request with the data at hand
    var parsedURL = require('url').parse(request.url);
    var proxyRequest = http.request({
        port: request.port,
        host: request.headers['host'],
        method: request.method,
        headers: request.headers,
        path: parsedURL.pathname + (parsedURL.search ? parsedURL.search : '')
    })

    //When there is a response;
    proxyRequest.addListener('response', function (proxyResponse) {
        proxyResponse.on('data', function(chunk) {
            response.write(chunk, 'binary');
        });

        //End the response
        proxyResponse.on('end', function() {
            response.end();
        });

        //Manipulate some headers - Here we repeat the original requests origin to the fake response
        if(request.headers['origin']) {
            proxyResponse.headers['access-control-allow-origin'] = request.headers['origin'];
            //Set any other headers you need
            //proxyResponse.headers['access-control-allow-credentials'] = 'true';
        }
        response.writeHead(proxyResponse.statusCode, proxyResponse.headers);
    });

    //return a 404 when the forwarded request throws an error
    proxyRequest.on('error', function(err) {
        response.statusCode = '404';
        response.end();
    });

    //Copy any data in the original request to the forwarded request
    request.addListener('data', function(chunk) {
        proxyRequest.write(chunk, 'binary');
    });

    //End the proxy request
    request.addListener('end', function() {
        proxyRequest.end();
    });
}).listen(8888);
