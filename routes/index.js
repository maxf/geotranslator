const express = require('express');
const request = require('request');
const url = require('url');
const router = express.Router();

/* GET home page. */
router.get('/', function(req, res, next) {
  res.render('index', { title: 'GeoTranslator' });
});


const w3wApiKey = process.env.W3WAPIKEY;
if (!w3wApiKey) {
  console.warn('missing environment variable W3WAPIKEY');
  process.exit(1);
}

const proxiedBaseUrl = 'https://api.what3words.com/v3';

router.get('/w3w/w2c', function(req, res, next) {
  const parsedUrl = url.parse(req.url, true);
  const words = parsedUrl.query.words;
  proxiedUrl = `${proxiedBaseUrl}/convert-to-coordinates?words=${words}&key=${w3wApiKey}`;
  request(proxiedUrl, function (error, response, body) {
    res.send(body);
  });
});

router.get('/w3w/c2w', function(req, res, next) {
  const parsedUrl = url.parse(req.url, true);
  const lon = parsedUrl.query.lon;
  const lat = parsedUrl.query.lat;
  proxiedUrl = `${proxiedBaseUrl}/convert-to-3wa?coordinates=${lon},${lat}&key=${w3wApiKey}`;
  console.log(23,proxiedUrl);
  request(proxiedUrl, function (error, response, body) {
    res.send(body);
  });
});

module.exports = router;
