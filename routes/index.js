const express = require('express');
const request = require('request');
const url = require('url');
const router = express.Router();



const sampleC2wResponse = '{"country":"ZZ","square":{"southwest":{"lng":51.499434,"lat":-0.136239},"northeast":{"lng":51.499461,"lat":-0.136212}},"nearestPlace":"","coordinates":{"lng":51.499447,"lat":-0.136225},"words":"rehired.envied.sickles","language":"en","map":"https:\/\/w3w.co\/rehired.envied.sicklezz"}';
const sampleW2cResponse = '{"country":"GB","square":{"southwest":{"lng":-2.359051,"lat":51.381064},"northeast":{"lng":-2.359008,"lat":51.381091}},"nearestPlace":"Bath, Somerset","coordinates":{"lng":-2.359029,"lat":51.381078},"words":"incomes.decide.bronze","language":"en","map":"https:\/\/w3w.co\/incomes.decide.bronze"}';

const DEV = !!process.env.DEV;

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
  if (DEV) {
    console.warn('DEV');
    res.send(sampleW2cResponse);
  } else {
    console.warn('LIVE');
    const parsedUrl = url.parse(req.url, true);
    const words = parsedUrl.query.words;
    proxiedUrl = `${proxiedBaseUrl}/convert-to-coordinates?words=${words}&key=${w3wApiKey}`;
    request(proxiedUrl, function (error, response, body) {
      res.send(body);
    });
  }
});

router.get('/w3w/c2w', function(req, res, next) {
  if (DEV) {
    console.warn('DEV');
    res.send(sampleC2wResponse);
  } else {
    console.warn('LIVE');
    const parsedUrl = url.parse(req.url, true);
    const lon = parsedUrl.query.lon;
    const lat = parsedUrl.query.lat;
    proxiedUrl = `${proxiedBaseUrl}/convert-to-3wa?coordinates=${lon},${lat}&key=${w3wApiKey}`;
    console.log(23,proxiedUrl);
    request(proxiedUrl, function (error, response, body) {
      res.send(body);
    });
  }
});

module.exports = router;
