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
    console.warn('requesting',proxiedUrl);
    request(proxiedUrl, function (error, response, body) {
      res.send(body);
    });
  }
});

// ======================================================================
// BNG proxy


// https://www.bgs.ac.uk/data/webservices/CoordConvert_LL_BNG.cfc?method=BNGtoLatLng&easting=429157&northing=623009
// https://www.bgs.ac.uk/data/webservices/CoordConvert_LL_BNG.cfc?method=LatLongToBNG&lat=-5.55&lon=-1.54

const bngBaseUrl = 'https://www.bgs.ac.uk/data/webservices/CoordConvert_LL_BNG.cfc';
const sampleBng2LatLonResponse = '{"DEGMINSECLNG":{"DEGREES":-1,"SECONDS":24.028476096768,"MINUTES":32},"EASTING":429157,"LONGITUDE":-1.54000791002688,"NORTHING":623009,"DEGMINSECLAT":{"DEGREES":55,"SECONDS":59.99859710664,"MINUTES":29},"LATITUDE":55.4999996103074}';
const sampleLatLon2BngResponse = '{"DEGMINSECLNG":{"DEGREES":-1,"SECONDS":24,"MINUTES":32},"EASTING":451030.444044407,"LONGITUDE":-1.54,"ERROR":false,"DEGMINSECLAT":{"DEGREES":-5,"SECONDS":0,"MINUTES":33},"NORTHING":-6141064.83570885,"LATITUDE":-5.55}';

router.get('/bng/bng2latlon', function(req, res, next) {
  if (DEV) {
    console.warn('DEV');
    res.send(sampleBng2LatLonResponse);
  } else {
    console.warn('LIVE');
    const parsedUrl = url.parse(req.url, true);
    const easting = parsedUrl.query.easting;
    const northing = parsedUrl.query.northing;
    proxiedUrl = `${bngBaseUrl}?method=BNGtoLatLng&easting=${easting}&northing=${northing}`;
    request(proxiedUrl, function (error, response, body) {
      res.send(body);
    });
  }
});

router.get('/bng/latlon2bng', function(req, res, next) {
  if (DEV) {
    console.warn('DEV');
    res.send(sampleLatLon2BngResponse);
  } else {
    console.warn('LIVE');
    const parsedUrl = url.parse(req.url, true);
    const lon = parsedUrl.query.lon;
    const lat = parsedUrl.query.lat;
    proxiedUrl = `${bngBaseUrl}?method=LatLongToBNG&lat=${lat}&lon=${lon}`;
    console.warn('requesting',proxiedUrl);
    request(proxiedUrl, function (error, response, body) {
      res.send(body);
    });
  }
});


module.exports = router;
