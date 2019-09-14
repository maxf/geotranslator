function log(text) {
  const msg = document.getElementById('message');
  msg.style.display = 'block';
  msg.innerHTML = msg.innerHTML + '<p>' + text + '</p>';
}

const $ = id => document.getElementById(id);

const populateTest = function(text) {
  if ($('final_span')) {
    $('final_span').innerHTML = text;
  }
  analyseTranscript(text);
};


const showDec = function(lon, lat) {
  $('lon-dec').innerHTML = lon.toFixed(5);
  $('lat-dec').innerHTML = lat.toFixed(5);

  if (window.view) {
    view.center = [lon, lat];
    view.zoom = 15;
  }
};

const showDms = function(lonDeg, lonMin, lonSec, lonDir, latDeg, latMin, latSec, latDir) {
  $('lon-dms-deg').innerHTML = lonDeg;
  $('lon-dms-min').innerHTML = lonMin;
  $('lon-dms-sec').innerHTML = lonSec.toFixed(3);
  $('lon-dms-dir').innerHTML = lonDir;
  $('lat-dms-deg').innerHTML = latDeg;
  $('lat-dms-min').innerHTML = latMin;
  $('lat-dms-sec').innerHTML = latSec.toFixed(3);
  $('lat-dms-dir').innerHTML = latDir;
};


const showW3w = function(w1, w2, w3) {
  $('w3w-1').innerHTML = w1;
  $('w3w-2').innerHTML = w2;
  $('w3w-3').innerHTML = w3;
};


const populateFromDec = function(lon, lat) {
  showDec(lon, lat);

  // calculate DMS from Dec
  const posLon = Math.abs(lon);
  const lonDf = Math.floor(posLon);
  const lonM = (posLon - lonDf) * 60;
  const lonMf = Math.floor(lonM);
  const lonS = (lonM - lonMf) * 60;
  const lonDir = lon > 0 ? 'E' : 'W';

  const posLat = Math.abs(lat);
  const latDf = Math.floor(posLat);
  const latM = (posLat - latDf) * 60;
  const latMf = Math.floor(latM);
  const latS = (latM - latMf) * 60;
  const latDir = lat > 0 ? 'N' : 'S';


  showDms(lonDf, lonMf, lonS, lonDir, latDf, latMf, latS, latDir);


  // calculate W3W from Dec
  showW3w('correct', 'horse', 'battery');

};



const populateFromDms = function(lonDeg, lonMin, lonSec, lonDir, latDeg, latMin, latSec, latDir) {
  showDms(lonDeg, lonMin, lonSec, lonDir, latDeg, latMin, latSec, latDir);

  // calculate Dec from DMS

  const lon = (lonDir==='W'?-1:1) * lonDeg + lonMin/60 + lonSec/3600;
  const lat = (latDir==='S'?-1:1) * latDeg + latMin/60 + latSec/3600;

  showDec(lon, lat);

  // calculate w3w from DMS
  showW3w('chicken', 'glass', 'indeed');

};


const populateFromW3w = function(w1, w2, w3) {
  showW3w(w1, w2, w3);
};


const tryDec = function(text) {
  log('trying decimal');
  const gpsDecRe = /(-?[\d]+\.[\d]+)[\s]+(-?[\d]+\.[\d]+)/;
  const found = text.match(gpsDecRe);
  if (found) {
    populateFromDec(parseFloat(found[1]), parseFloat(found[2]));
  }
  return found;
};

const tryDms = function(text) {
  log('trying dms');
  const gpsDmsRe = /(\d+)\s+degrees?\s+(\d+)\s+minutes?\s+([\d.]+)\s+seconds?\s+(north|south)\s+(\d+)\s+degrees?\s+(\d+)\s+minutes?\s+([\d.]+)\s+seconds?\s+(east|west)/;
  const found = text.match(gpsDmsRe);
  if (found) {
    populateFromDms(
      parseInt(found[1]),
      parseInt(found[2]),
      parseFloat(found[3]),
      found[4],
      parseInt(found[5]),
      parseInt(found[6]),
      parseFloat(found[7]),
      found[8]
    );
  }
  return found;
};

const tryW3w = function(text) {
  log('trying w3w');
  const re = /(\w+)\s+(\w+)\s+(\w+)/;
  const found = text.match(re);
  if (found) {
    populateFromW3w(found[1], found[2], found[3])
  }
  return found;
};

const analyseTranscript = function(text) {
  tryDec(text)
  || tryDms(text)
  || tryW3w(text)
  || log('failed');
};
