var final_transcript = '';
var recognizing = false;
var ignore_onend;
var start_timestamp;

if (!('webkitSpeechRecognition' in window)) {
  alert('webspeech not supported');
} else {
  start_button.style.display = 'inline-block';
  var recognition = new webkitSpeechRecognition();
  recognition.continuous = true;
  recognition.interimResults = true;

  recognition.onstart = function() {
    recognizing = true;
    showInfo('info_speak_now');
    start_img.src = 'mic-animate.gif';
  };

  recognition.onerror = function(event) {
    if (event.error == 'no-speech') {
      start_img.src = 'mic.gif';
      showInfo('info_no_speech');
      ignore_onend = true;
    }
    if (event.error == 'audio-capture') {
      start_img.src = 'mic.gif';
      showInfo('info_no_microphone');
      ignore_onend = true;
    }
    if (event.error == 'not-allowed') {
      if (event.timeStamp - start_timestamp < 100) {
        showInfo('info_blocked');
      } else {
        showInfo('info_denied');
      }
      ignore_onend = true;
    }
  };

  recognition.onend = function() {
    recognizing = false;
    if (ignore_onend) {
      return;
    }
    start_img.src = 'mic.gif';
    if (!final_transcript) {
      showInfo('info_start');
      return;
    }
    showInfo('');
    if (window.getSelection) {
      window.getSelection().removeAllRanges();
      var range = document.createRange();
      range.selectNode(document.getElementById('final_span'));
      window.getSelection().addRange(range);
    }
  };

  recognition.onresult = function(event) {
    var interim_transcript = '';
    if (typeof(event.results) == 'undefined') {
      recognition.onend = null;
      recognition.stop();
      upgrade();
      return;
    }

    for (var i = event.resultIndex; i < event.results.length; ++i) {
      if (event.results[i].isFinal) {
        final_transcript = event.results[i][0].transcript;
      } else {
        interim_transcript += event.results[i][0].transcript;
      }
    }
    final_transcript = capitalize(final_transcript);
    final_span.innerHTML = linebreak(final_transcript);
    analyseTranscript(final_transcript);
    interim_span.innerHTML = linebreak(interim_transcript);
    if (final_transcript || interim_transcript) {
      showButtons('inline-block');
    }
  };
}

function upgrade() {
  start_button.style.visibility = 'hidden';
  showInfo('info_upgrade');
}

var two_line = /\n\n/g;
var one_line = /\n/g;
function linebreak(s) {
  return s.replace(two_line, '<p></p>').replace(one_line, '<br>');
}

var first_char = /\S/;
function capitalize(s) {
  return s.replace(first_char, function(m) { return m.toUpperCase(); });
}


function startButton(event) {
  if (recognizing) {
    recognition.stop();
    return;
  }
  final_transcript = '';
  recognition.lang = 'en-GB';
  recognition.start();
  ignore_onend = false;
  final_span.innerHTML = '';
  interim_span.innerHTML = '';
  start_img.src = 'mic-slash.gif';
  showInfo('info_allow');
  showButtons('none');
  start_timestamp = event.timeStamp;
}

function showInfo(s) {
  if (s) {
    for (var child = info.firstChild; child; child = child.nextSibling) {
      if (child.style) {
        child.style.display = child.id == s ? 'inline' : 'none';
      }
    }
    info.style.visibility = 'visible';
  } else {
    info.style.visibility = 'hidden';
  }
}

var current_style;
function showButtons(style) {
  if (style == current_style) {
    return;
  }
  current_style = style;
}


function log(text) {
  const msg = document.getElementById('message');
  msg.innerHTML = msg.innerHTML + '<hr/>' + text
}


//------------------

const $ = id => document.getElementById(id);

const populateTest = function(text) {
  document.getElementById('final_span').innerHTML = text;
  analyseTranscript(text);
};


const showDec = function(lat, lon) {
  $('lat-dec').innerHTML = lat.toFixed(5);
  $('lon-dec').innerHTML = lon.toFixed(5);
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


const populateFromDec = function(lat, lon) {
  showDec(lat,lon);

  // calculate DMS from Dec
  const posLat = Math.abs(lat);
  const latDf = Math.floor(posLat);
  const latM = (posLat - latDf) * 60;
  const latMf = Math.floor(latM);
  const latS = (latM - latMf) * 60;
  const latDir = lat > 0 ? 'N' : 'S';

  const posLon = Math.abs(lon);
  const lonDf = Math.floor(posLon);
  const lonM = (posLon - lonDf) * 60;
  const lonMf = Math.floor(lonM);
  const lonS = (lonM - lonMf) * 60;
  const lonDir = lon > 0 ? 'E' : 'W';

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
    console.log(found);
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
    console.log(found);
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
