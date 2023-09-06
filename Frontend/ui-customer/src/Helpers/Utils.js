import { callbackMapper } from 'presto-ui';

var timerIdDebounce = null;
var driverWaitingTimerId = null;
var zoneOtpExpiryTimerId = null;
var inputForDebounce;
var timerIdForTimeout;
var tracking_id = 0;
export const getNewTrackingId = function (unit) {
  tracking_id += 1;
  return JSON.stringify(tracking_id);
};

export const getKeyInSharedPrefKeysConfigEff = function (key) {
    return JBridge.getFromSharedPrefs(key);
  };

export const validateInputPattern = function (input, pattern){
    const reg = new RegExp(pattern,'g');
    var result = reg.test(input);
    console.log("validateInputPattern " + result + " Values :- " + input + " Pattern :- " + pattern);
    return (result);
}

export const getLocationName = function(cb){
    return function (lat) {
        return function (lng){
            return function (defaultText) {
                return function (action) {
                    return function(){
                        var callback = callbackMapper.map(function (lat,lon,result){
                            var decodedString = decodeURIComponent(result).replace(/\+/g, ' ');
                            cb(action(parseFloat(lat))(parseFloat(lon))(decodedString))();
                        });
                        return window.JBridge.getLocationName(lat, lng, defaultText, callback);
                    }
                }
            }
        }
    }
}

export const getCurrentDate = function (string) {
  var today = new Date();
  var dd = String(today.getDate()).padStart(2, '0');
  var mm = String(today.getMonth() + 1).padStart(2, '0'); //January is 0!
  var yyyy = today.getFullYear();

  today = dd + '/' + mm + '/' + yyyy;
  return today;
}

export const validateEmail = function (email){
    const re = /^(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
    return re.test(String(email).toLowerCase());
}

export const factoryResetApp = function (str) {
    console.log("HERE IN RESET ===--->>")
    window.JBridge.factoryResetApp()
}

export const secondsToHms = function (d) {
    d = Number(d);
    var h = Math.floor(d / 3600);
    var m = Math.floor(d % 3600 / 60);
    // var s = Math.floor(d % 3600 % 60);

    var hDisplay = h > 0 ? h + (h == 1 ? " hr, " : " hrs, ") : "";
    var mDisplay = m > 0 ? m + (m == 1 ? " min " : " mins ") : "";
    // var sDisplay = s > 0 ? s + (s == 1 ? " second" : " seconds") : "";
    return hDisplay + mDisplay; //+ sDisplay;
}

export const getUTCDay = function (date){
    return date.getUTCDay();
}

export const getTime = function (unit){
    return Date.now();
}

export const requestKeyboardShow = function(id) {
    return function() {
        var delayInMilliseconds = 100;
        setTimeout(function() {
            window.JBridge.requestKeyboardShow(id);
        }, delayInMilliseconds);
    }
  }

export const storeCallBackLocateOnMap = function (cb) {
  try {
  return function (action) {
      return function () {
        var callback = callbackMapper.map(function (key, lat, lon) {
          console.log("in show storeCallBackLocateOnMap",action);
          if(timerIdDebounce){
            clearTimeout(timerIdDebounce);
          }
          window.x = cb;
          window.y = action;
          timerIdDebounce = setTimeout(() => {
            cb(action (key) (lat) (lon))();
          }, 200);
        });
          console.log("In storeCallBackLocateOnMap ---------- + " + action);
          window.JBridge.storeCallBackLocateOnMap(callback);
      }
  }}
  catch (error){
      console.log("Error occurred in storeCallBackLocateOnMap ------", error);
  }
}

export const storeCallBackCustomer = function (cb) {

    return function (action) {
        return function () {
          try {
            var callback = callbackMapper.map(function (notificationType) {
                cb(action (notificationType))();
            });
            var notificationCallBack = function (notificationType) {
              cb(action (notificationType))();
          };
            window.callNotificationCallBack = notificationCallBack;
            console.log("In storeCallBackCustomer ---------- + " + action);
            JBridge.storeCallBackCustomer(callback);
        }
        catch (error){
          console.log("Error occurred in storeCallBackCustomer ------", error);
      }
    }}

}

export const storeCallBackContacts = function (cb) {
  return function (action) {
    return function () {
      try {
        var callback = callbackMapper.map(function (contact) {
          var json = JSON.parse(contact);
          console.log("storeCallBackContacts js " + json);
          cb(action(json))();
        });

        console.log("In storeCallBackContacts ---------- + " + action);
        window.JBridge.storeCallBackContacts(callback);
      } catch (err) {
        console.log("storeCallBackContacts error " + err);
      }
    }
  }
}

export const parseNewContacts = function (String) {
    return JSON.parse(String);
}


export const makePascalCase = function (str){
    var changeToUpperCase = str[0].toUpperCase();
    for(var i = 1; i < str.length; i++){
        if(str[i-1] == " " || str[i-1] == ","){
            changeToUpperCase += str[i].toUpperCase();
        }else{
            changeToUpperCase += str[i].toLowerCase();
        }
    }
    return changeToUpperCase;
}

export const decodeError = function (er) {
  return function (key){
    try {
      var errorPayload = JSON.parse(er)[key];
      if(errorPayload === null)
        return "";
      return  errorPayload.toString();
    } catch (e) {
      console.log(e);
      return "";
    }
  }
  };

export const toString = function (attr) {
return JSON.stringify(attr);
};

export const zoneOtpExpiryTimer = function (startingTime) {
  return function(endingTime) {
    return function (cb) {
      return function (action) {
        return function () {
          if (startingTime >= endingTime){
            cb(action(zoneOtpExpiryTimerId)("")(0))();
          } else {
            var callback = callbackMapper.map(function () {
              var sec = endingTime - startingTime;
              if (zoneOtpExpiryTimerId) clearInterval(zoneOtpExpiryTimerId);
              zoneOtpExpiryTimerId = setInterval(
                convertInMinutesFromat,
                1000
              );
              function convertInMinutesFromat() {
                sec--;
                var minutes = getTwoDigitsNumber(Math.floor(sec / 60));
                var seconds = getTwoDigitsNumber(sec - minutes * 60);
                var timeInMinutesFormat = minutes + " : " + seconds;
                cb(action(zoneOtpExpiryTimerId)(timeInMinutesFormat)(sec))();
              }
            });
          }
          window.callUICallback(callback);
        };
      };
    };
  }
};

export const clearWaitingTimer = function (id){
  console.log("clearWaitingTimer" + id);
  if(__OS == "IOS" && id=="countUpTimerId") {
    if (window.JBridge.clearCountUpTimer) {
      window.JBridge.clearCountUpTimer();
    }
  } else {
    clearInterval(parseInt(id));
  }
}

export const clearCountDownTimer = function (id){
  if(__OS == "IOS"){
    if (window.JBridge.clearCountDownTimer) {
      window.JBridge.clearCountDownTimer();
    }
  }
  else {
    clearInterval(parseInt(id));
  }
}

function getTwoDigitsNumber(number) {
  return number >= 10 ? number : "0"+number.toString();
}

export const setRefreshing = function (id){
    return function (bool){
      if (window.__OS == "ANDROID") {
        var cmd = "set_v=ctx->findViewById:i_" + id + ";get_v->setRefreshing:b_" + bool + ";"
        window.Android.runInUI(cmd,null)
      }
    }
  }

export const setEnabled = function (id){
  return function (bool){
    if (window.__OS == "ANDROID") {
      var cmd = "set_v=ctx->findViewById:i_" + id + ";get_v->setEnabled:b_" + bool + ";"
      window.Android.runInUI(cmd,null)
    }
  }
}

  // exports ["debounceFunction"] = function (delay) { NEED TO HANDLE DEBOUNCING IN LOCATEONMAP
  //   return function (cb){
  //     return function (action){
  //       return function(){
  //         console.logs("debounceFunctiondebounceFunction");
  //         var callback = callbackMapper.map(function () {
  //           if (timerIdDebounce) clearTimeout(timerIdDebounce);
  //           timerIdDebounce = setTimeout(() => {
  //             timerIdDebounce = "MAKEAPICALL";
  //             cb(action (inputForDebounce))();
  //           },delay);
  //         });
  //         window.callUICallback(callback);
  //       }
  //     }
  //   }
  // }

var inputForDebounce;
export const updateInputString = function (a){
  console.log("UPDATED STRING NOW" + a);
  inputForDebounce = a;
}

var timerIdDebounce = null;
export const debounceFunction = function (delay) {
  return function (cb) {
    return function (action) {
      return function (isSource) {
        return function () {
          var callback = callbackMapper.map(function () {
            if (timerIdDebounce) clearTimeout(timerIdDebounce);
            timerIdDebounce = setTimeout(() => {
              timerIdDebounce = "MAKEAPICALL";
              cb(action(inputForDebounce)(isSource))();
            }, delay);
          });
          window.callUICallback(callback);
        }
      }
    }
  }
}

export const fetchFromLocalStoreImpl = function(key) {
    return function (just) {
        return function (nothing) {
          return function () {
            var state = JBridge.getFromSharedPrefs(key);
            if (state != "__failed" && state != "(null)") {
              return just(state);
            }
            return nothing;
          };
        };
      };
}

export const fetchFromLocalStoreTempImpl = function(key) {
  return function (just) {
      return function (nothing) {
        return function () {
          var state = JBridge.getFromSharedPrefs(key);
          var newState = JSON.parse(state);
          var predictionArray = newState.predictionArray;
          try {
                for(var i = 0; i < predictionArray.length; i++) {
                  if (!predictionArray[i].hasOwnProperty("fullAddress"))
                    {
                      predictionArray[i].fullAddress = {};
                    }
                }
            }
          catch(e) {
            console.log(e);
          }

          newState["predictionArray"] = predictionArray;
          if (state != "__failed" && state != "(null)") {
            return just(JSON.stringify(newState));
          }
          return nothing;
        };
      };
    };
}

export const saveToLocalStoreImpl = function(key) {
    return function (state) {
        console.log("==------>>>>>> SAVE SCREEN");
        console.log(key);
        console.log(state);
        window.JBridge.setKeysInSharedPrefs(key, state);
        return function () {
          console.log("==------>>>>>> SAVED SCREEN");
        };
      };
}

export const seperateByWhiteSpaces = function(string) {
    return string.replace(/\s+/g, ' ').trim();
};

// function uuidv4() {
//   return ([1e7]+-1e3+-4e3+-8e3+-1e11).replace(/[018]/g, c =>
//     (c ^ crypto.getRandomValues(new Uint8Array(1))[0] & 15 >> c / 4).toString(16)
//   );
// }

// exports["generateSessionToken"] = function (str){
//     var token = uuidv4();
//     return token;
// }

export const shuffle = function (array) {
  var shuffled = array
    .map(value => ({ value, sort: Math.random() }))
    .sort((a, b) => a.sort - b.sort)
    .map(({ value }) => value)
  return shuffled
}

export const withinTimeRange = function (startTime) {
  return function (endTime) {
    return function(timeStr){
      try {
        return startTime < endTime ? between(timeStr, startTime, endTime) : between(timeStr, startTime, "23:59:59") || between(timeStr, "00:00:01", endTime);
     }catch (err){
        return false;
      }
    }
  }
}
function between(x, min, max) {
  return x >= min && x <= max;
}

export const adjustViewWithKeyboard = function(flag) {
  return function() {
    if (window.JBridge.adjustViewWithKeyboard) {
      window.JBridge.adjustViewWithKeyboard(flag)
    }
  };
};

export const fetchAndUpdateCurrentLocation = function (cb) {
  return function (action) {
    return function (fallbackAction) {
      return function () {
        if (window.JBridge.fetchAndUpdateCurrentLocation) {
          var callback = callbackMapper.map(function (lat, lng) {
            cb(action(lat)(lng))();
          });
          return window.JBridge.fetchAndUpdateCurrentLocation(callback);
        } else {  // fallback for previous release
          var fallBackCallback = callbackMapper.map(function(){
            cb(fallbackAction)();
          });
          window.callUICallback(fallBackCallback);
        }
      };
    };
  };
};
export const contactPermission = function () {
  if(window.JBridge.contactPermission){
    return window.JBridge.contactPermission();
  }
}

export const performHapticFeedback = function () {
  if(window.JBridge.performHapticFeedback ){
      return window.JBridge.performHapticFeedback();
  }
}

export const storeOnResumeCallback = function (cb) {
  return function (action) {
    return function () {
      try {
        var callback = function () {
          cb(action)();
        }
        if (window.onResumeListeners){
        window.onResumeListeners.push(callback);
        }
      }
      catch (error) {
        console.log("Error occurred in storeOnResumeCallback ------", error);
      }
    }
  }
}

export const drawPolygon = function(geoJson) {
  return function (locationName) {
    return function() {
      if (JBridge.drawPolygon) {
        JBridge.drawPolygon(geoJson, locationName);
      }
    }
  }
}

export const removeLabelFromMarker = function(unit){
  return function () {
    if (JBridge.removeLabelFromMarker){
      return JBridge.removeLabelFromMarker();
    }
  }
}
export const addCarousel = function (modelArray) {
  return function (id) {
    var stringifyModelArray = JSON.stringify(modelArray)
    if(JBridge.addCarousel){
      return JBridge.addCarousel(stringifyModelArray, id);
    }
  };
};

export const strLenWithSpecificCharacters = function(input) {
  return function(pattern){
  const regex = new RegExp(pattern, 'g');
  const matches = input.match(regex);
  return matches ? matches.length : 0;
  }
}

export const getMobileNumber = function (signatureAuthData, maskedNumber) {
  try {
    const re = /^[6-9][)]?[-\s\.]?[0-9]{3}[-\s\.]?[0-9]{4,6}$/;
    var mobileNumber = JSON.parse(signatureAuthData).mobileNumber;
    if (re.test(mobileNumber)) {
      return mobileNumber;
    } else {
      return maskedNumber.replace("...", "****");
    }
  } catch (err) {
    console.log("Decode mobileNumber from SignatureAuthData Error => " + err);
  }
}

const base32 = '0123456789bcdefghjkmnpqrstuvwxyz'; // (geohash-specific) Base32 map

/**
 * Encodes latitude/longitude to geohash, either to specified precision or to automatically
 * evaluated precision.
 *
 * @param   {number} lat - Latitude in degrees.
 * @param   {number} lon - Longitude in degrees.
 * @param   {number} [precision] - Number of characters in resulting geohash.
 * @returns {string} Geohash of supplied latitude/longitude.
 * @throws  Invalid geohash.
 *
 * @example
 *     const geohash = encodeGeohash(52.205, 0.119, 7); // => 'u120fxw'
 */
export const encodeGeohash = function (lat) {
  // infer precision?
return function (lon){  
  return function(precision ){
      console.log("IN encode geohash", lat, lon, precision);
  if (typeof precision == 'undefined') {
    // refine geohash until it matches precision of supplied lat/lon
    for (let p = 1; p <= 12; p++) {
      const hash = encodeGeohash(lat, lon, p);
      const posn = decodeGeohash(hash);
      if (posn.lat == lat && posn.lon == lon) return hash;
    }
    precision = 12; // set to maximum
  }

  lat = Number(lat);
  lon = Number(lon);
  precision = Number(precision);

  if (isNaN(lat) || isNaN(lon) || isNaN(precision)) throw new Error('Invalid geohash');

  let idx = 0; // index into base32 map
  let bit = 0; // each char holds 5 bits
  let evenBit = true;
  let geohash = '';

  let latMin = -90, latMax = 90;
  let lonMin = -180, lonMax = 180;

  while (geohash.length < precision) {
    if (evenBit) {
      // bisect E-W longitude
      const lonMid = (lonMin + lonMax) / 2;
      if (lon >= lonMid) {
        idx = idx * 2 + 1;
        lonMin = lonMid;
      } else {
        idx = idx * 2;
        lonMax = lonMid;
      }
    } else {
      // bisect N-S latitude
      const latMid = (latMin + latMax) / 2;
      if (lat >= latMid) {
        idx = idx * 2 + 1;
        latMin = latMid;
      } else {
        idx = idx * 2;
        latMax = latMid;
      }
    }
    evenBit = !evenBit;

    if (++bit == 5) {
      // 5 bits gives us a character: append it and start over
      geohash += base32.charAt(idx);
      bit = 0;
      idx = 0;
    }
  }

  return geohash;
}
}
}

/**
 * Decode geohash to latitude/longitude (location is approximate centre of geohash cell,
 *     to reasonable precision).
 *
 * @param   {string} geohash - Geohash string to be converted to latitude/longitude.
 * @returns {{lat:number, lon:number}} (Center of) geohashed location.
 * @throws  Invalid geohash.
 *
 * @example
 *     const latlon = decodeGeohash('u120fxw'); // => { lat: 52.205, lon: 0.1188 }
 */
export const decodeGeohash = function (geohash) {
  const bounds = geohashBounds(geohash); // <-- the hard work
  // now just determine the centre of the cell...

  const latMin = bounds.sw.lat,
    lonMin = bounds.sw.lon;
  const latMax = bounds.ne.lat,
    lonMax = bounds.ne.lon;

  // cell centre
  let lat = (latMin + latMax) / 2;
  let lon = (lonMin + lonMax) / 2;

  // round to close to centre without excessive precision: ⌊2-log10(Δ°)⌋ decimal places
  lat = lat.toFixed(Math.floor(2 - Math.log(latMax - latMin) / Math.LN10));
  lon = lon.toFixed(Math.floor(2 - Math.log(lonMax - lonMin) / Math.LN10));

  return { lat: Number(lat), lon: Number(lon) };
}

/**
 * Returns SW/NE latitude/longitude bounds of specified geohash.
 *
 * @param   {string} geohash - Cell that bounds are required of.
 * @returns {{sw: {lat: number, lon: number}, ne: {lat: number, lon: number}}}
 * @throws  Invalid geohash.
 */
export const geohashBounds = function (geohash) {
  if (geohash.length == 0) throw new Error('Invalid geohash');

  geohash = geohash.toLowerCase();

  let evenBit = true;
  let latMin = -90,
    latMax = 90;
  let lonMin = -180,
    lonMax = 180;

  for (let i = 0; i < geohash.length; i++) {
    const chr = geohash.charAt(i);
    const idx = base32.indexOf(chr);
    if (idx == -1) throw new Error('Invalid geohash');

    for (let n = 4; n >= 0; n--) {
      const bitN = idx >> n & 1;
      if (evenBit) {
        // longitude
        const lonMid = (lonMin + lonMax) / 2;
        if (bitN == 1) {
          lonMin = lonMid;
        } else {
          lonMax = lonMid;
        }
      } else {
        // latitude
        const latMid = (latMin + latMax) / 2;
        if (bitN == 1) {
          latMin = latMid;
        } else {
          latMax = latMid;
        }
      }
      evenBit = !evenBit;
    }
  }

  const bounds = {
    sw: { lat: latMin, lon: lonMin },
    ne: { lat: latMax, lon: lonMax },
  };

  return bounds;
}

/**
 * Determines adjacent cell in given direction.
 *
 * @param   geohash - Cell to which adjacent cell is required.
 * @param   direction - Direction from geohash (N/S/E/W).
 * @returns {string} Geocode of adjacent cell.
 * @throws  Invalid geohash.
 */
export const adjacentGeohash = function (geohash, direction) {
  // based on github.com/davetroy/geohash-js

  geohash = geohash.toLowerCase();
  direction = direction.toLowerCase();

  if (geohash.length == 0) throw new Error('Invalid geohash');
  if ('nsew'.indexOf(direction) == -1) throw new Error('Invalid direction');

  const neighbour = {
    n: ['p0r21436x8zb9dcf5h7kjnmqesgutwvy', 'bc01fg45238967deuvhjyznpkmstqrwx'],
    s: ['14365h7k9dcfesgujnmqp0r2twvyx8zb', '238967debc01fg45kmstqrwxuvhjyznp'],
    e: ['bc01fg45238967deuvhjyznpkmstqrwx', 'p0r21436x8zb9dcf5h7kjnmqesgutwvy'],
    w: ['238967debc01fg45kmstqrwxuvhjyznp', '14365h7k9dcfesgujnmqp0r2twvyx8zb'],
  };
  const border = {
    n: ['prxz', 'bcfguvyz'],
    s: ['028b', '0145hjnp'],
    e: ['bcfguvyz', 'prxz'],
    w: ['0145hjnp', '028b'],
  };

  const lastCh = geohash.slice(-1); // last character of hash
  let parent = geohash.slice(0, -1); // hash without last character

  const type = geohash.length % 2;

  // check for edge-cases which don't share common prefix
  if (border[direction][type].indexOf(lastCh) != -1 && parent != '') {
    parent = adjacentGeohash(parent, direction);
  }

  // append letter for direction to parent
  return parent + base32.charAt(neighbour[direction][type].indexOf(lastCh));
}

/**
 * Returns all 8 adjacent cells to specified geohash.
 *
 * @param   {string} geohash - Geohash neighbours are required of.
 * @returns {{n,ne,e,se,s,sw,w,nw: string}}
 * @throws  Invalid geohash.
 */
export const geohashNeighbours = function (geohash) {
  let nDirection = adjacentGeohash(geohash, 'n');
  let sDirection = adjacentGeohash(geohash, 's');
  let eDirection = adjacentGeohash(geohash, 'e');
  let wDirection = adjacentGeohash(geohash, 'w');
  let neighbours = [nDirection, sDirection, eDirection, wDirection ,adjacentGeohash(nDirection, 'e'), adjacentGeohash(sDirection, 'e'),
                    adjacentGeohash(sDirection, 'w'), adjacentGeohash(nDirection, 'w')]
  console.log("geohashNeighbours", neighbours);
  return neighbours;
}
export const storeInLocal = function (map) {
    console.log("zxc ",map);
    // window.JBridge.setKeysInSharedPrefs("SUGGESTED_DESTS", map);
    return function () {
      console.log("==------>>>>>> SAVED SCREEN");
    };
}
export const mapSetSuggestedDestinationsJson = function (map) {
  console.log("zxc ",map);
  let jsonString = JSON.stringify(map);
  let unstring = JSON.parse(jsonString);
  console.log("zxc Stringified ",jsonString);
  console.log("zxc UN Stringified ", unstring);
  console.log("Return zxc ", Array.from(unstring))

  window.JBridge.setKeysInSharedPrefs("SUGGESTED_DESTS", jsonString);
  return Array.from(unstring);
}

export const getSuggestedDestinationsJsonFromLocal = function (key){
  console.log("IM here in getSuggestedDestinationsJsonFromLocal")
  
  let stringifiedMap=window.JBridge.getKeysInSharedPref ? window.JBridge.getKeysInSharedPref(key) : window.JBridge.getKeysInSharedPrefs(key); //  window.JBridge.getKeysInSharedPrefs(key);
  console.log("IM here in getSuggestedDestinationsJsonFromLocal", stringifiedMap)

            if (stringifiedMap != "__failed" && stringifiedMap != "(null)") {
                let unstring = JSON.parse(stringifiedMap);

                console.log("parsed ", unstring)

                return Array.from(unstring);
            } 
            console.log("err", stringifiedMap)
            return stringifiedMap;
}

export const getDifferenceBetweenDates = function (date1){
  return function (date2){
      const oneDay = 24 * 60 * 60 * 1000; // Milliseconds in a day
      const diff = Math.floor((new Date(date1) - new Date(date2))/oneDay);
      console.log("difference zxc", diff);
      return diff;
  }
}
