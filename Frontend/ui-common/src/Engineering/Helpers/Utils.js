import { callbackMapper } from "presto-ui";

const JBridge = window.JBridge;

export const saveToLocalStoreImpl = function(key) {
  return function (state) {
    window.JBridge.setKeysInSharedPrefs(key, state);
    return function () {
    };
  };
}

export const fetchFromLocalStoreImpl = function(key) {
  return function (just) {
    return function (nothing) {
      return function () {
        const state = JBridge.getFromSharedPrefs(key);
        if (state != "__failed" && state != "(null)") {
          return just(state);
        }
        return nothing;
      };
    };
  };
}
export const reboot = window.JOS.emitEvent("java")("onEvent")(JSON.stringify({event:"reboot"}))()

export const showSplash = window.JOS.emitEvent("java")("onEvent")(JSON.stringify({event:"show_splash"}))()


export const decrementMonth = function (month) {
  return function (year){
    try{
      const date = new Date(year, month-1, 1);
      const d = { utcDate: date.toISOString(), date: date.getDate(), shortMonth: date.toLocaleString("default", { month: "short" }), year: date.getFullYear(), intMonth : date.getMonth(),
        isInRange : false, isStart: false , isEnd: false }
      return d;
    } catch (e) {
      console.log("error in decrementMonth", e);
    }
  }
}

export const incrementMonth = function (month) {
  return function (year){
    try{
      const date = new Date(year, month+1, 1);
      const d= { utcDate: date.toISOString(), date: date.getDate(), shortMonth: date.toLocaleString("default", { month: "short" }), year: date.getFullYear(), intMonth : date.getMonth(),
        isInRange : false, isStart: false , isEnd: false }
      return d;
    } catch (e) {
      console.log("error in incrementMonth", e);
    }
  }
}

export const getWeeksInMonth = function (year) {
  return function (month) {
    try {
      const result = []
      const date = new Date(year, month, 1);
      const diff = date.getDay();

      let startPadding = diff;
      while (date.getMonth() == month){
        const week = [];
        for(let i = 0 ; i < 7; i++){
          if(startPadding){
            const obj = { utcDate: "", date: 0, shortMonth: "", year: year, intMonth: month,
              isInRange : false, isStart: false , isEnd: false }
            week.push(obj);
            startPadding --;
          }else{
            const obj = { utcDate: date.toISOString(), date: date.getDate(), shortMonth: date.toLocaleString("default", { month: "short" }), year: year, intMonth: month,
              isInRange : false, isStart: false , isEnd: false }
            week.push(obj)
            date.setDate(date.getDate() + 1);
          }

          if(date.getMonth() != month) break;
        }
        if(date.getMonth() != month && date.getDay() != 0) {
          let endPadding = 6 - date.getDay() + 1;
          while(endPadding --){
            const obj = { utcDate: "", date: 0, shortMonth: "", year: year, intMonth: month,
              isInRange : false, isStart: false , isEnd: false }
            week.push(obj);
          }
        }
        result.push({week: week})
      }
      return result;
    } catch (e) {
      console.log("error in getWeeksInMonth", e);
    }
  }
};

export const getCurrentDay = function (useMidnightTime) {
  const date = new Date();
  if(useMidnightTime)
    date.setHours(0,0,0,0);
  return { utcDate: date.toISOString(), date: date.getDate(), shortMonth: date.toLocaleString("default", { month: "short" }), year: date.getFullYear(), intMonth : date.getMonth(),
    isInRange : false, isStart: false , isEnd: false }
}


/* --------------------- GEOHASH FUNCTIONS ---------------------- */

const base32 = "0123456789bcdefghjkmnpqrstuvwxyz"; // (geohash-specific) Base32 map

export const geohashBounds = function (geohash) {
  if (geohash.length == 0) throw new Error("Invalid geohash");

  geohash = geohash.toLowerCase();

  let evenBit = true;
  let latMin = -90,
    latMax = 90;
  let lonMin = -180,
    lonMax = 180;

  for (let i = 0; i < geohash.length; i++) {
    const chr = geohash.charAt(i);
    const idx = base32.indexOf(chr);
    if (idx == -1) throw new Error("Invalid geohash");

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

export const decodeGeohash = function (geohash) {
  const bounds = geohashBounds(geohash); 

  const latMin = bounds.sw.lat,
    lonMin = bounds.sw.lon;
  const latMax = bounds.ne.lat,
    lonMax = bounds.ne.lon;

  let lat = (latMin + latMax) / 2;
  let lon = (lonMin + lonMax) / 2;

  lat = lat.toFixed(Math.floor(2 - Math.log(latMax - latMin) / Math.LN10));
  lon = lon.toFixed(Math.floor(2 - Math.log(lonMax - lonMin) / Math.LN10));

  return { lat: Number(lat), lon: Number(lon) };
}

export const encodeGeohash = function (lat, lon, precision) {
  if (typeof precision == "undefined") {
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

  if (isNaN(lat) || isNaN(lon) || isNaN(precision)) throw new Error("Invalid geohash");

  let idx = 0; 
  let bit = 0; 
  let evenBit = true;
  let geohash = "";

  let latMin = -90, latMax = 90;
  let lonMin = -180, lonMax = 180;

  while (geohash.length < precision) {
    if (evenBit) {
      const lonMid = (lonMin + lonMax) / 2;
      if (lon >= lonMid) {
        idx = idx * 2 + 1;
        lonMin = lonMid;
      } else {
        idx = idx * 2;
        lonMax = lonMid;
      }
    } else {
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
      geohash += base32.charAt(idx);
      bit = 0;
      idx = 0;
    }
  }

  return geohash;
}

export const adjacentGeohash = function (geohash, direction) {
  // based on github.com/davetroy/geohash-js

  geohash = geohash.toLowerCase();
  direction = direction.toLowerCase();

  if (geohash.length == 0) throw new Error("Invalid geohash");
  if ("nsew".indexOf(direction) == -1) throw new Error("Invalid direction");

  const neighbour = {
    n: ["p0r21436x8zb9dcf5h7kjnmqesgutwvy", "bc01fg45238967deuvhjyznpkmstqrwx"],
    s: ["14365h7k9dcfesgujnmqp0r2twvyx8zb", "238967debc01fg45kmstqrwxuvhjyznp"],
    e: ["bc01fg45238967deuvhjyznpkmstqrwx", "p0r21436x8zb9dcf5h7kjnmqesgutwvy"],
    w: ["238967debc01fg45kmstqrwxuvhjyznp", "14365h7k9dcfesgujnmqp0r2twvyx8zb"],
  };
  const border = {
    n: ["prxz", "bcfguvyz"],
    s: ["028b", "0145hjnp"],
    e: ["bcfguvyz", "prxz"],
    w: ["0145hjnp", "028b"],
  };

  const lastCh = geohash.slice(-1); 
  let parentHash = geohash.slice(0, -1); 

  const type = geohash.length % 2;

  if (border[direction][type].indexOf(lastCh) != -1 && parentHash != "") {
    parentHash = adjacentGeohash(parentHash, direction);
  }

  return parentHash + base32.charAt(neighbour[direction][type].indexOf(lastCh));
}


export const geohashNeighbours = function (geohash) {
  const nDirection = adjacentGeohash(geohash, "n");
  const sDirection = adjacentGeohash(geohash, "s");
  const eDirection = adjacentGeohash(geohash, "e");
  const wDirection = adjacentGeohash(geohash, "w");
  const neighbours = [nDirection, sDirection, eDirection, wDirection ,adjacentGeohash(nDirection, "e"), adjacentGeohash(sDirection, "e"),
    adjacentGeohash(sDirection, "w"), adjacentGeohash(nDirection, "w")]
  console.log("geohashNeighbours", neighbours);
  return neighbours;
}

export const stringifyGeoJson = function (geoJson) {
  console.log("debug zone utils geoJson features", geoJson.features);
  const features = geoJson.features;
  // return JSON.stringify(geoJson);
  for (let i=0; i<features.length; i++) {
    const feature = features[i];
    const properties = feature.properties;
    for (const key in properties) {
      if (properties[key]) {
        console.log("debug zone utils", key);
      }
      console.log("debug zone utils key value",properties[key]);
    }
    feature.geometry = JSON.parse(feature.geometry);
  }
  geoJson.features = features;
  return JSON.stringify(geoJson);
}

/*----------------------------------------------------------------- */
