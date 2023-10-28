
export const setSuggestionsMapInJson = function (map) {
    let jsonString = JSON.stringify(map);
    let unstring = JSON.parse(jsonString);
    window.JBridge.setKeysInSharedPrefs("SUGGESTIONS_MAP", jsonString);
    return Array.from(unstring);
  }
  
export const getSuggestedDestinationsJsonFromLocal = function (key){
    
    let stringifiedMap=window.JBridge.getKeysInSharedPref ? window.JBridge.getKeysInSharedPref(key) : window.JBridge.getKeysInSharedPrefs(key); //  window.JBridge.getKeysInSharedPrefs(key);
              if (stringifiedMap != "__failed" && stringifiedMap != "(null)") {
                  let unstring = JSON.parse(stringifiedMap);
                  return Array.from(unstring);
              } 
              console.log("err", stringifiedMap)
              return stringifiedMap;
  }
  
/* --------------------- GEOHASH FUNCTIONS ---------------------- */

const base32 = '0123456789bcdefghjkmnpqrstuvwxyz'; // (geohash-specific) Base32 map

export const encodeGeohash = function (lat, lon, precision) {
      if (typeof precision == 'undefined') {
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

      let idx = 0; 
      let bit = 0; 
      let evenBit = true;
      let geohash = '';

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

  const lastCh = geohash.slice(-1); 
  let parent = geohash.slice(0, -1); 

  const type = geohash.length % 2;

  if (border[direction][type].indexOf(lastCh) != -1 && parent != '') {
    parent = adjacentGeohash(parent, direction);
  }

  return parent + base32.charAt(neighbour[direction][type].indexOf(lastCh));
}


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
/*----------------------------------------------------------------- */
