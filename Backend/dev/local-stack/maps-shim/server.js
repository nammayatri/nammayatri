'use strict';
//
// Google Directions -> OSRM translator.
//
// The backend can only get routes from Google: this baseline's OSRM
// integration (shared-kernel 28bae0f, Kernel.External.Maps.Interface.OSRM)
// exports callOsrmMatch, getDistances and getOSRMTable and nothing else, so
// asking it for a route fails with
//     "Function getRoutes is not provided by service OSRM"
// and mock-google has no /directions/json endpoint either.
//
// So this speaks Google's Directions API and answers from OSRM underneath.
// The backend cannot tell the difference, and we get real Algerian roads
// without an API key.
//
// Anything that is not /directions/json is forwarded untouched to
// mock-google, so a single googleMapsUrl still covers place names and
// autocomplete.
//
// No dependencies: Node's built-in http plus global fetch.

const http = require('http');
const fleet = require('./fleet');
const avatars = require('./avatars');
const rating = require('./rating');
const subscription = require('./subscription');
const identity = require('./identity');
const restricted = require('./restricted');

const PORT           = Number(process.env.PORT || 8020);
const OSRM_URL       = (process.env.OSRM_URL || 'http://localhost:5000').replace(/\/$/, '');
const MOCK_GOOGLE_URL= (process.env.MOCK_GOOGLE_URL || 'http://localhost:8019').replace(/\/$/, '');
// Where to check that a caller is a signed-in passenger, for /fleet/nearby.
const RIDER_URL      = (process.env.RIDER_URL || 'http://localhost:8013').replace(/\/$/, '');
// Where to prove a caller is a signed-in driver, for avatar writes.
const DRIVER_URL     = (process.env.DRIVER_URL || 'http://localhost:8016').replace(/\/$/, '');

// Where the place index lives. Unset means "no search": the three geocoding
// paths fall through to mock-google exactly as before, which is what the stack
// did until geocoder-prepare.sh existed.
const PG_URL         = process.env.PG_URL || '';
const SEARCH_LIMIT   = Number(process.env.SEARCH_LIMIT || 8);

let pool = null;
if (PG_URL) {
  try {
    const { Pool } = require('pg');
    pool = new Pool({
      connectionString: PG_URL,
      max: 4,
      idleTimeoutMillis: 30000,
      connectionTimeoutMillis: 4000,
    });
    // Without this an idle client dropped by the server takes the process with
    // it -- pg emits 'error' on the pool, and an unhandled 'error' is fatal.
    pool.on('error', (err) => console.error(`[pg] idle client: ${err.message}`));
  } catch (err) {
    console.error(`[pg] driver unavailable (${err.message}); search falls back to mock-google`);
  }
}

// ---------------------------------------------------------------- polyline
// OSRM and Google use the same encoding (Google's algorithm, precision 5), so
// the encoded string is passed through untouched. It is decoded here only to
// derive exact step endpoints and route bounds.
function decodePolyline(str, precision = 5) {
  const factor = Math.pow(10, precision);
  const coords = [];
  let index = 0, lat = 0, lng = 0;
  while (index < str.length) {
    let result = 0, shift = 0, b;
    do { b = str.charCodeAt(index++) - 63; result |= (b & 0x1f) << shift; shift += 5; } while (b >= 0x20);
    lat += (result & 1) ? ~(result >> 1) : (result >> 1);
    result = 0; shift = 0;
    do { b = str.charCodeAt(index++) - 63; result |= (b & 0x1f) << shift; shift += 5; } while (b >= 0x20);
    lng += (result & 1) ? ~(result >> 1) : (result >> 1);
    coords.push([lat / factor, lng / factor]);
  }
  return coords;
}

// ------------------------------------------------------------- formatting
// Google returns both a machine value and a human string; the backend reads
// the value, but the string has to be there and be sane.
const metres = (m) => ({
  text: m >= 1000 ? `${(m / 1000).toFixed(1)} km` : `${Math.round(m)} m`,
  value: Math.round(m),
});

const seconds = (s) => {
  const total = Math.round(s);
  const mins = Math.round(total / 60);
  if (mins < 60) return { text: `${mins} min${mins === 1 ? '' : 's'}`, value: total };
  const h = Math.floor(mins / 60), m = mins % 60;
  return { text: m ? `${h} h ${m} min` : `${h} h`, value: total };
};

const loc = ([lat, lng]) => ({ lat, lng });

// "36.7538,3.0588" -> "3.0588,36.7538"   (Google is lat,lng; OSRM is lon,lat)
function toOsrmCoord(pair) {
  const [lat, lng] = String(pair).split(',').map((n) => parseFloat(n.trim()));
  if (!Number.isFinite(lat) || !Number.isFinite(lng)) return null;
  return `${lng},${lat}`;
}

// --------------------------------------------------------------- handler
async function directions(query, res) {
  const origin = toOsrmCoord(query.get('origin'));
  const destination = toOsrmCoord(query.get('destination'));
  if (!origin || !destination) return send(res, 200, { status: 'INVALID_REQUEST', routes: [] });

  // Google sends waypoints as "via:lat,lng|lat,lng" (or without the prefix).
  const waypoints = (query.get('waypoints') || '')
    .split('|').map((w) => w.trim()).filter(Boolean)
    .map((w) => toOsrmCoord(w.replace(/^via:/, ''))).filter(Boolean);

  const path = [origin, ...waypoints, destination].join(';');
  const alternatives = query.get('alternatives') === 'true';

  // steps=true is what makes per-step polylines available; the backend reads
  // those rather than the route-level overview.
  const url = `${OSRM_URL}/route/v1/driving/${path}`
            + `?overview=false&steps=true&geometries=polyline&alternatives=${alternatives}`;

  let osrm;
  try {
    const r = await fetch(url, { signal: AbortSignal.timeout(20000) });
    osrm = await r.json();
  } catch (err) {
    console.error(`[directions] OSRM unreachable: ${err.message}`);
    return send(res, 200, { status: 'UNKNOWN_ERROR', routes: [] });
  }

  if (osrm.code !== 'Ok' || !osrm.routes || !osrm.routes.length) {
    // NoRoute / NoSegment are legitimate answers, not failures: the caller
    // asked about somewhere with no road connection.
    console.log(`[directions] no route (${osrm.code})`);
    return send(res, 200, { status: 'ZERO_RESULTS', routes: [] });
  }

  const routes = osrm.routes.map((route) => {
    const all = [];
    const legs = (route.legs || []).map((leg) => {
      const steps = (leg.steps || []).map((step) => {
        const pts = step.geometry ? decodePolyline(step.geometry) : [];
        if (pts.length) all.push(...pts);
        const start = pts[0] || [step.maneuver?.location?.[1], step.maneuver?.location?.[0]];
        const end = pts[pts.length - 1] || start;
        return {
          distance: metres(step.distance || 0),
          duration: seconds(step.duration || 0),
          start_location: loc(start),
          end_location: loc(end),
          polyline: { points: step.geometry || '' },
          travel_mode: 'DRIVING',
        };
      });
      const first = steps[0], last = steps[steps.length - 1];
      return {
        distance: metres(leg.distance || 0),
        duration: seconds(leg.duration || 0),
        start_location: first ? first.start_location : { lat: 0, lng: 0 },
        end_location: last ? last.end_location : { lat: 0, lng: 0 },
        steps,
      };
    });

    // Bounds are derived from the decoded geometry rather than just the
    // endpoints, so they actually contain the route.
    const lats = all.map((p) => p[0]), lngs = all.map((p) => p[1]);
    const bounds = all.length
      ? { northeast: { lat: Math.max(...lats), lng: Math.max(...lngs) },
          southwest: { lat: Math.min(...lats), lng: Math.min(...lngs) } }
      : { northeast: { lat: 0, lng: 0 }, southwest: { lat: 0, lng: 0 } };

    return { bounds, legs };
  });

  const km = (osrm.routes[0].distance / 1000).toFixed(2);
  const min = (osrm.routes[0].duration / 60).toFixed(1);
  console.log(`[directions] ${routes.length} route(s), ${km} km, ${min} min`);
  send(res, 200, { status: 'OK', routes });
}

// ═══════════════════════════════════════════════════════════ search ════════
//
// Three more endpoints the backend calls, all of them the *legacy* Google Web
// Service shapes rather than the new Places API -- confirmed by logging what
// actually goes past, not by reading documentation:
//
//   GET /place/autocomplete/json ?input= &location=lat,lng &radius= &language=
//   GET /place/details/json      ?place_id= &fields=
//   GET /geocode/json            ?latlng=lat,lng   (reverse)
//
// Two of the three were broken before this: mock-google implements the *new*
// autocomplete API and no place details at all, so both answered 500, and its
// reverse geocoder replies with an address in Karnataka whatever you ask it.
//
// The answers now come from geo.place -- 113,341 named things extracted from
// the same Algeria .osm.pbf that feeds OSRM and the tiles.

// Google's legacy `types` vocabulary. Nothing downstream switches on it today,
// but sending our own words in a Google-shaped field would be a trap for
// whoever looks next.
const GOOGLE_TYPES = {
  place:     ['locality', 'political', 'geocode'],
  street:    ['route', 'geocode'],
  transport: ['transit_station', 'point_of_interest', 'establishment'],
  poi:       ['point_of_interest', 'establishment'],
};

const typesFor = (kind) => GOOGLE_TYPES[kind] || ['point_of_interest', 'establishment'];

// The one string the rider actually reads. The rider-app's AutoCompleteResp
// carries a single `description` -- there is no main/secondary pair anywhere in
// the chain -- so the locality has to be part of it.
const describe = (row) =>
  row.locality ? `${row.display_name}, ${row.locality}` : row.display_name;

function addressComponents(row) {
  const parts = [{
    long_name: row.display_name,
    short_name: row.display_name,
    types: typesFor(row.kind),
  }];
  if (row.locality) {
    parts.push({ long_name: row.locality, short_name: row.locality, types: ['locality', 'political'] });
  }
  parts.push({ long_name: 'Algérie', short_name: 'DZ', types: ['country', 'political'] });
  return parts;
}

const formatAddress = (row) =>
  [row.display_name, row.locality, 'Algérie'].filter(Boolean).join(', ');

// "36.7538,3.0588" -> [36.7538, 3.0588]
function parseLatLng(raw) {
  if (!raw) return null;
  const [lat, lng] = String(raw).split(',').map((n) => parseFloat(n.trim()));
  return Number.isFinite(lat) && Number.isFinite(lng) ? [lat, lng] : null;
}

async function autocomplete(query, res) {
  const input = (query.get('input') || '').trim();
  // The backend always sends `location`; it is a required field on its own
  // request type. The fallback is only so a hand-typed curl still works.
  const centre = parseLatLng(query.get('location')) || [36.7538, 3.0588];

  if (input.length < 2) return send(res, 200, { status: 'ZERO_RESULTS', predictions: [] });

  let rows;
  try {
    ({ rows } = await pool.query(
      'select place_id, display_name, locality, kind, distance_m from geo.search($1, $2, $3, $4)',
      [input, centre[0], centre[1], SEARCH_LIMIT],
    ));
  } catch (err) {
    console.error(`[autocomplete] "${input}": ${err.message}`);
    return send(res, 200, { status: 'UNKNOWN_ERROR', predictions: [] });
  }

  console.log(`[autocomplete] "${input}" -> ${rows.length}`);
  send(res, 200, {
    status: rows.length ? 'OK' : 'ZERO_RESULTS',
    predictions: rows.map((row) => ({
      description: describe(row),
      place_id: row.place_id,
      distance_meters: Math.round(row.distance_m),
      types: typesFor(row.kind),
    })),
  });
}

async function placeDetails(query, res) {
  const placeId = query.get('place_id');
  if (!placeId) return send(res, 200, { status: 'INVALID_REQUEST' });

  let rows;
  try {
    ({ rows } = await pool.query(
      'select place_id, display_name, locality, kind, lat, lon from geo.place where place_id = $1',
      [placeId],
    ));
  } catch (err) {
    console.error(`[details] ${placeId}: ${err.message}`);
    return send(res, 200, { status: 'UNKNOWN_ERROR' });
  }

  if (!rows.length) {
    // Only reachable if the index was rebuilt from a different extract: place
    // ids are derived from OSM identity precisely so they survive a rebuild.
    console.warn(`[details] unknown place_id ${placeId}`);
    return send(res, 200, { status: 'ZERO_RESULTS' });
  }

  const row = rows[0];
  send(res, 200, {
    status: 'OK',
    result: {
      place_id: row.place_id,
      formatted_address: formatAddress(row),
      address_components: addressComponents(row),
      geometry: { location: { lat: row.lat, lng: row.lon } },
    },
  });
}

async function reverseGeocode(query, res) {
  const at = parseLatLng(query.get('latlng'));
  const placeId = query.get('place_id');

  let rows;
  try {
    if (at) {
      ({ rows } = await pool.query('select * from geo.reverse($1, $2)', [at[0], at[1]]));
    } else if (placeId) {
      ({ rows } = await pool.query(
        'select place_id, display_name, locality, kind, lat, lon from geo.place where place_id = $1',
        [placeId],
      ));
    } else {
      return send(res, 200, { status: 'INVALID_REQUEST', results: [] });
    }
  } catch (err) {
    console.error(`[geocode] ${query.get('latlng') || placeId}: ${err.message}`);
    return send(res, 200, { status: 'UNKNOWN_ERROR', results: [] });
  }

  if (!rows.length) return send(res, 200, { status: 'ZERO_RESULTS', results: [] });

  const row = rows[0];
  console.log(`[geocode] ${at ? at.join(',') : placeId} -> ${row.display_name}`);
  send(res, 200, {
    status: 'OK',
    results: [{
      place_id: row.place_id,
      formatted_address: formatAddress(row),
      address_components: addressComponents(row),
      // The point that was asked about, not the feature's own node. The caller
      // is naming a pin the rider dropped; moving it to the centre of the
      // school we matched would make the pin jump under their finger.
      geometry: { location: { lat: at ? at[0] : row.lat, lng: at ? at[1] : row.lon } },
    }],
  });
}

// Everything else is mock-google's job. Forwarding keeps one googleMapsUrl
// working for place names and autocomplete as well as routes.
async function proxyToMockGoogle(req, res) {
  const target = `${MOCK_GOOGLE_URL}${req.url}`;
  // Logged because the backend's Google client lives in shared-kernel, which is
  // not in this repo -- the only reliable way to learn which paths it actually
  // calls, and with what, is to watch them go past.
  console.log(`[proxy] ${req.method} ${req.url}`);
  try {
    const upstream = await fetch(target, {
      method: req.method,
      headers: { accept: req.headers.accept || 'application/json' },
      signal: AbortSignal.timeout(20000),
    });
    const body = await upstream.text();
    res.writeHead(upstream.status, { 'content-type': upstream.headers.get('content-type') || 'application/json' });
    res.end(body);
  } catch (err) {
    console.error(`[proxy] ${req.url} -> ${err.message}`);
    send(res, 502, { status: 'UNKNOWN_ERROR', error: 'mock-google unreachable' });
  }
}

function send(res, code, obj) {
  const body = JSON.stringify(obj);
  res.writeHead(code, { 'content-type': 'application/json;charset=utf-8', 'content-length': Buffer.byteLength(body) });
  res.end(body);
}

http.createServer((req, res) => {
  const url = new URL(req.url, 'http://localhost');
  if (url.pathname === '/healthz') {
    return send(res, 200, {
      ok: true, osrm: OSRM_URL, mockGoogle: MOCK_GOOGLE_URL, search: Boolean(pool),
      // False means a driver pressing "payer" gets "payments not configured".
      // Cheaper to notice here than in his hands. Never the key itself.
      payments: subscription.configured(),
    });
  }
  if (url.pathname === '/directions/json') return directions(url.searchParams, res);

  // Who is nearby and what they drive. Nothing to do with Google, and kept in
  // its own file for that reason -- this shim answers as Google for the
  // backend, and this one route answers to the passenger app directly. See
  // fleet.js for why it exists and what it deliberately withholds.
  if (url.pathname === '/fleet/nearby') {
    return fleet.nearby({
      url,
      res,
      pool,
      riderUrl: RIDER_URL,
      token: req.headers.token || '',
    });
  }

  // A passenger's own star rating, for her own profile screen. Here rather
  // than on the rider backend because it is not on the rider backend:
  // `GET /v2/profile` returns eight fields and no rating, and the number a
  // driver gives her is written to the *provider* schema. See rating.js.
  //
  // A driver's is here too, for a narrower reason: his own profile route
  // returns the average and not how many people gave it, and the string
  // `totalRatings` is not in the binary at all.
  //
  //   GET /rating/phone/{number}     what her own profile screen shows
  //   GET /rating/driver/{driverId}  what his does
  if (url.pathname.startsWith('/rating/')) {
    if (req.method !== 'GET') return send(res, 405, { error: 'method not allowed' });
    const [, , kind, ...rest] = url.pathname.split('/');
    const who = decodeURIComponent(rest.join('/'));
    if (kind === 'phone') return rating.serveForPhone(pool, who, res);
    if (kind === 'driver') return rating.serveForDriver(pool, who, res);
    return send(res, 404, { error: 'no such rating' });
  }

  // The driver's 3 000 DA a month, through Chargily Pay. Here rather than on
  // the driver backend because the backend has nowhere to put it: no plan, fee,
  // subscription, invoice or order table in either schema, and none of those
  // words in the binary -- upstream's driver-subscription subsystem is not in
  // this build. See subscription.js, and probe-subscription.sql for the
  // measurement.
  //
  //   GET  /subscription/status              his screen, from his token
  //   POST /subscription/checkout?method=    opens a payment page
  //   GET  /subscription/history             what he has paid
  //   GET  /subscription/receipt/{checkout}  one of them, in full
  //   POST /subscription/webhook             Chargily. The only thing that
  //                                          ever extends a subscription.
  //   GET  /subscription/done?state=         where his browser lands after
  //
  // No route here takes a driver id: `status`, `checkout`, `history` and
  // `receipt` all derive it from the token by asking the driver backend who it
  // belongs to, so one driver cannot read or buy against another's account.
  if (url.pathname.startsWith('/subscription/')) {
    const [, , what, ...rest] = url.pathname.split('/');
    const token = req.headers.token || '';

    // Chargily first: it is the only caller here that is not the app, the only
    // one that POSTs a body, and the only one whose body must reach the handler
    // unparsed -- the signature is an HMAC over the exact bytes.
    if (what === 'webhook') {
      if (req.method !== 'POST') return send(res, 405, { error: 'method not allowed' });
      return subscription.webhook(pool, req, res);
    }
    if (what === 'done') return subscription.done(url.searchParams, res);

    if (what === 'status' && req.method === 'GET') return subscription.status(pool, token, res);
    if (what === 'history' && req.method === 'GET') return subscription.history(pool, token, res);
    if (what === 'receipt' && req.method === 'GET') {
      return subscription.receipt(pool, token, decodeURIComponent(rest.join('/')), res);
    }
    if (what === 'checkout' && req.method === 'POST') {
      return subscription.checkout(pool, token, url.searchParams.get('method'), res);
    }
    // ...and the state of one, which our own tables cannot answer: an
    // abandoned checkout and a late webhook are the same `pending` row here.
    if (what === 'checkout' && req.method === 'GET') {
      return subscription.checkoutState(pool, token, decodeURIComponent(rest.join('/')), res);
    }
    return send(res, 404, { error: 'no such subscription route' });
  }

  // Profile photographs. Nothing to do with Google either -- see avatars.js for
  // why the backend cannot hold an image and why passengers are keyed by a
  // hash of their number rather than by an id.
  //
  //   PUT    /avatar/driver/{driverId}   the driver's own, by his person id
  //   PUT    /avatar/phone/{number}      a passenger's own, by her number
  //   GET    /avatar/driver/{driverId}   what a passenger sees on an offer
  //   GET    /avatar/plate/{plate}       ...and on every screen after it
  //   GET    /avatar/ride/{rideId}       what a driver sees of his passenger
  //   DELETE either of the PUT forms     back to the initial
  if (url.pathname.startsWith('/avatar/')) {
    const [, , kind, ...rest] = url.pathname.split('/');
    const value = decodeURIComponent(rest.join('/'));

    if (kind === 'ride' && req.method === 'GET') {
      return avatars.serveForRide(value, pool, res);
    }
    // By the car, for every passenger screen after the booking: those
    // carry a plate and no driver id. See avatars.js.
    if (kind === 'plate' && req.method === 'GET') {
      return avatars.serveForPlate(value, pool, res);
    }

    // ── Reading stays open ─────────────────────────────────────────────────
    // An avatar is shown to the person at the other end of a ride either way,
    // and the keys are opaque UUIDs and one-way hashes. A passenger's key is a
    // database lookup rather than a hash, so this is async where a driver's
    // is not.
    if (req.method === 'GET') {
      const resolve =
        kind === 'driver'
          ? Promise.resolve(avatars.driverKey(value))
          : kind === 'phone'
            ? avatars.keyForPhone(pool, value)
            : Promise.resolve(null);
      return resolve.then((key) => avatars.serve(key, res));
    }

    // ── Writing does not, and did not until 2026-08-27 ─────────────────────
    //
    // This module's own header has always claimed PUT "takes the caller's
    // token and asks the backend whose it is". It was written and never
    // built: PUT and DELETE reached the store with no credential at all.
    // Measured against the live edge, `DELETE /avatar/driver/{id}` answered
    // 200 to a stranger, and PUT got as far as the content-type check. Anyone
    // knowing a driver's id could replace the face a passenger sees when
    // choosing him. **The comment describing the protection is why nobody
    // looked for it.**
    //
    // The id in the path is now ignored entirely. The key is built from
    // whoever the backend says the token belongs to, so naming somebody else
    // correctly achieves nothing — the same rule /subscription/ follows.
    if (req.method === 'PUT' || req.method === 'DELETE') {
      const token = req.headers.token || '';
      const owner =
        kind === 'driver'
          ? identity
              .driverFromToken(DRIVER_URL, token)
              .then((d) => (d ? avatars.driverKey(d.id) : null))
          : kind === 'phone'
            ? identity
                .riderFromToken(RIDER_URL, token)
                .then((r) => (r ? avatars.keyForRiderId(pool, r.id) : null))
            : Promise.resolve(null);

      return owner.then((key) => {
        if (!key) return send(res, 401, { error: 'sign in first' });
        if (req.method === 'PUT') return avatars.store(key, req, res);
        return avatars.remove(key, res);
      });
    }

    return send(res, 405, { error: 'method not allowed' });
  }

  // Without an index configured these fall through to mock-google, which is
  // the behaviour the stack had before search existed.
  if (pool) {
    if (url.pathname === '/place/autocomplete/json') return autocomplete(url.searchParams, res);
    if (url.pathname === '/place/details/json') return placeDetails(url.searchParams, res);
    if (url.pathname === '/geocode/json') return reverseGeocode(url.searchParams, res);
  }

  return proxyToMockGoogle(req, res);
}).listen(PORT, () => {
  // Who dispatch should skip. Published to Redis, where the driver binary
  // reads it -- see restricted.js for why the policy lives here and not there.
  restricted.start(pool);
  console.log(
    `maps-shim on :${PORT}  ->  OSRM ${OSRM_URL}, mock-google ${MOCK_GOOGLE_URL}, ` +
    `search ${pool ? 'from geo.place' : 'OFF (no PG_URL)'}`,
  );
});
