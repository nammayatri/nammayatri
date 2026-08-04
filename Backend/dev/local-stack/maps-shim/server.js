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

const PORT           = Number(process.env.PORT || 8020);
const OSRM_URL       = (process.env.OSRM_URL || 'http://localhost:5000').replace(/\/$/, '');
const MOCK_GOOGLE_URL= (process.env.MOCK_GOOGLE_URL || 'http://localhost:8019').replace(/\/$/, '');

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

// Everything else is mock-google's job. Forwarding keeps one googleMapsUrl
// working for place names and autocomplete as well as routes.
async function proxyToMockGoogle(req, res) {
  const target = `${MOCK_GOOGLE_URL}${req.url}`;
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
  if (url.pathname === '/healthz') return send(res, 200, { ok: true, osrm: OSRM_URL, mockGoogle: MOCK_GOOGLE_URL });
  if (url.pathname === '/directions/json') return directions(url.searchParams, res);
  return proxyToMockGoogle(req, res);
}).listen(PORT, () => {
  console.log(`maps-shim on :${PORT}  ->  OSRM ${OSRM_URL}, mock-google ${MOCK_GOOGLE_URL}`);
});
