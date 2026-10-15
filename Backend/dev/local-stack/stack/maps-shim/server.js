'use strict';
//
// maps-shim: what the backend thinks is Google, plus the services the apps
// call beside it. This file is the router and the start-up; each subject lives
// in its own module (phase 5, 2026-10-06):
//
//   directions.js   /directions/json -> OSRM          (Google Directions)
//   wallet.js, restricted.js, deletion.js, avatars.js, rating.js, fleet.js,
//   push-relay.js, driver-push.js, number-change.js
//
//   places.js       search, details, labels, reverse geocoding (geo.place)
//
// No dependencies beyond `pg`: Node's built-in http plus global fetch.

const http = require('http');
const fleet = require('./fleet');
const avatars = require('./avatars');
const rating = require('./rating');
const wallet = require('./wallet');
const identity = require('./identity');
const restricted = require('./restricted');
const deletion = require('./deletion');
const pushRelay = require('./push-relay');
const driverPush = require('./driver-push');
const numberChange = require('./number-change');
const { directions, OSRM_URL } = require('./directions');
const { send } = require('./reply');
const places = require('./places');

const PORT           = Number(process.env.PORT || 8020);
const MOCK_GOOGLE_URL= (process.env.MOCK_GOOGLE_URL || 'http://localhost:8019').replace(/\/$/, '');
// Where to check that a caller is a signed-in passenger, for /fleet/nearby.
const RIDER_URL      = (process.env.RIDER_URL || 'http://localhost:8013').replace(/\/$/, '');
// Where to prove a caller is a signed-in driver, for avatar writes.
const DRIVER_URL     = (process.env.DRIVER_URL || 'http://localhost:8016').replace(/\/$/, '');

// Where the place index lives. Unset means "no search": the three geocoding
// paths fall through to mock-google exactly as before, which is what the stack
// did until geocoder-prepare.sh existed.
const PG_URL         = process.env.PG_URL || '';

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
places.usePool(pool);


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


http.createServer((req, res) => {
  const url = new URL(req.url, 'http://localhost');
  if (url.pathname === '/healthz') {
    return send(res, 200, {
      ok: true, osrm: OSRM_URL, mockGoogle: MOCK_GOOGLE_URL, search: Boolean(pool),
      // Per country, false means a driver pressing "recharger" there gets
      // "payments not configured". Cheaper to notice here than in his hands.
      // Never the key itself.
      payments: wallet.gateways(),
      // False means iPhones get no push; Android is unaffected either way.
      push: pushRelay.status(),
    });
  }
  if (url.pathname === '/directions/json') return directions(url.searchParams, res);

  // Both backends' FCM sends, since 2026-09-16: fcm_url points here. Android
  // goes on to Google untouched; iPhones go to Apple with the app's words. Not
  // exposed by the edge, and refused from anywhere but loopback. See push-relay.js.
  if (url.pathname.startsWith('/push/')) return pushRelay.handle(req, res, url);

  // The office's own notifications to one driver -- accepted, refused
  // (2026-09-28). Called by admin-api from its container, so the docker bridge
  // is let in beside loopback; the edge routes nothing under /internal/ here,
  // so nothing public reaches it. See driver-push.js.
  if (url.pathname === '/internal/driver-push') {
    const from = String(req.socket.remoteAddress || '').replace(/^::ffff:/, '');
    const local = from === '127.0.0.1' || from === '::1' || /^172\.(1[6-9]|2\d|3[01])\./.test(from);
    if (!local) return send(res, 403, { error: 'local callers only' });
    if (req.method !== 'POST') return send(res, 405, { error: 'method not allowed' });
    const chunks = [];
    let size = 0;
    req.on('data', (c) => {
      size += c.length;
      if (size > 1024) return void req.destroy();
      chunks.push(c);
    });
    req.on('end', async () => {
      let body = {};
      try { body = JSON.parse(Buffer.concat(chunks).toString('utf8')); } catch { /* checked below */ }
      const driverId = typeof body.driverId === 'string' ? body.driverId : '';
      const type = typeof body.type === 'string' ? body.type : '';
      if (!/^[0-9a-f-]{36}$/i.test(driverId) || !driverPush.TYPES.has(type) || !pool) {
        return send(res, 400, { error: 'bad request' });
      }
      const r = await driverPush.notify(pool, driverId, type);
      send(res, r.ok ? 200 : 502, r);
    });
    return undefined;
  }

  // A person changing their own number, once the auth guard has proved the new
  // one (2026-10-03). Loopback only: the guard is the one caller, and it runs
  // on the host network. See number-change.js.
  if (url.pathname === '/internal/number-change') {
    const from = String(req.socket.remoteAddress || '').replace(/^::ffff:/, '');
    if (from !== '127.0.0.1' && from !== '::1') return send(res, 403, { error: 'local callers only' });
    if (req.method !== 'POST') return send(res, 405, { error: 'method not allowed' });
    const chunks = [];
    let size = 0;
    req.on('data', (c) => {
      size += c.length;
      if (size > 4096) return void req.destroy();
      chunks.push(c);
    });
    req.on('end', async () => {
      let body = {};
      try { body = JSON.parse(Buffer.concat(chunks).toString('utf8')); } catch { /* refused below */ }
      try {
        const r = await numberChange.run(pool, { riderUrl: RIDER_URL, driverUrl: DRIVER_URL }, body);
        send(res, r.ok ? 200 : 409, r);
      } catch (e) {
        console.error(`[number-change] ${e.message}`);
        send(res, 502, { ok: false, error: 'failed' });
      }
    });
    return undefined;
  }

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
    // Hers only, by her token -- never by a number anybody can type (2026-09-27).
    // An older app that sends no token sees "not yet rated", not someone else's.
    if (kind === 'phone') {
      return identity
        .riderFromToken(RIDER_URL, req.headers.token || '')
        .then((me) => (me ? rating.serveForRider(pool, me.id, res) : send(res, 401, { error: 'sign in first' })));
    }
    if (kind === 'driver') return rating.serveForDriver(pool, who, res);
    return send(res, 404, { error: 'no such rating' });
  }

  /* ── The wallet ────────────────────────────────────────────────────────
     A day's price, taken at the driver's first ride. It replaced the monthly
     /subscription/, retired 2026-10-07 (phase 6): no phone had called it
     since 2026-09-02, and the edge now answers it 410. See wallet.js. */
  if (url.pathname.startsWith('/wallet/')) {
    const [, , what, ...rest] = url.pathname.split('/');
    const token = req.headers.token || '';

    // Moosyl first: the only caller here that is not the app. Its body is
    // never trusted -- it only says which top-up to go and read back.
    if (what === 'webhook') {
      if (req.method !== 'POST') return send(res, 405, { error: 'method not allowed' });
      return wallet.webhook(pool, req, res);
    }
    // A browser lands here, not the app. HTML, and it claims no result.
    if (what === 'done') return wallet.done(url.searchParams, res);

    if (what === 'status' && req.method === 'GET') return wallet.status(pool, token, res);
    if (what === 'history' && req.method === 'GET') return wallet.history(pool, token, res);
    if (what === 'topup' && req.method === 'POST') {
      // `method` only matters in Algeria, where Chargily wants the card type.
      return wallet.topup(pool, token, url.searchParams.get('amount'),
        url.searchParams.get('method'), res);
    }
    // The state of one, which our own tables cannot answer on their own: an
    // abandoned checkout and a late webhook are the same `pending` row here.
    if (what === 'topup' && req.method === 'GET') {
      return wallet.topupState(pool, token, decodeURIComponent(rest.join('/')), res);
    }
    return send(res, 404, { error: 'no such wallet route' });
  }

  // Account deletion requests. Google Play requires the path to exist INSIDE
  // the app, and the deployed backend has no deletion route of any kind, so
  // the request is recorded here and an administrator carries it out.
  //
  // Nothing under this path deletes anything. See deletion.js.
  //
  //   GET    /account/deletion-request   what screen 21 draws when it opens
  //   POST   /account/deletion-request   record it
  //   DELETE /account/deletion-request   withdraw it
  if (url.pathname === '/account/deletion-request') {
    const token = req.headers.token || '';
    const backends = { RIDER_URL, DRIVER_URL };

    if (req.method === 'GET') return deletion.status(pool, backends, token, res);
    if (req.method === 'DELETE') return deletion.withdraw(pool, backends, token, res);
    if (req.method === 'POST') {
      // Small and ours, so it is read and parsed here rather than streamed:
      // unlike the Chargily webhook there is no signature over the raw bytes.
      let raw = '';
      req.on('data', (chunk) => {
        raw += chunk;
        if (raw.length > 4096) req.destroy();
      });
      req.on('end', () => {
        let body = {};
        try {
          body = raw ? JSON.parse(raw) : {};
        } catch {
          // A malformed body costs the reason, not the request. Somebody
          // leaving should not be stopped by a field that is optional anyway.
          body = {};
        }
        void deletion.request(pool, backends, token, body, res);
      });
      return undefined;
    }
    return send(res, 405, { error: 'method not allowed' });
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
      // A passenger's photograph was served to anybody who typed her number
      // (2026-09-27): a face, from a phone number, with no sign-in. The only
      // reader was her own profile screen, so it now takes her token and
      // serves hers -- the number in the path is not read. Drivers see her
      // through /avatar/ride/{rideId}, which needs a ride id they were given.
      const resolve =
        kind === 'driver'
          ? Promise.resolve(avatars.driverKey(value))
          : kind === 'phone'
            ? identity
                .riderFromToken(RIDER_URL, req.headers.token || '')
                .then((r) => (r ? avatars.keyForRiderId(pool, r.id) : null))
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
    // correctly achieves nothing — the same rule /wallet/ follows.
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
    if (url.pathname === '/place/autocomplete/json') return places.autocomplete(url.searchParams, res);
    if (url.pathname === '/place/details/json') return places.placeDetails(url.searchParams, res);
    // The only /place/ route the edge exposes publicly -- see the nginx
    // location, which names this exact path rather than the /place/ prefix so
    // autocomplete and details stay reachable only from the backend.
    if (url.pathname === '/place/labels/json') return places.placeLabels(url.searchParams, res);
    if (url.pathname === '/geocode/json') return places.reverseGeocode(url.searchParams, res);
  }

  return proxyToMockGoogle(req, res);
}).listen(PORT, () => {
  // Who dispatch should skip. Published to Redis, where the driver binary
  // reads it -- see restricted.js for why the policy lives here and not there.
  restricted.start(pool);

  /* Take the day off each driver's first ride.
     Polled rather than pushed: the shim is not in the ride flow, and the app
     must never be what triggers a charge -- a phone that is switched off would
     then be a free day. It shares a database with the driver backend, so it
     just looks at which rides have started. Runs before the restriction
     refresh's own interval so a driver who has just been charged is reflected
     in the pool within one cycle. See wallet.js. */
  if (pool) {
    const sweep = () => void wallet.chargeStartedRides(pool);
    sweep();
    setInterval(sweep, Number(process.env.WALLET_SWEEP_MS || 60 * 1000)).unref();
  }
  console.log(
    `maps-shim on :${PORT}  ->  OSRM ${OSRM_URL}, mock-google ${MOCK_GOOGLE_URL}, ` +
    `search ${pool ? 'from geo.place' : 'OFF (no PG_URL)'}`,
  );
});
