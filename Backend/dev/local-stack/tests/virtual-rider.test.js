'use strict';
// maps-shim/virtual-rider.js -- the virtual passenger (day 7, 2026-11-16) --
// against a real Postgres (PGlite) holding the tables it reads, a fake rider
// API that behaves like the real one, and a fake OSRM. The tester's own
// actions (arriving, starting, ending, cancelling, rating) are scripted: one
// step each time the engine waits.
//
//   cd Backend/dev/local-stack/tests && npm ci && node virtual-rider.test.js
//
// The five rules it must never break (plan, day 7, point 6):
//   1. never accept another REAL driver's offer
//   2. book nothing when the tester is offline, stale, blocked or cannot work
//   3. never sign in as the driver (never a driver route at all)
//   4. interrupted, cancel and leave nothing open
//   5. switched off, do nothing
// Then every scenario, 1 to 7, to its verdict; 8 refused until decision V3.
const crypto = require('crypto');
const fs = require('fs');
const http = require('http');
const path = require('path');
const { spawnSync } = require('child_process');
const { open } = require('./lib/db');
const fakes = require('./lib/fakes');

const SHIM = path.join(__dirname, '..', 'stack', 'maps-shim');
const VR = path.join(SHIM, 'virtual-rider.js');
const { run, cleanUp, parseNumber, COMMENT } = require(VR);

const SALT = 'test-salt';
const hash = (n) => crypto.createHash('sha256').update(SALT + n).digest();
const DZ = 'algeria0-0000-0000-0000-00000algeria';
const TESTER = 'd1d1d1d1-0000-4000-8000-0000000000a1';
const OTHER = 'd2d2d2d2-0000-4000-8000-0000000000a2';
const SIM = 'd3d3d3d3-0000-4000-8000-0000000000a3';
const AT = { lat: 36.7538, lon: 3.0588 };

const SHORT = {
  results: 3, offer: 6, assigned: 3, beforeCancel: 1, arrival: 10, afterArrival: 1,
  driverCancels: 10, ride: 10, driverRating: 5, others: 3, offerLife: 1, poll: 1,
};

/* ── The database: the columns the engine reads, nothing invented ─────────── */
async function database() {
  const db = await open();
  await db.exec(`
    ALTER TABLE atlas_driver_offer_bpp.person
      ADD COLUMN mobile_country_code text, ADD COLUMN mobile_number_hash bytea, ADD COLUMN first_name text;
    ALTER TABLE atlas_driver_offer_bpp.driver_information
      ADD COLUMN enabled boolean NOT NULL DEFAULT true, ADD COLUMN blocked boolean NOT NULL DEFAULT false,
      ADD COLUMN active boolean NOT NULL DEFAULT true;
    CREATE TABLE atlas_driver_offer_bpp.driver_location (driver_id text PRIMARY KEY, lat double precision, lon double precision, updated_at timestamptz);
    CREATE TABLE atlas_driver_offer_bpp.vehicle (driver_id text PRIMARY KEY, variant text);
    CREATE TABLE atlas_driver_offer_bpp.driver_quote (id text PRIMARY KEY, driver_id text NOT NULL);
    CREATE TABLE atlas_driver_offer_bpp.search_request_for_driver (driver_id text, created_at timestamptz DEFAULT now());
    CREATE TABLE atlas_driver_offer_bpp.booking_cancellation_reason (ride_id text, source text);
    CREATE TABLE atlas_app.driver_offer (id text PRIMARY KEY, bpp_quote_id text NOT NULL);
    CREATE TABLE atlas_app.quote (id text PRIMARY KEY, driver_offer_id text);
    CREATE TABLE movin.driver_rider_rating (ride_id text PRIMARY KEY, driver_id text NOT NULL, rider_id text, stars smallint NOT NULL);
  `);
  const driver = async (id, number, first) => {
    await db.sql(`INSERT INTO atlas_driver_offer_bpp.person (id, merchant_id, role, unencrypted_mobile_number, mobile_country_code, mobile_number_hash, first_name)
                  VALUES ($1, $2, 'DRIVER', $3, '+213', $4, $5)`, [id, DZ, number, hash(number), first]);
    await db.sql(`INSERT INTO atlas_driver_offer_bpp.driver_information (driver_id) VALUES ($1)`, [id]);
    await db.sql(`INSERT INTO atlas_driver_offer_bpp.driver_location VALUES ($1, $2, $3, now())`, [id, AT.lat, AT.lon]);
    await db.sql(`INSERT INTO atlas_driver_offer_bpp.vehicle VALUES ($1, 'SEDAN')`, [id]);
  };
  await driver(TESTER, '0555123456', 'Karim');
  await driver(OTHER, '0555999999', 'Réel');
  await driver(SIM, '0555100001', 'Simulé');
  await db.sql(`INSERT INTO movin.wallet (driver_id, balance) VALUES ($1, 500)`, [TESTER]);
  return db;
}

/* ── The rider API, and the tester's hands ─────────────────────────────────── */
function world(db) {
  const w = {
    requests: [],          // every request the engine made: [method, path, body]
    offers: [],            // per search, the drivers who offer, in order
    leftovers: [],         // bookings listed as open at the start
    script: [],            // the tester's steps after the booking, one per wait
    bookings: new Map(),   // id -> { status, ride }
    searches: 0,
    stopped: false,
    estimate: null,
  };
  let n = 0;
  w.quoteFor = async (driverId) => {
    n += 1;
    const q = `q${n}`;
    await db.sql(`INSERT INTO atlas_driver_offer_bpp.driver_quote VALUES ($1, $2)`, [`dq${n}`, driverId]);
    await db.sql(`INSERT INTO atlas_app.driver_offer VALUES ($1, $2)`, [`do${n}`, `dq${n}`]);
    await db.sql(`INSERT INTO atlas_app.quote VALUES ($1, $2)`, [q, `do${n}`]);
    return q;
  };
  w.quoteDriver = new Map();
  w.current = () => [...w.bookings.values()].at(-1);
  w.tick = async () => {
    if (w.stopped) throw new Error('stopped by the test');
    const b = w.current();
    if (!b || !w.script.length) return;
    await w.script.shift()(b);
  };
  // The tester's possible steps.
  w.arrive = async (b) => { b.ride.driverArrivalTime = new Date().toISOString(); };
  w.start = async (b) => {
    await db.sql(`UPDATE atlas_driver_offer_bpp.ride SET status = 'INPROGRESS' WHERE id = $1`, [b.ride.bppRideId]);
    b.ride.status = 'INPROGRESS';
  };
  // What the simulated cars do (measured 2026-11-16): arrive 282 m away in 26 s
  // and start the same second.
  w.arriveAndStart = async (b) => { await w.arrive(b); await w.start(b); };
  w.end = async (b) => {
    await db.sql(`UPDATE atlas_driver_offer_bpp.ride SET status = 'COMPLETED' WHERE id = $1`, [b.ride.bppRideId]);
    b.status = 'COMPLETED'; b.ride.status = 'COMPLETED'; b.ride.computedPrice = 812;
  };
  w.rate = async (b) => { await db.sql(`INSERT INTO movin.driver_rider_rating VALUES ($1, $2, 'r', 4)`, [b.ride.bppRideId, TESTER]); };
  w.driverCancels = async (b) => {
    await db.sql(`UPDATE atlas_driver_offer_bpp.ride SET status = 'CANCELLED' WHERE id = $1`, [b.ride.bppRideId]);
    await db.sql(`INSERT INTO atlas_driver_offer_bpp.booking_cancellation_reason VALUES ($1, 'ByDriver')`, [b.ride.bppRideId]);
    b.status = 'CANCELLED'; b.ride.status = 'CANCELLED';
  };
  w.nothing = async () => {};
  return w;
}

async function riderApi(db, w) {
  const server = http.createServer((req, res) => {
    let raw = '';
    req.on('data', (c) => { raw += c; });
    req.on('end', async () => {
      const body = raw ? JSON.parse(raw) : undefined;
      const url = new URL(req.url, 'http://x');
      const p = url.pathname;
      w.requests.push([req.method, p, body]);
      const json = (status, b) => { res.writeHead(status, { 'content-type': 'application/json' }); res.end(JSON.stringify(b)); };
      let m;
      if (p === '/v2/auth') return json(200, { authId: 'a1' });
      if (p === '/v2/auth/a1/verify') return json(200, body && body.otp === '7891' ? { token: 'rider-token' } : {});
      if (req.headers.token !== 'rider-token') return json(401, { errorCode: 'INVALID_TOKEN' });
      if (p === '/v2/rideBooking/list') return json(200, { list: w.leftovers });
      if (p === '/v2/rideSearch') {
        w.searches += 1;
        w.search = body;
        const who = w.offers[w.searches - 1] || [];
        w.estimate = { id: `e${w.searches}`, quotes: [] };
        for (const d of who) {
          const q = await w.quoteFor(d);
          w.quoteDriver.set(q, d);
          w.estimate.quotes.push(q);
        }
        return json(200, { searchId: `s${w.searches}` });
      }
      if ((m = p.match(/^\/v2\/rideSearch\/(s\d+)\/results$/))) {
        return json(200, { estimates: [{ id: w.estimate.id, vehicleVariant: 'SEDAN', estimatedTotalFare: 800 }] });
      }
      if ((m = p.match(/^\/v2\/estimate\/(e\d+)\/select$/))) return json(200, { result: 'Success' });
      if ((m = p.match(/^\/v2\/estimate\/(e\d+)\/quotes$/))) {
        return json(200, { selectedQuotes: w.estimate.quotes.map((id) => ({ id })) });
      }
      if ((m = p.match(/^\/v2\/estimate\/(e\d+)\/cancel$/))) return json(200, { result: 'Success' });
      if ((m = p.match(/^\/v2\/rideSearch\/quotes\/(q\d+)\/confirm$/))) {
        const id = `b${w.bookings.size + 1}`;
        const bpp = `dr${w.bookings.size + 1}`;
        await db.sql(`INSERT INTO atlas_driver_offer_bpp.ride (id, driver_id, status) VALUES ($1, $2, 'NEW')`, [bpp, w.quoteDriver.get(m[1])]);
        w.bookings.set(id, { id, quote: m[1], driver: w.quoteDriver.get(m[1]), status: 'TRIP_ASSIGNED',
          ride: { id: `rr${w.bookings.size + 1}`, bppRideId: bpp, status: 'NEW', driverArrivalTime: null } });
        return json(200, { bookingId: id });
      }
      if ((m = p.match(/^\/v2\/rideBooking\/(b\d+|left\d+)\/cancel$/))) {
        const b = w.bookings.get(m[1]);
        // The real server refuses once the ride has started (2026-11-16, live).
        if (b && ['INPROGRESS', 'COMPLETED'].includes(b.ride.status)) return json(400, { errorCode: 'BOOKING_INVALID_STATUS' });
        if (b && w.refuseCancel) return json(400, { errorCode: 'SOMETHING_ELSE' });
        if (b) {
          b.status = 'CANCELLED'; b.ride.status = 'CANCELLED';
          await db.sql(`UPDATE atlas_driver_offer_bpp.ride SET status = 'CANCELLED' WHERE id = $1`, [b.ride.bppRideId]);
        }
        return json(200, { result: 'Success' });
      }
      if ((m = p.match(/^\/v2\/rideBooking\/(b\d+)$/))) {
        const b = w.bookings.get(m[1]);
        return json(200, { id: b.id, status: b.status, rideList: [b.ride] });
      }
      if (p === '/v2/feedback/rateRide') return json(200, { result: 'Success' });
      return json(404, { errorCode: 'NOT_HERE' });
    });
  });
  await new Promise((r) => server.listen(0, '127.0.0.1', r));
  return { url: `http://127.0.0.1:${server.address().port}`, close: () => server.close() };
}

async function osrm() {
  const server = http.createServer((req, res) => {
    const m = req.url.match(/nearest\/v1\/driving\/([-\d.]+),([-\d.]+)/);
    res.writeHead(200, { 'content-type': 'application/json' });
    res.end(JSON.stringify({ waypoints: [{ location: [Number(m[1]), Number(m[2])], distance: 12 }] }));
  });
  await new Promise((r) => server.listen(0, '127.0.0.1', r));
  return { url: `http://127.0.0.1:${server.address().port}`, close: () => server.close() };
}

/* ── The cases ───────────────────────────────────────────────────────────────── */
(async () => {
  const t = fakes.checker();
  const routing = await osrm();

  /** A fresh database and rider API, one scenario run, everything it did. */
  async function scenario(n, setup = () => {}, opts = {}) {
    const db = await database();
    const w = world(db);
    await setup(w, db);
    const api = await riderApi(db, w);
    const lines = [];
    const state = opts.state || {};
    let verdict;
    try {
      verdict = await run({
        pool: db.pool, say: (s) => lines.push(s), sleep: () => (opts.onSleep ? opts.onSleep(w, state) : w.tick()),
        wait: SHORT, riderUrl: api.url, osrmUrl: routing.url, salt: opts.salt ?? SALT, enabled: opts.enabled ?? true, state,
      }, opts.number || '+213 0555 12 34 56', n);
    } catch (e) {
      verdict = { ok: false, reason: `threw: ${e.message}` };
    }
    api.close();
    const rides = await db.sql(`SELECT id, driver_id, status FROM atlas_driver_offer_bpp.ride ORDER BY id`);
    await db.close();
    const paths = w.requests.map(([m, p]) => `${m} ${p}`);
    const confirmedDrivers = [...w.bookings.values()].map((b) => b.driver);
    return { verdict, w, lines, rides, paths, confirmedDrivers, state };
  }
  const openBookings = (w) => [...w.bookings.values()].filter((b) => !['CANCELLED', 'COMPLETED'].includes(b.status));
  const noDriverRoute = (paths) => paths.every((p) => !/ \/ui\//.test(p));

  t.section('the number typed');
  t.ok(parseNumber('+213 0555 12 34 56').candidates.join() === '0555123456,555123456', 'Algeria with the trunk zero: both stored forms tried');
  t.ok(parseNumber('0555123456').country === 'DZ' && parseNumber('22123456').country === 'MR', 'without the dial code: the country from the shape');
  t.ok(parseNumber('+222 2212 3456').candidates[0] === '22123456', 'Mauritania: eight digits');
  t.ok(parseNumber('12') === null && parseNumber('+33612345678') === null, 'anything else: not recognised');

  t.section('rule 5: switched off, it does nothing');
  let r = await scenario(1, (w) => { w.offers = [[TESTER]]; }, { enabled: false });
  t.ok(!r.verdict.ok && /éteint/.test(r.verdict.reason), 'refused, and says the switch is off', r.verdict.reason);
  t.ok(r.paths.length === 0, 'not one request to the passenger API', r.paths.join(' | '));

  t.section('rule 2: it books nothing for a driver dispatch would not reach');
  for (const [label, change, word] of [
    ['offline (not « En service »)', `UPDATE atlas_driver_offer_bpp.driver_information SET active = false`, /pas en ligne/],
    ['blocked', `UPDATE atlas_driver_offer_bpp.driver_information SET blocked = true`, /bloqué/],
    ['not validated', `UPDATE atlas_driver_offer_bpp.driver_information SET enabled = false`, /validé/],
    ['already on a ride', `UPDATE atlas_driver_offer_bpp.driver_information SET on_ride = true`, /déjà en course/],
    ['a position 20 min old', `UPDATE atlas_driver_offer_bpp.driver_location SET updated_at = now() - interval '20 minutes'`, /position trop ancienne/],
    ['no credit and no day (cannot work)', `UPDATE movin.wallet SET balance = 0, day_until = NULL`, /ne peut pas travailler/],
  ]) {
    r = await scenario(1, async (w, db) => { w.offers = [[TESTER]]; await db.sql(`${change} ${/wallet/.test(change) ? '' : `WHERE driver_id = '${TESTER}'`}`); });
    t.ok(!r.verdict.ok && word.test(r.verdict.reason) && !r.paths.some((p) => /rideSearch|confirm/.test(p)),
      `${label}: refused with the reason, and no search, no booking`, r.verdict.reason);
  }
  r = await scenario(1, (w) => { w.offers = [[TESTER]]; }, { number: '0555000000' });
  t.ok(!r.verdict.ok && /aucun compte chauffeur/.test(r.verdict.reason) && r.paths.length === 0, 'a number with no driver account: refused, nothing asked', r.verdict.reason);
  r = await scenario(8);
  t.ok(!r.verdict.ok && /V3/.test(r.verdict.reason) && r.paths.length === 0, 'scenario 8: refused until decision V3', r.verdict.reason);
  r = await scenario(9);
  t.ok(!r.verdict.ok && /inconnu/.test(r.verdict.reason), 'scenario 9: unknown', r.verdict.reason);

  t.section('rules 1 and 3, on scenario 1: only the tester\'s offer; never a driver route');
  r = await scenario(1, (w) => { w.offers = [[OTHER, SIM, TESTER]]; w.script = [w.arrive, w.start, w.end, w.rate]; });
  t.ok(r.verdict.ok, 'scenario 1: RÉUSSI', r.verdict.reason);
  t.ok(r.confirmedDrivers.length === 1 && r.confirmedDrivers[0] === TESTER,
    'three offers (a real driver\'s first): only the tester\'s was confirmed', r.confirmedDrivers.join());
  t.ok(/COMPLETED/.test(r.verdict.reason) && /812/.test(r.verdict.reason) && /le chauffeur 4/.test(r.verdict.reason),
    'the verdict carries the end state, the price and both ratings', r.verdict.reason);
  const rate = r.w.requests.find(([, p]) => p === '/v2/feedback/rateRide');
  t.ok(rate && rate[2].rating === 5 && rate[2].feedbackDetails === COMMENT, `the passenger's rating says « ${COMMENT} »`);
  t.ok(noDriverRoute(r.paths), 'not one driver route (/ui/...): it never signs in as the driver');
  t.ok(r.w.search.contents.origin.address.area === 'Passager virtuel (essai)', 'the request reads « Passager virtuel (essai) » on his screen');
  t.ok(openBookings(r.w).length === 0, 'nothing left open');

  r = await scenario(1, (w) => { w.offers = [[OTHER, SIM]]; });
  t.ok(!r.verdict.ok && /pas fait d’offre/.test(r.verdict.reason), 'the tester never offers: PAS RÉUSSI, with the reason', r.verdict.reason);
  t.ok(r.confirmedDrivers.length === 0 && r.paths.includes('POST /v2/estimate/e1/cancel'),
    'and nobody else was taken in his place: the request is cancelled');

  r = await scenario(1, (w) => { w.offers = [[TESTER]]; w.script = [w.arrive, w.start, w.end]; });
  t.ok(!r.verdict.ok && /n’a pas noté/.test(r.verdict.reason), 'the ride ends but he never rates the passenger: PAS RÉUSSI (scenario 1 is « jusqu\'aux deux notes »)', r.verdict.reason);

  t.section('the console gives a driver id, not a number (day 8, V6)');
  r = await scenario(2, (w) => { w.offers = [[OTHER, TESTER]]; }, { number: { driverId: TESTER } });
  t.ok(r.verdict.ok && r.confirmedDrivers.join() === TESTER, 'by id: the same run, only his offer taken', r.verdict.reason);
  r = await scenario(1, (w) => { w.offers = [[TESTER]]; }, { number: { driverId: 'not-an-id' } });
  t.ok(!r.verdict.ok && /identifiant de chauffeur invalide/.test(r.verdict.reason) && r.paths.length === 0, 'a malformed id: refused, nothing asked');
  r = await scenario(1, (w) => { w.offers = [[TESTER]]; }, { number: { driverId: 'eeeeeeee-0000-4000-8000-000000000009' } });
  t.ok(!r.verdict.ok && /aucun compte chauffeur avec cet identifiant/.test(r.verdict.reason) && r.paths.length === 0, 'an id no driver has: refused, nothing asked');
  r = await scenario(2, (w) => { w.offers = [[TESTER]]; }, { number: { driverId: TESTER }, salt: '' });
  t.ok(r.verdict.ok, 'by id the number salt is not needed', r.verdict.reason);

  t.section('rule 4: before anything, what an earlier run left open is closed');
  r = await scenario(2, (w) => { w.offers = [[TESTER]]; w.leftovers = [{ id: 'left1', status: 'TRIP_ASSIGNED' }, { id: 'old', status: 'COMPLETED' }]; });
  t.ok(r.paths.indexOf('POST /v2/rideBooking/left1/cancel') !== -1 && r.paths.indexOf('POST /v2/rideBooking/left1/cancel') < r.paths.indexOf('POST /v2/rideSearch'),
    'the open booking left over is cancelled before the search', r.paths.join(' | '));
  t.ok(!r.paths.includes('POST /v2/rideBooking/old/cancel'), 'a finished one is left alone');

  t.section('rule 4: interrupted, it cancels what it opened');
  r = await scenario(1, (w) => { w.offers = [[TESTER]]; }, {
    onSleep: async (w, state) => {
      if (w.bookings.size && !w.stopped) {
        await cleanUp(state);      // what SIGTERM (« stop ») runs
        w.stopped = true;
      }
      return w.tick();
    },
  });
  t.ok(r.w.bookings.size === 1 && openBookings(r.w).length === 0, 'stopped while waiting for the driver: its booking is cancelled', JSON.stringify([...r.w.bookings.values()].map((b) => b.status)));
  t.ok(r.rides[0] && r.rides[0].status === 'CANCELLED', 'and the ride is CANCELLED on the driver side');

  // 2026-11-18: the stop said « rien n'est resté ouvert » whatever the cancel
  // got back. Stopped once the driver has STARTED, the cancel is refused and
  // the stop must say so.
  let left = null;
  r = await scenario(1, (w) => { w.offers = [[TESTER]]; w.script = [w.arriveAndStart]; }, {
    onSleep: async (w, state) => {
      const b = w.current();
      if (b && b.ride.status === 'INPROGRESS' && !w.stopped) {
        left = await cleanUp(state);
        w.stopped = true;
      }
      return w.tick();
    },
  });
  t.ok(left && left.closed === false && /a déjà démarré/.test(left.message) && /doit la terminer/.test(left.message),
    'stopped after the driver started: the stop says the ride must be ended by him, never « rien n’est resté ouvert »', JSON.stringify(left));
  left = null;
  r = await scenario(1, (w) => { w.offers = [[TESTER]]; }, {
    onSleep: async (w, state) => {
      if (w.bookings.size && !w.stopped) { left = await cleanUp(state); w.stopped = true; }
      return w.tick();
    },
  });
  t.ok(left && left.closed === true && /rien n’est resté ouvert/.test(left.message), 'stopped before he arrives: closed, and said so', JSON.stringify(left));
  r = await scenario(1, (w) => { w.offers = [[]]; }, {
    onSleep: async (w, state) => {
      if (w.searches && !w.stopped) { await cleanUp(state); w.stopped = true; }
      return w.tick();
    },
  });
  t.ok(r.paths.includes('POST /v2/estimate/e1/cancel') && r.w.bookings.size === 0, 'stopped while waiting for offers: the request is cancelled, nothing booked');

  t.section('scenarios 2 to 7');
  r = await scenario(2, (w) => { w.offers = [[OTHER, TESTER]]; });
  t.ok(r.verdict.ok && r.rides[0].status === 'CANCELLED' && r.confirmedDrivers.join() === TESTER, '2 · cancelled before arrival: RÉUSSI, CANCELLED on his side', r.verdict.reason);
  t.ok(!r.lines.some((l) => /est arrivé/.test(l)), '   and it did not wait for him to arrive');

  t.section('a cancel is never claimed blindly (live, 2026-11-16: the car started before the cancel)');
  r = await scenario(2, (w) => { w.offers = [[TESTER]]; w.script = [w.arriveAndStart]; });
  t.ok(!r.verdict.ok && /a démarré la course avant que le passager annule/.test(r.verdict.reason) && /pas joué/.test(r.verdict.reason),
    '2 · he arrives and starts during the wait: PAS RÉUSSI, « scénario pas joué »', r.verdict.reason);
  t.ok(!r.lines.some((l) => /Le passager annule avant/.test(l)), '   and it never says the passenger cancelled');
  r = await scenario(2, (w) => { w.offers = [[TESTER]]; w.script = [w.arrive]; });
  t.ok(!r.verdict.ok && /est arrivé avant que le passager annule/.test(r.verdict.reason) && openBookings(r.w).length === 0,
    '2 · he arrives during the wait: PAS RÉUSSI, and the booking is still closed', r.verdict.reason);
  r = await scenario(2, (w) => { w.offers = [[TESTER]]; w.refuseCancel = true; });
  t.ok(!r.verdict.ok && /refusé l’annulation/.test(r.verdict.reason) && /SOMETHING_ELSE/.test(r.verdict.reason),
    '2 · the server refuses the cancel: PAS RÉUSSI, with its answer', r.verdict.reason);
  r = await scenario(3, (w) => { w.offers = [[TESTER]]; w.script = [w.arrive, w.start]; });
  t.ok(!r.verdict.ok && /a démarré la course avant/.test(r.verdict.reason), '3 · he starts during the 20 s: PAS RÉUSSI, « pas joué »', r.verdict.reason);

  r = await scenario(3, (w) => { w.offers = [[TESTER]]; w.script = [w.arrive]; });
  t.ok(r.verdict.ok && r.rides[0].status === 'CANCELLED' && r.lines.some((l) => /est arrivé/.test(l)), '3 · cancelled after arrival: RÉUSSI', r.verdict.reason);

  r = await scenario(4, (w) => { w.offers = [[TESTER]]; w.script = [w.arrive, w.nothing, w.driverCancels]; });
  t.ok(r.verdict.ok && /chauffeur, comme prévu/.test(r.verdict.reason), '4 · the passenger does not come, the driver cancels: RÉUSSI', r.verdict.reason);
  t.ok(!r.paths.some((p) => /rideBooking\/b1\/cancel/.test(p)), '   and the passenger did not cancel it herself');
  r = await scenario(4, (w) => { w.offers = [[TESTER]]; w.script = [w.arrive, w.start]; });
  t.ok(!r.verdict.ok && /démarré/.test(r.verdict.reason), '4 · the driver STARTS with nobody in the car: PAS RÉUSSI', r.verdict.reason);

  r = await scenario(5, (w) => { w.offers = [[TESTER, OTHER, SIM]]; });
  t.ok(r.verdict.ok && r.confirmedDrivers.join() === SIM && openBookings(r.w).length === 0,
    '5 · a simulated car chosen, never the real one; its booking cancelled at once', `${r.verdict.reason} / ${r.confirmedDrivers}`);
  r = await scenario(5, (w) => { w.offers = [[TESTER, OTHER]]; });
  t.ok(r.verdict.ok && r.confirmedDrivers.length === 0 && r.paths.includes('POST /v2/estimate/e1/cancel'),
    '5 · no simulated car: nobody chosen (not the other real driver), the request cancelled after his offer expired', r.verdict.reason);
  r = await scenario(5, (w) => { w.offers = [[OTHER, SIM]]; });
  t.ok(!r.verdict.ok && r.confirmedDrivers.length === 0, '5 · the tester never offers: PAS RÉUSSI, and the simulated car is not taken either', r.verdict.reason);

  r = await scenario(6, (w) => { w.offers = [[TESTER], [OTHER, TESTER], [TESTER]]; });
  t.ok(r.verdict.ok && r.confirmedDrivers.join() === [TESTER, TESTER, TESTER].join() && r.rides.every((x) => x.status === 'CANCELLED'),
    '6 · three requests: three offers of his taken, three cancelled', r.verdict.reason);
  r = await scenario(6, (w) => { w.offers = [[TESTER], [OTHER]]; });
  t.ok(!r.verdict.ok && /pas fait d’offre/.test(r.verdict.reason) && r.confirmedDrivers.join() === TESTER, '6 · he misses the second: PAS RÉUSSI at that one', r.verdict.reason);

  r = await scenario(7, async (w, db) => {
    w.offers = [[TESTER]]; w.script = [w.arrive, w.start, w.end, w.rate];
    // An approved place names who decided (suggested_place_decision_named).
    await db.sql(`INSERT INTO movin.suggested_place (proposed_name, proposed_type, name, type, lat, lon, country, author_side, author_id, status, decided_at, decided_by_email)
                  VALUES ('Café du Port', 'cafe', 'Café du Port', 'cafe', 36.765, 3.060, 'DZ', 'rider', 'x', 'approved', now(), 'bureau@test'),
                         ('Loin', 'cafe', 'Loin', 'cafe', 35.0, 1.0, 'DZ', 'rider', 'x', 'approved', now(), 'bureau@test'),
                         ('En attente', 'cafe', 'En attente', 'cafe', 36.754, 3.059, 'DZ', 'rider', 'x', 'pending', NULL, NULL)`);
  });
  const dest = r.w.search && r.w.search.contents.destination;
  t.ok(r.verdict.ok && dest.address.area === 'Café du Port' && dest.gps.lat === 36.765,
    '7 · to the nearest APPROVED proposed place, under its name (not the pending one nearer)', `${r.verdict.reason} / ${dest && dest.address.area}`);
  r = await scenario(7, (w) => { w.offers = [[TESTER]]; });
  t.ok(!r.verdict.ok && /aucun lieu proposé approuvé/.test(r.verdict.reason) && !r.paths.includes('POST /v2/rideSearch'), '7 · no approved place: refused before any request', r.verdict.reason);

  t.section('the file itself');
  const src = fs.readFileSync(VR, 'utf8');
  t.ok(/process\.stdout\.on\('error'/.test(src), 'a closed terminal (EPIPE) does not kill a run before it cleans up');
  t.ok(!/\/ui\//.test(src.replace(/^\s*(\/\/|\*).*$/gm, '')) && !/DRIVER_URL/.test(src), 'no driver route and no driver API address anywhere in its code');
  const cli = (args, env) => spawnSync(process.execPath, [VR, ...args], {
    env: { PATH: process.env.PATH, VIRTUAL_RIDER_PID_DIR: fs.mkdtempSync('/tmp/vr-'), ...env }, encoding: 'utf8', timeout: 20000 });
  let c = cli(['+213 0555 12 34 56', '1'], { VIRTUAL_RIDER: 'off' });
  t.ok(c.status === 1 && /PAS RÉUSSI : le passager virtuel est éteint/.test(c.stdout), 'from the command line, switched off: exit 1, « éteint »', c.stdout.trim());
  c = cli(['list'], {});
  t.ok(c.status === 0 && c.stdout.split('\n').filter(Boolean).length === 8, '`list` names the eight scenarios');
  c = cli(['stop'], {});
  t.ok(c.status === 0 && /Aucun passager virtuel en cours/.test(c.stdout), '`stop` with nothing running says so');

  routing.close();
  t.finish();
})().catch((e) => { console.error(e); process.exit(1); });
