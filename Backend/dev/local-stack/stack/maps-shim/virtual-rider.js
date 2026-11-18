'use strict';
/**
 * The virtual passenger (day 7, point 5, 2026-11-16; the boss: « We may need
 * to do virtual riders … this way we can test the driver side extensively »).
 *
 * A passenger who does not exist books REAL rides for a REAL driver -- the
 * tester, on his own phone -- so every screen of the driver side can be tried
 * as often as needed. It is the ride test (ops/checks/probe-two-country-rides.py)
 * turned around: there the simulated cars drive and a script is the passenger;
 * here a person drives and this file is the passenger.
 *
 *   docker exec ny-maps-shim node virtual-rider.js <tester's number> <scenario>
 *   docker exec ny-maps-shim node virtual-rider.js stop
 *   docker exec ny-maps-shim node virtual-rider.js list
 *
 * ── What it never does ──────────────────────────────────────────────────────
 *   * Run without `VIRTUAL_RIDER=on` (the server's .env, passed by compose).
 *     It is a test tool on the live server; launch-gate.sh refuses the launch
 *     while it is on (day 8).
 *   * Sign in as the driver. It never calls the driver API at all: signing in
 *     as a driver revokes the session on his phone (the ride test learnt it).
 *     Everything about him is READ from the database.
 *   * Accept a real driver's offer other than the tester's. Any driver near the
 *     tester may see the request; his offer is ignored and expires. Only the
 *     scenario 5 may take another offer, and only a simulated car's (their
 *     numbers are known), which it then cancels at once.
 *   * Book for a driver dispatch would skip: online, a recent position, a
 *     vehicle, and the wallet's own `canWork` (wallet.workState) -- « pas de
 *     recharge, pas de travail » applies to the tester too.
 *   * Leave a booking open. An open one locks that passenger out of every
 *     later booking (ACTIVE_BOOKING_PRESENT). Every booking it opens, it closes
 *     -- on success, on failure, and on `stop` (SIGTERM); and it closes any it
 *     finds left over before starting.
 *
 * ── Who the passenger is ────────────────────────────────────────────────────
 * The two test passengers the ride test already uses, one per country
 * (+222 22778899, +213 0555000199), signed in on the rider API's loopback
 * port with the backend's fixed code -- the auth guard is not on that path.
 * Both are on launch-cleanup-targets.sql: they go before launch.
 *
 * ── How the tester's offer is recognised ────────────────────────────────────
 * Not by name or plate. Each offer the passenger receives is traced in the
 * database: the passenger's quote → its driver_offer → the driver server's
 * driver_quote → its driver_id. Only `driver_id = tester` is accepted.
 */
const crypto = require('crypto');
const fs = require('fs');
const wallet = require('./wallet');

const RIDER_URL = (process.env.RIDER_URL || 'http://localhost:8013').replace(/\/$/, '');
const OSRM_URL = (process.env.OSRM_URL || 'http://localhost:5000').replace(/\/$/, '');
const SALT = process.env.NUMBER_HASH_SALT || '';
const PID_DIR = process.env.VIRTUAL_RIDER_PID_DIR || '/tmp';

/** The backend's fixed sign-in code (useFakeSms), on loopback only. */
const OTP = '7891';
const RIDER_MERCHANT = 'YATRI';
const DZ_MERCHANT = 'algeria0-0000-0000-0000-00000algeria';
const COMMENT = 'Passager virtuel · essai';

const PASSENGERS = {
  DZ: { cc: '+213', number: '0555000199' },
  MR: { cc: '+222', number: '22778899' },
};

/** The simulated fleet (simulate-driver.py): the only offers scenario 5 may take. */
const SIMULATED = new Set([
  '22100001', '22100002', '22100003', '22100004', '22100005', '22100006',
  '0555100001', '0555100002', '0555100003', '0555100004', '0555100005', '0555100006',
]);

/** A booking in one of these still holds the passenger. */
const OPEN_BOOKING = new Set(['NEW', 'CONFIRMED', 'AWAITING_REASSIGNMENT', 'TRIP_ASSIGNED']);

const SCENARIOS = {
  1: 'une course normale, jusqu’aux deux notes',
  2: 'le passager annule avant l’arrivée du chauffeur',
  3: 'le passager annule après l’arrivée du chauffeur',
  4: 'le passager ne vient pas : le chauffeur attend, puis annule',
  5: 'le passager choisit une voiture simulée, ou personne : l’offre du testeur expire',
  6: 'trois demandes à la suite (chacune donnée au testeur, puis annulée)',
  7: 'une course vers un lieu proposé et approuvé',
  8: 'deux demandes en même temps',
};

/** How long each wait lasts, in seconds; the tests shrink them. */
const WAIT = {
  results: 20, offer: 120, assigned: 60, beforeCancel: 30, arrival: 20 * 60,
  afterArrival: 20, driverCancels: 30 * 60, ride: 60 * 60, driverRating: 5 * 60,
  // Scenario 5: how long other offers are awaited once his is in, and how
  // long an offer lives on his screen (driver_quote: 60 s, measured).
  others: 20, offerLife: 65,
  poll: 2, whole: 100 * 60,
};

/* ── Small things ─────────────────────────────────────────────────────────── */

/** Who is the tester, from what was typed: the country and the forms his number may be stored in. */
function parseNumber(raw) {
  const s = String(raw || '').replace(/[\s.-]/g, '');
  let m = s.match(/^\+213(\d{9,10})$/);
  if (m) return dz(m[1]);
  m = s.match(/^\+222(\d{8})$/);
  if (m) return { country: 'MR', cc: '+222', candidates: [m[1]] };
  if (/^0[5-7]\d{8}$/.test(s) || /^[5-7]\d{8}$/.test(s)) return dz(s);
  if (/^[2-4]\d{7}$/.test(s)) return { country: 'MR', cc: '+222', candidates: [s] };
  return null;
}
function dz(n) {
  const bare = n.replace(/^0/, '');
  if (!/^[5-7]\d{8}$/.test(bare)) return null;
  // Stored with the trunk zero (sign-in sends ten digits); the bare form too, to be sure.
  return { country: 'DZ', cc: '+213', candidates: [`0${bare}`, bare] };
}

const RIDE_ID = /^[0-9a-f-]{36}$/i;
const countryOfMerchant = (merchantId) => (String(merchantId).trim() === DZ_MERCHANT ? 'DZ' : 'MR');

/** A driver's country, from his merchant: what the console's route needs before it starts a run. */
async function countryOfDriver(pool, driverId) {
  if (!pool || !RIDE_ID.test(String(driverId))) return null;
  const { rows } = await pool.query(
    `SELECT merchant_id FROM atlas_driver_offer_bpp.person WHERE id = $1 AND role = 'DRIVER'`, [driverId]);
  return rows[0] ? countryOfMerchant(rows[0].merchant_id) : null;
}

const hashHex = (salt, number) => crypto.createHash('sha256').update(salt + number).digest('hex');

/** A point `metres` away from (lat, lon) towards `bearing` degrees. */
function offset({ lat, lon }, metres, bearing) {
  const r = (bearing * Math.PI) / 180;
  const dLat = (metres * Math.cos(r)) / 111320;
  const dLon = (metres * Math.sin(r)) / (111320 * Math.cos((lat * Math.PI) / 180));
  return { lat: lat + dLat, lon: lon + dLon };
}

function metresBetween(a, b) {
  const k = Math.PI / 180;
  const x = (b.lon - a.lon) * k * Math.cos(((a.lat + b.lat) / 2) * k);
  const y = (b.lat - a.lat) * k;
  return Math.round(Math.sqrt(x * x + y * y) * 6371000);
}

/* ── The run ──────────────────────────────────────────────────────────────── */

/**
 * One scenario, start to verdict.
 *
 * deps: { pool, say, sleep, wait?, riderUrl?, osrmUrl?, salt?, enabled? }
 * Returns { ok, reason, lines } -- `ok` is the verdict « réussi ».
 */
async function run(deps, target, scenarioRaw) {
  const say = deps.say || ((s) => console.log(s));
  const sleep = deps.sleep || ((ms) => new Promise((r) => setTimeout(r, ms)));
  const W = { ...WAIT, ...(deps.wait || {}) };
  const riderUrl = deps.riderUrl || RIDER_URL;
  const osrmUrl = deps.osrmUrl || OSRM_URL;
  const salt = deps.salt === undefined ? SALT : deps.salt;
  const enabled = deps.enabled === undefined ? process.env.VIRTUAL_RIDER === 'on' : deps.enabled;
  const pool = deps.pool;
  const state = deps.state || {};

  const fail = (reason) => ({ ok: false, reason });

  /* ── 0. may it run at all ── */
  if (!enabled) return fail('le passager virtuel est éteint (VIRTUAL_RIDER=on manque dans le .env du serveur)');
  const scenario = Number(scenarioRaw);
  if (!SCENARIOS[scenario]) return fail(`scénario inconnu « ${scenarioRaw} » : 1 à 8`);
  if (scenario === 8) return fail('scénario 8 pas encore : il faut un 2e passager d’essai par pays (décision V3 du plan)');
  // Who the tester is: a phone number (the command line), or the driver's id
  // (the console, which lists the drivers online -- day 8, decision V6).
  const byId = Boolean(target && typeof target === 'object' && typeof target.driverId === 'string');
  if (byId && !RIDE_ID.test(target.driverId)) return fail('identifiant de chauffeur invalide');
  const who = byId ? null : parseNumber(target);
  if (!byId && !who) return fail(`numéro non reconnu « ${target} » : +213 0555 12 34 56 ou +222 22 12 34 56`);
  if (!pool) return fail('pas de base de données (PG_URL)');
  if (!byId && !salt) return fail('NUMBER_HASH_SALT manque (fichier number-change.env) : impossible de trouver le chauffeur par son numéro');

  say(`Scénario ${scenario} : ${SCENARIOS[scenario]}.`);

  /* ── 1. the tester, and whether dispatch would reach him ── */
  const found = byId
    ? await pool.query(
      `SELECT id, first_name, merchant_id FROM atlas_driver_offer_bpp.person
        WHERE role = 'DRIVER' AND id = $1`, [target.driverId])
    : await pool.query(
      `SELECT id, first_name, merchant_id FROM atlas_driver_offer_bpp.person
        WHERE role = 'DRIVER' AND mobile_country_code = $1
          AND encode(mobile_number_hash, 'hex') = ANY($2::text[])
        LIMIT 2`,
      [who.cc, who.candidates.map((n) => hashHex(salt, n))]);
  if (found.rows.length !== 1) return fail(byId ? 'aucun compte chauffeur avec cet identifiant' : 'aucun compte chauffeur à ce numéro');
  const tester = found.rows[0];
  tester.id = String(tester.id).trim();
  const testerCountry = countryOfMerchant(tester.merchant_id);
  if (!byId && testerCountry !== who.country) return fail('le numéro et le pays du compte chauffeur ne vont pas ensemble');

  const r = (await pool.query(
    `SELECT di.enabled, di.blocked, di.active, di.on_ride,
            l.lat, l.lon, EXTRACT(EPOCH FROM (now() - l.updated_at))::int AS age,
            v.variant
       FROM atlas_driver_offer_bpp.person p
       LEFT JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
       LEFT JOIN atlas_driver_offer_bpp.driver_location l ON l.driver_id = p.id
       LEFT JOIN atlas_driver_offer_bpp.vehicle v ON v.driver_id = p.id
      WHERE p.id = $1`, [tester.id])).rows[0] || {};
  const name = (tester.first_name || 'le testeur').trim();
  if (!r.enabled) return fail(`${name} : compte pas encore validé par le bureau`);
  if (r.blocked) return fail(`${name} : compte bloqué`);
  if (!r.variant) return fail(`${name} : aucun véhicule enregistré`);
  if (!r.active) return fail(`${name} n’est pas en ligne : il doit passer « En service » dans l’application`);
  if (r.on_ride) return fail(`${name} est déjà en course : qu’il la termine d’abord`);
  if (r.lat === null || r.lat === undefined || r.age === null || r.age > 600) {
    return fail(`${name} : position trop ancienne (${r.age === null || r.age === undefined ? 'aucune' : `${Math.round(r.age / 60)} min`}) -- l’application doit être ouverte, en service`);
  }
  const work = await wallet.workState(pool, tester.id);
  if (!work.canWork) {
    return fail(`${name} ne peut pas travailler : solde ${work.w.balance} ${work.cfg.currency}, pas de journée en cours (« pas de recharge, pas de travail »)`);
  }
  const at = { lat: Number(r.lat), lon: Number(r.lon) };
  say(`Testeur : ${name} (${testerCountry}, ${r.variant}), en ligne, position d’il y a ${r.age} s, peut travailler.`);

  /* ── 2. where the passenger waits, and where she goes ── */
  const snap = async (p) => {
    try {
      const res = await fetch(`${osrmUrl}/nearest/v1/driving/${p.lon},${p.lat}`, { signal: AbortSignal.timeout(5000) });
      const j = await res.json();
      const w = j && j.waypoints && j.waypoints[0];
      if (w && Array.isArray(w.location)) return { point: { lat: w.location[1], lon: w.location[0] }, off: w.distance };
    } catch { /* OSRM down: the raw point */ }
    return { point: p, off: 0 };
  };
  const onRoad = async (from, metres) => {
    for (const bearing of [0, 90, 180, 270, 45, 135, 225, 315]) {
      const s = await snap(offset(from, metres, bearing));
      if (s.off < 150) return s.point;
    }
    return (await snap(offset(from, metres, 0))).point;
  };
  const pickup = await onRoad(at, 300);
  let drop;
  let dropName = 'Arrivée d’essai';
  if (scenario === 7) {
    const place = (await pool.query(
      `SELECT name, lat, lon FROM movin.suggested_place
        WHERE status = 'approved' AND country = $1
        ORDER BY (lat - $2) * (lat - $2) + (lon - $3) * (lon - $3) LIMIT 1`,
      [testerCountry, pickup.lat, pickup.lon])).rows[0];
    if (!place) return fail('aucun lieu proposé approuvé dans ce pays');
    drop = { lat: Number(place.lat), lon: Number(place.lon) };
    if (metresBetween(pickup, drop) > 15000) return fail(`le lieu approuvé le plus proche (« ${place.name} ») est à plus de 15 km`);
    dropName = place.name;
  } else {
    drop = await onRoad(pickup, 2000);
  }
  say(`Départ à ${metresBetween(at, pickup)} m de lui ; arrivée « ${dropName} » à ${metresBetween(pickup, drop)} m du départ.`);

  /* ── 3. the passenger ── */
  const call = async (method, path, body, token) => {
    try {
      const res = await fetch(`${riderUrl}${path}`, {
        method,
        headers: { 'content-type': 'application/json', ...(token ? { token } : {}) },
        body: body === undefined ? undefined : JSON.stringify(body),
        signal: AbortSignal.timeout(25000),
      });
      const text = await res.text();
      let json = null;
      try { json = JSON.parse(text); } catch { /* not JSON */ }
      return { status: res.status, json, text };
    } catch (e) {
      return { status: 0, json: null, text: e.message };
    }
  };

  const passenger = PASSENGERS[testerCountry];
  const a = await call('POST', '/v2/auth', { mobileCountryCode: passenger.cc, mobileNumber: passenger.number, merchantId: RIDER_MERCHANT });
  if (!a.json || !a.json.authId) return fail(`connexion du passager d’essai refusée (${a.status})`);
  const v = await call('POST', `/v2/auth/${a.json.authId}/verify`, { otp: OTP, deviceToken: 'virtual-rider' });
  const token = v.json && v.json.token;
  if (!token) return fail(`connexion du passager d’essai refusée (${v.status})`);
  state.token = token;
  state.call = call;

  await closeLeftovers(call, token, say);

  const book = (b) => call('POST', `/v2/rideBooking/${b}`, undefined, token);
  const cancelBooking = (b) => call('POST', `/v2/rideBooking/${b}/cancel`,
    { reasonCode: 'CHANGE_OF_PLANS', reasonStage: 'OnAssign', additionalInfo: COMMENT }, token);
  /** The passenger cancels, and the server's answer is READ: null when done, else why not. */
  const passengerCancels = async (b) => {
    const res = await cancelBooking(b);
    if (res.status >= 200 && res.status < 300) return null;
    const code = (res.json && (res.json.errorCode || res.json.errorMessage)) || String(res.text || '').slice(0, 80);
    return `le serveur a refusé l’annulation du passager (${res.status} ${code})`;
  };
  /** Where the ride is now, as the passenger's server says. */
  const where = async (b) => {
    const res = await book(b);
    const ride = (res.json && res.json.rideList && res.json.rideList[0]) || {};
    return { arrived: Boolean(ride.driverArrivalTime), started: ride.status === 'INPROGRESS' || ride.status === 'COMPLETED', status: ride.status };
  };
  const rideStatus = async (bppRideId) =>
    ((await pool.query(`SELECT status FROM atlas_driver_offer_bpp.ride WHERE id = $1`, [bppRideId])).rows[0] || {}).status;
  const driverOf = async (quoteId) =>
    ((await pool.query(
      `SELECT dq.driver_id, p.unencrypted_mobile_number AS num
         FROM atlas_app.quote q
         JOIN atlas_app.driver_offer o ON o.id = q.driver_offer_id
         JOIN atlas_driver_offer_bpp.driver_quote dq ON dq.id = o.bpp_quote_id
         JOIN atlas_driver_offer_bpp.person p ON p.id = dq.driver_id
        WHERE q.id = $1`, [quoteId])).rows[0]) || null;
  const polls = (seconds) => Math.max(1, Math.ceil(seconds / W.poll));

  /** One request: search, select, the offers. Returns { estimateId, offers } or a failure. */
  const request = async (label) => {
    const s = await call('POST', '/v2/rideSearch', {
      fareProductType: 'ONE_WAY',
      contents: {
        origin: { address: { area: 'Passager virtuel (essai)', city: '' }, gps: pickup },
        destination: { address: { area: dropName, city: '' }, gps: drop },
      },
    }, token);
    const searchId = s.json && s.json.searchId;
    if (!searchId) return { error: `recherche refusée (${s.status} ${String(s.text).slice(0, 120)})` };
    let est = null;
    for (let i = 0; i < polls(W.results) && !est; i += 1) {
      const res = await call('GET', `/v2/rideSearch/${searchId}/results`, undefined, token);
      const list = (res.json && res.json.estimates) || [];
      est = list.find((e) => e.vehicleVariant === r.variant) || null;
      if (!est) await sleep(W.poll * 1000);
    }
    if (!est) return { error: `aucune estimation ${r.variant} : le serveur ne propose pas sa catégorie ici (est-il vraiment disponible ?)` };
    state.estimateId = est.id;
    await call('POST', `/v2/estimate/${est.id}/select`, undefined, token);
    say(`${label}Demande envoyée (${est.estimatedTotalFare ?? '?'}). → Le chauffeur doit voir une demande de course « Passager virtuel (essai) » et pouvoir faire une offre.`);
    return { estimateId: est.id };
  };

  /** The offers on an estimate until `want` says stop; every one traced to its driver. */
  const offers = async (estimateId, want, seen = new Map(), seconds = W.offer) => {
    for (let i = 0; i < polls(seconds); i += 1) {
      const q = await call('GET', `/v2/estimate/${estimateId}/quotes`, undefined, token);
      for (const quote of (q.json && q.json.selectedQuotes) || []) {
        if (!quote.id || seen.has(quote.id)) continue;
        const d = await driverOf(quote.id);
        const kind = !d ? 'unknown' : String(d.driver_id).trim() === tester.id ? 'tester' : SIMULATED.has(String(d.num || '').trim()) ? 'simulated' : 'other';
        seen.set(quote.id, kind);
        say(`  offre reçue : ${{ tester: `celle de ${name}`, simulated: 'une voiture simulée', other: 'un autre chauffeur réel (ignorée)', unknown: 'introuvable dans la base (ignorée)' }[kind]}`);
      }
      const pick = want(seen);
      if (pick) return pick;
      await sleep(W.poll * 1000);
    }
    return null;
  };
  const testerOffer = (seen) => {
    for (const [id, kind] of seen) if (kind === 'tester') return id;
    return null;
  };

  const confirm = async (quoteId) => {
    const c = await call('POST', `/v2/rideSearch/quotes/${quoteId}/confirm`, undefined, token);
    const bookingId = c.json && c.json.bookingId;
    if (!bookingId) return { error: `confirmation refusée (${c.status} ${String(c.text).slice(0, 120)})` };
    state.bookingId = bookingId;
    state.estimateId = null;
    let ride = null;
    for (let i = 0; i < polls(W.assigned) && !ride; i += 1) {
      const b = await book(bookingId);
      ride = (b.json && b.json.rideList && b.json.rideList[0]) || null;
      if (!ride) await sleep(W.poll * 1000);
    }
    if (!ride) return { error: 'la course n’a jamais été attribuée', bookingId };
    return { bookingId, ride };
  };

  const noOffer = async (estimateId) => {
    await call('POST', `/v2/estimate/${estimateId}/cancel`, undefined, token);
    state.estimateId = null;
    const reached = await pool.query(
      `SELECT 1 FROM atlas_driver_offer_bpp.search_request_for_driver
        WHERE driver_id = $1 AND created_at > now() - interval '5 minutes' LIMIT 1`, [tester.id]).catch(() => null);
    const where = !reached ? '' : reached.rows.length ? ' La demande lui est bien parvenue.' : ' La demande ne lui est jamais parvenue (dispatch ne l’a pas choisi).';
    return fail(`${name} n’a pas fait d’offre en ${W.offer} s.${where}`);
  };

  const waitArrival = async (bookingId) => {
    for (let i = 0; i < polls(W.arrival); i += 1) {
      const b = await book(bookingId);
      const ride = (b.json && b.json.rideList && b.json.rideList[0]) || {};
      if (ride.driverArrivalTime) return { arrived: true, ride };
      if (ride.status === 'CANCELLED' || (b.json && b.json.status === 'CANCELLED')) return { arrived: false, cancelled: true };
      await sleep(W.poll * 1000);
    }
    return { arrived: false };
  };

  const done = (ok, reason) => {
    state.bookingId = null;
    state.estimateId = null;
    return { ok, reason };
  };

  /* ── 4. the scenarios ── */
  const rounds = scenario === 6 ? 3 : 1;
  for (let round = 1; round <= rounds; round += 1) {
    const label = rounds > 1 ? `[${round}/${rounds}] ` : '';
    const req = await request(label);
    if (req.error) return fail(req.error);

    if (scenario === 5) {
      // His offer first: the scenario is about what HE sees when he is not chosen.
      const seen = new Map();
      if (!(await offers(req.estimateId, testerOffer, seen))) return noOffer(req.estimateId);
      // Then a simulated car's, if one is near and offers soon. Never a real driver's.
      const simulated = await offers(req.estimateId, (s) => {
        for (const [id, kind] of s) if (kind === 'simulated') return id;
        return null;
      }, seen, W.others);
      if (simulated) {
        const c = await confirm(simulated);
        if (c.error) {
          if (c.bookingId) await cancelBooking(c.bookingId);
          return fail(c.error);
        }
        say(`Le passager a choisi une voiture simulée. → L’offre de ${name} doit disparaître : la course est partie à un autre.`);
        await cancelBooking(c.bookingId);
        return done(true, `${name} a fait une offre ; le passager a choisi une voiture simulée, réservation annulée aussitôt`);
      }
      say(`Le passager ne choisit personne. → L’offre de ${name} doit expirer sur son écran (environ 1 min).`);
      await sleep(W.offerLife * 1000);
      await call('POST', `/v2/estimate/${req.estimateId}/cancel`, undefined, token);
      state.estimateId = null;
      return done(true, `${name} a fait une offre ; personne n’a été choisi, l’offre a expiré, la demande est annulée`);
    }

    const quoteId = await offers(req.estimateId, testerOffer);
    if (!quoteId) return noOffer(req.estimateId);
    const c = await confirm(quoteId);
    if (c.error) {
      if (c.bookingId) await cancelBooking(c.bookingId);
      return fail(c.error);
    }
    const bpp = c.ride.bppRideId;
    say(`Offre de ${name} acceptée, course attribuée. → Il doit voir la course : aller chercher le passager.`);

    if (scenario === 2 || scenario === 6) {
      // He is on his way; the passenger cancels before he arrives. If he
      // arrives (or starts) first, this scenario was not played: say so.
      let early = null;
      for (let i = 0; i < polls(W.beforeCancel) && !early; i += 1) {
        await sleep(W.poll * 1000);
        const now = await where(c.bookingId);
        if (now.started) early = 'started';
        else if (now.arrived) early = 'arrived';
      }
      if (early === 'started') {
        state.bookingId = null;
        return fail(`${label}${name} a démarré la course avant que le passager annule (il était trop près : ${metresBetween(at, pickup)} m) -- scénario pas joué ; la course doit être terminée par lui`);
      }
      if (early === 'arrived') {
        const refused = await passengerCancels(c.bookingId);
        state.bookingId = null;
        return fail(`${label}${name} est arrivé avant que le passager annule (il était trop près) -- scénario pas joué${refused ? ` ; ${refused}` : ' ; réservation annulée'}`);
      }
      const refused = await passengerCancels(c.bookingId);
      if (refused) { state.bookingId = null; return fail(`${label}${refused}`); }
      say(`${label}Le passager annule avant l’arrivée. → Le chauffeur doit voir que la course est annulée par le passager.`);
      await sleep(W.poll * 1000);
      const st = await rideStatus(bpp);
      state.bookingId = null;
      if (st !== 'CANCELLED') return fail(`${label}annulée par le passager, mais la course est « ${st} » côté chauffeur`);
      if (round === rounds) return done(true, scenario === 6 ? 'trois demandes : trois offres, trois courses attribuées, trois annulations' : 'annulée avant l’arrivée ; la course est CANCELLED côté chauffeur');
      continue;
    }

    say('→ Le chauffeur va au départ et appuie sur « Je suis arrivé ».');
    const arr = await waitArrival(c.bookingId);
    if (!arr.arrived) {
      if (arr.cancelled) return done(false, 'la course a été annulée avant l’arrivée');
      await cancelBooking(c.bookingId);
      return done(false, `${name} n’est pas arrivé en ${Math.round(W.arrival / 60)} min (réservation annulée)`);
    }
    say('Le chauffeur est arrivé.');

    if (scenario === 3) {
      await sleep(W.afterArrival * 1000);
      if ((await where(c.bookingId)).started) {
        state.bookingId = null;
        return fail(`${name} a démarré la course avant que le passager annule -- scénario pas joué ; la course doit être terminée par lui`);
      }
      const refused = await passengerCancels(c.bookingId);
      if (refused) { state.bookingId = null; return fail(refused); }
      say('Le passager annule après l’arrivée. → Le chauffeur doit voir que la course est annulée par le passager.');
      await sleep(W.poll * 1000);
      const st = await rideStatus(bpp);
      return st === 'CANCELLED' ? done(true, 'arrivé, puis annulée par le passager ; CANCELLED côté chauffeur') : done(false, `annulée, mais « ${st} » côté chauffeur`);
    }

    if (scenario === 4) {
      say('Le passager ne vient pas. → Le chauffeur attend (le compteur d’attente), puis annule lui-même.');
      for (let i = 0; i < polls(W.driverCancels); i += 1) {
        const st = await rideStatus(bpp);
        if (st === 'CANCELLED') {
          const by = await pool.query(
            `SELECT source FROM atlas_driver_offer_bpp.booking_cancellation_reason WHERE ride_id = $1 LIMIT 1`, [bpp]).catch(() => null);
          const source = by && by.rows[0] ? by.rows[0].source : 'inconnu';
          return done(source === 'ByDriver' || source === 'inconnu', `annulée par ${source === 'ByDriver' ? 'le chauffeur, comme prévu' : `« ${source} »`}`);
        }
        if (st === 'INPROGRESS') {
          return done(false, 'le chauffeur a démarré la course alors que le passager n’est pas venu');
        }
        await sleep(W.poll * 1000);
      }
      await cancelBooking(c.bookingId);
      return done(false, `${name} n’a pas annulé en ${Math.round(W.driverCancels / 60)} min (réservation annulée par le passager)`);
    }

    // 1 and 7: the whole ride, then both ratings.
    say('→ Le chauffeur glisse « Glisser pour démarrer » (aucun code), roule, puis « Glisser pour terminer ».');
    let final = null;
    for (let i = 0; i < polls(W.ride); i += 1) {
      const st = await rideStatus(bpp);
      if (st === 'COMPLETED' || st === 'CANCELLED') { final = st; break; }
      await sleep(W.poll * 1000);
    }
    if (final !== 'COMPLETED') {
      if (!final) await cancelBooking(c.bookingId);
      return done(false, final ? `la course a fini « ${final} »` : `pas terminée en ${Math.round(W.ride / 60)} min (réservation annulée)`);
    }
    state.bookingId = null;
    const b = await book(c.bookingId);
    const ride = (b.json && b.json.rideList && b.json.rideList[0]) || {};
    await call('POST', '/v2/feedback/rateRide', { rideId: ride.id || c.ride.id, rating: 5, feedbackDetails: COMMENT }, token);
    say(`Course terminée, prix ${ride.computedPrice ?? '?'}. Le passager a noté 5 étoiles (« ${COMMENT} »). → Le chauffeur note le passager à son tour.`);
    let stars = null;
    for (let i = 0; i < polls(W.driverRating) && stars === null; i += 1) {
      const n = await pool.query(`SELECT stars FROM movin.driver_rider_rating WHERE ride_id = $1`, [bpp]).catch(() => null);
      if (n && n.rows[0]) stars = n.rows[0].stars;
      else await sleep(W.poll * 1000);
    }
    if (stars === null) return done(false, `course COMPLETED (${ride.computedPrice ?? '?'}), mais ${name} n’a pas noté le passager en ${Math.round(W.driverRating / 60)} min`);
    return done(true, `course COMPLETED, prix ${ride.computedPrice ?? '?'} ; le passager a donné 5 étoiles, le chauffeur ${stars}`);
  }
  return fail('rien à faire');
}

/** Bookings this passenger left open (a stop, a crash): closed before anything else. */
async function closeLeftovers(call, token, say) {
  const l = await call('GET', '/v2/rideBooking/list?limit=5', undefined, token);
  for (const b of (l.json && l.json.list) || []) {
    if (b && b.id && OPEN_BOOKING.has(b.status)) {
      await call('POST', `/v2/rideBooking/${b.id}/cancel`,
        { reasonCode: 'CHANGE_OF_PLANS', reasonStage: 'OnAssign', additionalInfo: COMMENT }, token);
      say(`Réservation restée ouverte d’un passage précédent : annulée (${b.id}).`);
    }
  }
}

/**
 * What an interrupted run leaves: closed -- and the server's answer READ, so
 * the stop says the truth (2026-11-18: « rien n'est resté ouvert » used to be
 * printed whatever the cancel got back). A ride the driver has already
 * started cannot be cancelled by the passenger: he must end it, and that is
 * what is said. Returns { closed, message }.
 */
async function cleanUp(state) {
  if (!state || !state.call || !state.token) return { closed: true, message: 'rien n’était ouvert.' };
  let closed = true;
  let message = 'rien n’est resté ouvert.';
  if (state.bookingId) {
    const id = state.bookingId;
    const res = await state.call('POST', `/v2/rideBooking/${id}/cancel`,
      { reasonCode: 'CHANGE_OF_PLANS', reasonStage: 'OnAssign', additionalInfo: COMMENT }, state.token);
    if (!(res && res.status >= 200 && res.status < 300)) {
      // Refused: why? Read where the booking is.
      const b = await state.call('POST', `/v2/rideBooking/${id}`, undefined, state.token);
      const ride = (b && b.json && b.json.rideList && b.json.rideList[0]) || {};
      const status = ride.status || (b && b.json && b.json.status) || 'inconnu';
      if (status === 'INPROGRESS') {
        closed = false;
        message = 'la course a déjà démarré : le passager ne peut plus l’annuler, le chauffeur doit la terminer.';
      } else if (status === 'COMPLETED' || status === 'CANCELLED') {
        message = `rien n’est resté ouvert (la course était déjà ${status === 'COMPLETED' ? 'terminée' : 'annulée'}).`;
      } else {
        closed = false;
        message = `le serveur a refusé l’annulation (${res ? res.status : 0}) et la course est « ${status} » : à vérifier dans la console.`;
      }
    }
  }
  if (state.estimateId) await state.call('POST', `/v2/estimate/${state.estimateId}/cancel`, undefined, state.token);
  state.bookingId = null;
  state.estimateId = null;
  return { closed, message };
}

/* ── From the command line, inside the maps-shim container ────────────────── */

const pidFile = (country) => `${PID_DIR}/virtual-rider-${country}.pid`;
const alive = (pid) => { try { process.kill(pid, 0); return true; } catch { return false; } };

async function main(argv) {
  // `--driver <id> <scenario>`: what maps-shim's route runs for the console.
  const byDriver = argv[0] === '--driver';
  const [first, second] = byDriver ? [{ driverId: String(argv[1] || '') }, argv[2]] : argv;
  if (first === 'list') {
    for (const [n, s] of Object.entries(SCENARIOS)) console.log(`${n}  ${s}`);
    return 0;
  }
  if (first === 'stop') {
    let stopped = 0;
    for (const c of ['DZ', 'MR']) {
      try {
        const pid = Number(fs.readFileSync(pidFile(c), 'utf8'));
        if (pid && alive(pid)) { process.kill(pid, 'SIGTERM'); stopped += 1; console.log(`${c} : arrêt demandé (il ferme sa réservation).`); }
      } catch { /* none */ }
    }
    if (!stopped) console.log('Aucun passager virtuel en cours.');
    return 0;
  }

  let pool = null;
  if (process.env.PG_URL) {
    const { Pool } = require('pg');
    pool = new Pool({ connectionString: process.env.PG_URL, max: 2 });
  }
  const who = byDriver ? null : parseNumber(first);
  const country = byDriver ? await countryOfDriver(pool, first.driverId).catch(() => null) : (who ? who.country : null);
  if (country) {
    try {
      const pid = Number(fs.readFileSync(pidFile(country), 'utf8'));
      if (pid && pid !== process.pid && alive(pid)) {
        console.log(`PAS RÉUSSI : un passager virtuel tourne déjà pour ce pays (arrêt : node virtual-rider.js stop).`);
        return 1;
      }
    } catch { /* none running */ }
    fs.writeFileSync(pidFile(country), String(process.pid));
  }
  // The terminal can go away (an ssh closed, a laptop asleep): writing to it
  // then fails with EPIPE, which would kill the run mid-ride and skip the
  // clean-up. It carries on, silently, and closes what it opened.
  process.stdout.on('error', () => {});
  process.stderr.on('error', () => {});
  const state = {};
  const t0 = Date.now();
  const say = (s) => console.log(`[${String(Math.round((Date.now() - t0) / 1000)).padStart(4)} s] ${s}`);
  let stopping = false;
  const stop = async () => {
    if (stopping) return;
    stopping = true;
    say('Arrêt demandé : le passager ferme ce qu’il a ouvert.');
    const left = await cleanUp(state).catch((e) => ({ closed: false, message: `erreur à la fermeture : ${e.message}` }));
    if (country) try { fs.unlinkSync(pidFile(country)); } catch { /* gone */ }
    console.log(`ARRÊTÉ : ${left.message}`);
    process.exit(130);
  };
  process.on('SIGTERM', stop);
  process.on('SIGINT', stop);
  const guard = setTimeout(() => { say(`Plus de ${WAIT.whole / 60} min : arrêt.`); stop(); }, WAIT.whole * 1000);

  let verdict;
  try {
    verdict = await run({ pool, say, state }, first, second);
  } catch (e) {
    await cleanUp(state).catch(() => {});
    verdict = { ok: false, reason: `erreur : ${e.message}` };
  }
  clearTimeout(guard);
  if (country) try { fs.unlinkSync(pidFile(country)); } catch { /* gone */ }
  if (pool) await pool.end().catch(() => {});
  console.log(`${verdict.ok ? 'RÉUSSI' : 'PAS RÉUSSI'} : ${verdict.reason}`);
  return verdict.ok ? 0 : 1;
}

if (require.main === module) {
  main(process.argv.slice(2)).then((code) => process.exit(code));
}

module.exports = {
  run, cleanUp, parseNumber, countryOfDriver, pidFile, alive,
  SCENARIOS, SIMULATED, PASSENGERS, COMMENT, WAIT,
};
