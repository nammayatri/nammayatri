'use strict';
// maps-shim/suggested-places.js -- /places/suggest, /places/mine, /places/nearby --
// against a real Postgres (PGlite) holding our own stack/db files, and the
// fake backends for « whose token is this » (day 2, 2026-11-09).
//
//   cd Backend/dev/local-stack/tests && npm ci && node suggested-places-routes.test.js
//
//   1. the routes and who is calling   (the token, never the body)
//   2. what a proposal must look like  (name, type, pin, country)
//   3. what is stored                  (and what the server decided)
//   4. duplicates                      (proposals and the map, 100 m, names folded)
//   5. the limits                      (5 per 24 h, 20 pending, two taps at once)
//   6. « Mes lieux proposés »
//   7. « search by type »              (5 km, 20, nearest; 15 km when 5 finds nothing)
//   8. the module agrees with the SQL  (types, name length, the box, the three-argument mapping)
//   9. the photos                      (day 4: the author, while it waits, an image by its bytes, 512 KB)
//
// The four questions only PostGIS and the place index can answer (country,
// locality, OSM places near a pin, OSM places of a type) are a scripted fake
// here -- `geo` below, which records every call. Their real SQL is
// investigations/probe-suggested-places.sql, run on the live index before a
// release. Everything else runs as written.
const path = require('path');
const { Readable } = require('stream');
const { open } = require('./lib/db');
const fakes = require('./lib/fakes');

const SHIM = path.join(__dirname, '..', 'stack', 'maps-shim');

// Nouakchott, Tevragh Zeina; Algiers centre.
const NKC = { lat: 18.0866, lon: -15.975 };
const ALG = { lat: 36.7538, lon: 3.0588 };
// Metres to degrees of latitude, for « 80 m north of ».
const north = (p, metres) => ({ lat: p.lat + metres / 111195, lon: p.lon });

(async () => {
  const t = fakes.checker();
  const db = await open();
  const who = await fakes.backends();
  const routes = require(path.join(SHIM, 'suggested-places.js'));

  /* The live database's four answers, scripted. */
  const geo = {
    calls: [],
    osm: [], // what osmNear returns
    ofType: [], // what osmOfType returns, before its radius filter
    failCountry: false,
    failLocality: false,
    async countryAt(lat, lon) {
      this.calls.push(['countryAt', lat, lon]);
      if (this.failCountry) throw new Error('index down (test)');
      if (lat > 30 && lon < -5) return null; // Morocco: outside both service areas
      return lon < 0 ? 'MR' : 'DZ';
    },
    async localityAt(lat, lon) {
      this.calls.push(['localityAt', lat, lon]);
      if (this.failLocality) throw new Error('locality down (test)');
      if (lat > 20 && lat < 30) return null; // the desert: nothing named near
      return lon < 0 ? 'Tevragh Zeina' : 'Alger Centre';
    },
    async osmNear(lat, lon, metres) {
      this.calls.push(['osmNear', lat, lon, metres]);
      return this.osm;
    },
    async osmOfType(type, lat, lon, metres, limit) {
      this.calls.push(['osmOfType', type, metres, limit]);
      return this.ofType.filter((p) => p.type === type && p.distance_m <= metres).slice(0, limit);
    },
    async localitiesInArabic(names) {
      this.calls.push(['localitiesInArabic', names]);
      const known = { 'Tevragh Zeina': 'تفرغ زينة', Ksar: 'لكصر' };
      return Object.fromEntries(names.filter((n) => known[n]).map((n) => [n, known[n]]));
    },
  };
  const opts = { pool: db.pool, riderUrl: who.url, driverUrl: who.url, geo };

  /** One request through the router, as server.js hands it over. */
  const call = async (method, pathAndQuery, { token, body, raw, pool, headers } = {}) => {
    const req = Readable.from(raw !== undefined ? [Buffer.from(raw)] : body !== undefined ? [Buffer.from(JSON.stringify(body))] : []);
    req.method = method;
    req.headers = { ...(token ? { token } : {}), ...(headers || {}) };
    const res = fakes.res();
    const url = new URL(pathAndQuery, 'http://localhost');
    await routes.handle(req, res, url, pool === undefined ? opts : { ...opts, pool });
    return res;
  };
  const suggest = (token, body) => call('POST', '/places/suggest', { token, body });
  const rows = (where = 'true', params = []) => db.sql(
    `SELECT id, place_id, proposed_name, proposed_type, name, type, status, country, locality,
            author_side, author_id, lat, lon FROM movin.suggested_place WHERE ${where} ORDER BY id`, params);
  const count = async () => (await db.sql('SELECT count(*)::int AS n FROM movin.suggested_place'))[0].n;
  const place = (name, type = 'pharmacy', at = NKC) => ({ name, type, lat: at.lat, lon: at.lon });

  who.riders.set('tok-r1', 'rider-1');
  who.riders.set('tok-r2', 'rider-2');
  who.drivers.set('tok-d1', 'driver-1');

  /* ── 1 ─────────────────────────────────────────────────────────────── */
  t.section('1. the routes, and who is calling');
  t.ok((await call('GET', '/places/everything', { token: 'tok-r1' })).status === 404, 'an unknown /places/ route: 404');
  t.ok((await call('GET', '/places/suggest', { token: 'tok-r1' })).status === 405, 'GET on suggest: 405');
  t.ok((await call('POST', '/places/mine', { token: 'tok-r1' })).status === 405, 'POST on mine: 405');
  t.ok((await call('GET', '/places/mine', { token: 'tok-r1', pool: null })).status === 503,
    'no database configured: 503, not a crash');
  t.ok((await call('GET', '/places/mine')).status === 401, 'mine with no token: 401');
  t.ok((await call('GET', '/places/nearby?type=cafe&lat=18.08&lon=-15.97')).status === 401,
    'nearby with no token: 401 (sign-in required, owner 2026-11-09)');
  let r = await suggest('tok-forged', { ...place('Pharmacie Forgée'), author_id: 'rider-1', authorSide: 'driver' });
  t.ok(r.status === 401 && (await count()) === 0, 'an unknown token, even naming someone in the body: 401, nothing stored');
  r = await suggest('tok-d1', { ...place('Station Total Ksar', 'fuel'), author_id: 'rider-2', author_side: 'rider', country: 'DZ', status: 'approved' });
  let row = (await rows('author_id = $1', ['driver-1']))[0];
  t.ok(r.status === 201 && row && row.author_side === 'driver' && row.status === 'pending' && row.country === 'MR',
    "a driver's token: stored as HIS, pending, in the pin's country -- the body's author, country and status are not read",
    JSON.stringify(row));
  t.ok((await rows('author_id = $1', ['rider-2'])).length === 0, 'and nothing was stored for the person the body named');

  /* ── 2 ─────────────────────────────────────────────────────────────── */
  t.section('2. what a proposal must look like');
  const before = await count();
  const refusedWith = async (body, status, error, label) => {
    const res = await suggest('tok-r1', body);
    t.ok(res.status === status && res.json().error === error, label, `${res.status} ${res.text}`);
  };
  await refusedWith(place('X'), 400, 'bad_name', 'a one-character name');
  await refusedWith(place('x'.repeat(81)), 400, 'bad_name', 'an 81-character name');
  await refusedWith(place('   '), 400, 'bad_name', 'a name of spaces');
  await refusedWith({ ...place('ok'), name: 42 }, 400, 'bad_name', 'a name that is not text');
  await refusedWith(place('Café\u0000Bar'), 400, 'bad_name', 'a control character in the name');
  await refusedWith(place('\u200B\u200B\u200B'), 400, 'bad_name', 'a name of zero-width spaces only');
  await refusedWith(place('X\u200B\uFEFF'), 400, 'bad_name', 'one letter padded with invisible characters to look long enough');
  await refusedWith(place('\u200C\u200D'), 400, 'bad_name', 'a name of joiners only');
  await refusedWith(place('-- ..'), 400, 'bad_name', 'a name of punctuation only');
  await refusedWith(place('Pharmacie', 'pharmacie'), 400, 'bad_type', 'the French word instead of the key');
  await refusedWith({ name: 'Café Sans Type', lat: NKC.lat, lon: NKC.lon }, 400, 'bad_type', 'no type');
  await refusedWith({ ...place('Pharmacie A'), lat: '18.08' }, 400, 'bad_position', 'a latitude sent as text');
  await refusedWith({ ...place('Pharmacie B'), lat: NKC.lon, lon: NKC.lat }, 400, 'bad_position', 'latitude and longitude swapped');
  await refusedWith(place('Pharmacie Paris', 'pharmacy', { lat: 48.85, lon: 2.35 }), 400, 'bad_position', 'Paris');
  await refusedWith(place('Pharmacie Rabat', 'pharmacy', { lat: 34.02, lon: -6.84 }), 422, 'outside_service_area',
    'inside the box but in Morocco: the service areas say no');
  r = await call('POST', '/places/suggest', { token: 'tok-r1', raw: '{not json' });
  t.ok(r.status === 400 && r.json().error === 'bad_body', 'a body that is not JSON: 400');
  r = await call('POST', '/places/suggest', { token: 'tok-r1', raw: JSON.stringify({ ...place('Big'), pad: 'x'.repeat(5000) }) });
  t.ok(r.status === 413, 'a body over 4 KB: 413');
  t.ok((await count()) === before, 'none of those stored anything');

  /* ── 3 ─────────────────────────────────────────────────────────────── */
  t.section('3. what is stored, and what the server decided');
  r = await suggest('tok-r1', place('  Pharmacie\n  de   la   Gare  '));
  let j = r.json();
  row = (await rows('place_id = $1', [j.placeId]))[0];
  t.ok(r.status === 201 && /^u\d+$/.test(j.placeId) && j.status === 'pending', `201, a u-id (${j.placeId}), pending`);
  t.ok(row.name === 'Pharmacie de la Gare' && row.proposed_name === row.name && row.type === 'pharmacy',
    'the name trimmed and its spaces collapsed, newline included; shown = proposed until the office edits', row.name);
  t.ok(row.country === 'MR' && row.locality === 'Tevragh Zeina' && j.locality === 'Tevragh Zeina',
    'country and locality are the server\'s, from the pin');
  r = await suggest('tok-r1', place('ﺻﻴﺪﻟﻴﺔ ﺍﻟﻨﻮﺭ', 'pharmacy', north(NKC, 400)));
  row = (await rows('place_id = $1', [r.json().placeId]))[0];
  t.ok(r.status === 201 && row.name === 'صيدلية النور',
    'Arabic typed in presentation forms is stored as ordinary letters (NFKC)', row && row.name);
  r = await suggest('tok-r1', place('Café ☕ du Port', 'cafe', ALG));
  t.ok(r.status === 201 && r.json().country === 'DZ' && r.json().locality === 'Alger Centre', 'Algiers: DZ, Alger Centre');
  r = await suggest('tok-r2', place('Puits du Désert', 'other', { lat: 23.5, lon: -12.0 }));
  t.ok(r.status === 201 && r.json().locality === null, 'a pin with no town near: stored with no locality');
  geo.failLocality = true;
  r = await suggest('tok-r2', place('Boulangerie Ksar', 'shop', north(NKC, 900)));
  geo.failLocality = false;
  t.ok(r.status === 201 && r.json().locality === null, 'the locality lookup failing costs the second line, not the proposal');
  geo.failCountry = true;
  const n0 = await count();
  r = await suggest('tok-r2', place('Pharmacie Hors Ligne', 'pharmacy', north(NKC, 1500)));
  geo.failCountry = false;
  t.ok(r.status === 503 && (await count()) === n0, 'the place index unreachable: 503, nothing stored (the country is not guessed)');

  /* ── 4 ─────────────────────────────────────────────────────────────── */
  t.section('4. duplicates: the same name within 100 m');
  r = await suggest('tok-r2', place('PHARMACIE DE LA GARE', 'pharmacy', north(NKC, 60)));
  t.ok(r.status === 409 && r.json().error === 'duplicate' && r.json().existing.status === 'pending'
    && r.json().existing.placeId === undefined,
    'someone else, 60 m away, other case: 409 -- « already proposed », and no id for a place nobody can see yet', r.text);
  r = await suggest('tok-r2', place('Pharmacie de la Gare', 'pharmacy', north(NKC, 150)));
  t.ok(r.status === 201, '150 m away, same name: a different place, accepted');
  r = await suggest('tok-r2', place('Pharmacie du Marché', 'pharmacy', north(NKC, 20)));
  t.ok(r.status === 201, '20 m away, another name: accepted');
  const gare = (await rows("name = 'Pharmacie de la Gare'"))[0];
  await db.sql(`UPDATE movin.suggested_place SET status = 'approved', decided_at = now(), decided_by_email = 'a@mova.test'
                 WHERE id = $1`, [gare.id]);
  r = await suggest('tok-d1', place('pharmacie de la gare', 'pharmacy', north(NKC, 30)));
  t.ok(r.status === 409 && r.json().existing.placeId === gare.place_id && r.json().existing.status === 'approved',
    'an APPROVED twin is named, with its id: the phone can offer it instead', r.text);
  await db.sql(`UPDATE movin.suggested_place SET status = 'refused', refusal_reason = 'duplicate'
                 WHERE id = $1`, [gare.id]);
  r = await suggest('tok-d1', place('Pharmacie de la Gare', 'pharmacy', north(NKC, 30)));
  t.ok(r.status === 201, 'a REFUSED twin does not block a new proposal');

  geo.osm = [{ place_id: 'n77', display_name: 'Clinique Chifa', alt_names: 'Chifa|عيادة الشفاء', name_ar: 'عيادة الشفاء', lat: 0, lon: 0 }];
  r = await suggest('tok-r2', place('clinique  CHIFA', 'hospital', north(NKC, 2000)));
  t.ok(r.status === 409 && r.json().existing.status === 'on_the_map' && r.json().existing.placeId === 'n77',
    'already on the map (OSM): 409 on_the_map, with its id', r.text);
  r = await suggest('tok-r2', place('عيادة الشفاء', 'hospital', north(NKC, 2000)));
  t.ok(r.status === 409, "the map's Arabic name matches too");
  geo.osm = [{ place_id: 'n78', display_name: 'Pharmacie', alt_names: 'أنواذيبو', name_ar: null, lat: 0, lon: 0 }];
  r = await suggest('tok-r2', place('انواذيبو', 'other', north(NKC, 2500)));
  t.ok(r.status === 409, 'أنواذيبو and انواذيبو are one name (hamza folded, as geo.normalise does)');
  geo.osm = [];
  t.ok(geo.calls.some((c) => c[0] === 'osmNear' && c[3] === 100), 'the map is asked within 100 m');

  /* ── 5 ─────────────────────────────────────────────────────────────── */
  t.section('5. the limits: 5 per 24 hours, 20 pending');
  who.riders.set('tok-busy', 'rider-busy');
  const spot = (i) => north(ALG, 300 * (i + 1));
  let codes = [];
  for (let i = 0; i < 6; i += 1) codes.push((await suggest('tok-busy', place(`Boutique ${i}`, 'shop', spot(i)))).status);
  t.ok(JSON.stringify(codes) === '[201,201,201,201,201,429]', `five pass, the sixth is refused (${codes})`);
  r = await suggest('tok-busy', place('Boutique 7', 'shop', spot(7)));
  t.ok(r.json().error === 'daily_limit' && r.json().limit === 5, 'the refusal says which limit: daily_limit, 5');
  await db.sql(`UPDATE movin.suggested_place SET created_at = now() - interval '25 hours' WHERE author_id = 'rider-busy'`);
  r = await suggest('tok-busy', place('Boutique 8', 'shop', spot(8)));
  t.ok(r.status === 201, 'a day later (rolling 24 hours), he may propose again');

  who.riders.set('tok-many', 'rider-many');
  for (let i = 0; i < 20; i += 1) {
    await db.sql(`INSERT INTO movin.suggested_place (proposed_name, proposed_type, name, type, lat, lon, country,
                    author_side, author_id, created_at)
                  VALUES ($1, 'shop', $1, 'shop', $2, $3, 'DZ', 'rider', 'rider-many', now() - interval '3 days')`,
    [`Ancien ${i}`, ALG.lat + 0.01 * (i + 1), ALG.lon]);
  }
  r = await suggest('tok-many', place('Encore Une', 'shop', north(ALG, 5000)));
  t.ok(r.status === 429 && r.json().error === 'too_many_pending' && r.json().limit === 20,
    '20 still pending (none today): the 21st waits for the office -- too_many_pending', r.text);
  await db.sql(`UPDATE movin.suggested_place SET status = 'approved', decided_at = now(), decided_by_email = 'a@mova.test'
                 WHERE author_id = 'rider-many' AND name = 'Ancien 0'`);
  r = await suggest('tok-many', place('Encore Une', 'shop', north(ALG, 5000)));
  t.ok(r.status === 201, 'one approved, 19 pending: he may propose again');

  who.riders.set('tok-race', 'rider-race');
  for (let i = 0; i < 4; i += 1) await suggest('tok-race', place(`Kiosque ${i}`, 'shop', north(NKC, 3000 + 300 * i)));
  const [a, b] = await Promise.all([
    suggest('tok-race', place('Kiosque A', 'shop', north(NKC, 6000))),
    suggest('tok-race', place('Kiosque B', 'shop', north(NKC, 6300))),
  ]);
  const raced = (await rows('author_id = $1', ['rider-race'])).length;
  t.ok(raced === 5 && [a.status, b.status].sort().join() === '201,429',
    `two taps at once on his 5th: one passes, one is refused, 5 stored (${a.status}/${b.status}, ${raced})`);
  // What that line does NOT prove: lib/db.js's pool runs one transaction at
  // a time (PGlite is one connection), so the two taps never truly overlap
  // here -- removing the lock still passes it (measured: mutated, 2026-11-09).
  // What protects the live server, two connections at once, is the advisory
  // lock; so check it is taken, per person, inside the transaction, BEFORE
  // the counts are read.
  const seen = [];
  const recording = {
    ...db.pool,
    query: (text, params) => db.pool.query(text, params),
    connect: async () => {
      const c = await db.pool.connect();
      return { query: (text, params) => { seen.push(text.replace(/\s+/g, ' ').trim()); return c.query(text, params); }, release: c.release };
    },
  };
  who.riders.set('tok-lock', 'rider-lock');
  await routes.handle(Object.assign(Readable.from([Buffer.from(JSON.stringify(place('Kiosque Verrou', 'shop', north(NKC, 9000))))]),
    { method: 'POST', headers: { token: 'tok-lock' } }), fakes.res(), new URL('http://localhost/places/suggest'), { ...opts, pool: recording });
  const at = (re) => seen.findIndex((q) => re.test(q));
  t.ok(at(/^BEGIN$/) === 0 && at(/pg_advisory_xact_lock/) === 1 && at(/count\(\*\)/) === 2,
    'each proposal takes the per-person lock first in its transaction, then counts -- what stops two real connections both passing',
    seen.slice(0, 3).join(' / '));

  /* ── 6 ─────────────────────────────────────────────────────────────── */
  t.section('6. « Mes lieux proposés »');
  const mineOf = async (token) => (await call('GET', '/places/mine', { token })).json().places;
  const mineR1 = await mineOf('tok-r1');
  t.ok(mineR1.length === 3 && mineR1.every((p) => /^u\d+$/.test(p.placeId)),
    `his own three, and only his (${mineR1.map((p) => p.name).join(' | ')})`);
  t.ok(mineR1[0].name === 'Café ☕ du Port' && mineR1[2].name === 'Pharmacie de la Gare', 'newest first');
  const station = (await rows("name = 'Station Total Ksar'"))[0];
  await db.sql(`UPDATE movin.suggested_place SET name = 'Station Total — Ksar', status = 'refused',
                  refusal_reason = 'not_a_place', decided_at = now(), decided_by_email = 'a@mova.test' WHERE id = $1`, [station.id]);
  const mineD1 = (await mineOf('tok-d1')).find((p) => p.placeId === station.place_id);
  t.ok(mineD1 && mineD1.status === 'refused' && mineD1.refusalReason === 'not_a_place'
    && mineD1.name === 'Station Total — Ksar' && mineD1.proposedName === 'Station Total Ksar',
    "the driver sees his refusal, its reason code, the office's name AND what he typed", JSON.stringify(mineD1));
  t.ok((await mineOf('tok-r1')).every((p) => p.placeId !== station.place_id), "a rider never sees the driver's");

  /* ── 7 ─────────────────────────────────────────────────────────────── */
  t.section('7. « search by type »: 5 km, 20, nearest first; 15 km when 5 finds nothing');
  // Approved proposals: a café 1 km from the person, one 7 km away, one pending, one refused.
  const person = { lat: 18.10, lon: -15.96 };
  const approve = (name, at, type = 'cafe', extra = '') => db.sql(
    `INSERT INTO movin.suggested_place (proposed_name, proposed_type, name, type, lat, lon, country, locality,
       author_side, author_id, status, decided_at, decided_by_email ${extra ? ', refusal_reason' : ''})
     VALUES ($1, $2, $1, $2, $3, $4, 'MR', 'Tevragh Zeina', 'rider', 'rider-x', $5, now(), 'a@mova.test' ${extra ? ', $6' : ''})`,
    extra ? [name, type, at.lat, at.lon, 'refused', extra] : [name, type, at.lat, at.lon, 'approved']);
  await approve('Café Proposé Proche', north(person, 1000));
  await approve('Café Proposé Loin', north(person, 7000));
  await approve('Café Refusé', north(person, 500), 'cafe', 'duplicate');
  await db.sql(`INSERT INTO movin.suggested_place (proposed_name, proposed_type, name, type, lat, lon, country,
                  author_side, author_id) VALUES ('Café En Attente', 'cafe', 'Café En Attente', 'cafe', $1, $2, 'MR', 'rider', 'rider-x')`,
  [north(person, 300).lat, person.lon]);
  geo.ofType = [
    { type: 'cafe', place_id: 'n1', display_name: 'Café OSM 2 km', name_ar: 'مقهى', locality: 'Ksar', lat: 1, lon: 1, distance_m: 2000 },
    { type: 'cafe', place_id: 'n2', display_name: 'Café OSM 400 m', name_ar: null, locality: 'Ksar', lat: 1, lon: 1, distance_m: 400 },
    { type: 'pharmacy', place_id: 'n3', display_name: 'Pharmacie OSM', name_ar: null, locality: null, lat: 1, lon: 1, distance_m: 100 },
  ];
  const near = (q, token = 'tok-r1') => call('GET', `/places/nearby?${q}`, { token });
  geo.calls = [];
  r = await near(`type=cafe&lat=${person.lat}&lon=${person.lon}`);
  j = r.json();
  t.ok(r.status === 200 && j.radius === 5000, '200, 5 km');
  t.ok(JSON.stringify(j.results.map((p) => p.name)) === JSON.stringify(['Café OSM 400 m', 'Café Proposé Proche', 'Café OSM 2 km']),
    'the map and approved proposals together, nearest first; pending, refused, other types and the 7 km one left out',
    j.results.map((p) => p.name).join(' | '));
  const proposedHit = j.results.find((p) => p.source === 'proposed');
  t.ok(proposedHit && /^u\d+$/.test(proposedHit.placeId) && proposedHit.localityAr === 'تفرغ زينة'
    && Math.abs(proposedHit.distance - 1000) <= 2,
    "a proposal carries its u-id, its locality's Arabic name and its distance in metres", JSON.stringify(proposedHit));
  t.ok(j.results[2].nameAr === 'مقهى' && j.results[2].localityAr === 'لكصر', 'an OSM place carries its Arabic names');
  t.ok(geo.calls.filter((c) => c[0] === 'osmOfType').length === 1, 'found something at 5 km: no second search');
  const arabicCalls = geo.calls.filter((c) => c[0] === 'localitiesInArabic');
  t.ok(arabicCalls.length === 1 && JSON.stringify([...arabicCalls[0][1]].sort()) === JSON.stringify(['Ksar', 'Tevragh Zeina']),
    'the localities in Arabic: ONE lookup for the map places and the proposals together (12 s on the live index when it was one per place)',
    JSON.stringify(arabicCalls));

  geo.calls = [];
  geo.ofType = [{ type: 'market', place_id: 'n9', display_name: 'Marché à 12 km', name_ar: null, locality: null, lat: 1, lon: 1, distance_m: 12000 }];
  r = await near(`type=market&lat=${person.lat}&lon=${person.lon}`, 'tok-d1');
  j = r.json();
  const radii = geo.calls.filter((c) => c[0] === 'osmOfType').map((c) => c[2]);
  t.ok(r.status === 200 && j.radius === 15000 && j.results.length === 1 && j.results[0].name === 'Marché à 12 km'
    && JSON.stringify(radii) === '[5000,15000]',
    'nothing at 5 km: asked again at 15 km, and says so (radius 15000) -- a driver may search too', `${radii} ${r.text}`);
  r = await near(`type=bank&lat=${person.lat}&lon=${person.lon}`);
  t.ok(r.status === 200 && r.json().radius === 15000 && r.json().results.length === 0, 'nothing even at 15 km: an empty list, not an error');

  geo.ofType = Array.from({ length: 30 }, (_, i) => ({ type: 'shop', place_id: `s${i}`, display_name: `Shop ${i}`,
    name_ar: null, locality: null, lat: 1, lon: 1, distance_m: 4000 - i * 100 }));
  r = await near(`type=shop&lat=${person.lat}&lon=${person.lon}`);
  j = r.json();
  t.ok(j.results.length === 20 && j.results[0].distance <= j.results[19].distance, '20 at most, nearest first');
  t.ok((await near(`type=other&lat=${person.lat}&lon=${person.lon}`)).status === 400, '« other » is not a search: 400');
  t.ok((await near(`type=pharmacie&lat=${person.lat}&lon=${person.lon}`)).status === 400, 'an unknown type: 400');
  t.ok((await near('type=cafe&lat=18.1')).status === 400, 'no longitude: 400');
  t.ok((await near('type=cafe&lat=-15.96&lon=18.1')).status === 400, 'latitude and longitude swapped: 400');

  /* ── 8 ─────────────────────────────────────────────────────────────── */
  t.section('8. the module agrees with the SQL');
  const def = async (name) => (await db.sql('SELECT pg_get_constraintdef(oid) AS d FROM pg_constraint WHERE conname = $1', [name]))[0].d;
  const checkTypes = [...(await def('suggested_place_type_known')).matchAll(/'([a-z_]+)'/g)].map((m) => m[1]);
  t.ok(JSON.stringify([...checkTypes].sort()) === JSON.stringify([...routes.PLACE_TYPES].sort()),
    "the module's 13 types are the table CHECK's");
  // Postgres prints BETWEEN back as >= / <=, and negative numbers quoted.
  const num = (d, re) => { const m = d.match(re); return m ? Number(m[1]) : NaN; };
  const nameDef = await def('suggested_place_name_shape');
  t.ok(num(nameDef, /char_length\(name\) >= (\d+)/) === routes.NAME_MIN && num(nameDef, /char_length\(name\) <= (\d+)/) === routes.NAME_MAX,
    `the name length is the CHECK's (${routes.NAME_MIN}..${routes.NAME_MAX})`, nameDef);
  const boxDef = await def('suggested_place_in_our_box');
  const bound = (col, op) => num(boxDef, new RegExp(`${col} ${op} \\(+'?(-?[\\d.]+)`));
  const sqlBox = [bound('lat', '>='), bound('lat', '<='), bound('lon', '>='), bound('lon', '<=')];
  t.ok(JSON.stringify(sqlBox) === JSON.stringify([14, 38, -18, 12.5]),
    `the CHECK's box is 14..38 N, 18 W..12.5 E (${sqlBox})`, boxDef);
  // ...and the module refuses exactly outside it: just inside passes the position check, just outside does not.
  const edge = async (lat, lon) => (await near(`type=cafe&lat=${lat}&lon=${lon}`)).status;
  t.ok(await edge(14, -18) === 200 && await edge(38, 12.5) === 200 && await edge(13.99, -15) === 400 && await edge(20, 12.51) === 400,
    'the module refuses a position exactly where the CHECK would');
  const liveSql = routes.liveGeo({ query: async () => ({ rows: [] }) }).osmOfType.toString();
  t.ok(liveSql.includes('movin.place_type_of(p.class, p.subclass, p.display_name)'),
    'the live search by type passes the NAME to place_type_of -- with two arguments churches are mosques again');
  t.ok(routes.normalise('Pharmacie de l’Espoir') === routes.normalise("PHARMACIE DE L'ESPOIR"),
    'names compare without case, accents or punctuation');
  t.ok(routes.cleanName('Café\u200B Tfeila\u00AD') === 'Café Tfeila',
    'invisible characters inside a real name are removed, not refused');
  t.ok(routes.cleanName('ZW') === 'ZW', 'two letters are still a name');
  // 😀 is outside the 16-bit range: .length counts it 2, char_length 1. (☕,
  // used here until 2026-11-11, is one unit either way and proved nothing.)
  t.ok(routes.cleanName('a'.repeat(78) + '😀😀') === 'a'.repeat(78) + '😀😀'
    && routes.cleanName('a'.repeat(79) + '😀😀') === null,
    'an emoji is one character, as char_length counts it: 78 letters + 2 emoji = 80 accepted, 81 refused');

  /* ── 9 ─────────────────────────────────────────────────────────────── */
  t.section('9. the photos: the author, while the place waits, an image by its bytes, 512 KB');
  // Real first bytes, then filler: what matters to the shim is the start.
  const JPEG = Buffer.concat([Buffer.from([0xff, 0xd8, 0xff, 0xe0]), Buffer.alloc(2000, 7)]);
  const PNG = Buffer.concat([Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]), Buffer.alloc(1500, 9)]);
  const put = (token, url, raw, headers) => call('PUT', url, { token, raw, headers });
  const del = (token, url) => call('DELETE', url, { token });
  const photos = (id) => db.sql(
    `SELECT n, content_type, bytes FROM movin.suggested_place_photo WHERE suggested_place_id = $1 ORDER BY n`, [id]);
  const version = async (id) => (await db.sql(
    'SELECT (extract(epoch from updated_at) * 1000000)::bigint::text AS v FROM movin.suggested_place WHERE id = $1', [id]))[0].v;
  const placeRow = async (pid) => (await rows('place_id = $1', [pid]))[0];

  who.riders.set('tok-photo', 'rider-photo');
  r = await suggest('tok-photo', place('Pharmacie aux Photos', 'pharmacy', north(ALG, 20000)));
  const P = await placeRow(r.json().placeId);
  const url1 = `/places/photos/${P.place_id}/1`;
  const url2 = `/places/photos/${P.place_id}/2`;

  let v0 = await version(P.id);
  r = await put('tok-photo', url1, JPEG, { 'content-type': 'image/png' });
  let stored = await photos(P.id);
  t.ok(r.status === 200 && r.json().type === 'image/jpeg' && r.json().n === 1 && r.json().bytes === JPEG.length
    && stored.length === 1 && stored[0].content_type === 'image/jpeg' && Buffer.from(stored[0].bytes).equals(JPEG),
    'the author, while it waits: photo 1 stored, byte for byte -- as a JPEG, though the header said PNG (the bytes decide)',
    `${r.status} ${r.text}`);
  t.ok((await version(P.id)) !== v0, "the place's version moved: an office page loaded before the photo can no longer decide on it");
  r = await put('tok-photo', url2, PNG);
  t.ok(r.status === 200 && r.json().type === 'image/png', 'photo 2, a PNG, with no header at all', r.text);
  r = await put('tok-photo', url1, PNG);
  stored = await photos(P.id);
  t.ok(r.status === 200 && stored.length === 2 && stored[0].content_type === 'image/png' && Buffer.from(stored[0].bytes).equals(PNG),
    'a second PUT on photo 1 replaces it: still two photos, never three');
  let mineP = (await mineOf('tok-photo')).find((p) => p.placeId === P.place_id);
  t.ok(mineP && JSON.stringify(mineP.photos) === '[1,2]', '« Mes lieux proposés » says which photos the office has: [1,2]',
    JSON.stringify(mineP && mineP.photos));
  t.ok((await mineOf('tok-r1')).every((p) => Array.isArray(p.photos) && p.photos.length === 0),
    'a place with no photo says [] -- never the photos themselves');

  t.section('9b. somebody else, and no one');
  const untouched = async (label, res, status) => {
    const now = await photos(P.id);
    t.ok(res.status === status && now.length === 2 && Buffer.from(now[0].bytes).equals(PNG), label, `${res.status} ${res.text}`);
  };
  v0 = await version(P.id);
  await untouched('another passenger: 404, as if the place did not exist; nothing changed',
    await put('tok-r2', url1, JPEG), 404);
  await untouched("another passenger's DELETE: 404, both photos still there", await del('tok-r2', url2), 404);
  who.drivers.set('tok-same-id', 'rider-photo');
  await untouched('a DRIVER whose id happens to equal the author\'s: 404 -- the side is part of who', await put('tok-same-id', url1, JPEG), 404);
  await untouched('no token: 401', await put(undefined, url1, JPEG), 401);
  await untouched('a forged token: 401', await put('tok-forged', url1, JPEG), 401);
  t.ok((await version(P.id)) === v0, "and none of them moved the place's version");
  r = await put('tok-photo', '/places/photos/u999999999/1', JPEG);
  t.ok(r.status === 404 && r.json().error === 'no_such_place', 'a place that does not exist: the same 404', r.text);

  t.section('9c. what is not a photo');
  const n9 = (await db.sql('SELECT count(*)::int AS n FROM movin.suggested_place_photo'))[0].n;
  const refused9 = async (res, status, error, label) =>
    t.ok(res.status === status && (!error || res.json().error === error), label, `${res.status} ${res.text}`);
  await refused9(await put('tok-photo', url1, Buffer.from('GIF89a' + 'x'.repeat(50)), { 'content-type': 'image/jpeg' }),
    415, 'not_an_image', 'a GIF that says it is a JPEG: 415');
  await refused9(await put('tok-photo', url1, Buffer.from('<svg xmlns="http://www.w3.org/2000/svg"/>')), 415, 'not_an_image', 'an SVG: 415');
  await refused9(await put('tok-photo', url1, Buffer.from([0xff, 0xd8, 0xff])), 415, 'not_an_image',
    "a JPEG's first three bytes and nothing after: 415 (the table's CHECK would refuse it too)");
  await refused9(await put('tok-photo', url1, Buffer.alloc(0)), 400, 'no_image', 'an empty body: 400');
  const atLimit = Buffer.concat([JPEG.subarray(0, 4), Buffer.alloc(routes.PHOTO_MAX_BYTES - 4, 1)]);
  await refused9(await put('tok-photo', url1, Buffer.concat([atLimit, Buffer.from([1])])), 413, 'too_large',
    'one byte over 512 KB: 413');
  t.ok((await db.sql('SELECT count(*)::int AS n FROM movin.suggested_place_photo'))[0].n === n9
    && Buffer.from((await photos(P.id))[0].bytes).equals(PNG), 'none of those stored or replaced anything');
  r = await put('tok-photo', url1, atLimit);
  t.ok(r.status === 200 && r.json().bytes === routes.PHOTO_MAX_BYTES, 'exactly 512 KB: stored (the table agrees)', r.text);
  for (const [path9, label] of [
    [`/places/photos/${P.place_id}/3`, 'photo 3'], [`/places/photos/${P.place_id}/0`, 'photo 0'],
    ['/places/photos/u0/1', 'u0'], [`/places/photos/${P.place_id.slice(1)}/1`, 'a number without its u'],
    ['/places/photos/u1234567890123456789/1', '19 digits (past a bigint)'], [`/places/photos/${P.place_id}`, 'no number'],
    [`/places/photos/${P.place_id}/1/x`, 'more after it'], ['/places/photos/n12/1', "an OpenStreetMap id (n…): not ours"]]) {
    r = await put('tok-photo', path9, JPEG);
    t.ok(r.status === 404 && r.json().error === 'no such places route', `${label}: 404 before anything is read`, `${r.status} ${r.text}`);
  }
  t.ok((await call('GET', url1, { token: 'tok-photo' })).status === 405, 'GET a photo: 405 -- nobody reads them back here (A3)');
  t.ok((await call('POST', url1, { token: 'tok-photo', raw: JPEG })).status === 405, 'POST: 405');
  t.ok((await call('PUT', url1, { token: 'tok-photo', raw: JPEG, pool: null })).status === 503, 'no database: 503');

  t.section('9d. taking one back, and a place already decided');
  v0 = await version(P.id);
  r = await del('tok-photo', url2);
  t.ok(r.status === 200 && r.json().deleted === true && (await photos(P.id)).map((x) => x.n).join() === '1'
    && (await version(P.id)) !== v0, 'DELETE photo 2: gone, photo 1 kept, the version moved', r.text);
  v0 = await version(P.id);
  r = await del('tok-photo', url2);
  t.ok(r.status === 200 && r.json().deleted === false && (await version(P.id)) === v0,
    'DELETE again: 200, deleted false, and the version did NOT move (nothing changed)', r.text);
  mineP = (await mineOf('tok-photo')).find((p) => p.placeId === P.place_id);
  t.ok(JSON.stringify(mineP.photos) === '[1]', '« Mes lieux proposés »: [1] -- photo 2 is what « Réessayer » may send');

  for (const [status, extra] of [['approved', ''], ['refused', ", refusal_reason = 'private'"]]) {
    await db.sql(`UPDATE movin.suggested_place SET status = '${status}' ${extra}, decided_at = now(),
                    decided_by_email = 'a@mova.test' WHERE id = $1`, [P.id]);
    const before9 = await photos(P.id);
    r = await put('tok-photo', url2, JPEG);
    t.ok(r.status === 409 && r.json().error === 'decided' && r.json().status === status,
      `${status}: a new photo is refused, 409 decided -- the office decided on what it saw`, r.text);
    r = await del('tok-photo', url1);
    t.ok(r.status === 409 && (await photos(P.id)).length === before9.length, `${status}: and its photo cannot be taken back either`);
    await db.sql(`UPDATE movin.suggested_place SET status = 'pending', refusal_reason = NULL, decided_at = NULL,
                    decided_by_email = NULL WHERE id = $1`, [P.id]);
  }

  // The order of the transaction: what keeps a photo and the office's
  // decision from crossing on the live server (two connections; PGlite has one).
  const seen9 = [];
  const recording9 = {
    ...db.pool,
    connect: async () => {
      const c = await db.pool.connect();
      return { query: (text, params) => { seen9.push(text.replace(/\s+/g, ' ').trim()); return c.query(text, params); }, release: c.release };
    },
  };
  await routes.handle(Object.assign(Readable.from([JPEG]), { method: 'PUT', headers: { token: 'tok-photo' } }),
    fakes.res(), new URL(`http://localhost${url2}`), { ...opts, pool: recording9 });
  const at9 = (re) => seen9.findIndex((q) => re.test(q));
  t.ok(at9(/^BEGIN$/) === 0 && at9(/FROM movin\.suggested_place WHERE id = \$1 AND author_side = \$2 AND author_id = \$3 FOR UPDATE$/) === 1
    && at9(/^INSERT INTO movin\.suggested_place_photo/) === 2 && at9(/^UPDATE movin\.suggested_place SET updated_at/) === 3
    && at9(/^COMMIT$/) === 4,
    'one transaction: the place locked (as the console locks it), checked against the token, then the photo, then the version',
    seen9.join(' / '));

  // The module and the table agree on the size.
  const sizeDef = (await db.sql(
    "SELECT pg_get_constraintdef(oid) AS d FROM pg_constraint WHERE conname = 'suggested_place_photo_size'"))[0].d;
  t.ok(Number((sizeDef.match(/<= (\d+)/) || [])[1]) === routes.PHOTO_MAX_BYTES,
    `PHOTO_MAX_BYTES (${routes.PHOTO_MAX_BYTES}) is the table's upper bound`, sizeDef);

  who.close();
  await db.close();
  t.finish();
})().catch((e) => { console.error(e); process.exit(1); });
