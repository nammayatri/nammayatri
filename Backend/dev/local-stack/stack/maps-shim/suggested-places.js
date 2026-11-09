'use strict';
/**
 * Places proposed by passengers and drivers (« Lieux proposés », day 2,
 * 2026-11-09), and « search by type ». Everything under /places/:
 *
 *   POST /places/suggest   {name, type, lat, lon}   propose a missing place
 *   GET  /places/mine                               my proposals and their state
 *   GET  /places/nearby?type=&lat=&lon=             places of one type around me
 *
 * The table is movin.suggested_place (stack/db/suggested-places.sql); the
 * office approves or refuses in the console (the website's admin-api), and an
 * approved place is found by everyone through places.js (place_id `u<id>`).
 *
 * ── Who is calling: the token, never the body ──────────────────────────────
 * Same rule as wallet.js and deletion.js. The person is whoever the driver or
 * the rider backend says the token belongs to; a body that names someone is
 * not read. All three routes need a token (« search by type » too: owner,
 * 2026-11-09 -- it stops anyone scraping the map through us).
 *
 * ── What the server decides, and the phone cannot ──────────────────────────
 * The country (the pin against the service areas: a pin outside both is
 * refused), the locality (the nearest town in the index), the author, the
 * status. The phone sends four fields and nothing else is read.
 *
 * ── The limits (owner, 2026-11-09) ─────────────────────────────────────────
 * 5 proposals per person in any 24 hours, and at most 20 pending at once.
 * Rolling 24 hours rather than a calendar day: two countries, two time zones,
 * and a midnight that a person can wait for is not a limit. Counted under a
 * per-person advisory lock, so two taps at once cannot both pass the count.
 *
 * ── Duplicates ─────────────────────────────────────────────────────────────
 * The same name within 100 m -- among pending and approved proposals, AND the
 * OpenStreetMap places already on the map (a « missing » pharmacy that is
 * already there is not missing). Names are compared after `normalise()` below:
 * NFKC, accents and case gone, the Arabic letter variants folded the way
 * geo.normalise folds them. A refused proposal does not block a new one.
 *
 * ── « Search by type » (owner, 2026-11-09) ─────────────────────────────────
 * 5 km around the person, 20 results, nearest first; if 5 km finds nothing,
 * once more up to 15 km (Nouakchott has 20 cafés within 10 km of the centre).
 * OpenStreetMap places through movin.place_type_of(class, subclass,
 * display_name) -- the NAME too, or the churches come back as mosques -- and
 * approved proposals. `other` is never a search.
 *
 * ── What only the live database can do ─────────────────────────────────────
 * Four questions need PostGIS or the place index: the pin's country, its
 * locality, the OSM places near a pin, the OSM places of a type near a person.
 * They are `liveGeo()` below, and only they: the tests replace that object,
 * and investigations/probe-suggested-places.sql runs these exact statements on
 * the live index before a release (no CI, and PGlite has no PostGIS).
 * Everything else here is plain SQL on movin.suggested_place, tested for real.
 */
const { driverFromToken, riderFromToken } = require('./identity');
const { send } = require('./reply');

/** The website's packages/domain place-types.ts; the SQL CHECK holds the same 13. */
const PLACE_TYPES = [
  'cafe', 'restaurant', 'pharmacy', 'hospital', 'mosque', 'school', 'shop',
  'market', 'bank', 'hotel', 'fuel', 'administration', 'other',
];
/** PLACE_NAME_MIN / PLACE_NAME_MAX of place-types.ts, in characters (code points). */
const NAME_MIN = 2;
const NAME_MAX = 80;

const PER_DAY = 5;
const MAX_PENDING = 20;
const DUPLICATE_METRES = 100;
const NEARBY_METRES = 5000;
const NEARBY_WIDER_METRES = 15000;
const NEARBY_LIMIT = 20;
const MINE_LIMIT = 50;
const BODY_LIMIT = 4096;

/** The box around both countries -- the table's suggested_place_in_our_box CHECK. */
const inOurBox = (lat, lon) => lat >= 14 && lat <= 38 && lon >= -18 && lon <= 12.5;

/* ── names ─────────────────────────────────────────────────────────────── */

// Bidi overrides and isolates: invisible, and able to make a name READ
// differently in the console from what it is. Nothing a place name needs.
const BIDI_CONTROLS = /[‪-‮⁦-⁩‎‏]/g;

/**
 * A typed name as it will be stored, or null if it cannot be one.
 *
 * NFKC first: Arabic presentation forms (U+FB50-U+FEFF) render like ordinary
 * letters and match nothing typed on a keyboard -- docs/maps.md, the Arabic
 * names. Then every run of whitespace (newlines included) becomes one space,
 * and the ends are trimmed: the table's CHECK refuses an untrimmed name, so a
 * friendly cleanup here beats a 500 there. Length in code points, as
 * Postgres's char_length counts them -- an emoji is one, not two.
 */
function cleanName(raw) {
  if (typeof raw !== 'string') return null;
  const name = raw.normalize('NFKC').replace(BIDI_CONTROLS, '').replace(/\s+/gu, ' ').trim();
  if (/\p{Cc}/u.test(name)) return null;
  const length = [...name].length;
  return length >= NAME_MIN && length <= NAME_MAX ? name : null;
}

/**
 * The key two names are compared by: « Pharmacie de l'Espoir » and
 * « PHARMACIE DE L ESPOIR » are one place, and so are أنواذيبو and انواذيبو.
 * The Arabic folding is geo.normalise's (geocoder/arabic-search.sql): أ إ آ ٱ
 * to ا, ى to ي, ة to ه, harakat and tatweel gone.
 */
function normalise(name) {
  return String(name || '')
    .normalize('NFKD')
    .replace(/\p{M}/gu, '') // accents; also the Arabic harakat, which are marks
    .replace(/ـ/g, '') // tatweel
    .replace(/[أإآٱ]/g, 'ا')
    .replace(/ى/g, 'ي')
    .replace(/ة/g, 'ه')
    .toLowerCase()
    .replace(/[^\p{L}\p{N}]+/gu, ' ')
    .trim();
}

/** Metres between two points, on a sphere -- for a 100 m test and a sort. */
function metresBetween(lat1, lon1, lat2, lon2) {
  const rad = (d) => (d * Math.PI) / 180;
  const a = Math.sin(rad(lat2 - lat1) / 2) ** 2
    + Math.cos(rad(lat1)) * Math.cos(rad(lat2)) * Math.sin(rad(lon2 - lon1) / 2) ** 2;
  return 2 * 6371008.8 * Math.asin(Math.sqrt(a));
}

/** Degrees of latitude / longitude that cover `metres` around a latitude: a box to scan. */
function box(lat, metres) {
  const dLat = metres / 111320;
  const dLon = metres / (111320 * Math.max(Math.cos((lat * Math.PI) / 180), 0.01));
  return { dLat, dLon };
}

/* ── who is calling ────────────────────────────────────────────────────── */

/**
 * The person behind a token: a driver or a passenger. Asked of the driver
 * backend first, as deletion.js does; a token belongs to one of the two.
 */
async function whoIs(env, token) {
  if (!token) return null;
  const driver = await driverFromToken(env.driverUrl, token);
  if (driver) return { side: 'driver', id: driver.id };
  const rider = await riderFromToken(env.riderUrl, token);
  if (rider) return { side: 'rider', id: rider.id };
  return null;
}

/* ── the live database's four answers ──────────────────────────────────── */

/**
 * The questions only PostGIS and the place index can answer. Each one is run
 * by investigations/probe-suggested-places.sql on the live index, word for
 * word, before a release; keep the two in step.
 */
function liveGeo(pool) {
  const point = (latParam, lonParam) =>
    `st_setsrid(st_makepoint(${lonParam}::float8, ${latParam}::float8), 4326)::geography`;
  return {
    /** 'MR', 'DZ', or null outside both service areas (unlike places.countryAt, no fallback). */
    async countryAt(lat, lon) {
      const { rows } = await pool.query(
        `SELECT CASE region WHEN 'Algeria' THEN 'DZ' ELSE 'MR' END AS country
           FROM atlas_app.geometry
          WHERE region IN ('Algeria', 'Mauritania')
            AND st_contains(geom, st_point($2::float8, $1::float8))
          LIMIT 1`,
        [lat, lon],
      );
      return rows[0] ? rows[0].country : null;
    },
    /** The nearest town or district in the index, or null. */
    async localityAt(lat, lon) {
      const { rows } = await pool.query(
        `SELECT geo.locality_of(${point('$1', '$2')}) AS locality`, [lat, lon]);
      return (rows[0] && rows[0].locality) || null;
    },
    /** OSM places within `metres`: their names, for the duplicate test. */
    async osmNear(lat, lon, metres) {
      const { rows } = await pool.query(
        `SELECT place_id, display_name, alt_names, name_ar, lat, lon
           FROM geo.place
          WHERE st_dwithin(geog, ${point('$1', '$2')}, $3)
          LIMIT 200`,
        [lat, lon, metres],
      );
      return rows;
    },
    /** OSM points of interest of one of our types within `metres`, nearest first. */
    async osmOfType(type, lat, lon, metres, limit) {
      const { rows } = await pool.query(
        `SELECT p.place_id, p.display_name, p.name_ar, p.locality, l.name_ar AS locality_ar,
                p.lat, p.lon, st_distance(p.geog, ${point('$2', '$3')}, false) AS distance_m
           FROM geo.place p
           LEFT JOIN LATERAL (
                SELECT l2.name_ar FROM geo.place l2
                 WHERE l2.display_name = p.locality AND l2.name_ar IS NOT NULL
                 ORDER BY l2.importance DESC LIMIT 1) l ON true
          WHERE p.kind = 'poi'
            AND st_dwithin(p.geog, ${point('$2', '$3')}, $4)
            AND movin.place_type_of(p.class, p.subclass, p.display_name) = $1
          ORDER BY p.geog <-> ${point('$2', '$3')}
          LIMIT $5`,
        [type, lat, lon, metres, limit],
      );
      return rows;
    },
    /** The Arabic names of these localities, where the index holds one. */
    async localitiesInArabic(names) {
      if (!names.length) return {};
      const { rows } = await pool.query(
        `SELECT DISTINCT ON (display_name) display_name, name_ar
           FROM geo.place
          WHERE display_name = ANY($1) AND name_ar IS NOT NULL
          ORDER BY display_name, importance DESC`,
        [names],
      );
      return Object.fromEntries(rows.map((r) => [r.display_name, r.name_ar]));
    },
  };
}

/* ── POST /places/suggest ──────────────────────────────────────────────── */

async function suggest(env, me, body, res) {
  const name = cleanName(body.name);
  if (!name) return send(res, 400, { error: 'bad_name', min: NAME_MIN, max: NAME_MAX });
  if (!PLACE_TYPES.includes(body.type)) return send(res, 400, { error: 'bad_type' });
  const lat = Number(body.lat);
  const lon = Number(body.lon);
  if (typeof body.lat !== 'number' || typeof body.lon !== 'number'
      || !Number.isFinite(lat) || !Number.isFinite(lon) || !inOurBox(lat, lon)) {
    return send(res, 400, { error: 'bad_position' });
  }

  // The live questions first, outside the transaction: they read the place
  // index, not the table, and a slow one must not hold the person's lock.
  let country;
  let osm;
  let locality = null;
  try {
    country = await env.geo.countryAt(lat, lon);
    if (!country) return send(res, 422, { error: 'outside_service_area' });
    osm = await env.geo.osmNear(lat, lon, DUPLICATE_METRES);
  } catch (err) {
    console.error(`[places] suggest, the place index: ${err.message}`);
    return send(res, 503, { error: 'unavailable' });
  }
  try {
    locality = await env.geo.localityAt(lat, lon);
  } catch (err) {
    // A locality is the second line of a suggestion, not a reason to refuse.
    console.error(`[places] locality ${lat},${lon}: ${err.message}`);
  }

  const key = normalise(name);
  const onTheMap = osm.find((p) => [p.display_name, p.name_ar, ...String(p.alt_names || '').split(/[|;]/)]
    .some((n) => n && normalise(n) === key));
  if (onTheMap) {
    return send(res, 409, {
      error: 'duplicate',
      existing: { placeId: onTheMap.place_id, name: onTheMap.display_name, status: 'on_the_map' },
    });
  }

  const client = await env.pool.connect();
  try {
    await client.query('BEGIN');
    // One person at a time: two taps at once must not both pass the counts.
    await client.query('SELECT pg_advisory_xact_lock(hashtext($1))', [`suggested_place:${me.side}:${me.id}`]);

    const { rows: [n] } = await client.query(
      `SELECT count(*) FILTER (WHERE created_at > now() - interval '24 hours')::int AS today,
              count(*) FILTER (WHERE status = 'pending')::int AS pending
         FROM movin.suggested_place
        WHERE author_side = $1 AND author_id = $2`,
      [me.side, me.id],
    );
    if (n.today >= PER_DAY) {
      await client.query('ROLLBACK');
      return send(res, 429, { error: 'daily_limit', limit: PER_DAY });
    }
    if (n.pending >= MAX_PENDING) {
      await client.query('ROLLBACK');
      return send(res, 429, { error: 'too_many_pending', limit: MAX_PENDING });
    }

    // Proposals near the pin, pending or approved (anyone's): a box first,
    // then the real distance and the name, here.
    const { dLat, dLon } = box(lat, DUPLICATE_METRES);
    const { rows: near } = await client.query(
      `SELECT place_id, name, proposed_name, status, lat, lon
         FROM movin.suggested_place
        WHERE status IN ('pending', 'approved')
          AND lat BETWEEN $1 AND $2 AND lon BETWEEN $3 AND $4`,
      [lat - dLat, lat + dLat, lon - dLon, lon + dLon],
    );
    const twin = near.find((p) => metresBetween(lat, lon, p.lat, p.lon) <= DUPLICATE_METRES
      && (normalise(p.name) === key || normalise(p.proposed_name) === key));
    if (twin) {
      await client.query('ROLLBACK');
      // A pending place is nobody's to see yet: say it is proposed, not where.
      return send(res, 409, {
        error: 'duplicate',
        existing: twin.status === 'approved'
          ? { placeId: twin.place_id, name: twin.name, status: 'approved' }
          : { status: 'pending' },
      });
    }

    const { rows: [row] } = await client.query(
      `INSERT INTO movin.suggested_place
              (proposed_name, proposed_type, name, type, lat, lon, country, locality, author_side, author_id)
       VALUES ($1, $2, $1, $2, $3, $4, $5, $6, $7, $8)
       RETURNING place_id, status, created_at`,
      [name, body.type, lat, lon, country, locality, me.side, me.id],
    );
    await client.query('COMMIT');
    // Ids only: who proposed what is in the table, not in a log line.
    console.log(`[places] ${row.place_id} proposed by ${me.side} ${me.id} (${country}, ${body.type})`);
    return send(res, 201, {
      placeId: row.place_id, status: row.status, name, type: body.type, locality, country,
      createdAt: row.created_at,
    });
  } catch (err) {
    await client.query('ROLLBACK').catch(() => {});
    console.error(`[places] suggest by ${me.side} ${me.id}: ${err.message}`);
    return send(res, 503, { error: 'unavailable' });
  } finally {
    client.release();
  }
}

/* ── GET /places/mine ──────────────────────────────────────────────────── */

async function mine(env, me, res) {
  try {
    const { rows } = await env.pool.query(
      `SELECT place_id, proposed_name, name, type, status, refusal_reason, locality, lat, lon,
              created_at, decided_at
         FROM movin.suggested_place
        WHERE author_side = $1 AND author_id = $2
        ORDER BY created_at DESC, id DESC
        LIMIT $3`,
      [me.side, me.id, MINE_LIMIT],
    );
    return send(res, 200, {
      places: rows.map((r) => ({
        placeId: r.place_id,
        // What search shows; what he typed is kept beside it when the office
        // corrected it, so his screen can say « corrigé ».
        name: r.name,
        proposedName: r.proposed_name,
        type: r.type,
        status: r.status,
        refusalReason: r.refusal_reason,
        locality: r.locality,
        lat: r.lat,
        lon: r.lon,
        createdAt: r.created_at,
        decidedAt: r.decided_at,
      })),
    });
  } catch (err) {
    console.error(`[places] mine for ${me.side} ${me.id}: ${err.message}`);
    return send(res, 503, { error: 'unavailable' });
  }
}

/* ── GET /places/nearby ────────────────────────────────────────────────── */

/** Approved proposals of one type within `metres`: plain SQL, then the distance here. */
async function proposedOfType(pool, type, lat, lon, metres) {
  const { dLat, dLon } = box(lat, metres);
  const { rows } = await pool.query(
    `SELECT place_id, name, name_ar, locality, lat, lon
       FROM movin.suggested_place
      WHERE status = 'approved' AND type = $1
        AND lat BETWEEN $2 AND $3 AND lon BETWEEN $4 AND $5`,
    [type, lat - dLat, lat + dLat, lon - dLon, lon + dLon],
  );
  return rows
    .map((r) => ({ ...r, distance_m: metresBetween(lat, lon, r.lat, r.lon) }))
    .filter((r) => r.distance_m <= metres);
}

async function within(env, type, lat, lon, metres) {
  const [osm, proposed] = await Promise.all([
    env.geo.osmOfType(type, lat, lon, metres, NEARBY_LIMIT),
    proposedOfType(env.pool, type, lat, lon, metres),
  ]);
  const arabic = await env.geo.localitiesInArabic(
    [...new Set(proposed.map((p) => p.locality).filter(Boolean))]);
  const all = [
    ...osm.map((p) => ({
      placeId: p.place_id, name: p.display_name, nameAr: p.name_ar || null,
      locality: p.locality || null, localityAr: p.locality_ar || null,
      lat: p.lat, lon: p.lon, distance: p.distance_m, source: 'map',
    })),
    ...proposed.map((p) => ({
      placeId: p.place_id, name: p.name, nameAr: p.name_ar || null,
      locality: p.locality || null, localityAr: (p.locality && arabic[p.locality]) || null,
      lat: p.lat, lon: p.lon, distance: p.distance_m, source: 'proposed',
    })),
  ];
  return all
    .sort((a, b) => a.distance - b.distance)
    .slice(0, NEARBY_LIMIT)
    .map((p) => ({ ...p, distance: Math.round(p.distance) }));
}

async function nearby(env, params, res) {
  const type = params.get('type');
  if (!PLACE_TYPES.includes(type) || type === 'other') return send(res, 400, { error: 'bad_type' });
  const lat = Number(params.get('lat'));
  const lon = Number(params.get('lon'));
  if (params.get('lat') === null || params.get('lon') === null
      || !Number.isFinite(lat) || !Number.isFinite(lon) || !inOurBox(lat, lon)) {
    return send(res, 400, { error: 'bad_position' });
  }
  try {
    let radius = NEARBY_METRES;
    let results = await within(env, type, lat, lon, radius);
    // Nothing at 5 km: once more, wider (owner, 2026-11-09). Not when there
    // are a few -- a near few beat a far many.
    if (!results.length) {
      radius = NEARBY_WIDER_METRES;
      results = await within(env, type, lat, lon, radius);
    }
    return send(res, 200, { type, radius, results });
  } catch (err) {
    console.error(`[places] nearby ${type} ${lat},${lon}: ${err.message}`);
    return send(res, 503, { error: 'unavailable' });
  }
}

/* ── the router ────────────────────────────────────────────────────────── */

/** The JSON body of a POST, at most BODY_LIMIT bytes; null when it is not JSON. */
function readJson(req) {
  return new Promise((resolve) => {
    const chunks = [];
    let size = 0;
    let over = false;
    req.on('data', (c) => {
      size += c.length;
      if (size > BODY_LIMIT) { over = true; return; }
      chunks.push(c);
    });
    req.on('end', () => {
      if (over) return resolve({ tooLarge: true });
      try {
        const body = JSON.parse(Buffer.concat(chunks).toString('utf8') || 'null');
        resolve(body && typeof body === 'object' && !Array.isArray(body) ? { body } : { body: null });
      } catch {
        resolve({ body: null });
      }
    });
    req.on('error', () => resolve({ body: null }));
  });
}

/**
 * Everything under /places/. `opts`: { pool, riderUrl, driverUrl, geo? } --
 * `geo` is the live database's answers unless a test brings its own.
 */
async function handle(req, res, url, opts) {
  const route = url.pathname.replace(/\/+$/, '');
  const known = { '/places/suggest': 'POST', '/places/mine': 'GET', '/places/nearby': 'GET' };
  if (!known[route]) return send(res, 404, { error: 'no such places route' });
  if (req.method !== known[route]) return send(res, 405, { error: 'method not allowed' });
  if (!opts.pool) return send(res, 503, { error: 'unavailable' });

  const env = { ...opts, geo: opts.geo || liveGeo(opts.pool) };

  // The body is read before the token is checked, so a refused caller's
  // upload is still drained; it is small and capped either way.
  const read = route === '/places/suggest' ? await readJson(req) : null;
  if (read && read.tooLarge) return send(res, 413, { error: 'too_large', limit: BODY_LIMIT });

  const me = await whoIs(env, req.headers.token || '');
  if (!me) return send(res, 401, { error: 'sign in first' });

  if (route === '/places/suggest') {
    if (!read.body) return send(res, 400, { error: 'bad_body' });
    return suggest(env, me, read.body, res);
  }
  if (route === '/places/mine') return mine(env, me, res);
  return nearby(env, url.searchParams, res);
}

module.exports = {
  handle,
  // For the tests, which hold these to the SQL and to each other.
  PLACE_TYPES, NAME_MIN, NAME_MAX, PER_DAY, MAX_PENDING, BODY_LIMIT,
  cleanName, normalise, metresBetween, liveGeo,
};
