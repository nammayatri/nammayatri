'use strict';
/**
 * Places proposed by passengers and drivers (« Lieux proposés », day 2,
 * 2026-11-09), and « search by type ». Everything under /places/:
 *
 *   POST   /places/suggest   {name, type, lat, lon}   propose a missing place
 *   GET    /places/mine                               my proposals and their state
 *   GET    /places/nearby?type=&lat=&lon=             places of one type around me
 *   PUT    /places/photos/u<id>/<1|2>   (the image)   a photo of my proposal (day 4)
 *   DELETE /places/photos/u<id>/<1|2>                 take it back
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
 *
 * ── The photos (day 4, 2026-11-12) ─────────────────────────────────────────
 * One or two per proposal, optional, for the office to check the place by;
 * nobody else sees them. The phone sends the proposal first, then each photo
 * on its own (a failed photo never costs the proposal). The rules:
 *   * only the AUTHOR, by the token. Somebody else's place answers 404, the
 *     same as a place that does not exist: a pending place is nobody else's
 *     to know about;
 *   * only while the place WAITS: once decided, 409 `decided`. The office
 *     decided on what it saw;
 *   * a JPEG or a PNG, recognised by its first bytes. The Content-Type is
 *     not read (avatars.js, 2026-09-27: a header is a claim, not a check);
 *   * PHOTO_MAX_BYTES, 512 KB, counted while reading; the edge refuses more
 *     before it reaches us (location /places/photos/, 512k).
 * A PUT replaces the photo of that number; both PUT and DELETE move the
 * place's `updated_at`, which is the version the console's decision must
 * name -- so the office cannot approve a place whose photos changed after
 * its page was loaded: it is told the place changed, and sees the new ones.
 */
const identity = require('./identity');
const { send } = require('./reply');
const { arabicLocalities } = require('./places');

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
/** 512 KB: the table's suggested_place_photo_size, and the edge's 512k. */
const PHOTO_MAX_BYTES = 512 * 1024;
/** /places/photos/u<id>/<1|2>. 18 digits at most: always a bigint. */
const PHOTO_ROUTE = /^\/places\/photos\/u([1-9][0-9]{0,17})\/([12])$/;
/** What an image's first bytes are, per type: the table's suggested_place_photo_is_its_type. */
const PHOTO_MAGIC = [
  ['image/jpeg', Buffer.from([0xff, 0xd8, 0xff])],
  ['image/png', Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a])],
];

/** The box around both countries -- the table's suggested_place_in_our_box CHECK. */
const inOurBox = (lat, lon) => lat >= 14 && lat <= 38 && lon >= -18 && lon <= 12.5;

/* ── names ─────────────────────────────────────────────────────────────── */

// Bidi overrides and isolates: invisible, and able to make a name READ
// differently in the console from what it is. Nothing a place name needs.
const BIDI_CONTROLS = /[‪-‮⁦-⁩‎‏]/g;
// Characters that print as nothing: zero-width space, word joiner, byte-order
// mark, soft hyphen. Removed, so they neither count towards the two-character
// minimum nor make two names that look alike differ. (ZWNJ and ZWJ stay:
// some Arabic-script spellings and emoji need them.)
const INVISIBLE = /[\u200B\u2060\uFEFF\u00AD]/g;

/**
 * A typed name as it will be stored, or null if it cannot be one.
 *
 * NFKC first: Arabic presentation forms (U+FB50-U+FEFF) render like ordinary
 * letters and match nothing typed on a keyboard -- docs/maps.md, the Arabic
 * names. Then every run of whitespace (newlines included) becomes one space,
 * and the ends are trimmed: the table's CHECK refuses an untrimmed name, so a
 * friendly cleanup here beats a 500 there. A name must hold at least one
 * letter or digit: « ---- », or zero-width spaces alone, name nothing. Length in code points, as
 * Postgres's char_length counts them -- an emoji is one, not two.
 */
function cleanName(raw) {
  if (typeof raw !== 'string') return null;
  const name = raw.normalize('NFKC').replace(BIDI_CONTROLS, '').replace(INVISIBLE, '')
    .replace(/\s+/gu, ' ').trim();
  if (/\p{Cc}/u.test(name) || !/[\p{L}\p{N}]/u.test(name)) return null;
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

/** The person behind a token, driver or passenger: identity.js, the one copy. */
const whoIs = (env, token) => identity.whoIs(env.driverUrl, env.riderUrl, token);

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
    /**
     * OSM points of interest of one of our types within `metres`, nearest
     * first. NO Arabic locality here: the first version joined it per row,
     * and the live probe of 2026-11-09 measured 12 s for cafés within 5 km of
     * Algiers -- the join ran for every café in the radius (hundreds), each a
     * scan of the whole index (display_name has no index), before the LIMIT.
     * The caller asks localitiesInArabic once, for the 20 kept.
     */
    async osmOfType(type, lat, lon, metres, limit) {
      const { rows } = await pool.query(
        `SELECT p.place_id, p.display_name, p.name_ar, p.locality,
                p.lat, p.lon, st_distance(p.geog, ${point('$2', '$3')}, false) AS distance_m
           FROM geo.place p
          WHERE p.kind = 'poi'
            AND st_dwithin(p.geog, ${point('$2', '$3')}, $4)
            AND movin.place_type_of(p.class, p.subclass, p.display_name) = $1
          ORDER BY p.geog <-> ${point('$2', '$3')}
          LIMIT $5`,
        [type, lat, lon, metres, limit],
      );
      return rows;
    },
    /** The Arabic names of these localities: places.js's arabicLocalities, ONE query, the labels route's rule. */
    localitiesInArabic(names) {
      return arabicLocalities(pool, names);
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
      `SELECT sp.place_id, sp.proposed_name, sp.name, sp.type, sp.status, sp.refusal_reason,
              sp.locality, sp.lat, sp.lon, sp.created_at, sp.decided_at,
              ARRAY(SELECT ph.n::int FROM movin.suggested_place_photo ph
                     WHERE ph.suggested_place_id = sp.id ORDER BY ph.n) AS photos
         FROM movin.suggested_place sp
        WHERE sp.author_side = $1 AND sp.author_id = $2
        ORDER BY sp.created_at DESC, sp.id DESC
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
        // The numbers of the photos the office has (A2): what is missing is
        // what « Réessayer » may still send while the place waits. Never the
        // photos themselves: they are for the office (A3).
        photos: r.photos,
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
  // The Arabic names of the localities, ONE query for all of them -- map
  // places and proposals alike -- rather than one per place (see osmOfType).
  const arabic = await env.geo.localitiesInArabic(
    [...new Set([...osm, ...proposed].map((p) => p.locality).filter(Boolean))]);
  const all = [
    ...osm.map((p) => ({
      placeId: p.place_id, name: p.display_name, nameAr: p.name_ar || null,
      locality: p.locality || null, localityAr: (p.locality && arabic[p.locality]) || null,
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

/* ── PUT / DELETE /places/photos/u<id>/<1|2> ───────────────────────────── */

/** 'image/jpeg', 'image/png', or null: what the bytes are, whatever the header said. */
function photoType(bytes) {
  const hit = PHOTO_MAGIC.find(([, magic]) =>
    bytes.length > magic.length && bytes.subarray(0, magic.length).equals(magic));
  return hit ? hit[0] : null;
}

/** The raw body, at most `limit` bytes; past it the rest is drained, not kept. */
function readBytes(req, limit) {
  return new Promise((resolve) => {
    const chunks = [];
    let size = 0;
    req.on('data', (c) => {
      size += c.length;
      if (size <= limit) chunks.push(c);
    });
    req.on('end', () => resolve(size > limit ? { tooLarge: true } : { bytes: Buffer.concat(chunks) }));
    req.on('error', () => resolve({ bytes: Buffer.alloc(0) }));
  });
}

async function photo(env, me, method, id, n, bytes, res) {
  const client = await env.pool.connect();
  const stop = async (code, body) => {
    await client.query('ROLLBACK');
    return send(res, code, body);
  };
  try {
    await client.query('BEGIN');
    // Locked, as the console's decision locks it: a photo and a decision
    // cannot cross -- one waits for the other, and then sees its result.
    const { rows: [place] } = await client.query(
      `SELECT id, place_id, status FROM movin.suggested_place
        WHERE id = $1 AND author_side = $2 AND author_id = $3
          FOR UPDATE`,
      [id, me.side, me.id],
    );
    if (!place) return await stop(404, { error: 'no_such_place' });
    if (place.status !== 'pending') return await stop(409, { error: 'decided', status: place.status });

    let changed;
    let type = null;
    if (method === 'PUT') {
      type = photoType(bytes);
      if (!bytes.length) return await stop(400, { error: 'no_image' });
      if (!type) return await stop(415, { error: 'not_an_image' });
      await client.query(
        `INSERT INTO movin.suggested_place_photo (suggested_place_id, n, content_type, bytes)
         VALUES ($1, $2, $3, $4)
         ON CONFLICT (suggested_place_id, n)
           DO UPDATE SET content_type = EXCLUDED.content_type, bytes = EXCLUDED.bytes, created_at = now()`,
        [place.id, n, type, bytes],
      );
      changed = true;
    } else {
      const del = await client.query(
        'DELETE FROM movin.suggested_place_photo WHERE suggested_place_id = $1 AND n = $2', [place.id, n]);
      changed = del.rowCount > 0;
    }
    // The console's version: an office page loaded before this is now stale.
    if (changed) await client.query('UPDATE movin.suggested_place SET updated_at = now() WHERE id = $1', [place.id]);
    await client.query('COMMIT');
    console.log(`[places] ${place.place_id} photo ${n} ${method === 'PUT' ? `stored (${type}, ${bytes.length} B)` : changed ? 'deleted' : 'was not there'} by ${me.side} ${me.id}`);
    return method === 'PUT'
      ? send(res, 200, { placeId: place.place_id, n, type, bytes: bytes.length })
      : send(res, 200, { placeId: place.place_id, n, deleted: changed });
  } catch (err) {
    await client.query('ROLLBACK').catch(() => {});
    console.error(`[places] photo ${n} of u${id} by ${me.side} ${me.id}: ${err.message}`);
    return send(res, 503, { error: 'unavailable' });
  } finally {
    client.release();
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
  if (route.startsWith('/places/photos/')) return handlePhoto(req, res, route, opts);
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

/** /places/photos/u<id>/<1|2>: the path, the method, the body, THEN the token. */
async function handlePhoto(req, res, route, opts) {
  const m = PHOTO_ROUTE.exec(route);
  if (!m) return send(res, 404, { error: 'no such places route' });
  if (req.method !== 'PUT' && req.method !== 'DELETE') return send(res, 405, { error: 'method not allowed' });
  if (!opts.pool) return send(res, 503, { error: 'unavailable' });
  // Read (capped) before the token, as /places/suggest is: a refused upload
  // is drained either way.
  const read = req.method === 'PUT' ? await readBytes(req, PHOTO_MAX_BYTES) : { bytes: Buffer.alloc(0) };
  if (read.tooLarge) return send(res, 413, { error: 'too_large', limit: PHOTO_MAX_BYTES });
  const me = await whoIs(opts, req.headers.token || '');
  if (!me) return send(res, 401, { error: 'sign in first' });
  return photo(opts, me, req.method, m[1], Number(m[2]), read.bytes, res);
}

module.exports = {
  handle,
  // For the tests, which hold these to the SQL and to each other.
  PLACE_TYPES, NAME_MIN, NAME_MAX, PER_DAY, MAX_PENDING, BODY_LIMIT, PHOTO_MAX_BYTES, photoType,
  cleanName, normalise, metresBetween, liveGeo,
};
