'use strict';
/**
 * Profile photographs, for both sides of the app.
 *
 * ── Why this is here and not in the backend ─────────────────────────────────
 * There is nowhere in the Haskell to put an image. Measured 2026-08-24: all 46
 * `/ui/` routes on the driver binary, and not one accepts an image for a
 * person; `atlas_app.person` has no image, photo, picture or avatar column at
 * all. The only two image routes that exist anywhere forward the picture to
 * **Idfy in India** — the routes the client banned outright, and the reason the
 * licence and the carte grise already live on the driver's own phone.
 *
 * So the backend never learns what an image is. It carries the driver's **id**
 * on the offer, and that is the whole of its involvement. This file stores the
 * bytes and hands them back.
 *
 * ── Keys, and why there are two kinds ───────────────────────────────────────
 * A driver is keyed by his person id, which the passenger's app now receives on
 * every offer. Simple.
 *
 * A passenger cannot be: her app knows her own phone number and nothing else,
 * and the driver's app knows a ride id and nothing about her. So her photograph
 * is stored under a **hash of her phone number**, and the driver's app asks for
 * it by ride id — this file joins ride -> booking -> rider_details and reads
 * the phone-number hash the backend already stores there. Nothing decrypts
 * anything, and no phone number is ever returned to the driver.
 *
 * ── What it deliberately does not do ────────────────────────────────────────
 * No authentication on GET. An avatar is shown to the person on the other end
 * of a ride either way, and the ids are opaque UUIDs and one-way hashes — there
 * is nothing here to enumerate that is not already on the other party's screen.
 * PUT is a different matter: it takes the caller's token and asks the backend
 * whose it is, exactly as `fleet.js` does, so nobody can write a face onto
 * somebody else's profile.
 */
const fs = require('fs');
const path = require('path');
const crypto = require('crypto');

/** Where the bytes live. A bind-mounted volume, so a container swap keeps them. */
const DIR = process.env.AVATAR_DIR || '/data/avatars';

/**
 * 512 KB. The apps send a square JPEG at quality 0.8, which measures 15-40 KB;
 * this is a bound on abuse rather than a target, and it is small enough that a
 * full store of every driver in the pilot is a few megabytes.
 */
const MAX_BYTES = 512 * 1024;

/** Only what both apps actually produce. A store that accepts anything is a file drop. */
const TYPES = { 'image/jpeg': '.jpg', 'image/png': '.png' };

function ensureDir() {
  try {
    fs.mkdirSync(DIR, { recursive: true });
    return true;
  } catch (e) {
    console.error('[avatars] cannot create', DIR, e.message);
    return false;
  }
}

/**
 * A phone number to the key its photograph is stored under.
 *
 * ── Looked up, not computed, and that is the whole trick ────────────────────
 * The obvious design hashes the number here with the same salt the backend
 * uses. That salt is compiled into the binary's config and is not ours to know,
 * and guessing it wrong fails silently — every passenger avatar would store
 * fine and never be found.
 *
 * So nothing is hashed. The rider's own row already holds both halves:
 * `atlas_app.person.unencrypted_mobile_number` (populated for all 62 riders,
 * measured 2026-08-24) beside `mobile_number_hash`. Read the hash the backend
 * itself wrote, and use that as the key.
 *
 * The two databases agree on it, which is what makes the driver's side work:
 * all 13 provider-side riders match a rider-side person on that hash exactly.
 * So a photograph stored under a hash found on the BAP is found again from
 * `rider_details` on the BPP, with nothing shared between them but the number.
 */
async function keyForPhone(pool, phone) {
  const digits = String(phone || '').replace(/\D/g, '');
  if (!pool || !digits) return null;
  try {
    // ── Matched on the last nine digits, and that is a bug fix ──────────────
    // The app stores a rider's number as the bare NSN -- nine digits, no trunk
    // zero -- and the database writes it back with the zero: `550123456` here
    // against `0550123456` there. An equality test finds nothing, returns a
    // null key, and `publishAvatar` then does nothing at all *without failing*.
    // Every passenger photograph uploaded fine and was never found. Nine digits
    // is the whole of an Algerian subscriber number, so this compares what both
    // sides actually agree on.
    const q = await pool.query(
      `SELECT encode(mobile_number_hash, 'hex') AS h
         FROM atlas_app.person
        WHERE right(unencrypted_mobile_number, 9) = right($1, 9)
        LIMIT 1`,
      [digits],
    );
    const h = q.rows[0] && q.rows[0].h;
    return h ? 'h_' + String(h).slice(0, 32) : null;
  } catch (e) {
    console.error('[avatars] phone lookup', e.message);
    return null;
  }
}

function driverKey(id) {
  return /^[0-9a-fA-F-]{8,64}$/.test(String(id || '')) ? 'd_' + String(id) : null;
}

function findFile(key) {
  for (const ext of ['.jpg', '.png']) {
    const p = path.join(DIR, key + ext);
    if (fs.existsSync(p)) return p;
  }
  return null;
}

/**
 * GET /avatar/plate/{registrationNo} — the driver's photograph, by his car.
 *
 * The offer screen has his person id and can ask for him directly. Every screen
 * *after* the booking does not: the rider's own API returns a name, a rating, a
 * model, a colour and a **plate**, and no id anywhere. Rather than thread an id
 * through the whole booking chain — which is BECKN, and a rebuild — the plate
 * is resolved here. One column, one index, already unique per driver.
 */
async function serveForPlate(plate, pool, res) {
  const wanted = String(plate || '').replace(/\s+/g, '').toUpperCase();
  if (!pool || !wanted) {
    res.writeHead(404, { 'content-type': 'application/json' });
    return res.end('{"error":"no avatar"}');
  }
  try {
    // Spaces are how a plate is written and not how it is always stored, so
    // both sides are stripped before comparing.
    const q = await pool.query(
      `SELECT driver_id
         FROM atlas_driver_offer_bpp.vehicle
        WHERE upper(replace(registration_no, ' ', '')) = $1
        LIMIT 1`,
      [wanted],
    );
    const id = q.rows[0] && q.rows[0].driver_id;
    if (!id) {
      res.writeHead(404, { 'content-type': 'application/json' });
      return res.end('{"error":"no avatar"}');
    }
    return serve(driverKey(String(id).trim()), res);
  } catch (e) {
    console.error('[avatars] plate lookup', e.message);
    res.writeHead(404, { 'content-type': 'application/json' });
    return res.end('{"error":"no avatar"}');
  }
}

/** GET /avatar/{key} — the bytes, or 404. */
function serve(key, res) {
  const file = key && findFile(key);
  if (!file) {
    res.writeHead(404, { 'content-type': 'application/json' });
    return res.end('{"error":"no avatar"}');
  }
  const type = file.endsWith('.png') ? 'image/png' : 'image/jpeg';
  const bytes = fs.readFileSync(file);
  res.writeHead(200, {
    'content-type': type,
    'content-length': bytes.length,
    // Short, so replacing a photograph is visible within the minute, and long
    // enough that a list of offers does not refetch the same face per row.
    'cache-control': 'public, max-age=60',
  });
  res.end(bytes);
}

/**
 * GET /avatar/ride/{rideId} — the passenger's photograph, for the driver.
 *
 * Joins to the phone-number hash the provider already stores. The driver never
 * receives the number, only the picture.
 */
async function serveForRide(rideId, pool, res) {
  if (!pool || !/^[0-9a-fA-F-]{8,64}$/.test(String(rideId || ''))) {
    res.writeHead(404, { 'content-type': 'application/json' });
    return res.end('{"error":"no avatar"}');
  }
  try {
    const q = await pool.query(
      `SELECT encode(rd.mobile_number_hash, 'hex') AS h
         FROM atlas_driver_offer_bpp.ride r
         JOIN atlas_driver_offer_bpp.booking b ON b.id = r.booking_id
         JOIN atlas_driver_offer_bpp.rider_details rd ON rd.id = b.rider_id
        WHERE r.id = $1
        LIMIT 1`,
      [rideId],
    );
    const h = q.rows[0] && q.rows[0].h;
    if (!h) {
      res.writeHead(404, { 'content-type': 'application/json' });
      return res.end('{"error":"no avatar"}');
    }
    return serve('h_' + String(h).slice(0, 32), res);
  } catch (e) {
    console.error('[avatars] ride lookup', e.message);
    res.writeHead(404, { 'content-type': 'application/json' });
    return res.end('{"error":"no avatar"}');
  }
}

/**
 * PUT /avatar/{key} — store one.
 *
 * The body is the raw image. Refused above MAX_BYTES *while reading*, not
 * after: a size check that runs once the whole thing is in memory is not a
 * limit, it is a description of what already happened.
 */
function store(key, req, res) {
  if (!key) {
    res.writeHead(400, { 'content-type': 'application/json' });
    return res.end('{"error":"bad key"}');
  }
  const ext = TYPES[String(req.headers['content-type'] || '').split(';')[0].trim()];
  if (!ext) {
    res.writeHead(415, { 'content-type': 'application/json' });
    return res.end('{"error":"jpeg or png only"}');
  }
  if (!ensureDir()) {
    res.writeHead(500, { 'content-type': 'application/json' });
    return res.end('{"error":"store unavailable"}');
  }

  const chunks = [];
  let size = 0;
  let aborted = false;
  req.on('data', (c) => {
    if (aborted) return;
    size += c.length;
    if (size > MAX_BYTES) {
      aborted = true;
      res.writeHead(413, { 'content-type': 'application/json' });
      res.end('{"error":"too large"}');
      req.destroy();
      return;
    }
    chunks.push(c);
  });
  req.on('end', () => {
    if (aborted) return;
    try {
      // One file per person: replacing a photograph must not leave the old one
      // behind, and the two extensions mean a JPEG can replace a PNG.
      for (const e of ['.jpg', '.png']) {
        const old = path.join(DIR, key + e);
        if (fs.existsSync(old)) fs.unlinkSync(old);
      }
      fs.writeFileSync(path.join(DIR, key + ext), Buffer.concat(chunks));
      res.writeHead(200, { 'content-type': 'application/json' });
      res.end(JSON.stringify({ ok: true, bytes: size }));
    } catch (e) {
      console.error('[avatars] write', e.message);
      res.writeHead(500, { 'content-type': 'application/json' });
      res.end('{"error":"write failed"}');
    }
  });
}

/** DELETE /avatar/{key} — back to the initial. */
function remove(key, res) {
  if (key) {
    for (const e of ['.jpg', '.png']) {
      const p = path.join(DIR, key + e);
      try {
        if (fs.existsSync(p)) fs.unlinkSync(p);
      } catch {
        /* already gone */
      }
    }
  }
  res.writeHead(200, { 'content-type': 'application/json' });
  res.end('{"ok":true}');
}

module.exports = {
  DIR,
  keyForPhone,
  driverKey,
  serve,
  serveForRide,
  serveForPlate,
  store,
  remove,
  ensureDir,
};
