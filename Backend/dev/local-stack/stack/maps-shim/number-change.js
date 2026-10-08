'use strict';
/**
 * A person changes their own phone number, and keeps their account (the boss,
 * 2026-10-03).
 *
 * ── Why ─────────────────────────────────────────────────────────────────────
 * There was no way to: signing in with a new number makes a NEW account, and a
 * driver's wallet, documents and acceptance stay on the old one. The boss asked
 * for the person to do it themselves, in « Mon compte », with no office step.
 *
 * ── Who does what ───────────────────────────────────────────────────────────
 * The auth guard proves the NEW number belongs to the caller -- the same code,
 * by the same channels and under the same limits as a sign-in -- and only then
 * calls `apply` here. This file never sees a code. It does the two things the
 * guard cannot: read and write the person row.
 *
 *   POST /internal/number-change   { step: "check"|"apply", side: "rider"|"driver",
 *                                    token, dialCode, number }
 *   loopback only; the edge routes nothing under /internal/.
 *
 * ── The rules ───────────────────────────────────────────────────────────────
 * - The person is whoever the TOKEN belongs to (identity.js) -- never an id
 *   from the request.
 * - Same country only (`WRONG_COUNTRY`). A driver belongs to his country's
 *   merchant, tariff and wallet currency, and a passenger's country is where
 *   the app finds them; moving either is not a change of number.
 * - Never a number another account on that side holds (`NUMBER_TAKEN`), which
 *   is what stops anyone taking an account over by "changing" to its number.
 *   Checked the way the backend looks a person up: country code + hash.
 * - The same number is `SAME_NUMBER`.
 *
 * ── What is written ─────────────────────────────────────────────────────────
 * The three places a number lives on `person`: `mobile_number_encrypted`
 * (passetto, exactly as the backend writes it), `mobile_number_hash`
 * (sha256 of the salt and the number -- what sign-in looks up), and
 * `unencrypted_mobile_number`. Neither backend caches a person (checked at the
 * deployed ref 03a7531), so the next sign-in with the new number finds this
 * account. Sessions are kept: the person who just proved the new number is
 * the one holding the token. One audit row, without either number.
 *
 * Needs `NUMBER_HASH_SALT` (the backend's encHashSalt) and passetto; without
 * either the route answers `not_configured` and nothing is changed.
 */
const crypto = require('crypto');
const identity = require('./identity');
const avatars = require('./avatars');

const PASSETTO_URL = (process.env.PASSETTO_URL || 'http://127.0.0.1:8021').replace(/\/$/, '');
const SALT = process.env.NUMBER_HASH_SALT || '';

const SCHEMA = { rider: 'atlas_app', driver: 'atlas_driver_offer_bpp' };

const hashOf = (number) => crypto.createHash('sha256').update(SALT + number).digest();

async function encrypt(number) {
  const r = await fetch(`${PASSETTO_URL}/encrypt`, {
    method: 'POST',
    headers: { 'content-type': 'application/json' },
    // Haskell's `show` of the text: what the backend sends, so what it reads.
    body: JSON.stringify({ value: `S${JSON.stringify(number)}` }),
    signal: AbortSignal.timeout(5000),
  });
  if (!r.ok) throw new Error(`passetto ${r.status}`);
  const { value } = await r.json();
  if (typeof value !== 'string' || !value.includes('|')) throw new Error('passetto: no value');
  return value;
}

/**
 * @returns {Promise<{ok: true} | {ok: false, error: string}>}
 */
async function run(pool, { riderUrl, driverUrl }, body) {
  const step = body && body.step;
  const side = body && body.side;
  const dialCode = body && typeof body.dialCode === 'string' ? body.dialCode : '';
  const number = body && typeof body.number === 'string' ? body.number : '';
  if (!['check', 'apply'].includes(step) || !SCHEMA[side]) return { ok: false, error: 'bad_request' };
  if (!/^\+\d{1,4}$/.test(dialCode) || !/^\d{6,12}$/.test(number)) return { ok: false, error: 'bad_request' };
  if (!pool || !SALT) return { ok: false, error: 'not_configured' };

  const who = side === 'driver'
    ? await identity.driverFromToken(driverUrl, body.token)
    : await identity.riderFromToken(riderUrl, body.token);
  if (!who) return { ok: false, error: 'not_signed_in' };

  const schema = SCHEMA[side];
  const me = await pool.query(
    `SELECT mobile_country_code AS cc, unencrypted_mobile_number AS num,
            encode(mobile_number_hash, 'hex') AS h
       FROM ${schema}.person WHERE id = $1`, [who.id]);
  if (!me.rows[0]) return { ok: false, error: 'not_signed_in' };
  const { cc, num, h: oldHash } = me.rows[0];

  if (cc && cc !== dialCode) return { ok: false, error: 'WRONG_COUNTRY' };
  if (num === number) return { ok: false, error: 'SAME_NUMBER' };

  const hash = hashOf(number);
  const taken = await pool.query(
    `SELECT 1 FROM ${schema}.person
      WHERE mobile_country_code = $1 AND mobile_number_hash = $2 AND id <> $3 LIMIT 1`,
    [dialCode, hash, who.id]);
  if (taken.rows.length) return { ok: false, error: 'NUMBER_TAKEN' };

  if (step === 'check') return { ok: true };

  const encrypted = await encrypt(number);

  // One transaction: the person and, for a passenger, the record her ratings
  // live on. Half of it -- the number changed, the ratings left on the old
  // one -- is exactly the bug this exists to prevent.
  const client = await pool.connect();
  try {
    await client.query('BEGIN');
    const done = await client.query(
      `UPDATE ${schema}.person
          SET mobile_country_code = $1, mobile_number_encrypted = $2, mobile_number_hash = $3,
              unencrypted_mobile_number = $4, updated_at = now()
        WHERE id = $5`,
      [dialCode, encrypted, hash, number, who.id]);
    if (done.rowCount !== 1) {
      await client.query('ROLLBACK');
      return { ok: false, error: 'not_signed_in' };
    }
    if (side === 'rider') await moveRiderDetails(client, oldHash, hash.toString('hex'), dialCode, encrypted);
    await client.query('COMMIT');
  } catch (e) {
    await client.query('ROLLBACK').catch(() => {});
    throw e;
  } finally {
    client.release();
  }

  // A passenger's photograph is keyed by her number's hash (avatars.js), so it
  // follows her to the new one -- or it is gone from her profile. A driver's
  // is keyed by his id and stays where it is.
  if (side === 'rider') {
    try {
      avatars.moveRiderKey(oldHash, hash.toString('hex'));
    } catch (e) {
      console.error(`[number-change] photograph not moved: ${e.message}`);
    }
  }

  // Who and when, never what: neither number goes into the trail.
  await pool.query(
    `INSERT INTO movin.admin_audit (user_id, actor_email, action, subject)
     VALUES (NULL, 'self (app)', 'number.change', $1)`,
    [`${side} ${who.id}`]).catch((e) => console.error(`[number-change] audit: ${e.message}`));
  console.log(`[number-change] ${side} ${who.id.slice(0, 8)} changed number`);
  return { ok: true };
}

/**
 * What drivers made of her follows her to the new number (the owner,
 * 2026-10-03: a changed number showed « Nouveau »).
 *
 * Her ratings are not on her person row: drivers rate a passenger through
 * the provider binary, which keeps them on `atlas_driver_offer_bpp.
 * rider_details`, one row per number (unique on hash + country code), and
 * the provider finds that row by the number's hash on every booking. So the
 * row is re-pointed at the new number, keeping its id -- her past bookings
 * stay linked, and her next ride with the new number lands on the same row
 * and the same ratings. The encrypted copy is passetto's, as the backend
 * writes it, so the provider can still read it.
 *
 * If the new number already has a row of its own (rides taken on it before),
 * the two are added together -- count and sum, and the average recomputed
 * from them exactly as the provider does -- and that row is set aside
 * (hash cleared) so the unique index holds. Nothing is deleted: bookings
 * point at both.
 */
async function moveRiderDetails(db, oldHashHex, newHashHex, countryCode, encrypted) {
  if (!oldHashHex || oldHashHex === newHashHex) return 0;
  const mine = await db.query(
    `SELECT id, total_ratings, total_rating_score FROM atlas_driver_offer_bpp.rider_details
      WHERE mobile_number_hash = decode($1, 'hex') AND mobile_country_code = $2 FOR UPDATE`,
    [oldHashHex, countryCode]);
  const row = mine.rows[0];
  if (!row) return 0; // never rated, never ridden: nothing to carry
  const theirs = await db.query(
    `SELECT id, total_ratings, total_rating_score FROM atlas_driver_offer_bpp.rider_details
      WHERE mobile_number_hash = decode($1, 'hex') AND mobile_country_code = $2 FOR UPDATE`,
    [newHashHex, countryCode]);
  const other = theirs.rows[0];
  let count = Number(row.total_ratings || 0);
  let score = Number(row.total_rating_score || 0);
  if (other) {
    count += Number(other.total_ratings || 0);
    score += Number(other.total_rating_score || 0);
    await db.query(
      `UPDATE atlas_driver_offer_bpp.rider_details
          SET mobile_number_hash = NULL, mobile_number_encrypted = 'merged:' || id, updated_at = now()
        WHERE id = $1`, [other.id]);
  }
  await db.query(
    `UPDATE atlas_driver_offer_bpp.rider_details
        SET mobile_number_hash = decode($1, 'hex'), mobile_number_encrypted = $2,
            total_ratings = $3, total_rating_score = $4,
            rating = CASE WHEN $3::int > 0 THEN $4::float / $3::int ELSE rating END,
            updated_at = now()
      WHERE id = $5`,
    [newHashHex, encrypted, count, score, row.id]);
  return count;
}

module.exports = { run, hashOf, moveRiderDetails };
