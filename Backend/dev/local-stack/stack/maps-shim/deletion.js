/**
 * Account deletion requests — recorded here, carried out by a person.
 *
 * Google Play requires a way to request deletion from INSIDE the app, and the
 * deployed backend has no deletion route: `deleteAccount`, `deleteProfile` and
 * `account/delete` are all absent from both running binaries, checked inside
 * the container rather than on the host (the host has no `strings`, and a
 * missing tool returns the same zero a real absence does). So this records the
 * request and the admin site works the queue.
 *
 * Nothing in this file deletes anything. Screens 21–23 say so in those words.
 *
 * ── The route takes no id ───────────────────────────────────────────────────
 * Same rule as subscription.js and the avatar fix: the caller sends a token and
 * nothing else, and the id comes from asking the backend whose token it is.
 * There is no request shape here that could delete somebody else's account,
 * because there is nowhere to put their id.
 *
 * ── The in-ride check is written backwards on purpose ───────────────────────
 * It asks whether a ride is NOT finished, rather than whether it is in one of
 * the in-flight states. Only COMPLETED and CANCELLED exist in our data today,
 * so enumerating the active names would be guesswork — and a guess that missed
 * one would produce a check that never fires, which is worse than no check
 * because it looks like one. Written this way, a status nobody here has seen
 * refuses the deletion, which is the safe direction.
 */
const { driverFromToken, riderFromToken } = require('./identity');

const TERMINAL = ['COMPLETED', 'CANCELLED'];

function send(res, code, body) {
  const payload = JSON.stringify(body);
  res.writeHead(code, {
    'content-type': 'application/json; charset=utf-8',
    'content-length': Buffer.byteLength(payload),
  });
  res.end(payload);
}

/**
 * Who is calling, and which of their two possible accounts this is.
 *
 * `side` comes from the token that was accepted, never from the body: a client
 * that could name its own side could ask the rider backend to vouch for a
 * driver id.
 */
async function whoIs(env, token) {
  if (!token) return null;
  const driver = await driverFromToken(env.DRIVER_URL, token);
  if (driver) return { side: 'driver', id: driver.id };
  const rider = await riderFromToken(env.RIDER_URL, token);
  if (rider) return { side: 'rider', id: rider.id };
  return null;
}

/** The phone number on the account, for the office to match a caller to a row. */
async function phoneOf(pool, side, personId) {
  const table = side === 'driver' ? 'atlas_driver_offer_bpp.person' : 'atlas_app.person';
  try {
    const { rows } = await pool.query(
      `SELECT coalesce(unencrypted_mobile_number, '') AS phone FROM ${table} WHERE id = $1`,
      [personId],
    );
    return rows[0]?.phone || null;
  } catch {
    // Never fatal: a missing number costs the office a lookup, and refusing the
    // request over it would block somebody from leaving.
    return null;
  }
}

/** True when this account has a ride that has not reached a terminal status. */
async function hasActiveRide(pool, side, personId) {
  try {
    if (side === 'driver') {
      const { rows } = await pool.query(
        `SELECT 1
           FROM atlas_driver_offer_bpp.ride
          WHERE driver_id = $1 AND status <> ALL($2::text[])
          LIMIT 1`,
        [personId, TERMINAL],
      );
      if (rows.length) return true;
      // The flag the dispatcher itself keeps. Belt and braces: a ride row can
      // lag, and this is the value the rest of the backend trusts.
      const flag = await pool.query(
        `SELECT on_ride FROM atlas_driver_offer_bpp.driver_information WHERE driver_id = $1`,
        [personId],
      );
      return flag.rows[0]?.on_ride === true;
    }

    const { rows } = await pool.query(
      `SELECT 1
         FROM atlas_app.ride r
         JOIN atlas_app.booking b ON b.id = r.booking_id
        WHERE b.rider_id = $1
          AND r.status <> ALL($2::text[])
        LIMIT 1`,
      [personId, TERMINAL],
    );
    return rows.length > 0;
  } catch (err) {
    // A failed check must not silently allow the thing it was guarding.
    console.error(`[deletion] active-ride check failed for ${side} ${personId}: ${err.message}`);
    return true;
  }
}

async function openRequest(pool, side, personId) {
  const { rows } = await pool.query(
    `SELECT requested_at, delete_by
       FROM movin.deletion_request
      WHERE side = $1 AND person_id = $2 AND status = 'open'`,
    [side, personId],
  );
  return rows[0] || null;
}

/** GET — what screen 21 draws when it opens. */
async function status(pool, env, token, res) {
  const who = await whoIs(env, token);
  if (!who) return send(res, 401, { error: 'unauthenticated' });

  const open = await openRequest(pool, who.side, who.id);
  if (open) {
    return send(res, 200, {
      state: 'pending',
      requestedAt: open.requested_at,
      deleteBy: open.delete_by,
    });
  }
  if (await hasActiveRide(pool, who.side, who.id)) {
    return send(res, 200, { state: 'blocked', blocker: 'active_ride' });
  }
  return send(res, 200, { state: 'none' });
}

/**
 * POST — record it.
 *
 * The partial unique index does the concurrency work: two taps on a slow
 * connection cannot both insert, and the second one reads back the first
 * rather than erroring. `ON CONFLICT DO NOTHING` plus a re-read, not
 * `RETURNING` — inside a conflicting insert there is no row to return.
 */
async function request(pool, env, token, body, res) {
  const who = await whoIs(env, token);
  if (!who) return send(res, 401, { error: 'unauthenticated' });

  const existing = await openRequest(pool, who.side, who.id);
  if (existing) {
    return send(res, 409, { ok: false, blocker: 'already_requested' });
  }
  if (await hasActiveRide(pool, who.side, who.id)) {
    return send(res, 409, { ok: false, blocker: 'active_ride' });
  }

  const reason =
    typeof body?.reason === 'string' && body.reason.trim() ? body.reason.trim().slice(0, 400) : null;
  const phone = await phoneOf(pool, who.side, who.id);

  await pool.query(
    `INSERT INTO movin.deletion_request (side, person_id, phone, reason)
          VALUES ($1, $2, $3, $4)
     ON CONFLICT DO NOTHING`,
    [who.side, who.id, phone, reason],
  );

  const row = await openRequest(pool, who.side, who.id);
  if (!row) return send(res, 500, { error: 'not recorded' });

  console.log(`[deletion] ${who.side} ${who.id} requested, act by ${row.delete_by.toISOString()}`);
  return send(res, 200, {
    ok: true,
    requestedAt: row.requested_at,
    deleteBy: row.delete_by,
  });
}

/**
 * DELETE — withdraw it.
 *
 * The thirty-day wait exists partly for this. An account that cannot be saved
 * during the wait makes the wait pointless, so the row is marked `withdrawn`
 * rather than removed — the office should be able to see that somebody asked
 * and changed their mind.
 */
async function withdraw(pool, env, token, res) {
  const who = await whoIs(env, token);
  if (!who) return send(res, 401, { error: 'unauthenticated' });

  const { rowCount } = await pool.query(
    `UPDATE movin.deletion_request
        SET status = 'withdrawn', handled_at = now(), handled_by = 'self'
      WHERE side = $1 AND person_id = $2 AND status = 'open'`,
    [who.side, who.id],
  );
  console.log(`[deletion] ${who.side} ${who.id} withdrew (${rowCount} row)`);
  return send(res, 200, { ok: rowCount > 0 });
}

module.exports = { status, request, withdraw };
