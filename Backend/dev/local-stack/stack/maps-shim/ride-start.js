'use strict';
/**
 * The ride's start code, for the auth guard only (day 6, point 4,
 * 2026-11-15: the boss's « the ride code goes »).
 *
 * ── Why the code is filled in rather than switched off ──────────────────────
 * The driver server we run (upstream 03a7531) compares the four digits the
 * driver sends with `ride.otp` and refuses a mismatch (StartRide.hs:
 * `when (driverReq.rideOtp /= ride.otp) $ throwError IncorrectOTP`). Upstream's
 * « ride without a code » (`enableOtpLessRide`) came after that ref: there is
 * no setting to change. Every driver request already passes through the auth
 * guard, which applies the wallet rule there; it now also puts the right code
 * into « start ». The day the backend is rebuilt for another reason, this
 * becomes one line removed from StartRide.hs.
 *
 * ── The one question, and who may ask it ────────────────────────────────────
 *   POST /internal/ride-start-code  {token, rideId}
 *     200 {code}                        the ride is his, and not started
 *     404 {error: 'no_such_ride'}       not his, not NEW, or no such ride
 *     401 {error: 'sign in first'}      the token is nobody's
 *
 * Loopback only (server.js), like /internal/number-change: the guard is the
 * one caller and runs on the host network; the edge routes nothing under
 * /internal/. The code goes back to the guard and from it to the driver
 * server, never to a phone: the guard does not put it in its answer.
 *
 * ── The identity is never an input ──────────────────────────────────────────
 * The driver is whoever the driver server says the token belongs to
 * (identity.driverFromToken), and the ride must be HIS: another driver's ride
 * id, even a real one, answers like a ride that does not exist. And only a
 * ride that has not started (status NEW, the only one StartRide.hs accepts):
 * a code is never handed out for a ride where it can no longer serve.
 */
const identity = require('./identity');

const RIDE_ID = /^[0-9a-f-]{36}$/i;

/**
 * The start code of `rideId`, if `token` is the driver of that ride and it
 * has not started. `{ status, body }` for server.js to send.
 */
async function startCode(pool, driverUrl, token, rideId) {
  if (!pool) return { status: 503, body: { error: 'unavailable' } };
  if (typeof rideId !== 'string' || !RIDE_ID.test(rideId)) return { status: 400, body: { error: 'bad_ride' } };
  const driver = await identity.driverFromToken(driverUrl, typeof token === 'string' ? token : '');
  if (!driver) return { status: 401, body: { error: 'sign in first' } };
  const { rows } = await pool.query(
    `SELECT otp FROM atlas_driver_offer_bpp.ride
      WHERE id = $1 AND driver_id = $2 AND status = 'NEW'`,
    [rideId, driver.id],
  );
  const code = rows[0] && typeof rows[0].otp === 'string' ? rows[0].otp.trim() : '';
  if (!code) return { status: 404, body: { error: 'no_such_ride' } };
  return { status: 200, body: { code } };
}

module.exports = { startCode };
