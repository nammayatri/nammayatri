'use strict';
/**
 * A passenger's own star rating, for her own profile screen.
 *
 * ── Why the rider app cannot simply ask its own backend ─────────────────────
 * The client asked on 2026-08-25 for the passenger to see her rating where she
 * sees her name. The obvious route is the rider API, and it does not have it:
 * `GET /v2/profile` was measured against the live edge the same day and returns
 * exactly eight fields — firstName, middleName, lastName, id, maskedEmail,
 * maskedMobileNumber, maskedDeviceToken, whatsappNotificationEnrollStatus. No
 * rating, no average, nothing.
 *
 * And the number does not live on that side of the house anyway. A driver rates
 * his passenger through `POST /ui/driver/ride/{rideId}/rateCustomer`, which our
 * own patch added to the **provider** binary, and it writes
 * `atlas_driver_offer_bpp.rider_details.rating` — see passenger-rating.sql.
 * `atlas_app.person.rating` exists but belongs to the rider binary's own notion
 * of a person and is never written for a passenger.
 *
 * Bridging the two inside Haskell means a field on a response type, which means
 * a rebuild and new binaries — and every measurement in this project was taken
 * against the current ones. Bridging them here is one query.
 *
 * ── The join, and why it is the same one avatars.js already trusts ──────────
 * The two schemas share a database and agree on the phone-number hash: all 13
 * provider-side riders matched a rider-side person on it exactly, measured
 * 2026-08-24. So a number given by the app finds the person row, and the hash
 * on that row finds the rider_details row the driver's rating was written to.
 *
 * The last-nine-digits comparison is not laziness, it is the fix for a real
 * defect: the app holds a bare NSN (`550123456`) and the database writes the
 * trunk zero (`0550123456`). An equality test finds nothing and returns
 * nothing, silently — which is exactly how every passenger avatar uploaded
 * fine and was never found once. Nine digits is the whole of an Algerian
 * subscriber number.
 *
 * ── Never a 404 for "nobody has rated her" ─────────────────────────────────
 * An unrated passenger is the normal state on the day this ships — the route
 * that writes ratings went live yesterday — so it answers 200 with a null
 * rating. A 404 would be indistinguishable from a broken network in the app,
 * and the screen would show a permanent error for a perfectly ordinary
 * passenger.
 */

/** Same shape as everything else here: JSON, always, with a status. */
function send(res, status, body) {
  const text = JSON.stringify(body);
  res.writeHead(status, {
    'content-type': 'application/json',
    'content-length': Buffer.byteLength(text),
    // Her own screen, read on arrival. Short enough that a rating given during
    // a ride shows up on the next visit rather than tomorrow.
    'cache-control': 'private, max-age=30',
  });
  res.end(text);
}

/**
 * GET /rating/phone/{number} -> {"rating": 4.5, "total": 3}
 *
 * `rating` is null and `total` is 0 for a passenger nobody has rated yet. The
 * app must render that as "Nouveau", never as zero stars — a zero is a verdict
 * somebody gave, and nobody gave it.
 */
async function serveForPhone(pool, phone, res) {
  const digits = String(phone || '').replace(/\D/g, '');
  if (!pool || digits.length < 9) return send(res, 200, { rating: null, total: 0 });
  /* `rides` is deliberately absent here: it exists for the passenger's offer
     list, which is about drivers. A passenger's own ride count is on her own
     backend, and nothing has asked for it. */

  try {
    const q = await pool.query(
      `SELECT rd.rating, rd.total_ratings
         FROM atlas_app.person p
         JOIN atlas_driver_offer_bpp.rider_details rd
           ON rd.mobile_number_hash = p.mobile_number_hash
        WHERE right(p.unencrypted_mobile_number, 9) = right($1, 9)
        -- One passenger can have a rider_details row per merchant. There is one
        -- merchant in this pilot, so this only ever picks between duplicates;
        -- the most-rated row is the right one to pick if that ever changes.
        ORDER BY rd.total_ratings DESC NULLS LAST
        LIMIT 1`,
      [digits],
    );
    const row = q.rows[0];
    if (!row || row.rating === null || row.rating === undefined) {
      return send(res, 200, { rating: null, total: 0 });
    }
    return send(res, 200, {
      rating: Number(row.rating),
      total: Number(row.total_ratings || 0),
    });
  } catch (e) {
    console.error('[rating] lookup', e.message);
    // Same answer as "not rated". The alternative is a 500 on a settings
    // screen, and the rating is the least important thing on it.
    return send(res, 200, { rating: null, total: 0 });
  }
}

/**
 * GET /rating/driver/{driverId} -> {"rating": 4.5, "total": 6, "rides": 148}
 *
 * ── Why the driver's own API cannot answer any of this ──────────────────────
 * `GET /ui/driver/profile` returns his average and not how many people gave
 * it. Measured rather than assumed: the deployed driver binary does not contain
 * the string `totalRatings` **anywhere** — `grep -a` over the executable finds
 * `totalEarnings` and nothing else of that shape — so there is no field to read
 * and no version of that response that has one.
 *
 * A driver has no running count either, unlike a passenger: his average is
 * rebuilt from the `rating` table by `calculateAverageRating`, so the count is
 * a `count(*)` over the same rows. See passenger-rating.sql for why passengers
 * were given three columns instead.
 *
 * ── `rides` is for the passenger's offer list, not for him ──────────────────
 * Added 2026-08-25: the client asked that a passenger choosing between offers
 * see how many rides each driver has completed. Nothing about the offer carries
 * it, and adding a sixth field to `vehicleDesc` would be a backend rebuild for
 * one integer. It is a `count(*)` over his COMPLETED rides.
 *
 * **Counted here and never inferred from the rating count.** They are different
 * numbers and the gap between them is the interesting part: most rides are
 * never rated, so a driver with 148 rides and 6 ratings is normal, and showing
 * 6 as his experience would badly undersell him.
 *
 * ── rating null does not mean rides zero ────────────────────────────────────
 * The early return this used to have bailed out whenever `rating` was null,
 * which would have hidden the ride count of every driver nobody has rated yet
 * — the exact drivers a passenger most needs a second number for.
 */
async function serveForDriver(pool, driverId, res) {
  const none = { rating: null, total: 0, rides: 0 };
  if (!pool || !/^[0-9a-fA-F-]{8,64}$/.test(String(driverId || ''))) {
    return send(res, 200, none);
  }
  try {
    const q = await pool.query(
      `SELECT p.rating,
              (SELECT count(*) FROM atlas_driver_offer_bpp.rating r
                WHERE r.driver_id = p.id)                            AS total,
              (SELECT count(*) FROM atlas_driver_offer_bpp.ride rd
                WHERE rd.driver_id = p.id AND rd.status = 'COMPLETED') AS rides
         FROM atlas_driver_offer_bpp.person p
        WHERE p.id = $1`,
      [String(driverId)],
    );
    const row = q.rows[0];
    if (!row) return send(res, 200, none);

    return send(res, 200, {
      // Null stays null. A driver nobody has rated is "Nouveau", never zero.
      rating: row.rating === null || row.rating === undefined ? null : Number(row.rating),
      total: Number(row.total || 0),
      rides: Number(row.rides || 0),
    });
  } catch (e) {
    console.error('[rating] driver lookup', e.message);
    return send(res, 200, none);
  }
}

module.exports = { serveForPhone, serveForDriver };
