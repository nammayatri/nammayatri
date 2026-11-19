'use strict';
//
// What a ride would cost, when no driver of that type is near (the boss, 2026-10-22).
//
// ── Why the shim computes a price at all ────────────────────────────────────
// The provider prices a search only for the vehicle types it found a driver
// for. On the binary we run, Search.hs builds the driver pool first and keeps
// the fare policies whose variant is in it (`zipMatched farePolicies
// driverPool`): with nobody of that type online the estimate is not wrong, it
// is ABSENT, and screen 8 had nothing to show but « no driver ». The newer
// upstream switch for this (`considerDriversForSearch`) is not in our binary.
//
// The tariff is two tables and the formula is a few lines, so it is reproduced
// here from the same rows, for the same route distance the provider would use:
//
//   min = dead_km_fare + round(base_distance_fare)
//         + round((distance - base_distance_meters) / 1000 * per_extra_km_fare)
//                                             (SharedLogic/FareCalculator/Calculator.hs,
//                                              calculateFareParameters, baseFareSum)
//   max = min + the cap: restricted_extra_fare's band with the greatest
//         min_trip_distance <= distance, else fare_policy.driver_max_extra_fee
//                                             (CachedQueries/FarePolicy/FarePolicy.hs,
//                                              getUpdatedFarePolicy: the list is
//                                              ordered DESC and the first match wins)
//
// `round` is Haskell's: half to even. Checked against real receipts in Algiers,
// Voiture: 7.12 km -> 215 + 356 = 571 DA; 3.3 km -> 215 + 165 = 380 DA.
//
// ── What it is not ──────────────────────────────────────────────────────────
// A preview, not a quote. There is no estimate behind it and nothing can be
// ordered from it: the app shows it marked « estimé » beside « no driver right
// now » and offers to search again. It declines (404) rather than guess when
// a night multiplier other than 1 is set (none is: the multiplier's window is
// read in IST on this binary, and copying that would copy a bug).

const { send } = require('./reply');

const MERCHANT_OF_REGION = {
  Mauritania: 'favorit0-0000-0000-0000-00000favorit',
  Algeria: 'algeria0-0000-0000-0000-00000algeria',
};

const VARIANTS = new Set(['SEDAN', 'SUV', 'HATCHBACK', 'AUTO_RICKSHAW']);

/** Metres. A longer "trip" is not one this service sells. */
const MAX_DISTANCE = 300000;

/** Haskell's `round`: to the nearest integer, a half to the even neighbour. */
function roundHalfEven(x) {
  const floor = Math.floor(x);
  const diff = x - floor;
  if (Math.abs(diff - 0.5) < 1e-9) return floor % 2 === 0 ? floor : floor + 1;
  return Math.round(x);
}

/** The two numbers, from one fare_policy row and its cap. Pure: tested on its own. */
function priceOf(policy, cap, distance) {
  const extraMetres = distance - policy.base_m;
  const extraKmFare = extraMetres > 0 ? roundHalfEven((extraMetres / 1000) * policy.per_km) : 0;
  const min = roundHalfEven(policy.dead) + roundHalfEven(policy.base) + extraKmFare;
  const ceiling = cap === null || cap === undefined ? policy.max_extra : cap;
  return { min, max: min + roundHalfEven(ceiling || 0) };
}

const REGION_SQL = `select region from atlas_app.geometry
  where region in ('Algeria', 'Mauritania')
    and st_contains(geom, st_point($2::float8, $1::float8))
  limit 1`;

const POLICY_SQL = `select base_distance_fare::float8   as base,
         coalesce(base_distance_meters, 0)::float8  as base_m,
         per_extra_km_fare::float8                  as per_km,
         coalesce(dead_km_fare, 0)::float8          as dead,
         night_shift_rate::float8                   as night,
         coalesce(driver_max_extra_fee, 0)::float8  as max_extra
    from atlas_driver_offer_bpp.fare_policy
   where merchant_id = $1 and vehicle_variant = $2`;

const CAP_SQL = `select driver_max_extra_fare::float8 as extra
    from atlas_driver_offer_bpp.restricted_extra_fare
   where merchant_id = $1 and vehicle_variant = $2 and min_trip_distance <= $3
   order by min_trip_distance desc
   limit 1`;

/**
 * GET /fleet/fare-preview?lat=&lon=&distance=&variant=
 *
 * `lat`/`lon` the pickup (its country decides the merchant, as the provider's
 * geofence does), `distance` the route's metres from the search, `variant` the
 * server's own word (SEDAN, SUV, HATCHBACK, AUTO_RICKSHAW).
 * 200 {min, max} | 400 bad input | 401 not signed in | 404 nothing to price.
 */
async function farePreview({ url, res, pool, isSignedIn }) {
  if (!pool) return send(res, 503, { error: 'no database' });
  if (!(await isSignedIn())) return send(res, 401, { error: 'sign in first' });

  const lat = Number(url.searchParams.get('lat'));
  const lon = Number(url.searchParams.get('lon'));
  const distance = Number(url.searchParams.get('distance'));
  const variant = url.searchParams.get('variant') || '';
  if (!Number.isFinite(lat) || !Number.isFinite(lon) || Math.abs(lat) > 90 || Math.abs(lon) > 180) {
    return send(res, 400, { error: 'lat and lon are required' });
  }
  if (!Number.isFinite(distance) || distance <= 0 || distance > MAX_DISTANCE) {
    return send(res, 400, { error: 'distance must be metres, above 0' });
  }
  if (!VARIANTS.has(variant)) return send(res, 400, { error: 'unknown variant' });

  try {
    const region = (await pool.query(REGION_SQL, [lat, lon])).rows[0]?.region;
    const merchant = MERCHANT_OF_REGION[region];
    if (!merchant) return send(res, 404, { error: 'not in a served country' });

    const policies = (await pool.query(POLICY_SQL, [merchant, variant])).rows;
    // One row per merchant and variant (dedupe-seed.sql's unique index); two
    // would mean the provider picks one we cannot know, so say nothing.
    if (policies.length !== 1) return send(res, 404, { error: 'no tariff for this vehicle' });
    const policy = policies[0];
    if (policy.night !== null && policy.night !== 1) {
      return send(res, 404, { error: 'night tariff: not previewed' });
    }

    const capRow = (await pool.query(CAP_SQL, [merchant, variant, Math.round(distance)])).rows[0];
    return send(res, 200, priceOf(policy, capRow ? capRow.extra : null, Math.round(distance)));
  } catch (e) {
    console.error('[fare-preview] query failed:', e.message);
    return send(res, 500, { error: 'query failed' });
  }
}

module.exports = { farePreview, priceOf, roundHalfEven };
