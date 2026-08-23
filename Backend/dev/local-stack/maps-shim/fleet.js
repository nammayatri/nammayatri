'use strict';
//
// Who is near the passenger, and what are they driving.
//
// ── Why this exists at all ──────────────────────────────────────────────────
// The client asked, three times, for the passenger to see the actual cars his
// request will reach -- the model, the colour, the driver -- instead of a count.
// The rider API cannot answer it and it is not a matter of digging harder:
//
//     EstimateAPIEntity.driversLatLong :: [LatLong]      -- {lat, lon}, nothing
//     DriverPoolResult { driverId, variant, lat, lon }   -- no vehicle either
//
// So the data never reaches the rider app, and inside the BPP the dispatch pool
// does not carry a vehicle in the first place. Fixing it properly means widening
// the pool, the BECKN payload and the rider entity -- Haskell in two services
// plus a rebuild.
//
// It is all sitting in one database, joinable in one query. Measured: driver
// name, rating, make, model, colour, plate, variant, position and its age, with
// positions eleven seconds old. So this serves it, the same way this shim
// already serves geocoding from the place index. **No Haskell, no rebuild** --
// /app is a read-only bind mount, so a restart is the whole deployment.
//
// ── What this is NOT ────────────────────────────────────────────────────────
// It is a *display* list, not the dispatch decision. The BPP still chooses who
// actually receives the request, from the same rows with the same filters. The
// two agree because they read the same table and apply the same rules, not
// because one drives the other, and the screen says "reçoivent votre demande"
// rather than claiming this list is the pool.
//
// ── What is deliberately withheld ───────────────────────────────────────────
// The plate. It is in the join and it is not returned: a signed-in rider could
// otherwise walk the map and enumerate every plate in the fleet, and no
// ride-hailing app shows one before a driver has accepted. Screen 11 already
// shows it at the right moment, once the car is genuinely coming.
//
// The year is not returned either, for the duller reason that there is no
// column for it anywhere -- the driver app collects it and the backend drops it.

const MAX_DRIVERS = 20;

/**
 * How stale a position may be and still count as "near you".
 *
 * Five minutes, matching what the passenger side already assumes: without a
 * foreground service Android batches location to roughly that, so anything
 * tighter would hide drivers who are genuinely there and driving.
 */
const FRESH_SECONDS = 300;

/** Metres. Beyond this nobody is "near", whatever the fleet size. */
const DEFAULT_RADIUS = 8000;

/**
 * The join, with the dispatch pool's own filters.
 *
 * `active AND NOT blocked AND NOT on_ride` is what makes a driver reachable —
 * the same three the BPP checks — so this list cannot advertise somebody who
 * could never answer. Distance is computed in SQL so the ordering and the
 * limit are the database's job rather than being done on the whole fleet in
 * Node.
 *
 * `earth_distance` is not assumed to exist: this uses the haversine spelled
 * out, which needs no extension and is exact enough at city scale.
 */
const SQL = `
  SELECT p.id                                                AS id,
         p.first_name                                        AS name,
         p.rating,
         v.make,
         v.model,
         v.color                                             AS colour,
         v.variant,
         l.lat,
         l.lon,
         (6371000 * acos(
            least(1, greatest(-1,
              cos(radians($1)) * cos(radians(l.lat)) *
              cos(radians(l.lon) - radians($2)) +
              sin(radians($1)) * sin(radians(l.lat))
            ))
         ))::int                                             AS metres
    FROM atlas_driver_offer_bpp.driver_location l
    JOIN atlas_driver_offer_bpp.person p
      ON p.id = l.driver_id
    JOIN atlas_driver_offer_bpp.vehicle v
      ON v.driver_id = l.driver_id
    JOIN atlas_driver_offer_bpp.driver_information di
      ON di.driver_id = l.driver_id
   WHERE di.active
     AND NOT di.blocked
     AND NOT di.on_ride
     AND l.updated_at > now() - ($3 || ' seconds')::interval
     AND ($4::text IS NULL OR v.variant = $4)
  ORDER BY metres
   LIMIT $5
`;

function send(res, code, body) {
  const json = JSON.stringify(body);
  res.writeHead(code, {
    'Content-Type': 'application/json; charset=utf-8',
    'Content-Length': Buffer.byteLength(json),
    // Positions move. A cached answer would show a driver who has left.
    'Cache-Control': 'no-store',
  });
  res.end(json);
}

/**
 * Prove the caller is a signed-in passenger before naming anybody.
 *
 * The shim has no session of its own, so it asks the rider app: a token that
 * can read its own profile is a token that belongs to a real account. One extra
 * call per request, and this endpoint is hit once per search rather than per
 * frame.
 *
 * Without this the endpoint would hand every driver's name, car and position to
 * anyone who found the URL.
 */
async function riderIsSignedIn(riderUrl, token) {
  if (!token) return false;
  try {
    const r = await fetch(`${riderUrl}/v2/profile`, { headers: { token } });
    return r.ok;
  } catch {
    return false;
  }
}

/**
 * GET /fleet/nearby?lat=&lon=&variant=&radius=
 *
 * `variant` is the server's own vocabulary — SEDAN, SUV, HATCHBACK,
 * AUTO_RICKSHAW — because that is what dispatch matches on. The app's four
 * names map onto it in lib/vehicle.ts and nowhere else.
 */
async function nearby({ url, res, pool, riderUrl, token }) {
  if (!pool) return send(res, 503, { error: 'no database' });

  if (!(await riderIsSignedIn(riderUrl, token))) {
    return send(res, 401, { error: 'sign in first' });
  }

  const lat = Number(url.searchParams.get('lat'));
  const lon = Number(url.searchParams.get('lon'));
  if (!Number.isFinite(lat) || !Number.isFinite(lon)) {
    return send(res, 400, { error: 'lat and lon are required' });
  }

  const variant = url.searchParams.get('variant') || null;
  const radius = Number(url.searchParams.get('radius')) || DEFAULT_RADIUS;

  let rows;
  try {
    const q = await pool.query(SQL, [lat, lon, FRESH_SECONDS, variant, MAX_DRIVERS]);
    rows = q.rows;
  } catch (e) {
    // Loud in the log, quiet on the wire: the passenger gets an empty list and
    // the screen falls back to the count it already had.
    console.error('[fleet] query failed:', e.message);
    return send(res, 500, { error: 'query failed' });
  }

  send(res, 200, {
    drivers: rows
      .filter((r) => r.metres <= radius)
      .map((r) => ({
        // The handle the passenger's choice is made of. Safe to hand out in a
        // way the plate is not: it is a UUID, it identifies nobody to someone
        // who does not already have it, and every driver endpoint still wants
        // that driver's own token. The plate is a thing you read off a car in
        // the street, which is exactly why it waits until one is coming.
        id: r.id,
        name: (r.name || '').trim() || null,
        // A driver nobody has rated is not a driver rated zero. Null travels.
        rating: r.rating === null ? null : Number(r.rating),
        make: (r.make || '').trim() || null,
        model: (r.model || '').trim() || null,
        colour: (r.colour || '').trim() || null,
        variant: r.variant,
        lat: Number(r.lat),
        lon: Number(r.lon),
        metres: r.metres,
      })),
  });
}

module.exports = { nearby };
