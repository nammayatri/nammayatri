'use strict';
// The place index, answered in Google's legacy shapes: autocomplete, place
// details, Arabic labels, reverse geocoding. Moved out of server.js
// unchanged in phase 5 (2026-10-06); tests/maps-shim-routes.test.js holds
// the answers -- and the SQL -- it must keep.

const { send } = require('./reply');
const { onEarth } = require('./directions');

const SEARCH_LIMIT = Number(process.env.SEARCH_LIMIT || 8);

/** The place index's pool, handed over by server.js at start-up (null: no search). */
let pool = null;
const usePool = (p) => { pool = p; };

// ═══════════════════════════════════════════════════════════ search ════════
//
// Three more endpoints the backend calls, all of them the *legacy* Google Web
// Service shapes rather than the new Places API -- confirmed by logging what
// actually goes past, not by reading documentation:
//
//   GET /place/autocomplete/json ?input= &location=lat,lng &radius= &language=
//   GET /place/details/json      ?place_id= &fields=
//   GET /geocode/json            ?latlng=lat,lng   (reverse)
//
// Two of the three were broken before this: mock-google implements the *new*
// autocomplete API and no place details at all, so both answered 500, and its
// reverse geocoder replies with an address in Karnataka whatever you ask it.
//
// The answers now come from geo.place -- 113,341 named things extracted from
// the same Algeria .osm.pbf that feeds OSRM and the tiles.

// Google's legacy `types` vocabulary. Nothing downstream switches on it today,
// but sending our own words in a Google-shaped field would be a trap for
// whoever looks next.
const GOOGLE_TYPES = {
  place:     ['locality', 'political', 'geocode'],
  street:    ['route', 'geocode'],
  transport: ['transit_station', 'point_of_interest', 'establishment'],
  poi:       ['point_of_interest', 'establishment'],
};

const typesFor = (kind) => GOOGLE_TYPES[kind] || ['point_of_interest', 'establishment'];

// The country every address is signed with.
//
// Was hardcoded 'Algérie' / 'DZ' in two places until 2026-09-03, when the
// pilot moved and a pickup in Tevragh Zeina came back as "F-Nord, Algérie" on
// a map drawing Nouakchott streets. Config now, because this is the second
// country and there should not be a third code change.
//
// The app strips this off the end of a display line before showing it
// (`locality()` in lib/places.ts), so the two have to agree -- a mismatch does
// not error, it just leaves the country on screen.
// The backend caps autocomplete at eight predictions, measured across every
// query tried, so a phone has no honest reason to ask for more than a screenful
// at once. The cap is here because this route is reachable from the internet
// and `ids` is a caller-supplied list.
const LABEL_LIMIT = Number(process.env.LABEL_LIMIT || 25);

const COUNTRY_NAME = process.env.COUNTRY_NAME || 'Mauritanie';
const COUNTRY_CODE = process.env.COUNTRY_CODE || 'MR';

// The one string the rider actually reads. The rider-app's AutoCompleteResp
// carries a single `description` -- there is no main/secondary pair anywhere in
// the chain -- so the locality has to be part of it.
const describe = (row) =>
  row.locality ? `${row.display_name}, ${row.locality}` : row.display_name;

// Which of our countries a point is in -- since 2026-09-13 there are two, and
// the single COUNTRY_NAME above signed an Algiers pickup "Cherarba, Mauritanie".
// Answered from the same service areas the rider app itself serves from
// (atlas_app.geometry, SRID 0 like the rows), so an address and serviceability
// can never disagree about the country. COUNTRY_NAME is now only the fallback.
const COUNTRY_OF_REGION = {
  Algeria: { name: 'Algérie', code: 'DZ' },
  Mauritania: { name: 'Mauritanie', code: 'MR' },
};
const FALLBACK_COUNTRY = { name: COUNTRY_NAME, code: COUNTRY_CODE };

async function countryAt(lat, lon) {
  try {
    const { rows } = await pool.query(
      `select region from atlas_app.geometry
        where region in ('Algeria', 'Mauritania')
          and st_contains(geom, st_point($2::float8, $1::float8))
        limit 1`,
      [lat, lon],
    );
    return (rows[0] && COUNTRY_OF_REGION[rows[0].region]) || FALLBACK_COUNTRY;
  } catch (err) {
    // A label with the fallback country beats no label at all.
    console.error(`[country] ${lat},${lon}: ${err.message}`);
    return FALLBACK_COUNTRY;
  }
}

function addressComponents(row, country = FALLBACK_COUNTRY) {
  const parts = [{
    long_name: row.display_name,
    short_name: row.display_name,
    types: typesFor(row.kind),
  }];
  if (row.locality) {
    parts.push({ long_name: row.locality, short_name: row.locality, types: ['locality', 'political'] });
  }
  parts.push({ long_name: country.name, short_name: country.code, types: ['country', 'political'] });
  return parts;
}

const formatAddress = (row, country = FALLBACK_COUNTRY) =>
  [row.display_name, row.locality, country.name].filter(Boolean).join(', ');

// "36.7538,3.0588" -> [36.7538, 3.0588]
function parseLatLng(raw) {
  if (!raw) return null;
  const [lat, lng] = String(raw).split(',').map((n) => parseFloat(n.trim()));
  return onEarth(lat, lng) ? [lat, lng] : null;
}

/**
 * The longest search worth running. A place name is a few words; a longer
 * input is a paste or an attack on the trigram index, and is cut here rather
 * than refused, so a real search with a long tail still finds something.
 */
const MAX_INPUT = 100;

async function autocomplete(query, res) {
  const input = (query.get('input') || '').trim().slice(0, MAX_INPUT);
  // The backend always sends `location`; it is a required field on its own
  // request type. The fallback is only so a hand-typed curl still works.
  const centre = parseLatLng(query.get('location')) || [36.7538, 3.0588];

  if (input.length < 2) return send(res, 200, { status: 'ZERO_RESULTS', predictions: [] });

  let rows;
  try {
    ({ rows } = await pool.query(
      'select place_id, display_name, locality, kind, distance_m from geo.search($1, $2, $3, $4)',
      [input, centre[0], centre[1], SEARCH_LIMIT],
    ));
  } catch (err) {
    console.error(`[autocomplete] "${input}": ${err.message}`);
    return send(res, 200, { status: 'UNKNOWN_ERROR', predictions: [] });
  }

  console.log(`[autocomplete] "${input}" -> ${rows.length}`);
  send(res, 200, {
    status: rows.length ? 'OK' : 'ZERO_RESULTS',
    predictions: rows.map((row) => ({
      description: describe(row),
      place_id: row.place_id,
      distance_meters: Math.round(row.distance_m),
      types: typesFor(row.kind),
    })),
  });
}

/**
 * Arabic labels for places the phone already has.
 *
 * ── Why this route exists at all ────────────────────────────────────────────
 * The obvious design is `language=ar` on autocomplete, and it cannot work. The
 * chain is app -> rider-app -> here, and the deployed rider-app binary settles
 * it twice over (checked 2026-09-10, not assumed):
 *
 *   1. `language` is a five-value enum -- ENGLISH HINDI KANNADA TAMIL MALAYALAM.
 *      Sending ARABIC is refused with a 400 before anything reaches us.
 *   2. Its Google client's whole query-parameter table is `sessiontoken place
 *      components fields latlng geocode distancematrix alternatives directions
 *      autocomplete`. There is no `language` in it. The backend validates the
 *      field and then drops it, so even a hijacked enum value would never
 *      arrive.
 *
 * Adding it means rebuilding the backend: 45 minutes, new binaries, and every
 * measurement in this project re-proved against them. So the app asks us
 * directly instead, with the place_ids the backend just handed it, and swaps
 * the labels in. Reversible by deleting one nginx location.
 *
 * ── What it deliberately does not do ────────────────────────────────────────
 * It never invents a name. A place with no `name_ar` is simply absent from the
 * answer and the phone keeps the French, which is today's behaviour and is
 * always readable. Half the country has no Arabic name in OSM and a
 * transliteration produced here would be a guess presented as data.
 *
 * The locality is looked up by name because `geo.place.locality` is text, not a
 * key -- it is filled from a display_name at index time. Where the locality has
 * no Arabic name the French one stays, so a suggestion can read "شارع دبي,
 * Nouadhibou". Mixed, and better than dropping the half that tells two
 * identically-named streets apart.
 */
async function placeLabels(query, res) {
  // One language for now. Answering an unknown one with an empty map rather
  // than an error keeps a future third language from breaking this build.
  const lang = (query.get('lang') || 'ar').toLowerCase();
  const ids = (query.get('ids') || '')
    .split(',')
    .map((s) => s.trim())
    .filter(Boolean)
    .slice(0, LABEL_LIMIT);

  if (lang !== 'ar' || !ids.length) return send(res, 200, { status: 'OK', labels: {} });

  let rows;
  try {
    ({ rows } = await pool.query(
      `select p.place_id,
              p.name_ar,
              p.locality,
              l.name_ar as locality_ar
         from geo.place p
         left join lateral (
              select l2.name_ar
                from geo.place l2
               where l2.display_name = p.locality
                 and l2.name_ar is not null
               order by l2.importance desc
               limit 1
              ) l on true
        where p.place_id = any($1)
          and p.name_ar is not null`,
      [ids],
    ));
  } catch (err) {
    // Never an error to the phone. A label lookup that fails must cost the
    // rider nothing more than seeing the French name he would have seen
    // anyway.
    console.error(`[labels] ${ids.length} ids: ${err.message}`);
    return send(res, 200, { status: 'UNKNOWN_ERROR', labels: {} });
  }

  const labels = {};
  for (const row of rows) {
    const where = row.locality_ar || row.locality;
    labels[row.place_id] = where ? `${row.name_ar}, ${where}` : row.name_ar;
  }
  console.log(`[labels] ${ids.length} asked -> ${rows.length} in ${lang}`);
  send(res, 200, { status: 'OK', labels });
}

async function placeDetails(query, res) {
  const placeId = query.get('place_id');
  if (!placeId) return send(res, 200, { status: 'INVALID_REQUEST' });

  let rows;
  try {
    ({ rows } = await pool.query(
      'select place_id, display_name, locality, kind, lat, lon from geo.place where place_id = $1',
      [placeId],
    ));
  } catch (err) {
    console.error(`[details] ${placeId}: ${err.message}`);
    return send(res, 200, { status: 'UNKNOWN_ERROR' });
  }

  if (!rows.length) {
    // Only reachable if the index was rebuilt from a different extract: place
    // ids are derived from OSM identity precisely so they survive a rebuild.
    console.warn(`[details] unknown place_id ${placeId}`);
    return send(res, 200, { status: 'ZERO_RESULTS' });
  }

  const row = rows[0];
  const country = await countryAt(row.lat, row.lon);
  send(res, 200, {
    status: 'OK',
    result: {
      place_id: row.place_id,
      formatted_address: formatAddress(row, country),
      address_components: addressComponents(row, country),
      geometry: { location: { lat: row.lat, lng: row.lon } },
    },
  });
}

async function reverseGeocode(query, res) {
  const at = parseLatLng(query.get('latlng'));
  const placeId = query.get('place_id');

  let rows;
  try {
    if (at) {
      ({ rows } = await pool.query('select * from geo.reverse($1, $2)', [at[0], at[1]]));
    } else if (placeId) {
      ({ rows } = await pool.query(
        'select place_id, display_name, locality, kind, lat, lon from geo.place where place_id = $1',
        [placeId],
      ));
    } else {
      return send(res, 200, { status: 'INVALID_REQUEST', results: [] });
    }
  } catch (err) {
    console.error(`[geocode] ${query.get('latlng') || placeId}: ${err.message}`);
    return send(res, 200, { status: 'UNKNOWN_ERROR', results: [] });
  }

  if (!rows.length) return send(res, 200, { status: 'ZERO_RESULTS', results: [] });

  const row = rows[0];
  // The country of the point asked about (the dropped pin), not of the feature
  // matched near it -- a pin by a border names the country it is standing in.
  const country = await countryAt(at ? at[0] : row.lat, at ? at[1] : row.lon);
  console.log(`[geocode] ${at ? at.join(',') : placeId} -> ${row.display_name}, ${country.name}`);
  send(res, 200, {
    status: 'OK',
    results: [{
      place_id: row.place_id,
      formatted_address: formatAddress(row, country),
      address_components: addressComponents(row, country),
      // The point that was asked about, not the feature's own node. The caller
      // is naming a pin the rider dropped; moving it to the centre of the
      // school we matched would make the pin jump under their finger.
      geometry: { location: { lat: at ? at[0] : row.lat, lng: at ? at[1] : row.lon } },
    }],
  });
}

module.exports = { usePool, autocomplete, placeDetails, placeLabels, reverseGeocode };
