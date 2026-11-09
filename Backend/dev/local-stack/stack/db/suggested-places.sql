-- Mova — places proposed by passengers and drivers (« Lieux proposés »).
--
-- The boss, 2026-11-08: a passenger or a driver proposes a place the map does
-- not have -- a name, a type, a pin. It waits as `pending`; the office edits,
-- approves or refuses it in the console; an approved place is found in search
-- by everyone, in both countries. The same list of types drives « search by
-- type » (café, pharmacie, mosquée…) over the OpenStreetMap places too.
-- The plan: docs/maps.md, « Proposed places ».
--
-- This file holds two things:
--   1. movin.place_type_of(class, subclass, name) -- which of our types an
--      OpenStreetMap place is, from the tag the place index kept;
--   2. movin.suggested_place -- the proposals, and their current state.
--
-- ── Why here, in stack/db/, and not beside the place index ─────────────────
-- A release applies stack/db/*.sql that is new or changed, and nothing else
-- (ops/release-remote.py, `plan['sql']`). Nothing applies geocoder/*.sql: those
-- are run by hand on a map rebuild. A mapping written there would work on a
-- laptop and simply not exist on the server, with nothing to say so. Here a
-- release installs it, a map rebuild (which DROPS geo.place) cannot erase it,
-- and the nightly backup carries it with the rest of `movin`.
--
-- ── Why `movin` and not `geo` ───────────────────────────────────────────────
-- Everything in `geo` is rebuilt from the OpenStreetMap file, and backup.sh
-- skips it as rebuildable. A proposal exists nowhere else: it must live in a
-- schema that is backed up. `movin` is, and a new schema would stop the nightly
-- backup until classified (backup.sh, DATA_SCHEMAS).
--
-- ── The type list is written in four places ────────────────────────────────
-- The website's packages/domain/src/place-types.ts is the source; the app has
-- a synced copy; and this file writes the keys twice -- the CHECK below and
-- the values place_type_of can return. Nothing syncs this file:
-- tests/suggested-places.test.js fails if the CHECK and the function disagree,
-- or if either differs from the list the test holds (a copy of the website's,
-- compared with the website checkout itself when one is beside this one).
--
-- ── Who writes what ────────────────────────────────────────────────────────
-- maps-shim inserts proposals (the person is taken from the TOKEN, never from
-- the request) and reads approved ones for search; admin-api, from the
-- console, changes `name`, `name_ar`, `type` and `status`, and writes the
-- history beside each change into movin.suggested_place_decision (website
-- migration 023). No foreign key either way: the two repositories' migrations
-- run in no fixed order on a fresh database.
--
-- Idempotent: a release re-applies this file whenever it changes.

BEGIN;

CREATE SCHEMA IF NOT EXISTS movin;

-- >>> place_type_of  (investigations/probe-place-types.sh copies this block)
-- Which of our types an OpenStreetMap place is.
--
-- `class` / `subclass` are geo.place's: the OSM key and value extract.py kept
-- -- ONE pair per place, the first key in its order (aeroway, railway,
-- amenity, shop, tourism, leisure, office, healthcare, public_transport,
-- historic, craft, military, building). So a pharmacy tagged both
-- amenity=pharmacy and healthcare=pharmacy arrives as amenity=pharmacy, and
-- one tagged only healthcare=pharmacy as healthcare=pharmacy: both are mapped.
--
-- What each type takes, and the choices in it:
--   * hospital       -- clinics, doctors and dentists too: « where do I get
--                       seen » (dentists: 314 in DZ, 14 in MR, owner 2026-11-09).
--   * mosque         -- place_of_worship and building=mosque, EXCEPT a
--                       place_of_worship whose `name` says church, chapel or
--                       temple: that is `other` (owner, 2026-11-09). The
--                       extract does not keep OSM's `religion` tag, so the name
--                       is the only sign left -- 26 of 4,978 places of worship,
--                       measured 2026-11-09 (Notre-Dame d'Afrique, the Sacré-
--                       Cœur, the Nouakchott mission…). A church whose name
--                       says nothing of it stays a « mosque » until somebody
--                       corrects OSM; a re-extract keeping `religion` is the
--                       real fix.
--   * school         -- universities, colleges and kindergartens too.
--   * shop           -- every shop=*, supermarkets and malls included; a
--                       shop=chemist is a drugstore, not a pharmacy.
--   * bank           -- banks and money-transfer agencies (where people go for
--                       money in Mauritania: 163 there, owner 2026-11-09). An
--                       ATM is not where anybody asks a driver to go: it stays
--                       `other` (2,165 in DZ would bury the banks).
--   * administration -- town halls, police, post offices, courts, government
--                       and diplomatic offices, embassies.
--   * a named building (class `building`) counts only where its value says
--                       what it is (building=school, =hospital, =hotel…).
-- Everything else is `other`, including every street, town and transport
-- stop: « search by type » never offers `other`.
--
-- `name` is geo.place.display_name, and optional: only the church test reads
-- it, and a caller without one gets the tag's answer.
--
-- IMMUTABLE and plain SQL, so it can sit in an index expression one day, runs
-- in PGlite (no PostGIS needed), and costs nothing per row.
CREATE OR REPLACE FUNCTION movin.place_type_of(class text, subclass text, name text DEFAULT NULL)
  RETURNS text
  LANGUAGE sql IMMUTABLE PARALLEL SAFE
AS $$
  SELECT CASE
    WHEN class = 'amenity' AND subclass = 'cafe'
      THEN 'cafe'
    WHEN class = 'amenity' AND subclass IN ('restaurant', 'fast_food', 'food_court')
      THEN 'restaurant'
    WHEN class IN ('amenity', 'healthcare') AND subclass = 'pharmacy'
      THEN 'pharmacy'
    WHEN class = 'amenity' AND subclass IN ('hospital', 'clinic', 'doctors', 'dentist')
      THEN 'hospital'
    WHEN class = 'healthcare' AND subclass IN ('hospital', 'clinic', 'doctor', 'dentist', 'centre')
      THEN 'hospital'
    WHEN class = 'building' AND subclass = 'hospital'
      THEN 'hospital'
    -- The same words investigations/probe-place-types.sql counts with.
    WHEN class = 'amenity' AND subclass = 'place_of_worship'
         AND coalesce(name, '') ~* '(église|eglise|cathédrale|cathedrale|chapelle|church|chapel|temple|كنيسة)'
      THEN 'other'
    WHEN class = 'amenity' AND subclass = 'place_of_worship'
      THEN 'mosque'
    WHEN class = 'building' AND subclass = 'mosque'
      THEN 'mosque'
    WHEN class = 'amenity' AND subclass IN ('school', 'college', 'university', 'kindergarten')
      THEN 'school'
    WHEN class = 'building' AND subclass IN ('school', 'college', 'university', 'kindergarten')
      THEN 'school'
    WHEN class = 'shop'
      THEN 'shop'
    WHEN class = 'building' AND subclass IN ('retail', 'supermarket')
      THEN 'shop'
    WHEN class = 'amenity' AND subclass = 'marketplace'
      THEN 'market'
    WHEN class = 'amenity' AND subclass IN ('bank', 'money_transfer')
      THEN 'bank'
    WHEN class = 'tourism' AND subclass IN ('hotel', 'guest_house', 'hostel', 'motel')
      THEN 'hotel'
    WHEN class = 'building' AND subclass = 'hotel'
      THEN 'hotel'
    WHEN class = 'amenity' AND subclass = 'fuel'
      THEN 'fuel'
    WHEN class = 'amenity' AND subclass IN ('townhall', 'police', 'post_office', 'courthouse', 'embassy')
      THEN 'administration'
    WHEN class = 'office' AND subclass IN ('government', 'diplomatic')
      THEN 'administration'
    ELSE 'other'
  END
$$;
-- <<< place_type_of

COMMENT ON FUNCTION movin.place_type_of(text, text, text) IS
  'Which of our place types (the website''s packages/domain place-types.ts) an '
  'OpenStreetMap class/subclass (and, for places of worship, display_name) '
  'from geo.place is. ''other'' when none.';

CREATE TABLE IF NOT EXISTS movin.suggested_place (
  id               bigint      GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  -- What search hands the app as Google's `place_id`, and what the app hands
  -- back for the details. OSM places are `n…` / `w…`; `u` can never collide
  -- with them. Derived from `id`, so it is never reused or renumbered: the
  -- rider app CACHES names by place_id (atlas_app.place_name_cache).
  place_id         text        GENERATED ALWAYS AS ('u' || id::text) STORED UNIQUE,

  -- What the person typed and chose. Never changed by anybody: the office's
  -- corrections go in name / type below, so the console can always show both
  -- (the same split as the website's driver_declaration / _correction).
  proposed_name    text        NOT NULL,
  proposed_type    text        NOT NULL,
  -- What search shows. Equal to the proposed values until the office edits.
  name             text        NOT NULL,
  type             text        NOT NULL,
  -- The Arabic name, added by the office. Without it a proposed place shows
  -- its name as typed to a reader of the Arabic app (the labels route,
  -- maps-shim placeLabels, answers from here).
  name_ar          text,

  -- WGS84, as the phone sends it. Plain numbers rather than PostGIS, so every
  -- line of this file is testable in PGlite.
  lat              double precision NOT NULL,
  lon              double precision NOT NULL,
  -- Decided by the SERVER from the pin (atlas_app.geometry, as maps-shim's
  -- countryAt does), never sent by the phone.
  country          text        NOT NULL CHECK (country IN ('MR', 'DZ')),

  -- Who proposed it: which person table, and the id in it. Taken from the
  -- token by maps-shim, never from the request body. Deliberately NOT a
  -- foreign key: those tables are upstream's, and a constraint pointing into
  -- them would make their next migration our outage.
  author_side      text        NOT NULL CHECK (author_side IN ('rider', 'driver')),
  author_id        text        NOT NULL,

  status           text        NOT NULL DEFAULT 'pending'
                                 CHECK (status IN ('pending', 'approved', 'refused')),
  -- A short code the app translates for the person (its list is decided with
  -- the console screen). Present exactly when the place is refused: an
  -- approval after a refusal clears it.
  refusal_reason   text        CHECK (refusal_reason IS NULL OR char_length(refusal_reason) BETWEEN 1 AND 40),
  -- The last approve / refuse. admin_user is the website's table, so the id
  -- has no foreign key; the email is copied in and outlives the account.
  decided_by       uuid,
  decided_by_email text,
  decided_at       timestamptz,

  created_at       timestamptz NOT NULL DEFAULT now(),
  updated_at       timestamptz NOT NULL DEFAULT now(),

  -- The thirteen keys of the website's place-types.ts. Named, so adding a
  -- type is DROP CONSTRAINT + ADD CONSTRAINT in a later change of this file.
  CONSTRAINT suggested_place_proposed_type_known CHECK (proposed_type IN (
    'cafe', 'restaurant', 'pharmacy', 'hospital', 'mosque', 'school', 'shop',
    'market', 'bank', 'hotel', 'fuel', 'administration', 'other')),
  CONSTRAINT suggested_place_type_known CHECK (type IN (
    'cafe', 'restaurant', 'pharmacy', 'hospital', 'mosque', 'school', 'shop',
    'market', 'bank', 'hotel', 'fuel', 'administration', 'other')),

  -- PLACE_NAME_MIN / PLACE_NAME_MAX of place-types.ts: 2 to 80 characters,
  -- trimmed, and no control character (a newline in a name is a broken line
  -- in the console and in every list that prints it).
  CONSTRAINT suggested_place_proposed_name_shape CHECK (
    char_length(proposed_name) BETWEEN 2 AND 80
    AND proposed_name = btrim(proposed_name)
    AND proposed_name !~ '[[:cntrl:]]'),
  CONSTRAINT suggested_place_name_shape CHECK (
    char_length(name) BETWEEN 2 AND 80
    AND name = btrim(name)
    AND name !~ '[[:cntrl:]]'),
  CONSTRAINT suggested_place_name_ar_shape CHECK (
    name_ar IS NULL OR (
      char_length(name_ar) BETWEEN 2 AND 80
      AND name_ar = btrim(name_ar)
      AND name_ar !~ '[[:cntrl:]]')),

  -- Inside the box around both countries: Mauritania reaches 14.7°N and
  -- 17.1°W, Algeria 37.1°N and 12.0°E. The country itself is checked by the
  -- server against the service areas; this catches the classic bug instead,
  -- latitude and longitude swapped -- Nouakchott swapped is -15.97°N, and
  -- Algiers swapped is 3.05°N, both outside.
  CONSTRAINT suggested_place_in_our_box CHECK (
    lat BETWEEN 14 AND 38 AND lon BETWEEN -18 AND 12.5),

  CONSTRAINT suggested_place_refusal_has_reason CHECK (
    (status = 'refused') = (refusal_reason IS NOT NULL)),
  -- Approved and refused are decisions, and a decision names a person.
  CONSTRAINT suggested_place_decision_named CHECK (
    status = 'pending' OR (decided_at IS NOT NULL AND decided_by_email IS NOT NULL))
);

-- The console's queue: what is pending, oldest first or newest first.
CREATE INDEX IF NOT EXISTS suggested_place_queue
  ON movin.suggested_place (status, created_at DESC);
-- One person's proposals: the daily cap, and « Mes lieux proposés ».
CREATE INDEX IF NOT EXISTS suggested_place_by_author
  ON movin.suggested_place (author_side, author_id, created_at DESC);

-- No position index, on purpose (owner, 2026-11-09). « Near me » and the
-- duplicate check (same name within 100 m, pending places included) read a
-- table of hundreds, at most a few thousand rows: a scan takes a millisecond.
-- A PostGIS GiST index was written first, and was the one part of this file
-- PGlite cannot run -- with the GitHub CI switched off (GitHub disabled
-- workflows on this fork in November 2026), its first real build would have
-- been a live release. If the table ever reaches tens of thousands of rows:
--   CREATE INDEX IF NOT EXISTS suggested_place_position ON movin.suggested_place
--     USING gist ((st_setsrid(st_makepoint(lon, lat), 4326)::geography))
--     WHERE status <> 'refused';

COMMENT ON TABLE movin.suggested_place IS
  'Places proposed by passengers and drivers, and their current state '
  '(pending / approved / refused). Approved rows are found in search with '
  'place_id u<id>. The office''s history of each change is the website''s '
  'movin.suggested_place_decision.';
COMMENT ON COLUMN movin.suggested_place.place_id IS
  '''u'' || id: the id search hands out. Never reused: the rider app caches '
  'place names by it.';
COMMENT ON COLUMN movin.suggested_place.proposed_name IS
  'As the person typed it. Never changed; the office corrects `name`.';
COMMENT ON COLUMN movin.suggested_place.author_id IS
  'atlas_app.person.id (author_side rider) or atlas_driver_offer_bpp.person.id '
  '(driver), from the token. Deliberately NOT a foreign key.';
COMMENT ON COLUMN movin.suggested_place.country IS
  'Set by the server from the pin against the service areas, never by the phone.';

COMMIT;
