-- How many places of each of our types does the place index already hold, per
-- country? And is the mapping right? (« Lieux proposés », day 1, 2026-11-09.)
--
-- Run it through probe-place-types.sh, never on its own: the wrapper first
-- creates pg_temp.place_type_of from stack/db/suggested-places.sql, so these
-- counts use exactly the mapping that will ship, before any release has
-- installed movin.place_type_of.
--
-- Reads geo.place (OpenStreetMap places, public map data) and
-- atlas_app.geometry (the service areas). No person, no ride, no number.
-- Writes nothing but this session's own temporary table and function, which
-- disappear when psql exits.
--
-- What each part answers:
--   1. per type, per country           -- is « search by type » worth offering?
--   2. near the two capitals           -- what a rider in town actually sees
--   3. the biggest `other` tags        -- what the mapping may be missing
--   4. places of worship named church  -- that none is left under « mosque »
--   5. name lengths                    -- is 80 characters (PLACE_NAME_MAX) enough?

\pset footer off
SET statement_timeout = '120s';

\echo ''
\echo '=== 0. the index, by kind ==='
SELECT kind, count(*) AS places FROM geo.place GROUP BY 1 ORDER BY 2 DESC;

-- Every point of interest once, with its type and its country. The country is
-- maps-shim countryAt()'s test (the rider's service areas, SRID 0), done one
-- polygon at a time: each st_contains then compares against ONE constant
-- geometry, which PostGIS prepares once instead of once per row.
CREATE TEMP TABLE poi AS
SELECT p.place_id, p.class, p.subclass, p.display_name, p.lat, p.lon, p.geog,
       pg_temp.place_type_of(p.class, p.subclass, p.display_name) AS type,
       'none'::text AS country
  FROM geo.place p
 WHERE p.kind = 'poi';

UPDATE poi SET country = 'DZ'
 WHERE st_contains((SELECT st_union(geom) FROM atlas_app.geometry WHERE region = 'Algeria'),
                   st_point(lon, lat));
UPDATE poi SET country = 'MR'
 WHERE country = 'none'
   AND st_contains((SELECT st_union(geom) FROM atlas_app.geometry WHERE region = 'Mauritania'),
                   st_point(lon, lat));

\echo ''
\echo '=== 1. points of interest per type and country (none = outside both service areas) ==='
SELECT type,
       count(*) FILTER (WHERE country = 'MR')   AS mr,
       count(*) FILTER (WHERE country = 'DZ')   AS dz,
       count(*) FILTER (WHERE country = 'none') AS none
  FROM poi
 GROUP BY type
 ORDER BY (type = 'other'), count(*) DESC;

\echo ''
\echo '=== 2. within 10 km of the centre of Nouakchott and of Algiers ==='
SELECT type,
       count(*) FILTER (WHERE st_dwithin(geog, st_makepoint(-15.9785, 18.0858)::geography, 10000)) AS nouakchott,
       count(*) FILTER (WHERE st_dwithin(geog, st_makepoint(3.0588, 36.7538)::geography, 10000))   AS algiers
  FROM poi
 WHERE type <> 'other'
 GROUP BY type
 ORDER BY type;

\echo ''
\echo '=== 3. the 25 biggest tags mapped to other, per country ==='
SELECT country, class, subclass, places
  FROM (SELECT country, class, subclass, count(*) AS places,
               row_number() OVER (PARTITION BY country ORDER BY count(*) DESC) AS rank
          FROM poi
         WHERE type = 'other' AND country <> 'none'
         GROUP BY 1, 2, 3) t
 WHERE rank <= 25
 ORDER BY country, places DESC;

\echo ''
\echo '=== 4. places of worship whose name says church, chapel or temple, and the type they got ==='
\echo '    (since 2026-11-09 the mapping makes them other: 26 of 4,978 that day, all under mosque before)'
SELECT country, type, display_name
  FROM poi
 WHERE class = 'amenity' AND subclass = 'place_of_worship'
   AND display_name ~* '(église|eglise|cathédrale|cathedrale|chapelle|church|chapel|temple|كنيسة)'
 ORDER BY type, country, display_name;
SELECT count(*) FILTER (WHERE type = 'mosque') AS mosque_total,
       count(*) FILTER (WHERE type = 'mosque'
                          AND display_name ~* '(église|eglise|cathédrale|cathedrale|chapelle|church|chapel|temple|كنيسة)') AS church_left_as_mosque
  FROM poi;

\echo ''
\echo '=== 5. name lengths of points of interest (characters) ==='
SELECT country, count(*) AS places, max(char_length(display_name)) AS longest,
       percentile_disc(0.95) WITHIN GROUP (ORDER BY char_length(display_name)) AS p95,
       percentile_disc(0.99) WITHIN GROUP (ORDER BY char_length(display_name)) AS p99,
       count(*) FILTER (WHERE char_length(display_name) > 80) AS over_80
  FROM poi GROUP BY country ORDER BY country;
SELECT char_length(display_name) AS chars, country, display_name
  FROM poi ORDER BY char_length(display_name) DESC LIMIT 8;
