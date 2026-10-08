-- Add a second country's places to the LIVE index, without rebuilding it.
--
-- ── Why not `geocoder-prepare.sh load` ─────────────────────────────────────
-- `load` runs index.sql, which DROPS geo.place and rebuilds it from one CSV.
-- That is how the Algerian index was destroyed on 2026-09-03, and today it
-- would also destroy `name_ar` — 3,324 reviewed Arabic names for Mauritania
-- that exist nowhere but in this table (see arabic-*.sql). So this file only
-- INSERTS, and only rows that are not there yet.
--
-- Expects geo.place_raw to hold the new country's extract (extract.py output,
-- loaded by maps-two-countries.sh). Written for Algeria on 2026-09-13, when
-- the app started serving both countries; nothing in it names a country.
--
-- The same four passes as index.sql, in the same order and with the same
-- de-duplication, so an appended row is indistinguishable from a built one:
--   1. places                  -- everything else is located relative to them
--   2. streets, collapsed      -- by (name, locality), locality from pass 1
--   3. POIs and transport
--   4. localities, ids, Arabic names, the search column
--
-- One transaction: a failure leaves the index exactly as it was.

begin;

create temp table before_max on commit drop as
  select coalesce(max(id), 0) as m from geo.place;

-- ── 1. places ──────────────────────────────────────────────────────────────
-- Inserted straight into geo.place, because pass 2 needs geo.locality_of to
-- find them there. A place already present (same OSM object — the Geofabrik
-- extracts overlap a little at a shared border) is skipped.
insert into geo.place
  (place_id, osm_type, osm_id, kind, class, subclass, display_name, alt_names,
   importance, lat, lon, geog, search_norm, name_norm)
select left(osm_type, 1) || osm_id,
       osm_type, osm_id, kind, class, subclass, display_name, alt_names,
       importance, lat, lon,
       st_setsrid(st_makepoint(lon, lat), 4326)::geography,
       geo.normalise(display_name || ' ' || replace(alt_names, '|', ' ')),
       geo.normalise(display_name)
  from (select distinct on (geo.normalise(display_name), round(lat::numeric, 3), round(lon::numeric, 3))
               r.*
          from geo.place_raw r
         where r.kind = 'place'
         order by geo.normalise(display_name), round(lat::numeric, 3), round(lon::numeric, 3),
                  importance desc) r
 where not exists (select 1 from geo.place p where p.place_id = left(r.osm_type, 1) || r.osm_id);

-- ── 2 + 3 into a staging table, so their ids can be made unique first ─────
-- `like ... including defaults` takes geo.place's id sequence with it, so ids
-- stay unique across the whole table.
create temp table staged (like geo.place including defaults) on commit drop;

with located as (
  select r.*,
         geo.locality_of(st_setsrid(st_makepoint(r.lon, r.lat), 4326)::geography) as locality
    from geo.place_raw r
   where r.kind = 'street'
)
insert into staged
  (place_id, osm_type, osm_id, kind, class, subclass, display_name, alt_names,
   locality, importance, lat, lon, geog, search_norm, name_norm)
select '', 'way', min(osm_id), 'street', 'highway',
       (array_agg(subclass order by importance desc))[1],
       (array_agg(display_name order by importance desc))[1],
       string_agg(distinct alt_names, '|'),
       locality,
       max(importance),
       avg(lat), avg(lon),
       st_setsrid(st_makepoint(avg(lon), avg(lat)), 4326)::geography,
       geo.normalise((array_agg(display_name order by importance desc))[1]
                     || ' ' || replace(string_agg(distinct alt_names, '|'), '|', ' ')),
       geo.normalise((array_agg(display_name order by importance desc))[1])
  from located
 group by geo.normalise(display_name), locality;

insert into staged
  (place_id, osm_type, osm_id, kind, class, subclass, display_name, alt_names,
   importance, lat, lon, geog, search_norm, name_norm)
select '', osm_type, osm_id, kind, class, subclass, display_name, alt_names,
       importance, lat, lon,
       st_setsrid(st_makepoint(lon, lat), 4326)::geography,
       geo.normalise(display_name || ' ' || replace(alt_names, '|', ' ')),
       geo.normalise(display_name)
  from (select distinct on (geo.normalise(display_name), subclass,
                            round(lat::numeric, 4), round(lon::numeric, 4))
               r.*
          from geo.place_raw r
         where r.kind in ('poi', 'transport')
         order by geo.normalise(display_name), subclass,
                  round(lat::numeric, 4), round(lon::numeric, 4), importance desc) r;

-- An object already in the index (border overlap) is not appended twice.
delete from staged s
 using geo.place p
 where p.osm_type = s.osm_type and p.osm_id = s.osm_id and p.kind = s.kind;

-- Stable public ids, as index.sql makes them: `n`/`w` + the OSM id, with a
-- suffix only when that id is already taken — here or in the live table.
update staged s
   set place_id = t.pid
  from (select id,
               left(osm_type, 1) || osm_id
                 || case when rn = 1 and not taken then '' else '_' || (rn + 1) end as pid
          from (select s2.id, s2.osm_type, s2.osm_id,
                       row_number() over (partition by s2.osm_type, s2.osm_id order by s2.id) as rn,
                       exists (select 1 from geo.place p
                                where p.place_id = left(s2.osm_type, 1) || s2.osm_id) as taken
                  from staged s2) x) t
 where t.id = s.id;

insert into geo.place select * from staged;

-- ── 4. what the four passes leave for last ────────────────────────────────
update geo.place p
   set locality = geo.locality_of(p.geog)
 where p.id > (select m from before_max) and p.locality is null;

update geo.place
   set locality = null
 where id > (select m from before_max) and locality = display_name;

-- Arabic names, for the NEW rows only. arabic-names.sql's rule — exactly one
-- clean Arabic candidate, no Latin letter in it — but never run over the old
-- rows: its update rewrites any row whose candidate differs, and a reviewed
-- Mauritanian name differs from its raw OSM candidate by design. NFKC, because
-- presentation-form Arabic renders identically and matches nothing typed.
with cand as (
  select p.id, btrim(c) as name_ar
    from geo.place p,
         lateral regexp_split_to_table(p.alt_names, '[|;]') as c
   where p.id > (select m from before_max)
     and btrim(c) ~ '[ء-ي]'
     and btrim(c) !~ '[A-Za-zÀ-ÿ]'
),
sure as (
  select id, min(name_ar) as name_ar
    from cand
   group by id
  having count(distinct name_ar) = 1
)
update geo.place p
   set name_ar = normalize(s.name_ar, NFKC)
  from sure s
 where p.id = s.id;

-- The search column, with the Arabic name in it, exactly as arabic-search.sql
-- builds it for every row.
update geo.place
   set search_norm = geo.normalise(display_name || ' ' || replace(alt_names, '|', ' ')
                                   || ' ' || coalesce(name_ar, ''))
 where id > (select m from before_max);

select count(*) filter (where id > (select m from before_max))                      as appended,
       count(*) filter (where id > (select m from before_max) and name_ar is not null) as with_arabic,
       count(*)                                                                      as total
  from geo.place;

commit;

-- The indexes are expression and trigram indexes; the planner needs fresh
-- statistics for a table that just grew tenfold.
analyze geo.place;
