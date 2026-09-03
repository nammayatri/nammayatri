-- ---------------------------------------------------------------------------
-- The search index.
--
-- Built from geocoder/places.csv (see extract.py) into a schema of its own, in
-- the Postgres the stack already runs. No Elasticsearch, no second database:
-- the whole of Algeria is a few hundred thousand rows, and pg_trgm gives us
-- real typo tolerance for free -- which matters more here than usual, because
-- riders type French transliterations of Arabic names and no two people spell
-- "Bab Ezzouar" the same way.
--
-- Idempotent: run it as often as you like. It rebuilds geo.place from
-- geo.place_raw, which geocoder-prepare.sh has just filled from the CSV.
-- ---------------------------------------------------------------------------

create extension if not exists pg_trgm;
create extension if not exists unaccent;
create extension if not exists postgis;

create schema if not exists geo;

-- ── normalisation ──────────────────────────────────────────────────────────
-- One rule, applied to the stored text and to the query, so they can never
-- drift apart. `unaccent(text)` on its own is STABLE, not IMMUTABLE, because
-- it depends on the default dictionary -- and a STABLE function cannot be
-- indexed. Naming the dictionary explicitly is what makes it immutable.
create or replace function geo.normalise(t text) returns text
  language sql immutable strict parallel safe
as $$
  select regexp_replace(
           lower(unaccent('unaccent'::regdictionary, t)),
           '[^a-z0-9]+', ' ', 'g')
$$;

-- ── the table ──────────────────────────────────────────────────────────────
drop table if exists geo.place;

create table geo.place (
  id            bigserial primary key,
  -- What we hand the app as Google's `place_id`, and what it hands back later.
  -- Derived from the OSM identity, never from `id`: the rider-app *caches*
  -- place names by placeId in `atlas_app.place_name_cache`, so an id that
  -- changed every rebuild would leave the cache pointing at nothing.
  place_id      text,
  osm_type      text        not null,
  osm_id        bigint      not null,
  kind          text        not null,   -- place | street | transport | poi
  class         text        not null,
  subclass      text        not null,
  display_name  text        not null,   -- what the rider reads (French first)
  alt_names     text        not null default '',
  -- The neighbourhood or town this sits in, filled in below. It is the second
  -- line of every suggestion: "Rue Didouche Mourad" is ambiguous, "Rue
  -- Didouche Mourad, Alger-Centre" is not.
  locality      text,
  importance    real        not null,
  lat           double precision not null,
  lon           double precision not null,
  geog          geography(Point, 4326) not null,
  -- Everything searchable, normalised once at build time. Includes the Arabic
  -- names, so typing Arabic works even though we display French.
  search_norm   text        not null,
  -- Just the shown name, normalised. Only used to group duplicates at query
  -- time -- but that runs per keystroke over a few thousand candidates, and
  -- calling unaccent() that often is not free.
  name_norm     text
);

-- ── places first: everything else is located relative to them ──────────────
insert into geo.place
  (osm_type, osm_id, kind, class, subclass, display_name, alt_names,
   importance, lat, lon, geog, search_norm)
select distinct on (geo.normalise(display_name), round(lat::numeric, 3), round(lon::numeric, 3))
       osm_type, osm_id, kind, class, subclass, display_name, alt_names,
       importance, lat, lon,
       st_setsrid(st_makepoint(lon, lat), 4326)::geography,
       geo.normalise(display_name || ' ' || replace(alt_names, '|', ' '))
  from geo.place_raw
 where kind = 'place'
 order by geo.normalise(display_name), round(lat::numeric, 3), round(lon::numeric, 3),
          importance desc;

create index place_geog_gix on geo.place using gist (geog);

-- ── which place something belongs to ───────────────────────────────────────
-- The second line of a suggestion, and it has to be a name a rider recognises.
-- Nearest-place-of-any-kind is the obvious rule, and it produces things like
-- "Bab Ezzouar, Cité EPLF 1080 Logts" -- true, and useless, because the cité
-- is a housing estate nobody outside it has heard of. So: the nearest real
-- municipality or district first, falling back to whatever is nearest only
-- when there is no town within reach.
--
-- Defined here rather than at the top of the file because its body is checked
-- at CREATE time, and geo.place has to exist by then.
create or replace function geo.locality_of(g geography) returns text
  language sql stable parallel safe
as $$
  select coalesce(
    (select p.display_name
       from geo.place p
      where p.kind = 'place'
        and p.subclass in ('city', 'town', 'borough', 'suburb')
        and st_dwithin(p.geog, g, 12000)
      order by p.geog <-> g
      limit 1),
    (select p.display_name
       from geo.place p
      where p.kind = 'place'
      order by p.geog <-> g
      limit 1))
$$;

-- ── streets, collapsed ─────────────────────────────────────────────────────
-- A street is many ways. "Rue Didouche Mourad" is 40-odd rows in the extract
-- and must be one suggestion -- but only within one neighbourhood: the same
-- street name in Bab El Oued and in Hussein Dey are two different places a
-- rider might mean, and collapsing them nationally would put the pin halfway
-- between the two. So the grouping key is (name, locality), and the locality
-- is the nearest named place.
with located as (
  select r.*,
         geo.locality_of(st_setsrid(st_makepoint(r.lon, r.lat), 4326)::geography) as locality
    from geo.place_raw r
   where r.kind = 'street'
)
insert into geo.place
  (osm_type, osm_id, kind, class, subclass, display_name, alt_names,
   locality, importance, lat, lon, geog, search_norm)
select 'way',
       min(osm_id),
       'street',
       'highway',
       (array_agg(subclass order by importance desc))[1],
       (array_agg(display_name order by importance desc))[1],
       string_agg(distinct alt_names, '|'),
       locality,
       max(importance),
       avg(lat), avg(lon),
       st_setsrid(st_makepoint(avg(lon), avg(lat)), 4326)::geography,
       geo.normalise((array_agg(display_name order by importance desc))[1]
                     || ' ' || replace(string_agg(distinct alt_names, '|'), '|', ' '))
  from located
 group by geo.normalise(display_name), locality;

-- ── everything else ────────────────────────────────────────────────────────
-- POIs and transport stops are kept individually: two pharmacies with the
-- same name are two pharmacies. Only exact duplicates at the same spot are
-- dropped, which is a mapping artefact rather than a real distinction.
insert into geo.place
  (osm_type, osm_id, kind, class, subclass, display_name, alt_names,
   importance, lat, lon, geog, search_norm)
select distinct on (geo.normalise(display_name), subclass,
                    round(lat::numeric, 4), round(lon::numeric, 4))
       osm_type, osm_id, kind, class, subclass, display_name, alt_names,
       importance, lat, lon,
       st_setsrid(st_makepoint(lon, lat), 4326)::geography,
       geo.normalise(display_name || ' ' || replace(alt_names, '|', ' '))
  from geo.place_raw
 where kind in ('poi', 'transport')
 order by geo.normalise(display_name), subclass,
          round(lat::numeric, 4), round(lon::numeric, 4), importance desc;

-- Localities for everything that did not get one above.
update geo.place p
   set locality = geo.locality_of(p.geog)
 where p.locality is null;

-- A place is its own locality; repeating it reads as a stutter.
update geo.place set locality = null where locality = display_name;

update geo.place set name_norm = geo.normalise(display_name);
alter table geo.place alter column name_norm set not null;

-- ── stable public ids ──────────────────────────────────────────────────────
-- `n`/`w` + the OSM id. Collapsing streets can in principle land two groups on
-- the same osm_id, so the row_number suffix guarantees uniqueness without
-- making the common case ugly.
update geo.place p
   set place_id = t.pid
  from (select id,
               left(osm_type, 1) || osm_id
                 || case when rn = 1 then '' else '_' || rn end as pid
          from (select id, osm_type, osm_id,
                       row_number() over (partition by osm_type, osm_id order by id) as rn
                  from geo.place) x) t
 where t.id = p.id;

alter table geo.place alter column place_id set not null;
create unique index place_place_id_uix on geo.place (place_id);

-- ── indexes ────────────────────────────────────────────────────────────────
-- One GIN trigram index serves both matchers: LIKE '%...%' for what someone
-- is typing, and % (similarity) for what they misspelled.
create index place_search_trgm on geo.place using gin (search_norm gin_trgm_ops);
create index place_kind_ix     on geo.place (kind);
drop index if exists geo.place_geog_gix;
create index place_geog_gix    on geo.place using gist (geog);

-- Partial indexes for geo.reverse, which asks three separate "what is the
-- nearest X" questions. On the single index above, each one walks outward in
-- distance order discarding rows of the wrong kind -- for the nearest notable
-- landmark that can be thousands of entries, and the three lookups together
-- took 165 ms. Given an index containing only the right kind, each is a page
-- or two.
create index place_geog_street_gix on geo.place using gist (geog) where kind = 'street';
create index place_geog_place_gix  on geo.place using gist (geog) where kind = 'place';
create index place_geog_spot_gix   on geo.place using gist (geog)
  where kind in ('poi', 'transport') and importance >= 0.6;

analyze geo.place;
