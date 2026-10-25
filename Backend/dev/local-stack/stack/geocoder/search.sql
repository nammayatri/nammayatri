-- ---------------------------------------------------------------------------
-- Search and reverse-geocode, as functions.
--
-- Kept apart from index.sql on purpose: the ranking is the part that will be
-- tuned repeatedly, and reloading a function is instant while rebuilding the
-- table is not.
-- ---------------------------------------------------------------------------

-- ── forward search ─────────────────────────────────────────────────────────
--
-- Two matchers, because they answer different questions:
--
--   LIKE '%q%'   -- what the rider is typing *now*. Prefix and word-start
--                   matches are what autocomplete is actually for, and
--                   trigram similarity is terrible at them: "did" against
--                   "rue didouche mourad" scores about 0.15.
--   % (trigram)  -- what the rider misspelled. Only worth running once there
--                   is enough typed to be wrong about.
--
-- One GIN trigram index accelerates both.
create or replace function geo.search(
  q             text,
  centre_lat    double precision,
  centre_lon    double precision,
  max_results   int  default 10
)
returns table (
  place_id      text,
  display_name  text,
  locality      text,
  kind          text,
  subclass      text,
  lat           double precision,
  lon           double precision,
  distance_m    double precision,
  score         double precision
)
language plpgsql stable parallel safe
-- Both of these cost more than they save on a query this small. Measured:
-- 148 ms total, of which the scan and sort were 20 ms -- 30 ms went to JIT
-- compiling a query that runs in twenty, and the parallel workers spent
-- longer starting up than the extra cores saved. Turning them off here rather
-- than server-wide keeps the change to the one workload it was measured on.
set jit = off
set max_parallel_workers_per_gather = 0
as $$
declare
  qn     text := trim(geo.normalise(q));
  centre geography := st_setsrid(st_makepoint(centre_lon, centre_lat), 4326)::geography;
begin
  -- One character matches most of the country; it is not a search yet.
  if qn is null or length(qn) < 2 then
    return;
  end if;

  return query
  with candidate as (
    -- Near: anything at all, however minor. This is almost every search --
    -- riders look for somewhere they are about to be driven to.
    select p.*,
           -- `false` = sphere, not spheroid. The spheroid maths is the single
           -- most expensive thing in this query -- it was most of a 190 ms
           -- response -- and it buys accuracy measured in centimetres, for a
           -- number we use to sort a list and print to the nearest 100 m.
           st_distance(p.geog, centre, false) as dist
      from geo.place p
     where st_dwithin(p.geog, centre, 50000, false)
       and (p.search_norm like '%' || qn || '%'
            or (length(qn) >= 4 and p.search_norm % qn))
    union all
    -- Far: only landmarks worth crossing the country for, and only when the
    -- name genuinely starts with what was typed. Without this, "Constantine"
    -- typed in Algiers finds Rue de Constantine and never the city. With the
    -- bound, it stays cheap: an unbounded search for a word as common as
    -- "rue" scores twenty thousand rows and takes 120 ms.
    select p.*, st_distance(p.geog, centre, false)
      from geo.place p
     where not st_dwithin(p.geog, centre, 50000, false)
       and p.importance >= 0.70
       and p.search_norm like qn || '%'
  ),
  scored as (
    select c.*,
           case
             -- Starts with what they typed: the thing they are reaching for.
             when c.search_norm like qn || '%'        then 1.00
             -- A *word* starts with it: "mourad" finding "Rue Didouche Mourad".
             when c.search_norm like '% ' || qn || '%' then 0.90
             -- Buried mid-word. Real, but weaker.
             when c.search_norm like '%' || qn || '%'  then 0.70
             -- Only the fuzzy matcher found it: a misspelling.
             else 0.40 + 0.30 * similarity(c.search_norm, qn)
           end as text_score,
           -- 1.0 at the map centre, 0.5 at 2.5 km, 0.2 at 10 km. Near things
           -- win ties; far things are still reachable by typing more.
           1.0 / (1.0 + c.dist / 2500.0) as proximity
      from candidate c
  ),
  deduped as (
    -- OSM maps the same name several times over: "Bab Ezzouar" is a place, a
    -- bus stop and a shop, all within a few hundred metres. Three lines saying
    -- the same words is worse than one, and it pushes real alternatives off a
    -- list that only holds five. Keep the best-scoring of each name-in-a-place
    -- and let the rest go.
    select s.*,
           (0.55 * s.text_score + 0.30 * s.proximity + 0.15 * s.importance) as total,
           row_number() over (
             partition by s.name_norm, coalesce(s.locality, '')
             order by (0.55 * s.text_score + 0.30 * s.proximity + 0.15 * s.importance) desc,
                      -- A place beats a bus stop of the same name: it is what
                      -- the rider meant, and it has the better coordinates.
                      case s.kind when 'place' then 0 when 'poi' then 1
                                  when 'street' then 2 else 3 end,
                      s.dist asc
           ) as rn
      from scored s
  )
  select d.place_id,
         d.display_name,
         d.locality,
         d.kind,
         d.subclass,
         d.lat,
         d.lon,
         d.dist,
         d.total::double precision
    from deduped d
   where d.rn = 1
   order by d.total desc, d.dist asc
   limit max_results;
end;
$$;

-- ── reverse ────────────────────────────────────────────────────────────────
--
-- "What is here?" for a dropped pin. A street within a short walk is the best
-- answer -- that is what a rider would say to a driver. Failing that, the
-- neighbourhood. Never a house number: this data has 5,204 of them for the
-- whole of Algiers, so an address would be invented far more often than found.
create or replace function geo.reverse(
  at_lat double precision,
  at_lon double precision
)
returns table (
  place_id      text,
  display_name  text,
  locality      text,
  kind          text,
  lat           double precision,
  lon           double precision,
  distance_m    double precision
)
language plpgsql stable parallel safe
set jit = off
set max_parallel_workers_per_gather = 0
as $$
declare
  -- A local variable, not a CTE column. PostGIS's KNN operator (`<->`) only
  -- uses the GiST index when one side is a constant or a parameter -- put the
  -- point in a `with here as (...)` and the planner silently falls back to
  -- scanning and sorting every row. That mistake cost 200 ms a call and was
  -- invisible: the answers were correct throughout.
  here geography := st_setsrid(st_makepoint(at_lon, at_lat), 4326)::geography;
begin
  return query
  -- Three nearest-neighbour lookups, ranked by how good an answer they are
  -- rather than by distance. `prio` is that rank.
  with candidate as (
      (select p.place_id, p.display_name, p.locality, p.kind, p.lat, p.lon,
              st_distance(p.geog, here, false) as d, 1 as prio
         from geo.place p
        where p.kind in ('poi', 'transport') and p.importance >= 0.6
        order by p.geog <-> here
        limit 1)
    union all
      (select p.place_id, p.display_name, p.locality, p.kind, p.lat, p.lon,
              st_distance(p.geog, here, false), 2
         from geo.place p
        where p.kind = 'street'
        order by p.geog <-> here
        limit 1)
    union all
      (select p.place_id, p.display_name, p.locality, p.kind, p.lat, p.lon,
              st_distance(p.geog, here, false), 3
         from geo.place p
        where p.kind = 'place'
        order by p.geog <-> here
        limit 1)
  )
  -- A landmark you can see beats a street name you cannot -- but only if the
  -- pin is genuinely on it. 120 m is about "you can see it from here"; 250 m
  -- is about "you are on that street". The neighbourhood always answers, so
  -- there is no such thing as a pin we cannot name.
  select c.place_id, c.display_name, c.locality, c.kind, c.lat, c.lon, c.d
    from candidate c
   where (c.prio = 1 and c.d <= 120)
      or (c.prio = 2 and c.d <= 250)
      or  c.prio = 3
   order by c.prio
   limit 1;
end;
$$;
