-- Phase 1 of the Arabic place names: hold the name we ALREADY have, in a
-- column of its own.
--
-- Nothing reads this column yet, so nothing a rider sees changes when it runs.
-- It is additive, and `alter table geo.place drop column name_ar` undoes it.
--
-- ⚠ geocoder-prepare.sh drops and rebuilds geo.place. This column does not
-- survive an index rebuild on its own -- extract.py and index.sql carry
-- `name:ar` through for that reason, and this file exists only to fill the
-- index that is already deployed without rebuilding it.
\set ar '[ء-ي]'
\set latin '[A-Za-zÀ-ÿ]'

alter table geo.place add column if not exists name_ar text;

-- Filled ONLY where the answer is not a choice.
--
-- Two separators. `|` is ours, from extract.py joining the name tags; `;` is
-- OSM's own convention for stuffing several names into one tag, and it is why
-- Rosso carries "القوارب;Roco;Rusu;Rosso" -- splitting on `|` alone hands back
-- that entire string as one "Arabic" name.
--
-- A candidate holding any Latin letter is rejected. Without that, Aleg's best
-- candidate is "Aleg ألاك", a `name` tag carrying both scripts, which would put
-- a French word in the middle of the Arabic label.
--
-- Measured 2026-09-10: 2,928 rows hold at least one clean Arabic name and
-- 2,835 of them hold exactly one. The other 93 are real disagreements -- three
-- spellings of Boghé, or two different names for Cap Blanc (رأس الأبيض, the
-- white cape, and رأس نواذيبو, the cape of Nouadhibou) -- and a machine picking
-- `min()` between those is a machine inventing an answer. They are left null
-- for a human who reads Arabic.
with cand as (
  select p.id, btrim(c) as name_ar
    from geo.place p,
         lateral regexp_split_to_table(p.alt_names, '[|;]') as c
   where btrim(c) ~ :'ar'
     and btrim(c) !~ :'latin'
),
sure as (
  select id, min(name_ar) as name_ar
    from cand
   group by id
  having count(distinct name_ar) = 1
)
update geo.place p
   set name_ar = s.name_ar
  from sure s
 where p.id = s.id
   and p.name_ar is distinct from s.name_ar;

select count(*) filter (where name_ar is not null)                     as filled,
       count(*) filter (where name_ar is not null and kind = 'street') as streets,
       count(*)                                                        as total
  from geo.place;
