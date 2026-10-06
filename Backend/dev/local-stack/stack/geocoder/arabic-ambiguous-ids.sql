-- The same rows as arabic-ambiguous.sql, carrying geo.place.id.
--
-- The review list was exported without the id, so the answers come back keyed
-- by position. Re-exporting with the id is only safe if the order is identical,
-- which is why the three identifying fields are emitted alongside it: the
-- loader asserts them against the original export line by line and refuses to
-- write anything if a single row has moved. Two rows really are called
-- "Ambassade du Maroc, E-Nord", so a join on the name alone would put one
-- answer on the wrong place.
\set ar '[ء-ي]'
\set latin '[A-Za-zÀ-ÿ]'

with cand as (
  select p.id, p.kind, p.display_name, p.locality, p.importance,
         btrim(c) as name_ar
    from geo.place p,
         lateral regexp_split_to_table(p.alt_names, '[|;]') as c
   where btrim(c) ~ :'ar'
     and btrim(c) !~ :'latin'
)
select id,
       kind,
       display_name || coalesce(', ' || locality, '') as place,
       array_to_string(array_agg(distinct name_ar), '   |   ') as candidates
  from cand
 group by id, kind, display_name, locality, importance
having count(distinct name_ar) > 1
 order by kind, importance desc, display_name;
