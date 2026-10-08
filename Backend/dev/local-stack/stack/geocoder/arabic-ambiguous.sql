-- The rows where "take the Arabic name" is a choice, not a lookup.
--
-- Same cleaning rule as arabic-names.sql -- split on `|` and `;`, reject any
-- candidate carrying a Latin letter -- kept in step with it deliberately: if
-- the two rules drift, this list stops describing the rows that were left
-- empty and starts describing some other set.
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
select kind,
       display_name || coalesce(', ' || locality, '') as place,
       array_to_string(array_agg(distinct name_ar), '   |   ') as candidates
  from cand
 group by id, kind, display_name, locality, importance
having count(distinct name_ar) > 1
 order by kind, importance desc, display_name;
