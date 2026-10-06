-- The 152 rows whose only Arabic is trapped inside a mixed-script string.
--
-- arabic-names.sql deliberately rejected any candidate carrying a Latin letter,
-- because "Aleg ألاك" as a whole would have put a French word inside the Arabic
-- label. That rejection threw away a real name along with the French: the
-- Arabic RUN inside those strings is the name, and pulling it out is mechanical.
--
-- "Centre de santé النقطة الصحية بأم لحياظ"  ->  النقطة الصحية بأم لحياظ
-- "La traversée المعبر sh"                   ->  المعبر
-- "Hôpital d'oncologie مستشفى الانكولوجيHôpital d'oncologie ا"
--                                            ->  مستشفى الانكولوجي
--
-- That last one is why the rule is LONGEST RUN and not first run: OSM holds
-- some of these doubled and truncated, and the longest run is the intact one.
--
-- Two guards, because a wrong name is worse than a French one:
--   * at least three Arabic letters -- the trailing "ا" above is not a name
--   * exactly one longest run, so a row offering two equally long different
--     names is left for a human like the 93 before it
\set ar '[ء-ي]'

with runs as (
  select p.id,
         btrim((regexp_matches(c, '[ء-ي][ء-ي ]*', 'g'))[1]) as run
    from geo.place p,
         lateral regexp_split_to_table(p.alt_names, '[|;]') as c
   where p.name_ar is null
     and p.alt_names ~ :'ar'
),
kept as (
  select id, run, length(replace(run, ' ', '')) as letters
    from runs
   where length(replace(btrim(run), ' ', '')) >= 3
),
best as (
  select id, max(letters) as letters from kept group by id
),
sure as (
  select k.id, min(k.run) as run
    from kept k join best b on b.id = k.id and b.letters = k.letters
   group by k.id
  having count(distinct k.run) = 1
)
update geo.place p
   set name_ar = s.run
  from sure s
 where p.id = s.id;

select count(*) filter (where name_ar is not null)                     as filled,
       count(*) filter (where name_ar is not null and kind = 'street') as streets,
       count(*)                                                        as total
  from geo.place;
