-- What is left without an Arabic name, and where.
--
-- Read-only. The point is to size phase 2 against the only place we actually
-- operate: a name in Oualata is a name no driver has ever been sent to.
\set ar '[ء-ي]'
\set latin '[A-Za-zÀ-ÿ]'

-- Nouakchott, by the localities the index itself uses. `geo.locality_of` filled
-- these from the place rows, so matching on them is matching on our own data
-- rather than on a bounding box somebody drew.
create temporary view nkc as
select *
  from geo.place
 where locality in (
         select display_name from geo.place
          where kind = 'place'
            and st_dwithin(geog, st_point(-15.9785, 18.0858)::geography, 25000)
       )
    or st_dwithin(geog, st_point(-15.9785, 18.0858)::geography, 25000);

select 'country'    as scope, kind, count(*) as rows,
       count(*) filter (where name_ar is not null) as named,
       count(*) filter (where name_ar is null)     as gap
  from geo.place group by kind
union all
select 'nouakchott', kind, count(*),
       count(*) filter (where name_ar is not null),
       count(*) filter (where name_ar is null)
  from nkc group by kind
 order by scope, kind;

-- The free win, if there is one: rows holding Arabic ONLY inside a mixed-script
-- string. "Aleg ألاك" is a `name` tag carrying both scripts; the Arabic run
-- inside it is a real name and pulling it out is mechanical, not a judgement.
select count(*) as mixed_only
  from geo.place
 where name_ar is null
   and alt_names ~ :'ar';

select display_name, locality,
       array(select x from unnest(string_to_array(alt_names, '|')) as x
              where x ~ :'ar') as mixed
  from geo.place
 where name_ar is null
   and alt_names ~ :'ar'
 order by importance desc
 limit 8;
