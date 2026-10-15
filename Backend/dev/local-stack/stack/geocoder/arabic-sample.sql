-- Coverage, and four real place_ids to probe the public route with.
select kind,
       count(*) filter (where name_ar is not null) as named,
       count(*)                                    as rows,
       round(100.0 * count(*) filter (where name_ar is not null) / count(*)) as pct
  from geo.place
 group by rollup(kind)
 order by kind nulls last;

select string_agg(place_id, ',') as ids
  from (select place_id
          from geo.place
         where kind = 'street' and name_ar is not null
         order by importance desc
         limit 4) s;
