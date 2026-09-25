-- Where the Arabic coverage stands, country-wide and in the only city we serve.
\set ar '[ء-ي]'

select kind,
       count(*)                                    as rows,
       count(*) filter (where name_ar is not null) as named,
       count(*) filter (where name_ar is null)     as gap
  from geo.place
 group by rollup(kind)
 order by kind nulls last;

-- Ten of the names pulled out of mixed strings, to be looked at rather than
-- trusted: the display name is French, the Arabic came from inside a string
-- that also held French.
select display_name, locality, name_ar
  from geo.place
 where name_ar is not null
   and display_name !~ :'ar'
   and alt_names ~ '[A-Za-z]'
   and alt_names !~ ('\|' || name_ar || '\|')
 order by importance desc
 limit 10;
