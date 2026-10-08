-- The streets and transport stops still without an Arabic name.
--
-- Ordered by importance so that, if the review is ever cut short, what got done
-- is what riders actually type.
select id, kind,
       display_name || coalesce('~' || locality, '~') as place
  from geo.place
 where name_ar is null
   and kind in ('street', 'transport')
 order by kind, importance desc, display_name;
