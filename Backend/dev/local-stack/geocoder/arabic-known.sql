-- Every French->Arabic pair the index already holds, as a lookup table.
--
-- The point: a street called "Route Rosso - Boghé" should be composed from the
-- Arabic names we ALREADY have for Rosso and Boghé, not from a fresh guess. Our
-- own data is the most reliable source available, and using it keeps the map
-- internally consistent -- a town cannot end up spelled one way as a locality
-- and another way inside a road name.
select display_name, name_ar
  from geo.place
 where name_ar is not null
   and display_name !~ '[ء-ي]'
 group by display_name, name_ar
 order by length(display_name) desc;
