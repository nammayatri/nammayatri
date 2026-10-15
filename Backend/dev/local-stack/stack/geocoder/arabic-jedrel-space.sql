-- Jedrel Mohguen, the one row where normalising was not enough.
--
-- The place and the POI are the same name. The reviewer picked the spaced form
-- for the place (جدر المحكن) and, for the POI, the only candidate OSM offered
-- -- which was written in Arabic PRESENTATION FORMS (U+FE9F ARABIC LETTER JEEM
-- INITIAL FORM and friends). Those render identically to ordinary letters,
-- which is why nothing looked wrong in the list, and they match nothing typed
-- on a keyboard. NFKC folded them back in arabic-reviewed.sql, but NFKC does
-- not insert a space that was never in the source, so the POI came out
-- جدرالمحكن against the place's جدر المحكن.
--
-- Confirmed by the reviewer on 2026-09-10 rather than assumed: one word or two
-- is a spelling question, not a mechanical one.
update geo.place
   set name_ar = 'جدر المحكن'
 where id = 8104
   and name_ar = 'جدرالمحكن';

select id, display_name, name_ar
  from geo.place
 where display_name in ('Jedrel Mouhguen', 'Jedrel Mohguen')
 order by id;
