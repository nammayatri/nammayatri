-- Make typing Arabic find something.
--
-- ── The bug ────────────────────────────────────────────────────────────────
-- geo.normalise squashed `[^a-z0-9]+` to a space. Every Arabic character is
-- outside `a-z0-9`, so Arabic normalised to a string of spaces -- both in the
-- stored `search_norm` and in the query. `geo.normalise('شارع عبد الناصر')`
-- returned ''.
--
-- index.sql said, above that column: "Includes the Arabic names, so typing
-- Arabic works even though we display French." It never did. The Arabic went
-- in and was erased on the way. Nothing errored: an empty query matched
-- everything or nothing depending on the branch, and the screen showed no
-- suggestions, which reads as "there is no such place".
--
-- ── The fix, and the two extra things it does ──────────────────────────────
-- The character class now admits U+0621..U+064A. Two more steps earn their
-- place, because Mauritanian OSM data spells the same name several ways and a
-- rider types a third:
--
--   * harakat and tatweel are stripped. `الحلـ__ه` (a real row, elongated with
--     U+0640 for display) would otherwise never match `الحله`.
--   * أ إ آ ٱ fold to ا, ى to ي, ة to ه, ؤ to و, ئ to ي. Our own index holds
--     both أنواذيبو and انواذيبو for Nouadhibou; without folding, typing one
--     misses the other.
--
-- Latin behaviour is unchanged: lower() and unaccent() still run first, and
-- everything outside both alphabets still becomes a space.
create or replace function geo.normalise(t text) returns text
  language sql immutable strict parallel safe
as $$
  select regexp_replace(
           translate(
             regexp_replace(
               lower(unaccent('unaccent'::regdictionary, t)),
               '[ًٌٍَُِّْـ]', '', 'g'),
             'أإآٱىةؤئ', 'اااايهوي'),
           '[^a-z0-9ء-ي]+', ' ', 'g')
$$;

-- ── Rebuild what the old definition wrote ──────────────────────────────────
-- `search_norm` is a stored column, filled at index build time, so redefining
-- the function alone changes nothing that is already on disk. And `name_ar`
-- did not exist when this index was built, so it was never in there at all --
-- which is the second reason Arabic found nothing even where we hold it.
update geo.place
   set search_norm = geo.normalise(
         display_name || ' ' ||
         replace(alt_names, '|', ' ') || ' ' ||
         coalesce(name_ar, '')),
       name_norm = geo.normalise(display_name);

-- The indexes are on expressions of an IMMUTABLE function whose definition just
-- changed. Postgres does not know that and will happily use stale entries.
reindex table geo.place;
analyze geo.place;

-- Proof, not hope: three Arabic queries that returned nothing before.
select 'شارع'    as typed, count(*) from geo.place where search_norm like '%' || geo.normalise('شارع') || '%'
union all
select 'نواكشوط',        count(*) from geo.place where search_norm like '%' || geo.normalise('نواكشوط') || '%'
union all
select 'تفرغ',           count(*) from geo.place where search_norm like '%' || geo.normalise('تفرغ') || '%';
