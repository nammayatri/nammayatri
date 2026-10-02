-- Make the stars real: keep person.rating equal to the average of the ratings
-- the rider has actually given.
--
-- ── The gap this closes ─────────────────────────────────────────────────────
-- Riders have been able to rate since screen 14 shipped, and every rating lands
-- correctly in `atlas_driver_offer_bpp.rating`. Nothing ever reads them back.
-- `person.rating` — the column the driver's offer carries to the rider over
-- Beckn — is written by nobody, so `driverRatings` and the offer's `rating`
-- arrive null on every single ride and the app cannot draw a star.
--
-- Upstream has a subsystem that maintains this. Our binary predates it (see
-- probe-subscription.sql for the same story about subscriptions), and rebuilding
-- to get one column is exactly the trade this fork refuses. So: SQL.
--
-- ── Why a trigger rather than a timer ───────────────────────────────────────
-- backup.sh runs on a systemd timer because a backup is a periodic thing. This
-- is not: `person.rating` is a *derived* column, and the only moment it can
-- change is when a row in `rating` changes. A trigger makes it correct
-- immediately and cannot drift, go unscheduled, or need a service enabling on a
-- rebuilt server. There is no timer to forget.
--
-- Idempotent: safe to re-run. Applied by ./apply-ratings.sh.

\set ON_ERROR_STOP on

-- ── the recompute, one driver at a time ─────────────────────────────────────
--
-- AVG over bigint returns numeric, and person.rating is double precision.
-- Rounded to 2 dp so the value stored is the one a human would write down;
-- the app formats it to one decimal for display.
--
-- A driver whose every rating is deleted goes back to NULL rather than to 0 --
-- "no ratings yet" and "rated zero" are different things, and 0 is not even a
-- rating the API accepts (the scale is 1-5).
create or replace function atlas_driver_offer_bpp.refresh_driver_rating(target char(36))
returns void
language sql
as $$
  update atlas_driver_offer_bpp.person p
     set rating = (
           select round(avg(r.rating_value)::numeric, 2)
             from atlas_driver_offer_bpp.rating r
            where r.driver_id = target
         )
   where p.id = target;
$$;

create or replace function atlas_driver_offer_bpp.rating_changed()
returns trigger
language plpgsql
as $$
begin
  -- On UPDATE the rating may have been moved between drivers. Refreshing both
  -- sides costs nothing and leaves no stale average behind on the old one.
  if (tg_op = 'DELETE' or tg_op = 'UPDATE') then
    perform atlas_driver_offer_bpp.refresh_driver_rating(old.driver_id);
  end if;
  if (tg_op = 'INSERT' or tg_op = 'UPDATE') then
    perform atlas_driver_offer_bpp.refresh_driver_rating(new.driver_id);
  end if;
  return null;
end;
$$;

drop trigger if exists rating_maintains_person_rating on atlas_driver_offer_bpp.rating;

create trigger rating_maintains_person_rating
after insert or update or delete on atlas_driver_offer_bpp.rating
for each row execute function atlas_driver_offer_bpp.rating_changed();

-- ── backfill ────────────────────────────────────────────────────────────────
--
-- Every driver who has ever been rated, including the ones whose person.rating
-- was set by hand during testing and is now simply wrong. Measured before this
-- ran: Karim carried 3.67 while his three real ratings (2, 2, 5) average 3.00.
update atlas_driver_offer_bpp.person p
   set rating = agg.average
  from (
         select driver_id, round(avg(rating_value)::numeric, 2) as average
           from atlas_driver_offer_bpp.rating
          group by driver_id
       ) agg
 where p.id = agg.driver_id
   and p.rating is distinct from agg.average;

-- Anyone carrying a rating with no ratings behind it is showing a number that
-- came from nowhere. Clear it rather than leave it.
update atlas_driver_offer_bpp.person p
   set rating = null
 where p.rating is not null
   and not exists (select 1 from atlas_driver_offer_bpp.rating r where r.driver_id = p.id);

\echo == drivers with a rating, after backfill
select p.first_name,
       p.rating,
       (select count(*) from atlas_driver_offer_bpp.rating r where r.driver_id = p.id) as ratings
  from atlas_driver_offer_bpp.person p
 where p.rating is not null
 order by p.first_name;
