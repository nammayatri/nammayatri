-- What does the deployed backend know about switching a driver off?
--
-- The business model here is not a commission: passengers pay drivers in cash,
-- and we sell each driver a monthly subscription. So the one thing the system
-- must eventually do is stop an unpaid driver receiving work. The client asked
-- for that to happen automatically.
--
-- Two separate questions, and they have opposite answers:
--   (a) is there a flag that stops a driver receiving requests?
--   (b) is there anywhere to record that he paid?
--
--   docker cp probe-subscription.sql ny-postgres:/tmp/ && \
--   docker exec ny-postgres psql -U postgres -d atlas_dev -f /tmp/probe-subscription.sql
--
-- (Container is `ny-postgres`, user `postgres`, db `atlas_dev`. Not `atlas`,
-- and not `atlas-postgres` -- both were guessed once and both were wrong.)
--
-- MEASURED 2026-08-16 -- the answer, so nobody has to re-run this to plan:
--
--   (a) THE SWITCH EXISTS. driver_information carries `enabled`, `blocked`,
--       `active`, `verified`, `last_enabled_on`. Flipping one boolean stops
--       dispatch immediately. Cutting a driver off is a one-field UPDATE.
--
--   (b) THE RECORD DOES NOT EXIST -- AT ALL. Nothing anywhere in the schema is
--       about plans, subscriptions, fees or invoices. Every `%subscri%` hit is
--       either the BECKN registry's `subscriber` table or a pg_catalog view.
--       Upstream's driver-subscription subsystem is simply not in this binary.
--
-- So "switch him off automatically" is cheap, and the expensive half is the
-- half nobody asks about: somewhere to record who paid and until when. Plan is
-- a `paid_until` date per driver plus a nightly job flipping `enabled` -- the
-- marking stays manual because drivers pay cash or CIB, outside the app.
-- That admin screen is website work, not app work.
--
-- Fleet at time of measurement: 12 rows, all enabled=t blocked=f -- six real
-- simulator drivers active, upstream `favorit-*` seeds active, and the
-- `ND-closest-driver-*` fixture inactive.
\pset pager off

\echo == driver_information: flags that could gate dispatch
select column_name, data_type
  from information_schema.columns
 where table_schema = 'atlas_driver_offer_bpp'
   and table_name = 'driver_information'
   and (column_name ilike '%enable%' or column_name ilike '%block%'
     or column_name ilike '%subscri%' or column_name ilike '%plan%'
     or column_name ilike '%verif%'  or column_name ilike '%payment%'
     or column_name ilike '%active%')
 order by 1;

\echo == any table at all about plans, subscriptions, fees, invoices
select table_schema, table_name
  from information_schema.tables
 where table_name ilike '%plan%'    or table_name ilike '%subscri%'
    or table_name ilike '%fee%'     or table_name ilike '%invoice%'
    or table_name ilike '%payment%'
 order by 1, 2;

\echo == current fleet: who is enabled, blocked, active
select di.driver_id, di.enabled, di.blocked, di.active
  from atlas_driver_offer_bpp.driver_information di
 order by di.driver_id
 limit 12;
