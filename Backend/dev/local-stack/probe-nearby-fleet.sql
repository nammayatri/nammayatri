-- Can we list the nearby drivers WITH their cars, without touching Haskell?
--
--     scp probe-nearby-fleet.sql ny:/tmp/
--     ssh ny 'docker cp /tmp/probe-nearby-fleet.sql ny-postgres:/tmp/ \
--             && docker exec ny-postgres psql -U postgres -d atlas_dev \
--                       -f /tmp/probe-nearby-fleet.sql'
--
-- Read-only. The client has asked three times for the passenger to see the
-- actual cars -- model, colour, plate, driver -- of the drivers his request
-- will reach. The rider API cannot answer it: `driversLatLong` is {lat, lon},
-- and the BPP's own DriverPoolResult carries driverId, variant, lat, lon and no
-- vehicle at all.
--
-- Before proposing a Haskell change and a rebuild, this asks the cheaper
-- question: is all of it already sitting in the database, joinable in one
-- query? If it is, a small read-only service can serve it the same way
-- maps-shim serves geocoding -- no rebuild, no new Haskell.

\echo ''
\echo '=== 1. where a driver position lives, and how fresh ==='
SELECT string_agg(column_name, ', ' ORDER BY ordinal_position)
  FROM information_schema.columns
 WHERE table_schema = 'atlas_driver_offer_bpp' AND table_name = 'driver_location';

\echo ''
\echo '=== 2. what marks a driver as available ==='
SELECT string_agg(column_name, ', ' ORDER BY ordinal_position)
  FROM information_schema.columns
 WHERE table_schema = 'atlas_driver_offer_bpp' AND table_name = 'driver_information';

\echo ''
\echo '=== 3. THE JOIN -- everything the client asked for, in one row each ==='
\echo '    driver, rating, car, colour, plate, type, position, freshness.'
SELECT p.first_name                        AS driver,
       p.rating,
       v.make, v.model, v.color, v.registration_no AS plate,
       v.variant,
       round(l.lat::numeric, 5) AS lat,
       round(l.lon::numeric, 5) AS lon,
       date_trunc('second', now() - l.updated_at) AS position_age,
       di.active, di.on_ride, di.blocked
  FROM atlas_driver_offer_bpp.driver_location l
  JOIN atlas_driver_offer_bpp.person p  ON p.id = l.driver_id
  JOIN atlas_driver_offer_bpp.vehicle v ON v.driver_id = l.driver_id
  JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = l.driver_id
 ORDER BY l.updated_at DESC
 LIMIT 12;

\echo ''
\echo '=== 4. how many would actually be listable right now ==='
\echo '    Same filters the dispatch pool uses: active, not blocked, not on a'
\echo '    ride, and a position fresher than the staleness window.'
SELECT count(*) FILTER (WHERE di.active AND NOT di.blocked AND NOT di.on_ride
                          AND l.updated_at > now() - interval '5 minutes') AS listable_now,
       count(*)                                                            AS drivers_with_a_position
  FROM atlas_driver_offer_bpp.driver_location l
  JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = l.driver_id;

\echo ''
\echo '=== 5. is the year there? (the one field the mock-up needs and we lack) ==='
SELECT count(*) AS vehicles,
       count(*) FILTER (WHERE v.make IS NOT NULL AND v.make <> '') AS with_make,
       count(*) FILTER (WHERE v.color IS NOT NULL AND v.color <> '') AS with_colour,
       count(*) FILTER (WHERE v.registration_no IS NOT NULL) AS with_plate
  FROM atlas_driver_offer_bpp.vehicle v;
