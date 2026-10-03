-- ClickHouse: atlas_driver_offer_bpp.bpp_transaction_join — columns for the dashboard ride list
--
-- Read by Storage/Clickhouse/BppTransactionJoin.hs (dynamic-offer-driver-app). With these columns the ride list can serve
-- the hasSos / paymentCollectedBy filters and the SAFETY / TAX detail groups from ClickHouse instead of Postgres.
-- The driver-app selects these columns, so they must exist (and be backfilled) before the code that reads them is deployed.
--
-- Column                            Source (Postgres atlas_driver_offer_bpp)
-- ride_booking_id                   ride.booking_id
-- ride_sos_id                       ride.sos_id
-- ride_driver_deviated_from_route   ride.driver_deviated_from_route
-- ride_safety_alert_triggered       ride.safety_alert_triggered
-- booking_payment_method_id         booking.payment_method_id
--
-- The two Bool columns are read back as strings ('true' / 'false' or '1' / '0'), the same way the table's existing
-- Bool columns are, so keep them as String like those (or any type that FORMAT JSON emits as a quoted string).

ALTER TABLE atlas_driver_offer_bpp.bpp_transaction_join
    ADD COLUMN IF NOT EXISTS `ride_booking_id` Nullable(String),
    ADD COLUMN IF NOT EXISTS `ride_sos_id` Nullable(String),
    ADD COLUMN IF NOT EXISTS `ride_driver_deviated_from_route` Nullable(String),
    ADD COLUMN IF NOT EXISTS `ride_safety_alert_triggered` Nullable(String),
    ADD COLUMN IF NOT EXISTS `booking_payment_method_id` Nullable(String);
-- On a replicated/distributed setup, run with ON CLUSTER <cluster> (and on the local tables behind any Distributed table).

-- Backfill
-- New rows: add the five source columns above to whatever populates bpp_transaction_join (the ride / booking join).
-- Existing rows: re-insert historical rows with the new columns filled (e.g. re-run that population query for past
-- dates); the table is read with FINAL, so the newer version of each row replaces the old one.
