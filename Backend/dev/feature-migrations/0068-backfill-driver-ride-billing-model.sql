-- Backfill ride_billing_model for drivers already on prepaid. NO DDL here; the column
-- is added by the NammaDSL generator (migrations-read-only/dynamic-offer-driver-app/
-- driver_information.sql) and must exist before this runs.
--
-- The column is new, so it is null for every existing row, and null reads as "not
-- prepaid" (isPrepaidBillingModel / isExemptFromPostpaidDuesFlag). Dispatch would
-- therefore keep gating an existing prepaid driver on `subscribed` -- a flag that is
-- only ever set for postpaid -- and they would silently stop receiving rides until
-- some other path happened to set the model. DriverPoolMigrations version 7 copies
-- this column into each pool entry, so the backfill has to land before that
-- migration propagates the nulls.
--
-- Authoritative signal is the driver's own active, unexpired PREPAID_SUBSCRIPTION
-- purchase: the same predicate as findAllActiveByOwnerAndServiceName, plus the
-- expiry check that findLatestActiveByOwnerAndServiceName applies on top of it.
--
-- Fleet drivers are deliberately NOT backfilled. Their exemption comes from the
-- fleet clause in isExemptFromPostpaidDuesFlag at a prepaid merchant, and the
-- subscription belongs to the fleet owner rather than to them; writing a model they
-- do not own would outlive their leaving the fleet.
--
-- Postpaid drivers are left null rather than written as YATRI_SUBSCRIPTION: null is
-- already the registration default for them (Registration.hs), and the gates treat
-- null as postpaid.
UPDATE atlas_driver_offer_bpp.driver_information di
   SET ride_billing_model = 'PREPAID_SUBSCRIPTION'
 WHERE di.ride_billing_model IS NULL
   AND EXISTS
         ( SELECT 1
             FROM atlas_driver_offer_bpp.subscription_purchase sp
            WHERE sp.owner_id = di.driver_id
              AND sp.owner_type = 'DRIVER'
              AND sp.status = 'ACTIVE'
              AND sp.service_name = 'PREPAID_SUBSCRIPTION'
              AND (sp.expiry_date IS NULL OR sp.expiry_date > now()) );
