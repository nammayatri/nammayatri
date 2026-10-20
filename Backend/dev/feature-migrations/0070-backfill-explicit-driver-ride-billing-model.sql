-- Give every driver an explicit ride_billing_model. NO DDL here.
--
-- 0068 set PREPAID_SUBSCRIPTION only for drivers holding an active prepaid purchase and
-- left everyone else null. Null is ambiguous: the go-online gate resolves it from the
-- catalogue (modelOrCatalogue), while dispatch reads it as postpaid.
-- Switching between models needs one answer, so this resolves every null the way
-- go-online already does:
--
--   (a) PREPAID_SUBSCRIPTION in services_enabled_for_subscription -> PREPAID_SUBSCRIPTION.
--       These drivers already cannot go online without an active prepaid purchase and
--       balance above the threshold (checkPrepaidGoOnlineEligibility), so this records the
--       model they are on rather than moving them.
--   (b) Everyone else -> YATRI_SUBSCRIPTION. A null catalogue reads as [YATRI_SUBSCRIPTION]
--       (backfillServiceEnabledForSubscription), so it falls here.
--
-- Fleet drivers get their own model like anyone else. While they are in a fleet at a
-- prepaid merchant the fleet owner pays, whatever this column holds; it only takes effect
-- if they leave the fleet.
--
-- Ordering: run BEFORE deploying the code that ships DriverPoolMigrations v8, which copies
-- this column into each driver's pool entry once and then never again. Re-running after
-- the deploy is safe (only nulls are touched) and catches drivers who registered on the
-- old code in between.

-- (a)
UPDATE atlas_driver_offer_bpp.driver_information
   SET ride_billing_model = 'PREPAID_SUBSCRIPTION'
 WHERE ride_billing_model IS NULL
   AND 'PREPAID_SUBSCRIPTION' = ANY (services_enabled_for_subscription);

-- (b)
UPDATE atlas_driver_offer_bpp.driver_information
   SET ride_billing_model = 'YATRI_SUBSCRIPTION'
 WHERE ride_billing_model IS NULL;
