ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN estimate_id character(36);

-- backfilling only for cancellable bookings of last 24 hrs
UPDATE atlas_driver_offer_bpp.booking AS b
SET estimate_id = dq.estimate_id
FROM atlas_driver_offer_bpp.driver_quote AS dq
WHERE b.created_at >= CURRENT_TIMESTAMP - INTERVAL '1 day'
AND dq.created_at >= CURRENT_TIMESTAMP - INTERVAL '1 day' -- only to make query faster
AND b.status NOT IN ('COMPLETED', 'CANCELLED')
AND b.quote_id = dq.id
AND b.trip_category IN ('OneWay_OneWayOnDemandDynamicOffer', '{"contents":"OneWayOnDemandDynamicOffer","tag":"OneWay"}','{"contents":"OneWayOnDemandStaticOffer","tag":"InterCity"}','InterCity_OneWayOnDemandDynamicOffer');