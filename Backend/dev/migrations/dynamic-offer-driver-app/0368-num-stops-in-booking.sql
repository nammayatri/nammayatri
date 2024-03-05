ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN num_stops int;

UPDATE atlas_driver_offer_bpp.booking SET num_stops = 0 where to_location_id is NULL;

UPDATE atlas_driver_offer_bpp.booking AS b
SET num_stops = subquery.max_order
FROM (
    SELECT entity_id, MAX("order") AS max_order
    FROM atlas_driver_offer_bpp.location_mapping
    WHERE entity_id IN (
        SELECT id
        FROM atlas_driver_offer_bpp.booking
        WHERE trip_category IN ('{"contents":"OnDemandStaticOffer","tag":"Rental"}', 'Rental_OnDemandStaticOffer')
    )
    AND "order" > 1
    AND version = 'LATEST'
    GROUP BY entity_id
) AS subquery
WHERE b.id = subquery.entity_id;

UPDATE atlas_driver_offer_bpp.booking SET num_stops = 1 where num_stops is NULL;