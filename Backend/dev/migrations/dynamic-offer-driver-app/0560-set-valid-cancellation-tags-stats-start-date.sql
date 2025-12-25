-----------------------------BEWARE-HEAVY-QUERY---------------------------

------------RUN BELOW QUERY UNTIL YOU DON'T SEE UPDATE LESS THAN LIMIT VALUE-------------

WITH batch AS (
    SELECT driver_id
    FROM atlas_driver_offer_bpp.driver_stats
    WHERE valid_cancellation_tags_stats_start_date IS NULL
    LIMIT 100000
)
UPDATE atlas_driver_offer_bpp.driver_stats
SET valid_cancellation_tags_stats_start_date = CURRENT_TIMESTAMP
WHERE driver_id IN (SELECT driver_id FROM batch);
