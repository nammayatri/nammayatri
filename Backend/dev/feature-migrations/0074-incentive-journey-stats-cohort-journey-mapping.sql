-- Backfill cohort_journey_mapping_id after the column exists.
-- The column and unique key come from the generated migrations
-- (migrations-read-only/*/incentive_journey_stats.sql). This file only fills
-- existing rows. Safe only while (merchant_operating_city_id, cohort_id, journey_id)
-- is unique on cohort_journey_mapping, so each stats row matches one campaign.
UPDATE atlas_driver_offer_bpp.incentive_journey_stats s
SET cohort_journey_mapping_id = cjm.id
FROM atlas_driver_offer_bpp.cohort_journey_mapping cjm
WHERE s.cohort_journey_mapping_id IS NULL
  AND cjm.merchant_operating_city_id = s.merchant_operating_city_id
  AND cjm.cohort_id = s.cohort_id
  AND cjm.journey_id = s.journey_id;

UPDATE atlas_app.incentive_journey_stats s
SET cohort_journey_mapping_id = cjm.id
FROM atlas_app.cohort_journey_mapping cjm
WHERE s.cohort_journey_mapping_id IS NULL
  AND cjm.merchant_operating_city_id = s.merchant_operating_city_id
  AND cjm.cohort_id = s.cohort_id
  AND cjm.journey_id = s.journey_id;
