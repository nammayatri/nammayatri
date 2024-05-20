ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ADD COLUMN free_wating_time integer;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ADD COLUMN free_wating_time integer;

-- ONLY FOR LOCAL
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN waiting_time_estimated_threshold int;
UPDATE atlas_driver_offer_bpp.transporter_config SET waiting_time_estimated_threshold = 3;


WITH WaitingTimeEstimatedThreshold1 AS (
  SELECT T1.id,
    T2.waiting_time_estimated_threshold
  FROM atlas_driver_offer_bpp.fare_policy AS T1
    JOIN atlas_driver_offer_bpp.transporter_config AS T2
    ON T1.merchant_id = T2.merchant_id
)

UPDATE atlas_driver_offer_bpp.fare_policy_slabs_details_slab AS T1 SET free_wating_time =
    (SELECT T2.waiting_time_estimated_threshold FROM WaitingTimeEstimatedThreshold1 AS T2 WHERE T2.id = T1.fare_policy_id);

WITH WaitingTimeEstimatedThreshold2 AS (
  SELECT T1.id,
    T2.waiting_time_estimated_threshold
  FROM atlas_driver_offer_bpp.fare_policy AS T1
    JOIN atlas_driver_offer_bpp.transporter_config AS T2
    ON T1.merchant_id = T2.merchant_id
)

UPDATE atlas_driver_offer_bpp.fare_policy_progressive_details AS T1 SET free_wating_time =
    (SELECT T2.waiting_time_estimated_threshold FROM WaitingTimeEstimatedThreshold2 AS T2 WHERE T2.id = T1.fare_policy_id);


-------------------------------------------------------------------------------------------
-------------------------------DROPS-------------------------------------------------------
-------------------------------------------------------------------------------------------

ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab ALTER COLUMN free_wating_time SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details ALTER COLUMN free_wating_time SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.transporter_config DROP COLUMN waiting_time_estimated_threshold;
