ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_charge integer;
UPDATE atlas_app.estimate AS T1 SET night_shift_charge = T1.estimated_total_fare - T1.estimated_fare;

-------------------------------------------------------------------------------------------
-------------------------------DROPS-------------------------------------------------------
-------------------------------------------------------------------------------------------

ALTER TABLE atlas_app.estimate DROP COLUMN waiting_time_estimated_threshold;