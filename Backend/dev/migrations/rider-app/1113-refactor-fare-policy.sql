ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_charge integer;

-------------------------------------------------------------------------------------------
-------------------------------DROPS-------------------------------------------------------
-------------------------------------------------------------------------------------------

ALTER TABLE atlas_app.estimate DROP COLUMN waiting_time_estimated_threshold;