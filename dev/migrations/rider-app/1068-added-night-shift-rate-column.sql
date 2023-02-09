ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_multiplier numeric(10,2);
ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_start time without time zone;
ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_end time without time zone;
