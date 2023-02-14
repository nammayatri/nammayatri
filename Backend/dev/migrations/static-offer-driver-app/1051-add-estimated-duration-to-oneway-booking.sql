ALTER TABLE atlas_transporter.one_way_booking ADD COLUMN estimated_duration integer;

UPDATE atlas_transporter.one_way_booking SET estimated_duration = estimated_distance / 16.6667; --60 km/h

ALTER TABLE atlas_transporter.one_way_booking ALTER COLUMN estimated_duration SET NOT NULL;