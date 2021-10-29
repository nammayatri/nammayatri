ALTER TABLE atlas_transporter.ride_booking ADD COLUMN requestor_mobile_number character varying(255);
UPDATE atlas_transporter.ride_booking SET requestor_mobile_number = 'UNKNOWN';
ALTER TABLE atlas_transporter.ride_booking ALTER COLUMN requestor_mobile_number SET NOT NULL;