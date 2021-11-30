ALTER TABLE atlas_app.quote ADD COLUMN bpp_quote_id character(36);
UPDATE atlas_app.quote AS T1 SET bpp_quote_id = T1.id;
ALTER TABLE atlas_app.quote ALTER COLUMN bpp_quote_id SET NOT NULL;

ALTER TABLE atlas_app.ride_booking ADD COLUMN bpp_ride_booking_id character(36);
UPDATE atlas_app.ride_booking AS T1 SET bpp_ride_booking_id = 'USE_QUOTE_TO_FIND_IN_BPP_TABLE';
ALTER TABLE atlas_app.ride_booking ALTER COLUMN bpp_ride_booking_id SET NOT NULL;

ALTER TABLE atlas_app.ride ADD COLUMN bpp_ride_id character(36);
UPDATE atlas_app.ride AS T1 SET bpp_ride_id = 'USE_QUOTE_TO_FIND_IN_BPP_TABLE';
ALTER TABLE atlas_app.ride ALTER COLUMN bpp_ride_id SET NOT NULL;