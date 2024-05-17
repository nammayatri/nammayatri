ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN vehicle_variant character(255);

update atlas_driver_offer_bpp.search_request
	set vehicle_variant='AUTO_RICKSHAW'
	where vehicle_variant is null;

ALTER TABLE atlas_driver_offer_bpp.search_request ALTER COLUMN vehicle_variant SET NOT NULL;