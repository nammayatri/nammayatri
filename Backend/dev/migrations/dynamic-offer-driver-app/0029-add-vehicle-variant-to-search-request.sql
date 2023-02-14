ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN vehicle_variant character(255);

update atlas_driver_offer_bpp.search_request AS T1
	set vehicle_variant=upd.vehicle_variant
	from atlas_driver_offer_bpp.driver_quote  as upd
	where T1.id=upd.search_request_id;

update atlas_driver_offer_bpp.search_request
	set vehicle_variant='AUTO_RICKSHAW'
	where vehicle_variant is null;

ALTER TABLE atlas_driver_offer_bpp.search_request ALTER COLUMN vehicle_variant SET NOT NULL;