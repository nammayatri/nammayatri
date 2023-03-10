ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN driver_min_extra_charge  double precision;
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN driver_max_extra_charge  double precision;

update atlas_driver_offer_bpp.search_request_for_driver set driver_min_extra_charge=0;
update atlas_driver_offer_bpp.search_request_for_driver set driver_max_extra_charge=0;

alter table atlas_driver_offer_bpp.search_request_for_driver alter column driver_min_extra_charge set not null;

alter table atlas_driver_offer_bpp.search_request_for_driver alter column driver_max_extra_charge set not null;