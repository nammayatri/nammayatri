ALTER TABLE atlas_driver_offer_bpp.payment_transaction ADD COLUMN split_settlement_details json;

-- Local testing --
INSERT INTO atlas_driver_offer_bpp.split_details(
	id, amount_percentage, fixed_amount, service_name, vendor_id, merchant_id, merchant_operating_city_id, created_at, updated_at)
	VALUES ('favorit0-0000-0000-0000-00000vendor1', 33.33, 3.0, 'YATRI_SUBSCRIPTION', 'VENDOR1','favorit0-0000-0000-0000-00000favorit', 'favorit0-0000-0000-0000-00000000city', now(), now());

INSERT INTO atlas_driver_offer_bpp.split_details(
	id, amount_percentage, fixed_amount, service_name, vendor_id, merchant_id, merchant_operating_city_id, created_at, updated_at)
	VALUES ('favorit0-0000-0000-0000-00000vendor2', 33.33, 3.0, 'YATRI_SUBSCRIPTION', 'VENDOR2','favorit0-0000-0000-0000-00000favorit', 'favorit0-0000-0000-0000-00000000city', now(), now());