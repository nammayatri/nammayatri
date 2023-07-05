ALTER TABLE atlas_driver_offer_bpp.driver_fee ALTER COLUMN short_id SET DATA TYPE character varying (36);

UPDATE atlas_driver_offer_bpp.driver_fee
  set short_id = replace(short_id, ' ', '');

UPDATE atlas_driver_offer_bpp.payment_order
  set short_id = replace(short_id, ' ', '');
