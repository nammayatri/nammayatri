ALTER TABLE atlas_driver_offer_bpp.vehicle ADD COLUMN vehicle_class CHARACTER varying(255);
UPDATE atlas_driver_offer_bpp.vehicle SET vehicle_class = '3WT';
ALTER TABLE atlas_driver_offer_bpp.vehicle ALTER COLUMN vehicle_class SET NOT NULL;
