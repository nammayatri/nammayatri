ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN num_of_locks Int  NOT NULL  DEFAULT 0;
UPDATE atlas_driver_offer_bpp.driver_information SET num_of_locks = 1 WHERE blocked = true;