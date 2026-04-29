ALTER TABLE atlas_driver_offer_bpp.quote_special_zone ADD column special_location_tag text;
ALTER TABLE atlas_driver_offer_bpp.search_request_special_zone ADD column "area" text;
--ALTER TABLE atlas_driver_offer_bpp.booking ADD column "area" text;
ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD column "description" text;
CREATE TABLE atlas_driver_offer_bpp.special_location_priority (
  id character(36) NOT NULL PRIMARY KEY,
  merchant_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id),
  category character varying(255) NOT NULL,
  pickup_priority integer NOT NULL,
  drop_priority integer NOT NULL
);
CREATE INDEX idx_special_location_priority ON atlas_driver_offer_bpp.special_location_priority USING btree (merchant_id, category);

CREATE INDEX idx_fare_product ON atlas_driver_offer_bpp.fare_product USING btree (merchant_id, vehicle_variant, "area");

-- Special Location Fare Product with RideOTP Quotes For All Merchants
-- SpecialLocation : 1 SpecialLocation x 4 VehicleVariants x 2 PickupOrDrop = Maximum of 8 Entries Per Merchant (Optional, Fallback is Default but it will result in 2 DB Queries rather then One. So its good to have link)
CREATE TEMPORARY TABLE pickup_drop_table (
  pickup_drop_type text
);
    -- where merchant_id='...' -- can be merchant specific, for NY we may want Normal Estimates but for JaatriSaathi we may need Special Zone Quotes

------------------------------------------------------------------------------------------------------------------------
--------------------------------------------DROP------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
ALTER TABLE atlas_driver_offer_bpp.fare_policy DROP column vehicle_variant;
ALTER TABLE atlas_driver_offer_bpp.fare_policy DROP column merchant_id;