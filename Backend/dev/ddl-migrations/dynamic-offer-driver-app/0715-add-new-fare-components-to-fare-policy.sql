ALTER TABLE atlas_driver_offer_bpp.fare_policy
ADD COLUMN return_fee JSON,
ADD COLUMN booth_charges JSON,
ADD COLUMN per_luggage_charge double precision;

ALTER TABLE atlas_driver_offer_bpp.fare_parameters
ADD COLUMN booth_charge double precision,
ADD COLUMN luggage_charge double precision,
ADD COLUMN return_fee_charge double precision;