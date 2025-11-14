ALTER TABLE atlas_driver_offer_bpp.fare_policy
ADD COLUMN return_fee JSON,
ADD COLUMN booth_charges JSON,
ADD COLUMN per_luggage_charge double precision;


ALTER TABLE atlas_driver_offer_bpp.fare_parameters
ADD COLUMN booth_charge double precision,
ADD COLUMN luggage_charge double precision,
ADD COLUMN return_fee_charge double precision;

-- Don't run anywhere, this is for local testing --
UPDATE atlas_driver_offer_bpp.fare_policy
SET
  per_luggage_charge = 5,
  return_fee = '{"tag":"ReturnFeeFixed","contents":10.0}',
  booth_charges = '{"tag":"BoothChargePercentage","contents":10.0}'
