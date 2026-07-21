-- Commission-on-cancellation rate for dues folded into the next ride's fare.
-- Same FareChargeConfig JSON shape as its 0379 siblings, e.g.
-- {"value":"15%","appliesOn":["CustomerCancellationChargeComponent"]}.
-- NULL => computeConfiguredCharge returns 0 => no cancellation commission.
-- fare_policy has no NammaDSL storage spec, hence hand-written (mirrors 0379).

ALTER TABLE atlas_driver_offer_bpp.fare_policy
ADD COLUMN IF NOT EXISTS cancellation_commission_charge_config TEXT;
