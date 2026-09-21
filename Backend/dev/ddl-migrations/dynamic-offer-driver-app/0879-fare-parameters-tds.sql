-- Per-ride TDS on fare_parameters.
--
-- tds_amount is the deduction actually applied to the driver's / fleet owner's
-- take home; tds_rate is the rate used to compute it.
--
-- Both are set at ride end or at cancellation, never at quote time: the deduction
-- is not decided until the FY threshold gate runs. They stay NULL for rides that
-- fell below the threshold, and for merchants with no TDS configured.
--
-- tds_rate is stored rather than re-derived because it is per-entity and mutable
-- (driver_information.tds_rate / fleet_owner_information.tds_rate change on
-- LDC / TAN / UDYAM approval), so a past deduction cannot be explained without it.

-- tds_processed_at marks that the FY accumulator has already been incremented for
-- this ride. The accumulator is a running total with no per-ride key, so without
-- this a replay would double-count earnings and could wrongly trip the threshold.
-- It is set even when no TDS was due, so it distinguishes "not processed" from
-- "processed, nothing deducted" -- which tds_amount alone cannot.

ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN tds_amount numeric(30,2);
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN tds_rate double precision;
ALTER TABLE atlas_driver_offer_bpp.fare_parameters ADD COLUMN tds_processed_at timestamp with time zone;
