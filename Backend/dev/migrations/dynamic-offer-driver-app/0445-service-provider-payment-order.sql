ALTER TABLE atlas_driver_offer_bpp.payment_order add column service_provider text default 'Juspay';

UPDATE atlas_driver_offer_bpp.vehicle_service_tier set air_conditioned_threshold = air_conditioned;
UPDATE atlas_driver_offer_bpp.vehicle_service_tier set is_air_conditioned = true where air_conditioned_threshold is not null;