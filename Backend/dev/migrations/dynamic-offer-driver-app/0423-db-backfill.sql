ALTER TABLE atlas_driver_offer_bpp.fare_policy ADD COLUMN congestion_charge text;

UPDATE atlas_driver_offer_bpp.fare_policy
SET congestion_charge =
    CASE
        WHEN congestion_charge_multiplier IS NOT NULL
            THEN REPLACE('{"contents":???,"tag":"ExtraDistanceFare"}'::text, '???'::text, congestion_charge_multiplier::text)::json
        ELSE NULL
    END;