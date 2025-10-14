-- Enable DigiLocker for Rajkot and Noida cities
-- This migration adds DigiLocker configuration for Rajkot and Noida merchant operating cities

-- ============================================
-- RAJKOT QUERIES
-- ============================================

-- QUERIES FOR MASTER - Rajkot
UPDATE atlas_driver_offer_bpp.transporter_config
SET digilocker_enabled = true
WHERE merchant_operating_city_id = '344811cd-5bdc-4e94-ab47-22c6d733f8f2';


-- QUERIES FOR PROD - Rajkot
UPDATE atlas_driver_offer_bpp.transporter_config
SET digilocker_enabled = true
WHERE merchant_operating_city_id = '28e88d3a-5200-4eea-810e-d7e23b0bf6ba';


-- QUERIES FOR MASTER - Rajkot
-- Will be handed over separately by the PR author


-- QUERIES FOR PROD - Rajkot
-- Will be handed over separately by the PR author


-- ============================================
-- NOIDA QUERIES
-- ============================================

-- QUERIES FOR MASTER - Noida
UPDATE atlas_driver_offer_bpp.transporter_config
SET digilocker_enabled = true
WHERE merchant_operating_city_id = 'f9fdb4d4-e92c-4112-863c-1c43092ab340';



-- QUERIES FOR PROD - Noida
UPDATE atlas_driver_offer_bpp.transporter_config
SET digilocker_enabled = true
WHERE merchant_operating_city_id = '78fc29d9-c53c-41df-9e6c-60ede613f0be';


-- QUERIES FOR MASTER - Noida
-- Will be handed over separately by the PR author


-- QUERIES FOR PROD - Noida
-- Will be handed over separately by the PR author


-- ============================================
-- SYSTEM CONFIGS UPDATE (Run once in Master and Prod)
-- ============================================

-- QUERIES FOR MASTER AND PROD
UPDATE atlas_driver_offer_bpp.system_configs
SET config_value = (
    jsonb_set(
        config_value::jsonb,
        '{disableForKV}',
        (
            (config_value::jsonb -> 'disableForKV') || '"digilocker_verification"'
        )
    )::text
)
WHERE id = 'kv_configs';

