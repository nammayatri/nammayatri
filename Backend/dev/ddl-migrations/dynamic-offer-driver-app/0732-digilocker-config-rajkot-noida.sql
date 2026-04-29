

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

