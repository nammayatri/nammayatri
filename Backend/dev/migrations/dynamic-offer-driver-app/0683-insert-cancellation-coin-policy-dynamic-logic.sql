-- Migration: Insert cancellation coin policy dynamic logic
-- Description: Add dynamic logic rollout configuration for cancellation coin policy

INSERT INTO atlas_driver_offer_bpp.app_dynamic_logic_rollout (
    domain,
    merchant_operating_city_id,
    percentage_rollout,
    time_bounds,
    version,
    created_at,
    updated_at,
    version_description,
    merchant_id,
    modified_by,
    experiment_status,
    is_base_version
) VALUES (
    'CANCELLATION-COIN-POLICY',
    'favorit0-0000-0000-0000-00000000city',
    100,
    'Unbounded',
    1,
    '2025-06-26 13:23:55.918964+00',
    '2025-06-26 13:23:55.918964+00',
    'Dynamic Coin Distribution Policy on Cancellation',
    'favorit0-0000-0000-0000-00000favorit',
    NULL,
    'CONCLUDED',
    NULL
);

-- Insert coin configuration for cancellation compensation
INSERT INTO atlas_driver_offer_bpp.coin_config (
    id,
    event_function,
    event_name,
    merchant_id,
    merchant_opt_city_id,
    coins,
    expiration_at,
    active,
    vehicle_category
) VALUES (
    '11812zw0-9ad6-b53z-0195-90cd8172f085',
    'BookingCancellationCompensation',
    'Cancellation',
    'favorit0-0000-0000-0000-00000favorit',
    'favorit0-0000-0000-0000-00000000city',
    -50,
    7776000,
    TRUE,
    'CAR'
);

-- Insert coin configuration for cancellation penalisation
INSERT INTO atlas_driver_offer_bpp.coin_config (
    id,
    event_function,
    event_name,
    merchant_id,
    merchant_opt_city_id,
    coins,
    expiration_at,
    active,
    vehicle_category
) VALUES (
    '11812rw0-9ad6-b53a-0155-41cd8143f785',
    'BookingCancellationPenalisaton',
    'Cancellation',
    'favorit0-0000-0000-0000-00000favorit',
    'favorit0-0000-0000-0000-00000000city',
    -50,
    7776000,
    TRUE,
    'CAR'
);