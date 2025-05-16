INSERT INTO atlas_app.frfs_gtfs_stage_fare (
    id,
    amount,
    currency,
    merchant_id,
    merchant_operating_city_id,
    stage,
    vehicle_service_tier_id,
    vehicle_type,
    cess_charge,
    created_at,
    updated_at
)
SELECT DISTINCT ON (sf.stage, rfp.vehicle_service_tier_id)
    gen_random_uuid() as id,
    sf.amount,
    sf.currency,
    sf.merchant_id,
    sf.merchant_operating_city_id,
    sf.stage,
    rfp.vehicle_service_tier_id,
    'BUS' as vehicle_type,
    1,
    NOW() as created_at,
    NOW() as updated_at
FROM atlas_app.frfs_route_fare_product rfp
JOIN atlas_app.frfs_fare_policy fp ON rfp.fare_policy_id = fp.id
JOIN atlas_app.frfs_stage_fare sf ON sf.fare_policy_id = fp.id
WHERE fp.merchant_operating_city_id = 'c7e3c3eb-cc15-46d4-ba04-5af55ac87874'
    AND fp.merchant_id = '4b17bd06-ae7e-48e9-85bf-282fb310209c'
    AND rfp.vehicle_type = 'BUS'
    AND fp.type = 'StageBased'