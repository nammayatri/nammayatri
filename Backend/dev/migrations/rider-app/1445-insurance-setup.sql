--- ONLY FOR LOCAL AND MASTER --
INSERT INTO atlas_app.insurance_config (
    id,
    merchant_id,
    merchant_operating_city_id,
    trip_category,
    vehicle_category,
    city,
    state,
    hours,
    partner_id,
    plan,
    plan_type,
    allowed_vehicle_service_tiers,
    created_at,
    updated_at
)
SELECT
    atlas_app.uuid_generate_v4(),
    m.merchant_id,
    m.id,
    'OneWay_OneWayOnDemandDynamicOffer',
    'CAR',
    m.city,
    'WEST BENGAL',
    6,
    'Yatri_Sathi',
    'yatri_sathi_taxi',
    1,
    NULL,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city m WHERE m.city = 'Kolkata';

--- ONLY FOR LOCAL AND MASTER --
INSERT INTO atlas_app.insurance_config (
    id,
    merchant_id,
    merchant_operating_city_id,
    trip_category,
    vehicle_category,
    city,
    state,
    hours,
    partner_id,
    plan,
    plan_type,
    allowed_vehicle_service_tiers,
    created_at,
    updated_at
)
SELECT
    atlas_app.uuid_generate_v4(),
    m.merchant_id,
    m.id,
    'OneWay_OneWayRideOtp',
    'CAR',
    m.city,
    'WEST BENGAL',
    6,
    'Yatri_Sathi',
    'yatri_sathi_taxi',
    1,
    NULL,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city m WHERE m.city = 'Kolkata';

--- ONLY FOR LOCAL AND MASTER --
INSERT INTO atlas_app.insurance_config (
    id,
    merchant_id,
    merchant_operating_city_id,
    trip_category,
    vehicle_category,
    city,
    state,
    hours,
    partner_id,
    plan,
    plan_type,
    allowed_vehicle_service_tiers,
    created_at,
    updated_at
)
SELECT
    atlas_app.uuid_generate_v4(),
    m.merchant_id,
    m.id,
    'OneWay_OneWayOnDemandDynamicOffer',
    'MOTORCYCLE',
    m.city,
    'WEST BENGAL',
    6,
    'Yatri_Sathi',
    'yatri_sathi_taxi',
    1,
    NULL,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city m WHERE m.city = 'Kolkata';

--- ONLY FOR LOCAL AND MASTER --
INSERT INTO atlas_app.insurance_config (
    id,
    merchant_id,
    merchant_operating_city_id,
    trip_category,
    vehicle_category,
    city,
    state,
    hours,
    partner_id,
    plan,
    plan_type,
    allowed_vehicle_service_tiers,
    created_at,
    updated_at
)
SELECT
    atlas_app.uuid_generate_v4(),
    m.merchant_id,
    m.id,
    'OneWay_OneWayRideOtp',
    'MOTORCYCLE',
    m.city,
    'WEST BENGAL',
    6,
    'Yatri_Sathi',
    'yatri_sathi_taxi',
    1,
    NULL,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city m WHERE m.city = 'Kolkata';







--------------------------- FOR LOCAL TESTING ONLY -------------------------------

--- ONLY FOR LOCAL --
INSERT INTO atlas_app.insurance_config (
    id,
    merchant_id,
    merchant_operating_city_id,
    trip_category,
    vehicle_category,
    city,
    state,
    hours,
    partner_id,
    plan,
    plan_type,
    allowed_vehicle_service_tiers,
    created_at,
    updated_at
)
SELECT
    atlas_app.uuid_generate_v4(),
    m.merchant_id,
    m.id,
    'OneWay_OneWayOnDemandDynamicOffer',
    'CAR',
    m.city,
    'WEST BENGAL',
    6,
    'Yatri_Sathi',
    'yatri_sathi_taxi',
    1,
    NULL,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city m;

--- ONLY FOR LOCAL --
INSERT INTO atlas_app.insurance_config (
    id,
    merchant_id,
    merchant_operating_city_id,
    trip_category,
    vehicle_category,
    city,
    state,
    hours,
    partner_id,
    plan,
    plan_type,
    allowed_vehicle_service_tiers,
    created_at,
    updated_at
)
SELECT
    atlas_app.uuid_generate_v4(),
    m.merchant_id,
    m.id,
    'OneWay_OneWayRideOtp',
    'CAR',
    m.city,
    'WEST BENGAL',
    6,
    'Yatri_Sathi',
    'yatri_sathi_taxi',
    1,
    NULL,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city m;

--- ONLY FOR LOCAL --
INSERT INTO atlas_app.insurance_config (
    id,
    merchant_id,
    merchant_operating_city_id,
    trip_category,
    vehicle_category,
    city,
    state,
    hours,
    partner_id,
    plan,
    plan_type,
    allowed_vehicle_service_tiers,
    created_at,
    updated_at
)
SELECT
    atlas_app.uuid_generate_v4(),
    m.merchant_id,
    m.id,
    'OneWay_OneWayOnDemandDynamicOffer',
    'MOTORCYCLE',
    m.city,
    'WEST BENGAL',
    6,
    'Yatri_Sathi',
    'yatri_sathi_taxi',
    1,
    NULL,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city m;

--- ONLY FOR LOCAL --
INSERT INTO atlas_app.insurance_config (
    id,
    merchant_id,
    merchant_operating_city_id,
    trip_category,
    vehicle_category,
    city,
    state,
    hours,
    partner_id,
    plan,
    plan_type,
    allowed_vehicle_service_tiers,
    created_at,
    updated_at
)
SELECT
    atlas_app.uuid_generate_v4(),
    m.merchant_id,
    m.id,
    'OneWay_OneWayRideOtp',
    'MOTORCYCLE',
    m.city,
    'WEST BENGAL',
    6,
    'Yatri_Sathi',
    'yatri_sathi_taxi',
    1,
    NULL,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city m;


update atlas_app.insurance_config set insured_amount = '₹ 2,00,000' where vehicle_category = 'CAR' and plan = 'yatri_sathi_taxi';
update atlas_app.insurance_config set driver_insured_amount = '₹ 2,00,000' where vehicle_category = 'CAR' and plan = 'yatri_sathi_taxi';

update atlas_app.insurance_config set insured_amount = '₹ 50,000' where vehicle_category = 'MOTORCYCLE' and plan = 'yatri_sathi_taxi';
update atlas_app.insurance_config set driver_insured_amount = '₹ 50,000' where vehicle_category = 'MOTORCYCLE' and plan = 'yatri_sathi_taxi';