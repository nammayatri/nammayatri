-- NOTE: Below block queries only for Local, DON'T RUN IN MATSER OR PROD
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
INSERT INTO atlas_app.partner_organization (org_id, merchant_id, name, api_key_hash, api_key_encrypted, created_at, updated_at)
VALUES
    (md5(random()::TEXT || clock_timestamp()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT)::UUID,
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'GOOGLE_MAPS_METRO', '\x55a60c1b94f887e87311ae5e85b9799e7876eae10d52655bafdf6eab0406fca2',
    'here goes the encrypted api key',
    now(), now());
-- Local api key: d4212a34-9beb-ae42-124d-31e3c48a935c

DELETE FROM atlas_app.station where code in ('LC5TKn9rGqWQHwXxWbdzeEZ6vXjdNV', '6ZVHrYAsHdGxShBlNypUstSxs5wRk4');

UPDATE atlas_app.beckn_config SET vehicle_category = 'METRO' WHERE vehicle_category = 'CAB' and domain = 'FRFS';

INSERT INTO atlas_app.station
    (integrated_bpp_config_id, address, code, id, lat, lon, name, vehicle_type, merchant_id, merchant_operating_city_id, created_at, updated_at)
VALUES
    ('abcd23a5-3ce6-4c37-8b9b-41377c3c1a52', null, 'LC5TKn9rGqWQHwXxWbdzeEZ6vXjdNV', atlas_app.uuid_generate_v4(), 9.9834, 76.2823, 'MG Road', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'namma-yatri-0-0000-0000-00000000city', now(), now()),
    ('abcd23a5-3ce6-4c37-8b9b-41377c3c1a52', null, '6ZVHrYAsHdGxShBlNypUstSxs5wRk4', atlas_app.uuid_generate_v4(), 10.0251, 76.3083, 'Edapally Jn.', 'METRO', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'namma-yatri-0-0000-0000-00000000city', now(), now());

WITH MGS AS (SELECT * from atlas_app.station WHERE code = 'LC5TKn9rGqWQHwXxWbdzeEZ6vXjdNV' and merchant_id = 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52'),
POrg AS (SELECT * from atlas_app.partner_organization WHERE name = 'GOOGLE_MAPS_METRO')
INSERT INTO atlas_app.partner_org_station (partner_org_id, station_id, name, partner_org_station_id)
SELECT org_id as partner_org_id, id as station_id, 'M.G. Road' as name, 'ChIJfeB9G1ANCDsRWdypiGiYrJo' as partner_org_station_id FROM MGS CROSS JOIN POrg;

WITH EDS AS (SELECT * from atlas_app.station WHERE code = '6ZVHrYAsHdGxShBlNypUstSxs5wRk4' and merchant_id = 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52'),
POrg AS (SELECT * from atlas_app.partner_organization WHERE name = 'GOOGLE_MAPS_METRO')
INSERT INTO atlas_app.partner_org_station (partner_org_id, station_id, name, partner_org_station_id)
SELECT org_id as partner_org_id, id as station_id, 'Edapally' as name, 'ChIJrwwkGa8NCDsRSc6sGbTBQC4' as partner_org_station_id FROM EDS CROSS JOIN POrg;
----------------------------------------------------------------------------** END **--------------------------------------------------------------------------------------------


-- INSERT entries for partner_org_config for each partner_organization for REGISTRATION, RATE_LIMIT, TICKET_SMS, BPP_STATUS_CALL and WALLET_CLASS_NAME config types
WITH RCfg AS (SELECT 'REGISTRATION' as config_type, '{ "fakeOtp": "7891", "sessionConfig": {"attempts": 0, "authExpiry": 0, "tokenExpiry": 1} }'::json as config_json)
INSERT INTO atlas_app.partner_org_config (partner_org_id, config_type, config_json)
SELECT org_id as partner_org_id, RCfg.config_type as config_type, RCfg.config_json as config_json FROM atlas_app.partner_organization CROSS JOIN RCfg;

WITH RLCfg AS (SELECT 'RATE_LIMIT' as config_type, '{ "rateLimitOptions": {"limit": 10000, "limitResetTimeInSec": 10} }'::json as config_json)
INSERT INTO atlas_app.partner_org_config (partner_org_id, config_type, config_json)
SELECT org_id as partner_org_id, RLCfg.config_type as config_type, RLCfg.config_json as config_json FROM atlas_app.partner_organization CROSS JOIN RLCfg;

WITH TSCfg AS (SELECT 'TICKET_SMS' as config_type, '{ "template": "Your metro {#TICKET_PLURAL#} booked! {#URL#} Click the link to view and manage your booking. Thank you for choosing Namma Yatri!", "publicUrl": "<domain>/<path>?bookingId={#FRFS_BOOKING_ID#}" }'::json as config_json)
INSERT INTO atlas_app.partner_org_config (partner_org_id, config_type, config_json)
SELECT org_id as partner_org_id, TSCfg.config_type as config_type, TSCfg.config_json as config_json FROM atlas_app.partner_organization CROSS JOIN TSCfg;

WITH BPPStCallCfg AS (SELECT 'BPP_STATUS_CALL' as config_type, '{ "intervalInSec": 10 }'::json as config_json)
INSERT INTO atlas_app.partner_org_config (partner_org_id, config_type, config_json)
SELECT org_id as partner_org_id, BPPStCallCfg.config_type as config_type, BPPStCallCfg.config_json as config_json FROM atlas_app.partner_organization CROSS JOIN BPPStCallCfg;

-- Don't Run in Master or Prod. Only for Local Testing

WITH WalletClassNameCfg AS (SELECT 'WALLET_CLASS_NAME' as config_type, '{ "className": {"namma-yatri-0-0000-0000-00000000city":"namma_yatri_metro_kmrl"} }'::json as config_json)
INSERT INTO atlas_app.partner_org_config (partner_org_id, config_type, config_json)
SELECT org_id as partner_org_id, WalletClassNameCfg.config_type as config_type, WalletClassNameCfg.config_json as config_json FROM atlas_app.partner_organization CROSS JOIN WalletClassNameCfg;
