-- Already ran in master --
UPDATE atlas_driver_offer_bpp.document_verification_config
SET supported_vehicle_classes_json = (
    supported_vehicle_classes_json::jsonb || '{"manufacturer":"crysta","vehicleClass":"lmv","vehicleVariant":"SUV_PLUS","priority":6}'::jsonb
) where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri') and document_type = 'VehicleRegistrationCertificate' and vehicle_category = 'CAR';

UPDATE atlas_driver_offer_bpp.document_verification_config
SET supported_vehicle_classes_json = (
    supported_vehicle_classes_json::jsonb || '{"manufacturerModel":"crysta","vehicleClass":"lmv","vehicleVariant":"SUV_PLUS","priority":6}'::jsonb
) where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri')  and document_type = 'VehicleRegistrationCertificate' and vehicle_category = 'CAR';

UPDATE atlas_driver_offer_bpp.document_verification_config
SET supported_vehicle_classes_json = (
    supported_vehicle_classes_json::jsonb || '{"manufacturerModel":"crysta","vehicleClass":"lpv","vehicleVariant":"SUV_PLUS","priority":6}'::jsonb
) where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri')  and document_type = 'VehicleRegistrationCertificate' and vehicle_category = 'CAR';

UPDATE atlas_driver_offer_bpp.document_verification_config
SET supported_vehicle_classes_json = (
    supported_vehicle_classes_json::jsonb || '{"manufacturer":"crysta","vehicleClass":"lpv","vehicleVariant":"SUV_PLUS","priority":6}'::jsonb
) where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri') and document_type = 'VehicleRegistrationCertificate' and vehicle_category = 'CAR';


insert into atlas_driver_offer_bpp.vehicle_service_tier (id, name, merchant_id, merchant_operating_city_id, seating_capacity, air_conditioned, driver_rating, vehicle_rating, luggage_capacity, short_description, long_description, allowed_vehicle_variant, default_for_vehicle_variant, service_tier_type, created_at, updated_at, auto_selected_vehicle_variant, oxygen, ventilator, is_air_conditioned, air_conditioned_threshold)
(select
    atlas_driver_offer_bpp.uuid_generate_v4(),
    'SUV Plus',
    m.merchant_id,
    m.id,
    null,
    2,
    null,
    null,
    null,
    'AC, Extra Spacious rides',
    null,
    '{SUV_PLUS}',
    '{SUV_PLUS}',
    'SUV_PLUS',
    now(),
    now(),
    '{SUV_PLUS}',
    null,
    null,
    true,
    2
    from atlas_driver_offer_bpp.merchant_operating_city as m where city = 'Siliguri');

UPDATE atlas_driver_offer_bpp.vehicle_service_tier SET allowed_vehicle_variant = '{SUV,SEDAN,SUV_PLUS}' where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri') and service_tier_type = 'SEDAN';

UPDATE atlas_driver_offer_bpp.vehicle_service_tier SET allowed_vehicle_variant = '{SUV,HATCHBACK,SEDAN,SUV_PLUS}' where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri') and service_tier_type = 'HATCHBACK';

UPDATE atlas_driver_offer_bpp.vehicle_service_tier SET allowed_vehicle_variant = '{SUV,HATCHBACK,SEDAN,TAXI,SUV_PLUS}' where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri') and service_tier_type = 'TAXI';

UPDATE atlas_driver_offer_bpp.vehicle_service_tier SET allowed_vehicle_variant = '{SUV,SUV_PLUS}' where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri') and service_tier_type = 'SUV';