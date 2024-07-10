-- DISABLING SUBSCRIPTION IN SILIGURI
-- MASTER + PROD
update atlas_driver_offer_bpp.transporter_config
  set is_plan_mandatory = false
  where merchant_operating_city_id = (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri');

-- DISABLING SUBSCRIPTION IN SILIGURI
-- MASTER + PROD
update atlas_driver_offer_bpp.transporter_config
  set variants_to_enable_for_subscription = '{}'
  where merchant_operating_city_id = (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri');

-- ADDING BPP EXOPHONE FOR SILIGURI
-- MASTER + PROD
update atlas_driver_offer_bpp.exophone
  set primary_phone = '03340585247'
  where merchant_operating_city_id = (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri');

-- DELETING SUBSCRIPTION PLAN DOCUMENT VERIFICATION CONFIG FOR SILIGURI
-- MASTER + PROD
delete from atlas_driver_offer_bpp.document_verification_config
  where merchant_operating_city_id = (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri') and document_type = 'SubscriptionPlan';

-- REMOVING BIKE SUBSCRIPTION PLAN IN SILIGURI
-- MASTER + PROD
delete from atlas_driver_offer_bpp.plan
  where merchant_op_city_id = (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri') and vehicle_variant = 'BIKE';

-- REMOVING BIKE SUBSCRIPTION PLAN IN KOLKATA
-- MASTER ONLY
delete from atlas_driver_offer_bpp.plan
  where merchant_op_city_id = (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata') and vehicle_variant = 'BIKE';