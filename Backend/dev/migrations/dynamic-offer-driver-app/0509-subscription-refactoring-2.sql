update atlas_driver_offer_bpp.plan
set vehicle_category = 'AUTO_CATEGORY'
where service_name = 'YATRI_SUBSCRIPTION' and vehicle_category is null and merchant_id = 'NAMMYATRI_MERCHANT_ID';

update atlas_driver_offer_bpp.plan
set vehicle_category = 'CAR'
where service_name = 'YATRI_SUBSCRIPTION' and vehicle_category is null and merchant_id = 'JATRI_SAATHI_MERCHANT_ID';

update atlas_driver_offer_bpp.plan
set vehicle_category = 'AUTO_CATEGORY'
where service_name = 'YATRI_RENTAL' and vehicle_category is null;

update atlas_driver_offer_bpp.subscription_config
set default_city_vehicle_category = 'AUTO_CATEGORY',
    number_of_free_trial_rides = 21,
    subscription_enabled_for_vehicle_categories = '{AUTO_CATEGORY}',
    free_trial_rides_applicable = true,
    execution_enabled_for_vehicle_categories = '{AUTO_CATEGORY}'
where merchant_id = 'NAMMYATRI_MERCHANT_ID';

update atlas_driver_offer_bpp.subscription_config
set default_city_vehicle_category = 'CAR',
    number_of_free_trial_rides = 21,
    subscription_enabled_for_vehicle_categories = '{CAR}',
    free_trial_rides_applicable = true,
    execution_enabled_for_vehicle_categories = '{CAR}'
where merchant_id = 'JATRI_SAATHI_MERCHANT_ID';
