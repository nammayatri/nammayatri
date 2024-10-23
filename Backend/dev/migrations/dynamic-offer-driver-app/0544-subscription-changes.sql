-- Kolkata ('7a0a3891-5945-4f86-a2f2-90c89c197c56') --

UPDATE atlas_driver_offer_bpp.transporter_config set free_trial_days = 7 where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata');
UPDATE atlas_driver_offer_bpp.plan set vehicle_category = 'MOTORCYCLE' where vehicle_variant = 'BIKE';
UPDATE atlas_driver_offer_bpp.plan SET max_credit_limit = 450 where merchant_op_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata') and vehicle_category = 'CAR';

-- Other cities --
-- Cab Manual Plans --

INSERT INTO atlas_driver_offer_bpp.plan
    (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount, service_name, based_on_entity, vehicle_variant, vehicle_category)
SELECT
    atlas_driver_offer_bpp.uuid_generate_v4(),
    m.merchant_id,
    'MANUAL',
    'DAILY',
    'PERRIDE_15.0',
    'PER RIDE',
    'Unlimited number of rides per day',
    5000,
    1,
    true,
    675,
    0,
    'SUBSCRIPTION',
    0.09,
    225.0,
    m.id,
    0.09,
    true,
    false,
    false,
    'YATRI_SUBSCRIPTION',
    'RIDE',
    'HATCHBACK',
    'CAR'
FROM
    atlas_driver_offer_bpp.merchant_operating_city as m
WHERE
    city IN ('Asansol', 'Siliguri', 'Durgapur', 'Petrapole');

INSERT INTO atlas_driver_offer_bpp.plan
    (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount, service_name, based_on_entity, vehicle_variant, vehicle_category)
SELECT
    atlas_driver_offer_bpp.uuid_generate_v4(),
    m.merchant_id,
    'MANUAL',
    'DAILY',
    'PERRIDE_15.0',
    'PER RIDE',
    'Unlimited number of rides per day',
    5000,
    1,
    true,
    675,
    0,
    'SUBSCRIPTION',
    0.09,
    225.0,
    m.id,
    0.09,
    true,
    false,
    false,
    'YATRI_SUBSCRIPTION',
    'RIDE',
    'TAXI',
    'CAR'
FROM
    atlas_driver_offer_bpp.merchant_operating_city as m
WHERE
    city IN ('Asansol', 'Siliguri', 'Durgapur', 'Petrapole');

INSERT INTO atlas_driver_offer_bpp.plan
    (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount, service_name, based_on_entity, vehicle_variant, vehicle_category)
SELECT
    atlas_driver_offer_bpp.uuid_generate_v4(),
    m.merchant_id,
    'MANUAL',
    'DAILY',
    'PERRIDE_15.0',
    'PER RIDE',
    'Unlimited number of rides per day',
    5000,
    1,
    true,
    675,
    0,
    'SUBSCRIPTION',
    0.09,
    225.0,
    m.id,
    0.09,
    true,
    false,
    false,
    'YATRI_SUBSCRIPTION',
    'RIDE',
    'SEDAN',
    'CAR'
FROM
    atlas_driver_offer_bpp.merchant_operating_city as m
WHERE
    city IN ('Asansol', 'Siliguri', 'Durgapur', 'Petrapole');

INSERT INTO atlas_driver_offer_bpp.plan
    (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount, service_name, based_on_entity, vehicle_variant, vehicle_category)
SELECT
    atlas_driver_offer_bpp.uuid_generate_v4(),
    m.merchant_id,
    'MANUAL',
    'DAILY',
    'PERRIDE_15.0',
    'PER RIDE',
    'Unlimited number of rides per day',
    5000,
    1,
    true,
    675,
    0,
    'SUBSCRIPTION',
    0.09,
    225.0,
    m.id,
    0.09,
    true,
    false,
    false,
    'YATRI_SUBSCRIPTION',
    'RIDE',
    'SUV',
    'CAR'
FROM
    atlas_driver_offer_bpp.merchant_operating_city as m
WHERE
    city IN ('Asansol', 'Siliguri', 'Durgapur', 'Petrapole');

INSERT INTO atlas_driver_offer_bpp.plan
    (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount, service_name, based_on_entity, vehicle_variant, vehicle_category)
SELECT
    atlas_driver_offer_bpp.uuid_generate_v4(),
    m.merchant_id,
    'MANUAL',
    'DAILY',
    'PERRIDE_15.0',
    'PER RIDE',
    'Unlimited number of rides per day',
    5000,
    1,
    true,
    675,
    0,
    'SUBSCRIPTION',
    0.09,
    225.0,
    m.id,
    0.09,
    true,
    false,
    false,
    'YATRI_SUBSCRIPTION',
    'RIDE',
    'SUV_PLUS',
    'CAR'
FROM
    atlas_driver_offer_bpp.merchant_operating_city as m
WHERE
    city IN ('Siliguri', 'Petrapole');

-- Cab Autopay Plans --

INSERT INTO atlas_driver_offer_bpp.plan
    (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount, service_name, based_on_entity, vehicle_variant, vehicle_category)
SELECT
    p.id,
    m.merchant_id,
    'AUTOPAY',
    'DAILY',
    'PERRIDE_15.0',
    'PER RIDE',
    'Unlimited number of rides per day',
    5000,
    1,
    true,
    675,
    0,
    'SUBSCRIPTION',
    0.09,
    225.0,
    m.id,
    0.09,
    true,
    false,
    false,
    'YATRI_SUBSCRIPTION',
    'RIDE',
    'HATCHBACK',
    'CAR'
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS m
JOIN
    atlas_driver_offer_bpp.plan AS p ON m.id = p.merchant_op_city_id AND p.payment_mode = 'MANUAL' AND p.service_name = 'YATRI_SUBSCRIPTION' and p.vehicle_variant = 'HATCHBACK'
    AND m.city IN ('Asansol','Siliguri', 'Durgapur','Petrapole')


INSERT INTO atlas_driver_offer_bpp.plan
    (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount, service_name, based_on_entity, vehicle_variant, vehicle_category)
SELECT
    p.id,
    m.merchant_id,
    'AUTOPAY',
    'DAILY',
    'PERRIDE_15.0',
    'PER RIDE',
    'Unlimited number of rides per day',
    5000,
    1,
    true,
    675,
    0,
    'SUBSCRIPTION',
    0.09,
    225.0,
    m.id,
    0.09,
    true,
    false,
    false,
    'YATRI_SUBSCRIPTION',
    'RIDE',
    'TAXI',
    'CAR'
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS m
JOIN
    atlas_driver_offer_bpp.plan AS p ON m.id = p.merchant_op_city_id AND p.payment_mode = 'MANUAL' AND p.service_name = 'YATRI_SUBSCRIPTION' and p.vehicle_variant = 'TAXI'
    AND m.city IN ('Asansol','Siliguri', 'Durgapur','Petrapole')

INSERT INTO atlas_driver_offer_bpp.plan
    (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount, service_name, based_on_entity, vehicle_variant, vehicle_category)
SELECT
    p.id,
    m.merchant_id,
    'AUTOPAY',
    'DAILY',
    'PERRIDE_15.0',
    'PER RIDE',
    'Unlimited number of rides per day',
    5000,
    1,
    true,
    675,
    0,
    'SUBSCRIPTION',
    0.09,
    225.0,
    m.id,
    0.09,
    true,
    false,
    false,
    'YATRI_SUBSCRIPTION',
    'RIDE',
    'SEDAN',
    'CAR'
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS m
JOIN
    atlas_driver_offer_bpp.plan AS p ON m.id = p.merchant_op_city_id AND p.payment_mode = 'MANUAL' AND p.service_name = 'YATRI_SUBSCRIPTION' and p.vehicle_variant = 'SEDAN'
    AND m.city IN ('Asansol','Siliguri', 'Durgapur','Petrapole')

INSERT INTO atlas_driver_offer_bpp.plan
    (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount, service_name, based_on_entity, vehicle_variant, vehicle_category)
SELECT
    p.id,
    m.merchant_id,
    'AUTOPAY',
    'DAILY',
    'PERRIDE_15.0',
    'PER RIDE',
    'Unlimited number of rides per day',
    5000,
    1,
    true,
    675,
    0,
    'SUBSCRIPTION',
    0.09,
    225.0,
    m.id,
    0.09,
    true,
    false,
    false,
    'YATRI_SUBSCRIPTION',
    'RIDE',
    'SUV',
    'CAR'
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS m
JOIN
    atlas_driver_offer_bpp.plan AS p ON m.id = p.merchant_op_city_id AND p.payment_mode = 'MANUAL' AND p.service_name = 'YATRI_SUBSCRIPTION' and p.vehicle_variant = 'SUV'
    AND m.city IN ('Asansol','Siliguri', 'Durgapur','Petrapole');

INSERT INTO atlas_driver_offer_bpp.plan
    (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount, service_name, based_on_entity, vehicle_variant, vehicle_category)
SELECT
    p.id,
    m.merchant_id,
    'AUTOPAY',
    'DAILY',
    'PERRIDE_15.0',
    'PER RIDE',
    'Unlimited number of rides per day',
    5000,
    1,
    true,
    675,
    0,
    'SUBSCRIPTION',
    0.09,
    225.0,
    m.id,
    0.09,
    true,
    false,
    false,
    'YATRI_SUBSCRIPTION',
    'RIDE',
    'SUV_PLUS',
    'CAR'
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS m
JOIN
    atlas_driver_offer_bpp.plan AS p ON m.id = p.merchant_op_city_id AND p.payment_mode = 'MANUAL' AND p.service_name = 'YATRI_SUBSCRIPTION' and p.vehicle_variant = 'SUV_PLUS'
    AND m.city IN ('Siliguri', 'Petrapole');

-- Bike Manual plan --

INSERT INTO atlas_driver_offer_bpp.plan
    (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount, service_name, based_on_entity, vehicle_variant, vehicle_category)
SELECT
    atlas_driver_offer_bpp.uuid_generate_v4(),
    m.merchant_id,
    'MANUAL',
    'DAILY',
    'PERRIDE_3.0',
    'PER RIDE',
    'Unlimited number of rides per day',
    5000,
    1,
    true,
    250,
    0,
    'SUBSCRIPTION',
    0.09,
    80.0,
    m.id,
    0.09,
    true,
    false,
    false,
    'YATRI_SUBSCRIPTION',
    'RIDE',
    'BIKE',
    'MOTORCYCLE'
FROM
    atlas_driver_offer_bpp.merchant_operating_city as m
WHERE
    city IN ('Asansol', 'Siliguri', 'Durgapur', 'Petrapole');

-- Bike Autopay plan --

INSERT INTO atlas_driver_offer_bpp.plan
    (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount, service_name, based_on_entity, vehicle_variant, vehicle_category)
SELECT
    p.id,
    m.merchant_id,
    'AUTOPAY',
    'DAILY',
    'PERRIDE_3.0',
    'PER RIDE',
    'Unlimited number of rides per day',
    5000,
    1,
    true,
    250,
    0,
    'SUBSCRIPTION',
    0.09,
    80.0,
    m.id,
    0.09,
    true,
    false,
    false,
    'YATRI_SUBSCRIPTION',
    'RIDE',
    'BIKE',
    'MOTORCYCLE'
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS m
JOIN
    atlas_driver_offer_bpp.plan AS p ON m.id = p.merchant_op_city_id AND p.payment_mode = 'MANUAL' AND p.service_name = 'YATRI_SUBSCRIPTION' and p.vehicle_variant = 'BIKE'
    AND m.city IN ('Asansol', 'Siliguri', 'Durgapur', 'Petrapole');

-- Auto Manual plan --

INSERT INTO atlas_driver_offer_bpp.plan
    (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount, service_name, based_on_entity, vehicle_variant, vehicle_category)
SELECT
    atlas_driver_offer_bpp.uuid_generate_v4(),
    m.merchant_id,
    'MANUAL',
    'DAILY',
    'PERRIDE_4.0',
    'PER RIDE',
    'Unlimited number of rides per day',
    5000,
    1,
    true,
    250,
    0,
    'SUBSCRIPTION',
    0.09,
    80.0,
    m.id,
    0.09,
    true,
    false,
    false,
    'YATRI_SUBSCRIPTION',
    'RIDE',
    'AUTO_RICKSHAW',
    'AUTO_CATEGORY'
FROM
    atlas_driver_offer_bpp.merchant_operating_city as m
WHERE
    city IN ('Asansol', 'Durgapur');

-- Auto Autopay plan --

INSERT INTO atlas_driver_offer_bpp.plan
    (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage, max_mandate_amount, merchant_op_city_id, sgst_percentage, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount, service_name, based_on_entity, vehicle_variant, vehicle_category)
SELECT
    p.id,
    m.merchant_id,
    'AUTOPAY',
    'DAILY',
    'PERRIDE_4.0',
    'PER RIDE',
    'Unlimited number of rides per day',
    5000,
    1,
    true,
    250,
    0,
    'SUBSCRIPTION',
    0.09,
    80.0,
    m.id,
    0.09,
    true,
    false,
    false,
    'YATRI_SUBSCRIPTION',
    'RIDE',
    'AUTO_RICKSHAW',
    'AUTO_CATEGORY'
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS m
JOIN
    atlas_driver_offer_bpp.plan AS p ON m.id = p.merchant_op_city_id AND p.payment_mode = 'MANUAL' AND p.service_name = 'YATRI_SUBSCRIPTION' and p.vehicle_variant = 'AUTO_RICKSHAW'
    AND m.city IN ('Asansol', 'Durgapur');

-- Cabs SZ Fare Policy --

UPDATE atlas_driver_offer_bpp.fare_policy set platform_fee_charges_by = 'SlabBased' where id in (
select fare_policy_id from atlas_driver_offer_bpp.fare_product where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city in ('Asansol', 'Siliguri', 'Durgapur', 'Petrapole'))
and area != 'Default' and vehicle_variant in ('SUV_PLUS', 'SUV', 'HATCHBACK', 'SEDAN', 'TAXI') and fare_policy_type = 'Slabs');

-- Line 483 is only for master and prod
-- UPDATE atlas_driver_offer_bpp.fare_policy_slabs_details_slab set platform_fee_charge = '{"contents":12.7,"tag":"ConstantPlatformFee"}' where fare_policy_id in (select fare_policy_id from atlas_driver_offer_bpp.fare_product where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city in ('Asansol', 'Siliguri', 'Durgapur', 'Petrapole')) and area != 'Default' and vehicle_variant in ('SUV_PLUS', 'SUV', 'HATCHBACK', 'SEDAN', 'TAXI'));

insert into atlas_driver_offer_bpp.subscription_config (allow_manual_payment_links, payment_link_channel, payment_link_job_time, generic_job_reschedule_time, use_overlay_service, generic_batch_size_for_jobs, deep_link_expiry_time_in_minutes, max_retry_count, service_name,send_in_app_fcm_notifications , allow_due_addition, payment_service_name,send_deep_link, merchant_id, merchant_operating_city_id, allow_driver_fee_calc_schedule, is_triggered_at_end_ride , created_at, updated_at)
select false, 'WHATSAPP', 21600, 60, true, 30, 15, 4, 'YATRI_SUBSCRIPTION', true, true, 'Payment_Juspay' , false, m.merchant_id, m.id, false, true,now(), now() from atlas_driver_offer_bpp.merchant_operating_city as m where m.city in ('Asansol', 'Siliguri', 'Durgapur');

UPDATE atlas_driver_offer_bpp.subscription_config set execution_enabled_for_vehicle_categories = '{CAR, MOTORCYCLE, AUTO_CATEGORY}', subscription_enabled_for_vehicle_categories = '{CAR, MOTORCYCLE, AUTO_CATEGORY}', is_subscription_enabled_at_category_level = true where merchant_id in (select id from atlas_driver_offer_bpp.merchant where short_id = 'JATRI_SAATHI_PARTNER');

update atlas_driver_offer_bpp.transporter_config set free_trial_days = 0 where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city in ('Asansol','Durgapur','Siliguri'));

UPDATE atlas_driver_offer_bpp.subscription_config set enable_city_based_fee_switch = true where merchant_id in (select id from atlas_driver_offer_bpp.merchant where short_id = 'JATRI_SAATHI_PARTNER');