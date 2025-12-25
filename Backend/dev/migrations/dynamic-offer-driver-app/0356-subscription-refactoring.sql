
 --- alter in driver plan table --

Alter table atlas_driver_offer_bpp.driver_plan add column vehicle_number Text;

--- alter in driver fee ---


----------- NOTE :- don't run this query in prod, just to rectify the column in local------------
Alter table atlas_driver_offer_bpp.driver_fee alter column collected_at SET DATA TYPE  timestamp with time zone;
---------------------------------- -------------------------------    -------------------
-------- alter in invoice ---


----- alter in plan table ----

Alter table atlas_driver_offer_bpp.plan drop CONSTRAINT plan_pkey;
Alter table atlas_driver_offer_bpp.plan add PRIMARY KEY(id, service_name, payment_mode, merchant_op_city_id);

--------- alter in subscription config -----
ALTER TABLE atlas_driver_offer_bpp.subscription_config ALTER COLUMN generic_job_reschedule_time SET DATA TYPE integer USING generic_job_reschedule_time::integer;
ALTER TABLE atlas_driver_offer_bpp.subscription_config ALTER COLUMN payment_link_job_time SET DATA TYPE integer USING payment_link_job_time::integer;

---------- insert in subscription config -----
insert into atlas_driver_offer_bpp.subscription_config (allow_manual_payment_links, payment_link_channel, payment_link_job_time, generic_job_reschedule_time, use_overlay_service, generic_batch_size_for_jobs, deep_link_expiry_time_in_minutes, max_retry_count, service_name,send_in_app_fcm_notifications , allow_due_addition, payment_service_name,send_deep_link, merchant_id, merchant_operating_city_id, allow_driver_fee_calc_schedule, is_triggered_at_end_ride , created_at, updated_at)
select false, 'WHATSAPP', 21600, 60, true, 30, 15, 4, 'YATRI_SUBSCRIPTION', true, true, 'Payment_Juspay' , false, m.merchant_id, m.id, false, true,now(), now()
from atlas_driver_offer_bpp.merchant_operating_city as m;

insert into atlas_driver_offer_bpp.subscription_config (allow_manual_payment_links, payment_link_channel, payment_link_job_time, generic_job_reschedule_time, use_overlay_service, generic_batch_size_for_jobs, deep_link_expiry_time_in_minutes, max_retry_count,service_name,send_in_app_fcm_notifications , allow_due_addition, payment_service_name,send_deep_link, merchant_id, merchant_operating_city_id, is_triggered_at_end_ride , allow_driver_fee_calc_schedule,created_at, updated_at)
select true, 'WHATSAPP', 21600, 60, false, 30, 15, 4, 'YATRI_RENTAL', false, true, 'RentalPayment_Juspay', false,m.merchant_id, m.id,false, true, now(), now()
from atlas_driver_offer_bpp.merchant_operating_city as m;

-------- alter payment order in both rider and driver ---

Alter table atlas_driver_offer_bpp.payment_order add column deep_link Text;

--------- backfill in driver plan table -----

update atlas_driver_offer_bpp.driver_plan as dp
set merchant_id = p.merchant_id,
    merchant_op_city_id = p.merchant_operating_city_id
from (select * from atlas_driver_offer_bpp.person) as p
where p.id = dp.driver_id;

update atlas_driver_offer_bpp.driver_plan as dp
set auto_pay_status = di.auto_pay_status,
    payer_vpa = di.payer_vpa
from (select * from atlas_driver_offer_bpp.driver_information) as di
where di.driver_id = dp.driver_id;

--------- backfill and insertion in plan table -----
update atlas_driver_offer_bpp.plan as p
set merchant_op_city_id = m.mocId
from (select moc.id as mocId , mer.id as merId from atlas_driver_offer_bpp.merchant as mer join atlas_driver_offer_bpp.merchant_operating_city as moc on mer.city = moc.city) as m
where m.merId  = p.merchant_id;


------- !!! IMPORTANT :-  run this post prod release and check for duplicate plans (with this change need to check offers in offers dashboard also fill the plan translation table accordingly) !!! ------
------- !!! IMPORTANT :- don't run this in master !!! ---------------------------------------------------------------------------------------------------
insert into atlas_driver_offer_bpp.plan (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, cgst_percentage , sgst_percentage, max_mandate_amount, merchant_op_city_id, service_name, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount)
SELECT md5(moc.id || moc.merchant_id || plan.id)::uuid , moc.merchant_id, plan.payment_mode, plan.frequency, plan.plan_base_amount, plan.name, plan.description, plan.max_amount, plan.registration_amount, plan.is_offer_applicable, plan.max_credit_limit, plan.free_ride_count, plan.plan_type, plan.cgst_percentage , plan.sgst_percentage, plan.max_mandate_amount, moc.id, plan.service_name, plan.subscribed_flag_toggle_allowed, plan.is_deprecated, plan.eligible_for_coin_discount
FROM atlas_driver_offer_bpp.plan as plan, atlas_driver_offer_bpp.merchant_operating_city as moc
where moc.merchant_short_id = 'NAMMA_YATRI_PARTNER' and not city = 'Bangalore' and not plan.id = '25ade579-fd9c-4288-a015-337af085e66c';
---------- !!!! IMPORTANT :- need to backfill the plan for the cities in driver plan table accordingly after plan creations ------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-------- alter in merchant service config / update with adequete prod/master creds-----
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'RentalPayment_Juspay',
  json_build_object(
      'apiKey','0.1.0|2|aH69syF+qmjP8wpjdwy5KdrHqhsTd1s7lH6TupSUYwMAS5rpi4jGDsA6Nt1uqGPWZxdMshc18cDhmQ=='
    , 'returnUrl','dummyReturnUrl'
    , 'url','dummyUrl'
    , 'merchantId', 'dummyMerchantId'
    , 'username', 'dummyUsername'
    , 'password','0.1.0|0|MbGCmY0OMu39bi7dEokkZ4kvgN17S+whz29QJa+XXUy+mue72jMsAHfVGd4lM9AEWbCqRywCu2RTpA=='
  )
FROM atlas_driver_offer_bpp.merchant_operating_city as m;

update atlas_driver_offer_bpp.merchant_service_config as msc
set merchant_operating_city_id = moc.id
from (select * from atlas_driver_offer_bpp.merchant_operating_city) as moc
where moc.merchant_id = msc.merchant_id;

ALTER TABLE atlas_driver_offer_bpp.merchant_service_config DROP CONSTRAINT merchant_service_config_pkey;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_config ADD PRIMARY KEY(service_name, merchant_operating_city_id);

insert into atlas_driver_offer_bpp.merchant_service_config (merchant_id, service_name, config_json, merchant_operating_city_id)
select msc.merchant_id, msc.service_name, msc.config_json, moc.id
from atlas_driver_offer_bpp.merchant_service_config as msc, atlas_driver_offer_bpp.merchant_operating_city as moc
where msc.merchant_id = moc.merchant_id and not msc.merchant_operating_city_id is null
ON CONFLICT (service_name, merchant_operating_city_id)
DO NOTHING;

---- add coins to cash and total coins left in driver stats table ----

---- backfill coin feilds in driver stats table ----

update atlas_driver_offer_bpp.driver_stats as ds
set coin_coverted_to_cash_left = dp.coin_coverted_to_cash_left,
    total_coins_converted_cash = dp.total_coins_converted_cash
from atlas_driver_offer_bpp.driver_plan as dp
where dp.driver_id = ds.driver_id;

--------- message templates ---------


INSERT INTO atlas_driver_offer_bpp.merchant_message
(merchant_id, message_key, template_id, json_data, message, contains_url_button, merchant_operating_city_id)
SELECT 'c7b14a23-9c0e-44da-81cc-f5500a6f44e6', 'WHATSAPP_VEHICLE_LINKED_MESSAGE', '6921315', null, '*Namma Yatri Mahila Shakti Programme*
*ನಮ್ಮ ಯಾತ್ರಿ ಮಹಿಳಾ ಶಕ್ತಿ ಕಾರ್ಯಕ್ರಮ*

Vehicle has been assigned successfully.

ವಾಹನವನ್ನು ಯಶಸ್ವಿಯಾಗಿ ನಿಯೋಜಿಸಲಾಗಿದೆ.

Vehicle Number / ವಾಹನ ಸಂಖ್ಯೆ: {#var1#}
Vehicle assigned Date / ವಾಹನ ನಿಗದಿಪಡಿಸಲಾದ ದಿನಾಂಕ : {#var2#}', false, m.id
FROM atlas_driver_offer_bpp.merchant_operating_city as m;


INSERT INTO atlas_driver_offer_bpp.merchant_message
(merchant_id, message_key, template_id, json_data, message, contains_url_button, merchant_operating_city_id)
SELECT '1858d3ab-2917-4243-85a4-9d6331338471', 'WHATSAPP_VEHICLE_UNLINKED_MESSAGE', '6921318', null, '*Namma Yatri Mahila Shakti Programme*
*ನಮ್ಮ ಯಾತ್ರಿ ಮಹಿಳಾ ಶಕ್ತಿ ಕಾರ್ಯಕ್ರಮ*

Vehicle has been unassigned successfully.

ವಾಹನವನ್ನು ಯಶಸ್ವಿಯಾಗಿ ನಿಯೋಜಿಸಲಾಗಿಲ್ಲ.

Vehicle Number / ವಾಹನ ಸಂಖ್ಯೆ: {#var1#}
Vehicle unassigned Date / ವಾಹನ ನಿಗದಿಪಡಿಸದ ದಿನಾಂಕ: {#var2#}
', false, m.id
FROM atlas_driver_offer_bpp.merchant_operating_city as m;


INSERT INTO atlas_driver_offer_bpp.merchant_message
(merchant_id, message_key, template_id, json_data, message, contains_url_button, merchant_operating_city_id)
SELECT '4584134c-0652-4858-923c-af36ca4c6185', 'YATRI_RENTAL_PAUSE', '6921321', null, '*Namma Yatri Mahila Shakti Programme*
*ನಮ್ಮ ಯಾತ್ರಿ ಮಹಿಳಾ ಶಕ್ತಿ ಕಾರ್ಯಕ್ರಮ*

Your Rental payment is temporarily paused.

ನಿಮ್ಮ ಬಾಡಿಗೆ ಪಾವತಿಯನ್ನು ತಾತ್ಕಾಲಿಕವಾಗಿ ವಿರಾಮಗೊಳಿಸಲಾಗಿದೆ.

Vehicle Number / ವಾಹನ ಸಂಖ್ಯೆ: {#var1#}
Pause Date / ವಿರಾಮ ದಿನಾಂಕ: {#var2#}
Pause Reason / ವಿರಾಮ ಕಾರಣ: {#var3#}', false, m.id
FROM atlas_driver_offer_bpp.merchant_operating_city as m;


INSERT INTO atlas_driver_offer_bpp.merchant_message
(merchant_id, message_key, template_id, json_data, message, contains_url_button, merchant_operating_city_id)
SELECT '7f12cb91-66de-4705-af24-d871a77e6d43', 'YATRI_RENTAL_RESUME', '6921324', null, '*Namma Yatri Mahila Shakti Programme*
*ನಮ್ಮ ಯಾತ್ರಿ ಮಹಿಳಾ ಶಕ್ತಿ ಕಾರ್ಯಕ್ರಮ*

Your Rental payment has been resumed.

ನಿಮ್ಮ ಬಾಡಿಗೆ ಪಾವತಿಯನ್ನು ಪುನರಾರಂಭಿಸಲಾಗಿದೆ.

Vehicle Number / ವಾಹನ ಸಂಖ್ಯೆ: {#var1#}
Resume Date / ಪುನರಾರಂಭ ದಿನಾಂಕ: {#var2#}', false, m.id
FROM atlas_driver_offer_bpp.merchant_operating_city as m;


INSERT INTO atlas_driver_offer_bpp.merchant_message
(merchant_id, message_key, template_id, json_data, message, contains_url_button, merchant_operating_city_id)
SELECT 'e99936b9-695e-49a7-bed4-20fcfbb8156c', 'WHATSAPP_SETUP_MANDATE_MESSAGE', '6921325', null, '*Namma Yatri Mahila Shakti Programme*
*ನಮ್ಮ ಯಾತ್ರಿ ಮಹಿಳಾ ಶಕ್ತಿ ಕಾರ್ಯಕ್ರಮ*

Please click on the link to set-up autopay for rental payments.

ಬಾಡಿಗೆ ಪಾವತಿಗಳಿಗಾಗಿ ಸ್ವಯಂ ಪಾವತಿಯನ್ನು ಹೊಂದಿಸಲು ದಯವಿಟ್ಟು ಲಿಂಕ್ ಅನ್ನು ಕ್ಲಿಕ್ ಮಾಡಿ.

Setup Autopay - {#var1#}', true, m.id
FROM atlas_driver_offer_bpp.merchant_operating_city as m;


INSERT INTO atlas_driver_offer_bpp.merchant_message
(merchant_id, message_key, template_id, json_data, message, contains_url_button, merchant_operating_city_id)
SELECT '0075b0b5-4e13-4915-990f-c9dd5db806fe', 'WHATSAPP_SEND_MANUAL_PAYMENT_LINK', '6921326', null, '*Namma Yatri Mahila Shakti Programme*
*ನಮ್ಮ ಯಾತ್ರಿ ಮಹಿಳಾ ಶಕ್ತಿ ಕಾರ್ಯಕ್ರಮ*

Please click on the link to clear your pending rental dues.

ನಿಮ್ಮ ಉಳಿದಿರುವ ಬಾಡಿಗೆ ಬಾಕಿಗಳನ್ನು ಕಟ್ಟಲು ದಯವಿಟ್ಟು ಲಿಂಕ್ ಅನ್ನು ಕ್ಲಿಕ್ ಮಾಡಿ.
Pending Due / ಬಾಕಿ ಉಳಿದಿದೆ : Rs. {#var1#}

Pay Now - {#var1#}', true, m.id
FROM atlas_driver_offer_bpp.merchant_operating_city as m;

---- Post all changes in the plan table, need to insert the translations for the plans in the plan_translation table. -----

INSERT INTO atlas_driver_offer_bpp.plan (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount, service_name, merchant_op_city_id, max_mandate_amount , sgst_percentage, cgst_percentage, based_on_entity)
  select  md5('f8c3de3d-1fea-4d7c-a8b0-29f63c4c3454'|| m.id) :: uuid,m.merchant_id, 'MANUAL', 'DAILY', 'DAILY_257.0', 'DAILY UNLIMITED' , 'Enjoy UNLIMITED rides, every day!', 257, 1, false, 1285, 0, 'SUBSCRIPTION',false, false, false, 'YATRI_RENTAL', m.id, 300.0, 0.025, 0.025, 'NONE'
  from atlas_driver_offer_bpp.merchant_operating_city as m;

INSERT INTO atlas_driver_offer_bpp.plan (id, merchant_id, payment_mode, frequency, plan_base_amount, name, description, max_amount, registration_amount, is_offer_applicable, max_credit_limit, free_ride_count, plan_type, subscribed_flag_toggle_allowed, is_deprecated, eligible_for_coin_discount, service_name, merchant_op_city_id, max_mandate_amount , sgst_percentage, cgst_percentage, based_on_entity)
  select md5('f8c3de3d-1fea-4d7c-a8b0-29f63c4c3454'|| m.id) :: uuid, m.merchant_id,  'AUTOPAY', 'DAILY', 'DAILY_257.0', 'DAILY UNLIMITED' , 'Enjoy UNLIMITED rides, every day!', 257, 1, false, 1285, 0, 'SUBSCRIPTION',false, false, false, 'YATRI_RENTAL', m.id, 300.0, 0.025, 0.025, 'NONE'
  from atlas_driver_offer_bpp.merchant_operating_city as m;

 ----- driver fee merchantOpCity backfill ----

update atlas_driver_offer_bpp.driver_fee as df
set merchant_operating_city_id = p.merchant_operating_city_id
from (select * from atlas_driver_offer_bpp.person ) as p
where p.id = df.driver_id;