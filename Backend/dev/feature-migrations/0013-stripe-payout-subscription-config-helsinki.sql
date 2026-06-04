-- Enable PREPAID_SUBSCRIPTION config for Stripe payout (Helsinki / BRIDGE_FINLAND_PARTNER)
-- BPP merchant_id:               a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e  (BRIDGE_FINLAND_PARTNER)
-- BPP merchant_operating_city_id: beabba6a-c817-43d2-93b2-a916f5cf2ceb  (Helsinki)

INSERT INTO atlas_driver_offer_bpp.subscription_config (
  service_name, merchant_id, merchant_operating_city_id,
  payment_service_name, payout_service_name,
  allow_driver_fee_calc_schedule, allow_due_addition, allow_manual_payment_links,
  deep_link_expiry_time_in_minutes, generic_batch_size_for_jobs, generic_job_reschedule_time,
  is_triggered_at_end_ride, max_retry_count, payment_link_channel, payment_link_job_time,
  send_deep_link, send_in_app_fcm_notifications, use_overlay_service,
  default_city_vehicle_category, subscription_enabled_for_vehicle_categories,
  number_of_free_trial_rides, free_trial_rides_applicable,
  execution_enabled_for_vehicle_categories, is_subscription_enabled_at_category_level,
  enable_city_based_fee_switch, is_vendor_split_enabled, subscription_down,
  is_ui_enabled, is_free_trial_days_applicable, enable_service_usage_charge_default,
  autopay_enabled, show_manual_plans_in_ui,
  created_at, updated_at
) VALUES (
  'PREPAID_SUBSCRIPTION',
  'a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e',
  'beabba6a-c817-43d2-93b2-a916f5cf2ceb',
  'Payment_Stripe',
  'RidePayout_Stripe',
  false, false, false, 15, 30, 60, false, 4, 'WHATSAPP', 21600,
  false, false, false, 'AUTO_CATEGORY', '{}', 20, false, '{}', false, false, false,
  false, false, false, false, false, false,
  now(), now()
) ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.merchant_service_config
  (merchant_id, merchant_operating_city_id, service_name, config_json, created_at, updated_at)
VALUES (
  'a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e',
  'beabba6a-c817-43d2-93b2-a916f5cf2ceb',
  'RidePayout_Stripe',
  '{"url": "http://localhost:8080/stripe/", "apiKey": "0.1.0|2|5ZL4MNhQ2ynjgp/RjacXY6Sd8Bl2SV0k3TFvu/VHPGvjK/lX16LumiEn4z/iR/k9/jsvCvtjD2ifKI2mrz0=", "serviceMode": "Live"}',
  now(), now()
) ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.merchant_service_config
  (merchant_id, merchant_operating_city_id, service_name, config_json, created_at, updated_at)
VALUES (
  'a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e',
  'beabba6a-c817-43d2-93b2-a916f5cf2ceb',
  'Payout_StripeTest',
  '{"apiKey": "0.1.0|0|hMiwf44YibY/N5esKqfgy97pr+nB6zbGnr/HY4+iIAhihc+2d21CA4mrcCHnVvBPe5zlt5VLerKCiKhyl0k=", "returnUrl": "https://www.nammayatri.in/return", "refreshUrl": "https://www.nammayatri.in/refresh", "url": "http://localhost:8080/stripe/", "chargeDestination": "Platform", "serviceMode": "Test"}',
  now(), now()
) ON CONFLICT DO NOTHING;
