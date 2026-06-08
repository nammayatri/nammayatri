-- MOC 391b4b7a exists in merchant_operating_city (from prod_international config-sync)
-- but has no matching merchant_service_usage_config, causing MERCHANT_SERVICE_USAGE_CONFIG_NOT_FOUND
-- on rider OTP verification. Copy the config from f9903ef6 (prod Helsinki MOC).

INSERT INTO atlas_app.merchant_service_usage_config (
  aadhaar_verification_service, auto_complete, created_at, enable_dashboard_sms,
  get_distances, get_distances_for_cancel_ride, get_exophone, get_pickup_routes,
  get_place_details, get_place_name, get_routes, get_trip_routes, initiate_call,
  issue_ticket_service, merchant_id, merchant_operating_city_id, notify_person,
  sms_providers_priority_list, snap_to_road, updated_at, use_fraud_detection,
  whatsapp_providers_priority_list, update_payment_method_in_intent,
  update_amount_in_payment_intent, get_card_list, create_setup_intent,
  create_payment_intent, create_payment_customer, create_ephemeral_keys,
  capture_payment_intent, delete_card, get_distances_for_scheduled_rides,
  cancel_payment_intent, get_frfs_autocomplete_distances, get_multi_modal_service,
  get_first_pickup_route, get_multimodal_walk_distance, insurance_service,
  create_refunds, get_refunds, get_instruction_route, payout_order_status,
  create_payout_order, event_tracking_providers
)
SELECT
  aadhaar_verification_service, auto_complete, NOW(), enable_dashboard_sms,
  get_distances, get_distances_for_cancel_ride, get_exophone, get_pickup_routes,
  get_place_details, get_place_name, get_routes, get_trip_routes, initiate_call,
  issue_ticket_service, merchant_id, '391b4b7a-3cc2-429d-b18f-034dbab6e90d',
  notify_person, sms_providers_priority_list, snap_to_road, NOW(), use_fraud_detection,
  whatsapp_providers_priority_list, update_payment_method_in_intent,
  update_amount_in_payment_intent, get_card_list, create_setup_intent,
  create_payment_intent, create_payment_customer, create_ephemeral_keys,
  capture_payment_intent, delete_card, get_distances_for_scheduled_rides,
  cancel_payment_intent, get_frfs_autocomplete_distances, get_multi_modal_service,
  get_first_pickup_route, get_multimodal_walk_distance, insurance_service,
  create_refunds, get_refunds, get_instruction_route, payout_order_status,
  create_payout_order, event_tracking_providers
FROM atlas_app.merchant_service_usage_config
WHERE merchant_operating_city_id = 'f9903ef6-f595-428e-b5ac-e8816cbdf979'
ON CONFLICT (merchant_operating_city_id) DO NOTHING;
