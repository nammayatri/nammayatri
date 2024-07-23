-- QUERIES FOR LOCAL ONLY
INSERT INTO atlas_app.beckn_config(
	bap_ifsc, buyer_finder_fee, collected_by, confirm_buffer_ttl_sec, confirm_ttl_sec, domain, gateway_url, id, init_ttl_sec, payment_params_json, registry_url, search_ttl_sec, select_ttl_sec, settlement_type, settlement_window, static_terms_url, subscriber_id, subscriber_url, vehicle_category, merchant_id, merchant_operating_city_id, created_at, updated_at, track_ttl_sec, status_ttl_sec, rating_ttl_sec, cancel_ttl_sec, unique_key_id)
	VALUES
	(null, null, 'BPP', 10, 120, 'MOBILITY', 'http://localhost:8015/v1', 'dd22a05d-29a3-42c8-9c8d-2de340f9b619', 120, '{"bankAccNumber": "xyz@upi","bankCode": "xyz"}', 'http://localhost:8020', 120, 120, null, null, null, 'localhost:8013/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'http://localhost:8013/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'AUTO_RICKSHAW', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'namma-yatri-0-0000-0000-00000000city', now(), now(), 30, 30, 120, 30, 'shrey00'),
	(null, null, 'BPP', 10, 120, 'MOBILITY', 'http://localhost:8015/v1', 'dd22a05d-29a3-42c8-9c8d-2de340f9b618', 120, '{"bankAccNumber": "xyz@upi","bankCode": "xyz"}', 'http://localhost:8020', 120, 120, null, null, null, 'localhost:8013/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a51', 'http://localhost:8013/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a51', 'AUTO_RICKSHAW', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51', 'namma-yatri-0-0000-0000-00000000city', now(), now(), 30, 30, 120, 30, 'shrey01');

-- QUERIES FOR PROD/MASTER
