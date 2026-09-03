-- {"api":"PostPaymentRefundRequestBookingInitiate","migration":"capability","param":"finance.adjustment.write","schema":"atlas_bap_dashboard"}
INSERT INTO atlas_bap_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.adjustment.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PAYMENT/POST_PAYMENT_REFUND_REQUEST_BOOKING_INITIATE' ) ON CONFLICT DO NOTHING;
