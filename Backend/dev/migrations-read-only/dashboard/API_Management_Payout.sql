-- {"api":"PostPayoutPayoutRetrigger","migration":"capability","param":"finance.payout.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'finance.payout.write', 'DASHBOARD', 'RIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_RETRIGGER' ) ON CONFLICT DO NOTHING;
