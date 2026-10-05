-- {"api":"PostSearchTryRecentSearchTries","migration":"capability","param":"city-operations.scheduled-bookings.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.scheduled-bookings.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/SEARCH_TRY/POST_SEARCH_TRY_RECENT_SEARCH_TRIES' ) ON CONFLICT DO NOTHING;
