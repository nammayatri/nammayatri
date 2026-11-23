-- {"api":"GetShuttleInterestList","migration":"capability","param":"corporate-shuttle.interest.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'corporate-shuttle.interest.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/SHUTTLE_INTEREST/GET_SHUTTLE_INTEREST_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"GetShuttleInterestMapPoints","migration":"capability","param":"corporate-shuttle.interest.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'corporate-shuttle.interest.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/SHUTTLE_INTEREST/GET_SHUTTLE_INTEREST_MAP_POINTS' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"GetShuttleInterestPublishedRoutes","migration":"capability","param":"corporate-shuttle.interest.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'corporate-shuttle.interest.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/SHUTTLE_INTEREST/GET_SHUTTLE_INTEREST_PUBLISHED_ROUTES' ) ON CONFLICT DO NOTHING;
