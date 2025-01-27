-- {"api":"PostRideStart","migration":"endpoint","param":"RideAPI RideStartEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_START'
  WHERE endpoint = 'RideAPI RideStartEndpoint';

-- {"api":"PostRideStart","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP RIDES RIDE_START","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_START' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'RIDE_START' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRideEnd","migration":"endpoint","param":"RideAPI RideEndEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_END'
  WHERE endpoint = 'RideAPI RideEndEndpoint';

-- {"api":"PostRideEnd","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP RIDES RIDE_END","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_END' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'RIDE_END' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRideCurrentActiveRide","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP RIDES CURRENT_ACTIVE_RIDE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/RIDE/GET_RIDE_CURRENT_ACTIVE_RIDE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'CURRENT_ACTIVE_RIDE' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRideCancel","migration":"endpoint","param":"RideAPI RideCancelEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_CANCEL'
  WHERE endpoint = 'RideAPI RideCancelEndpoint';

-- {"api":"PostRideCancel","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP RIDES RIDE_CANCEL","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_CANCEL' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'RIDE_CANCEL' ) ON CONFLICT DO NOTHING;

-- {"api":"PostRideBookingWithVehicleNumberAndPhone","migration":"endpoint","param":"RideAPI BookingWithVehicleNumberAndPhoneEndpoint","schema":"atlas_bpp_dashboard"}
UPDATE atlas_bpp_dashboard.transaction
  SET endpoint = 'PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_BOOKING_WITH_VEHICLE_NUMBER_AND_PHONE'
  WHERE endpoint = 'RideAPI BookingWithVehicleNumberAndPhoneEndpoint';

-- {"api":"PostRideBookingWithVehicleNumberAndPhone","migration":"userActionType","param":"ApiAuth DRIVER_OFFER_BPP RIDES BOOKING_WITH_VEHICLE_NUMBER_AND_PHONE","schema":"atlas_bpp_dashboard"}
INSERT INTO atlas_bpp_dashboard.access_matrix (id, role_id, api_entity, user_access_type, user_action_type) ( SELECT atlas_bpp_dashboard.uuid_generate_v4(), T1.role_id, 'DSL', 'USER_FULL_ACCESS', 'PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_BOOKING_WITH_VEHICLE_NUMBER_AND_PHONE' FROM atlas_bpp_dashboard.access_matrix AS T1 WHERE T1.user_access_type = 'USER_FULL_ACCESS' AND T1.api_entity = 'RIDES' AND T1.user_action_type = 'BOOKING_WITH_VEHICLE_NUMBER_AND_PHONE' ) ON CONFLICT DO NOTHING;
