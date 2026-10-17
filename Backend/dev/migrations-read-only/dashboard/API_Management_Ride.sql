-- {"api":"PostRideEndMultiple","migration":"endpoint","param":"RideAPI MultipleRideEndEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_END_MULTIPLE'
  WHERE endpoint = 'RideAPI MultipleRideEndEndpoint';

-- {"api":"PostRideEndMultiple","migration":"endpointV2","param":null,"schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_END_MULTIPLE'
  WHERE endpoint = 'RideAPI PostRideEndMultipleEndpoint';

-- {"api":"PostRideCancelMultiple","migration":"endpoint","param":"RideAPI MultipleRideCancelEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_CANCEL_MULTIPLE'
  WHERE endpoint = 'RideAPI MultipleRideCancelEndpoint';

-- {"api":"PostRideCancelMultiple","migration":"endpointV2","param":null,"schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_CANCEL_MULTIPLE'
  WHERE endpoint = 'RideAPI PostRideCancelMultipleEndpoint';

-- {"api":"PostRideSync","migration":"endpoint","param":"RideAPI RideSyncEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC'
  WHERE endpoint = 'RideAPI RideSyncEndpoint';

-- {"api":"PostRideSync","migration":"endpointV2","param":null,"schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC'
  WHERE endpoint = 'RideAPI PostRideSyncEndpoint';

-- {"api":"PostRideSyncMultiple","migration":"endpoint","param":"RideAPI MultipleRideSyncEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC_MULTIPLE'
  WHERE endpoint = 'RideAPI MultipleRideSyncEndpoint';

-- {"api":"PostRideSyncMultiple","migration":"endpointV2","param":null,"schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC_MULTIPLE'
  WHERE endpoint = 'RideAPI PostRideSyncMultipleEndpoint';

-- {"api":"GetRideKaptureList","migration":"endpoint","param":"RideAPI TicketRideListEndpoint","schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_KAPTURE_LIST'
  WHERE endpoint = 'RideAPI TicketRideListEndpoint';

-- {"api":"GetRideKaptureList","migration":"endpointV2","param":null,"schema":"atlas_dashboard"}
UPDATE atlas_dashboard.transaction
  SET endpoint = 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_KAPTURE_LIST'
  WHERE endpoint = 'RideAPI GetRideKaptureListEndpoint';

------- SQL updates -------

------- SQL updates -------

------- SQL updates -------


------- SQL updates -------

-- {"api":"GetRideEvidence","migration":"capability","param":"city-operations.ride.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.ride.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_EVIDENCE' ) ON CONFLICT DO NOTHING;

-- {"api":"GetRideEvidenceFile","migration":"capability","param":"city-operations.ride.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.ride.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_EVIDENCE_FILE' ) ON CONFLICT DO NOTHING;


------- SQL updates -------

-- {"api":"PostRideEvidenceVisibility","migration":"capability","param":"city-operations.ride.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-operations.ride.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_EVIDENCE_VISIBILITY' ) ON CONFLICT DO NOTHING;
