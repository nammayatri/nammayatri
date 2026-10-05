-- {"api":"GetGeohashAreaList","migration":"capability","param":"city-config.geo.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.geo.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/GEOHASH_AREA/GET_GEOHASH_AREA_LIST' ) ON CONFLICT DO NOTHING;

-- {"api":"PostGeohashAreaUpsert","migration":"capability","param":"city-config.geo.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.geo.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/GEOHASH_AREA/POST_GEOHASH_AREA_UPSERT' ) ON CONFLICT DO NOTHING;

-- {"api":"PostGeohashAreaUpsertCsv","migration":"capability","param":"city-config.geo.write","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'city-config.geo.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/GEOHASH_AREA/POST_GEOHASH_AREA_UPSERT_CSV' ) ON CONFLICT DO NOTHING;
