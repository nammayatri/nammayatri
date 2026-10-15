-- {"api":"GetLiveMapDemandHotspots","migration":"capability","param":"fleet.live.read","schema":"atlas_dashboard"}
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES ( 'fleet.live.read', 'DASHBOARD', 'PROVIDER_FLEET/LIVE_MAP/GET_LIVE_MAP_DEMAND_HOTSPOTS' ) ON CONFLICT DO NOTHING;
