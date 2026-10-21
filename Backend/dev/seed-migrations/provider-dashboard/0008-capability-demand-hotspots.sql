-- Fleet live map demand hotspots
-- (PROVIDER_FLEET/LIVE_MAP/GET_DEMAND_HOTSPOTS).
-- Same capability as the existing live map drivers endpoint.

INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES
    ('fleet.live.read', 'DASHBOARD', 'PROVIDER_FLEET/LIVE_MAP/GET_DEMAND_HOTSPOTS')
ON CONFLICT DO NOTHING;
