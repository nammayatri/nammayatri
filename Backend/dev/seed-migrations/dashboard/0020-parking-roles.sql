-- Parking operators: capabilities, cluster (TICKET_PLACE) scoping and three roles.
--
-- City level (no resource_type): every cluster, city-wide revenue. Cluster staff never get these.
-- Cluster level (resource_type TICKET_PLACE): every endpoint under /parking/clusters/{clusterId}/. The dashboard
-- gate reads the id after the "clusters" path segment and checks it against person_resource_access
-- (no rows for the person = whole city; assign clusters with /person/{personId}/assignResourceAccess).
--
-- The capability_endpoint rows are also emitted by the generator (migrations-read-only/dashboard/API_AppManagement_Parking.sql,
-- ON CONFLICT DO NOTHING); they are inserted here first so they carry the binding.

INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('city-operations.parking.read', 'city-operations', 'See every parking cluster and the city''s parking revenue', false),
    ('city-operations.parking_cluster.read', 'city-operations', 'See a parking cluster and its zones'' occupancy', false),
    ('city-operations.parking_cluster.write', 'city-operations', 'Take a parking cluster live or back to draft', false),
    ('city-operations.parking_revenue.read', 'city-operations', 'See a parking cluster''s revenue', false),
    ('city-operations.parking_marshal.execute', 'city-operations', 'Check parking tickets and take cash bookings in a cluster', false)
ON CONFLICT (id) DO NOTHING;

UPDATE atlas_dashboard.capability
   SET resource_type = 'TICKET_PLACE'
 WHERE id IN ('city-operations.parking_cluster.read', 'city-operations.parking_cluster.write',
              'city-operations.parking_revenue.read', 'city-operations.parking_marshal.execute');

INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id, resource_id_param) VALUES
    ('city-operations.parking_cluster.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PARKING/GET_PARKING_CLUSTER', 'param:clusters'),
    ('city-operations.parking_cluster.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PARKING/SET_PARKING_CLUSTER_STATUS', 'param:clusters'),
    ('city-operations.parking_cluster.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PARKING/GET_PARKING_OCCUPANCY', 'param:clusters'),
    ('city-operations.parking_revenue.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PARKING/GET_PARKING_CLUSTER_REVENUE', 'param:clusters'),
    ('city-operations.parking_marshal.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PARKING/VERIFY_PARKING_TICKET', 'param:clusters'),
    ('city-operations.parking_marshal.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PARKING/CASH_BOOK_PARKING', 'param:clusters')
ON CONFLICT (capability_id, server_name, endpoint_id) DO UPDATE SET resource_id_param = EXCLUDED.resource_id_param;

INSERT INTO atlas_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at)
SELECT 'f6f28cb6-90ed-425f-b689-d068dff633d0', 'PARKING_CITY_MANAGER', 'DASHBOARD_USER', 'Runs parking for a city: all clusters, zones, revenue', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.role WHERE name = 'PARKING_CITY_MANAGER');
INSERT INTO atlas_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at)
SELECT '74d85886-66ad-43c9-8344-eed2249e3b75', 'PARKING_AREA_MANAGER', 'DASHBOARD_USER', 'Runs the parking clusters assigned to them', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.role WHERE name = 'PARKING_AREA_MANAGER');
INSERT INTO atlas_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at)
SELECT 'ccffc66d-1e2c-41dd-9998-92c69a686e1a', 'PARKING_MARSHAL', 'DASHBOARD_USER', 'Checks tickets and takes cash in the parking clusters assigned to them', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.role WHERE name = 'PARKING_MARSHAL');

-- City manager: city level + every cluster (no person_resource_access rows) + the zone editor.
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('city-operations.parking.read'), ('city-operations.parking_cluster.read'),
             ('city-operations.parking_cluster.write'), ('city-operations.parking_revenue.read'), ('city-operations.parking_marshal.execute'),
             ('city-operations.ticket_place.read'), ('city-operations.ticket_place.write')) AS c(cap)
WHERE r.name = 'PARKING_CITY_MANAGER' ON CONFLICT DO NOTHING;

-- Area manager: their clusters' occupancy, revenue and marshal actions; taking a cluster live stays with the city manager.
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('city-operations.parking_cluster.read'), ('city-operations.parking_revenue.read'), ('city-operations.parking_marshal.execute')) AS c(cap)
WHERE r.name = 'PARKING_AREA_MANAGER' ON CONFLICT DO NOTHING;

-- Marshal: their clusters' occupancy, ticket checks and cash bookings.
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('city-operations.parking_cluster.read'), ('city-operations.parking_marshal.execute')) AS c(cap)
WHERE r.name = 'PARKING_MARSHAL' ON CONFLICT DO NOTHING;
