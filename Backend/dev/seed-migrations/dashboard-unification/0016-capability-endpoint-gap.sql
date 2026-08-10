\set ON_ERROR_STOP on

-- ===========================================================================
-- 0016 — endpoints that post-date the capability inventory.
--
-- `0009` was generated from `raw/ny-endpoints.md`, extracted on 2026-08-05.
-- 31 DSL endpoints have shipped since, so they have an access_matrix row
-- (their `userActionType` migration ran) but NO capability_endpoint row.
--
-- That was harmless while the matrix was still a fallback. It is not harmless
-- now: Tools.Auth.Capability denies an endpoint it cannot resolve, so without
-- this file all 31 return 403 the moment the no-fallback binary ships, and
-- log CAPABILITY_UNMAPPED_ENDPOINT.
--
-- Capabilities below were derived by running the seed generator's own RULES
-- over the new endpoint ids — the same mapping the other 855 got, not
-- hand-assignment. All 31 mapped cleanly; none needed a judgement call.
--
-- MUST run BEFORE 0017: that backfill grants roles the capability behind each
-- endpoint they hold in the matrix, and it can only see endpoints that have a
-- capability_endpoint row.
--
-- Found by diffing the generated migrations against the baseline:
--   ids in migrations-read-only/*/**.sql  vs  Backend/dev/dsl-capability-baseline.txt
-- Re-run that diff after any long gap between codegen and reseeding.
-- ===========================================================================

INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES
    ('fleet.driver.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_DASHBOARD_INTERNAL_HELPER_GET_FLEET_OWNER_ID'),
    ('fleet.driver.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_DASHBOARD_INTERNAL_HELPER_GET_FLEET_OWNER_IDS'),
    ('fleet.driver.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_CHANGE_DRIVER'),
    ('fleet.onboarding.write', 'DASHBOARD', 'PROVIDER_FLEET/REGISTRATION_V2/POST_REGISTRATION_V2_LOGIN_OTP'),
    ('fleet.onboarding.write', 'DASHBOARD', 'PROVIDER_FLEET/REGISTRATION_V2/POST_REGISTRATION_V2_VERIFY_OTP'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_FLEET_OPERATOR_CHANGE'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_FLEET_OPERATOR_CREATE'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_OPERATOR_CHANGE'),
    ('system-config.merchant.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CITY_LIST'),
    ('system-config.merchant.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_MERCHANT_DOCUMENT'),
    ('system-config.config_pilot.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_CONCLUDE_OR_ABORT_OR_REVERT'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/METER_RIDE/GET_METER_RIDE_PRICE'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKET_PLACE_SERVICE_CATEGORY_DEL_PEOPLE'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKET_PLACE_SERVICE_CATEGORY_UPDATE_PEOPLE'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_LOGIN_AUTH'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_LOGIN_VERIFY'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_REGISTER'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_SENDVERIFYOTP'),
    ('city-operations.customer.write', 'DASHBOARD', 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_CANCELLATION_DUES_SYNC'),
    ('transit-config.gtfs.write', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_TICKET/POST_FRFS_TICKET_FRFS_ROUTE_ADD'),
    ('transit-config.gtfs.write', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_TICKET/POST_FRFS_TICKET_FRFS_ROUTE_DELETE'),
    ('transit-config.gtfs.write', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_TICKET/POST_FRFS_TICKET_FRFS_STATION_ADD'),
    ('transit-config.gtfs.write', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_TICKET/POST_FRFS_TICKET_FRFS_STATION_DELETE'),
    ('finance.invoice.read', 'DASHBOARD', 'RIDER_MANAGEMENT/INVOICE/GET_INVOICE_FINANCE_INVOICE_PDF'),
    ('city-operations.ride.read', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE/GET_RIDE_PICKUP_ROUTE'),
    ('city-operations.ride.read', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE/GET_RIDE_TRIP_ROUTE'),
    ('city-operations.ride.read', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE/GET_SHARE_RIDE_INFO'),
    ('city-operations.ride.read', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE/GET_SHARE_RIDE_INFO_BY_SHORT_ID'),
    ('city-operations.sos.read', 'DASHBOARD', 'RIDER_MANAGEMENT/SOS/GET_SOS_DETAILS'),
    ('city-operations.sos.read', 'DASHBOARD', 'RIDER_MANAGEMENT/SOS/GET_SOS_TRACKING'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/BOOKING/GET_BOOKING_BOOKING')
ON CONFLICT DO NOTHING;

-- Gate: every endpoint id referenced by access_matrix now resolves. Expect 0.
SELECT count(*) AS unresolvable_matrix_endpoints
FROM (
  SELECT DISTINCT m.api_entity, m.user_action_type
  FROM atlas_dashboard.access_matrix m
  WHERE m.user_access_type = 'USER_FULL_ACCESS'
    AND NOT EXISTS (
        SELECT 1 FROM atlas_dashboard.capability_endpoint ce
        WHERE ce.endpoint_id = CASE WHEN m.api_entity = 'SPECIAL_ZONES'
               THEN 'LEGACY/SPECIAL_ZONES/' || m.user_action_type
               ELSE m.user_action_type END)
) x;
