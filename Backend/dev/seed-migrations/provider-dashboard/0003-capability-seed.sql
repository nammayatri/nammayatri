-- Reset the GENERATED capability data so a re-seed is a true replace.
-- person_capability is not touched HERE (overrides survive a plain re-seed),
-- but a taxonomy rename purge further down may drop overrides whose capability
-- id no longer exists - re-issue those by hand.
DELETE FROM atlas_dashboard.role_capability;
DELETE FROM atlas_dashboard.capability_endpoint;
DELETE FROM atlas_dashboard.capability
 WHERE id NOT IN (SELECT capability_id FROM atlas_dashboard.person_capability);

-- Overrides pointing at capabilities that no longer exist after a rename.
-- Review and re-issue these by hand; they are kept (FK) but grant nothing.
-- SELECT pc.person_id, pc.capability_id, pc.mode, pc.reason
-- FROM atlas_dashboard.person_capability pc
-- LEFT JOIN atlas_dashboard.capability c ON c.id = pc.capability_id
-- WHERE c.id IS NULL;

-- Capability catalog (source of truth: docs/access-unification/capability-seed.md)
-- v1 -> v2 domain-scheme cleanup (see header comment in the generator).
DELETE FROM atlas_dashboard.capability_endpoint WHERE capability_id IN
    (SELECT id FROM atlas_dashboard.capability WHERE domain IN ('ops', 'agent', 'transit', 'config', 'city', 'tickets', 'system', 'utility', 'access'))
    OR capability_id IN ('analytics.incentive.read');
DELETE FROM atlas_dashboard.role_capability WHERE capability_id IN
    (SELECT id FROM atlas_dashboard.capability WHERE domain IN ('ops', 'agent', 'transit', 'config', 'city', 'tickets', 'system', 'utility', 'access'))
    OR capability_id IN ('analytics.incentive.read');
DELETE FROM atlas_dashboard.person_capability WHERE capability_id IN
    (SELECT id FROM atlas_dashboard.capability WHERE domain IN ('ops', 'agent', 'transit', 'config', 'city', 'tickets', 'system', 'utility', 'access'))
    OR capability_id IN ('analytics.incentive.read');
DELETE FROM atlas_dashboard.capability WHERE domain IN ('ops', 'agent', 'transit', 'config', 'city', 'tickets', 'system', 'utility', 'access') OR id IN ('analytics.incentive.read');
INSERT INTO atlas_dashboard.capability (id, domain, description, is_system) VALUES
    ('city-operations.scheduled-bookings.read', 'city-operations', '', false),
    ('city-operations.scheduled-bookings.write', 'city-operations', '', false),
    ('city-operations.customer.read', 'city-operations', '', false),
    ('city-operations.customer.write', 'city-operations', '', false),
    ('city-operations.driver.read', 'city-operations', '', false),
    ('city-operations.driver.write', 'city-operations', '', false),
    ('city-operations.onboarding.read', 'city-operations', '', false),
    ('city-operations.onboarding.write', 'city-operations', '', false),
    ('city-operations.pii.read', 'city-operations', '', false),
    ('city-operations.ride.read', 'city-operations', '', false),
    ('city-operations.ride.write', 'city-operations', '', false),
    ('city-operations.customer_issue.read', 'city-operations', '', false),
    ('city-operations.customer_issue.write', 'city-operations', '', false),
    ('city-operations.driver_issue.read', 'city-operations', '', false),
    ('city-operations.driver_issue.write', 'city-operations', '', false),
    ('city-operations.sos.read', 'city-operations', '', false),
    ('city-operations.sos.write', 'city-operations', '', false),
    ('city-operations.payment.read', 'city-operations', '', false),
    ('city-operations.payment.write', 'city-operations', '', false),
    ('city-operations.wallet.read', 'city-operations', '', false),
    ('city-operations.wallet.write', 'city-operations', '', false),
    ('city-operations.subscription.read', 'city-operations', '', false),
    ('city-operations.subscription.write', 'city-operations', '', false),
    ('city-operations.vehicle.read', 'city-operations', '', false),
    ('city-operations.vehicle.write', 'city-operations', '', false),
    ('city-operations.airport_queue.read', 'city-operations', '', false),
    ('city-operations.airport_queue.write', 'city-operations', '', false),
    ('city-operations.volunteer.read', 'city-operations', '', false),
    ('city-operations.volunteer.write', 'city-operations', '', false),
    ('city-operations.grievance.read', 'city-operations', '', false),
    ('city-operations.grievance.write', 'city-operations', '', false),
    ('city-operations.membership.read', 'city-operations', '', false),
    ('city-operations.training.read', 'city-operations', '', false),
    ('city-operations.incentive.read', 'city-operations', '', false),
    ('city-operations.fleet_owner.read', 'city-operations', '', false),
    ('city-operations.fleet_owner.write', 'city-operations', '', false),
    ('city-operations.booth_booking.execute', 'city-operations', '', false),
    ('city-operations.booth_customer.write', 'city-operations', '', false),
    ('city-operations.ticket.read', 'city-operations', '', false),
    ('city-operations.ticket.execute', 'city-operations', '', false),
    ('city-operations.pass.read', 'city-operations', '', false),
    ('city-operations.pass.execute', 'city-operations', '', false),
    ('city-operations.pass_org.read', 'city-operations', '', false),
    ('city-operations.pass_org.write', 'city-operations', '', false),
    ('city-operations.pass_org.approve', 'city-operations', '', false),
    ('city-operations.frfs.read', 'city-operations', '', false),
    ('city-operations.frfs.execute', 'city-operations', '', false),
    ('city-operations.edc.read', 'city-operations', '', false),
    ('city-operations.edc.write', 'city-operations', '', false),
    ('city-operations.ticket_place.read', 'city-operations', '', false),
    ('city-operations.ticket_place.write', 'city-operations', '', false),
    ('city-operations.ticket_place.approve', 'city-operations', '', false),
    ('city-operations.ticket_booking.read', 'city-operations', '', false),
    ('communication.message.read', 'communication', '', false),
    ('communication.message.write', 'communication', '', false),
    ('communication.shortener.execute', 'communication', '', false),
    ('transit-operations.master.read', 'transit-operations', '', false),
    ('transit-operations.master.write', 'transit-operations', '', false),
    ('transit-operations.waybill.write', 'transit-operations', '', false),
    ('transit-operations.device.read', 'transit-operations', '', false),
    ('transit-operations.device.write', 'transit-operations', '', false),
    ('transit-operations.trip.execute', 'transit-operations', '', false),
    ('transit-config.stops.read', 'transit-config', '', false),
    ('transit-config.stops.write', 'transit-config', '', false),
    ('transit-config.gtfs.read', 'transit-config', '', false),
    ('transit-config.gtfs.write', 'transit-config', '', false),
    ('transit-config.fare.read', 'transit-config', '', false),
    ('transit-config.fare.write', 'transit-config', '', false),
    ('transit-config.seat_layout.read', 'transit-config', '', false),
    ('transit-config.seat_layout.write', 'transit-config', '', false),
    ('system-config.merchant.read', 'system-config', '', false),
    ('system-config.merchant.write', 'system-config', '', false),
    ('system-config.fare_policy.read', 'system-config', '', false),
    ('system-config.fare_policy.write', 'system-config', '', false),
    ('system-config.fare_policy.export', 'system-config', '', false),
    ('system-config.customer_issue_config.read', 'system-config', '', false),
    ('system-config.customer_issue_config.write', 'system-config', '', false),
    ('system-config.driver_issue_config.read', 'system-config', '', false),
    ('system-config.driver_issue_config.write', 'system-config', '', false),
    ('system-config.namma_tag.read', 'system-config', '', false),
    ('system-config.namma_tag.write', 'system-config', '', false),
    ('system-config.dynamic_logic.read', 'system-config', '', false),
    ('system-config.dynamic_logic.write', 'system-config', '', false),
    ('system-config.config_pilot.read', 'system-config', '', false),
    ('system-config.config_pilot.write', 'system-config', '', false),
    ('system-config.coins.read', 'system-config', '', false),
    ('system-config.coins.write', 'system-config', '', false),
    ('system-config.rewards.read', 'system-config', '', false),
    ('system-config.rewards.write', 'system-config', '', false),
    ('system-config.knowledge.read', 'system-config', '', false),
    ('system-config.knowledge.write', 'system-config', '', false),
    ('system-config.firebase.read', 'system-config', '', false),
    ('system-config.firebase.write', 'system-config', '', false),
    ('system-config.registry.read', 'system-config', '', false),
    ('system-config.registry.write', 'system-config', '', false),
    ('system-config.release.read', 'system-config', '', false),
    ('system-config.release.write', 'system-config', '', false),
    ('system-config.scheduler.execute', 'system-config', '', false),
    ('system-config.failover.execute', 'system-config', '', false),
    ('city-config.cancel.read', 'city-config', '', false),
    ('city-config.cancel.write', 'city-config', '', false),
    ('city-config.geo.read', 'city-config', '', false),
    ('city-config.geo.write', 'city-config', '', false),
    ('city-config.offer.read', 'city-config', '', false),
    ('city-config.offer.write', 'city-config', '', false),
    ('city-config.plan.read', 'city-config', '', false),
    ('city-config.plan.write', 'city-config', '', false),
    ('city-config.service_tier.read', 'city-config', '', false),
    ('city-config.service_tier.write', 'city-config', '', false),
    ('city-config.launch.read', 'city-config', '', false),
    ('city-config.launch.write', 'city-config', '', false),
    ('city-config.merchant_onboarding.read', 'city-config', '', false),
    ('city-config.merchant_onboarding.write', 'city-config', '', false),
    ('city-config.merchant_onboarding.approve', 'city-config', '', false),
    ('fleet.driver.read', 'fleet', '', false),
    ('fleet.driver.write', 'fleet', '', false),
    ('fleet.vehicle.read', 'fleet', '', false),
    ('fleet.vehicle.write', 'fleet', '', false),
    ('fleet.trip.read', 'fleet', '', false),
    ('fleet.trip.write', 'fleet', '', false),
    ('fleet.earnings.read', 'fleet', '', false),
    ('fleet.live.read', 'fleet', '', false),
    ('fleet.onboarding.read', 'fleet', '', false),
    ('fleet.onboarding.write', 'fleet', '', false),
    ('fleet.profile.read', 'fleet', '', false),
    ('fleet.profile.write', 'fleet', '', false),
    ('fleet.operator.read', 'fleet', '', false),
    ('fleet.operator.write', 'fleet', '', false),
    ('finance.tds_reimbursement.submit', 'finance', '', false),
    ('finance.report.read', 'finance', '', false),
    ('finance.settlement.read', 'finance', '', false),
    ('finance.settlement.export', 'finance', '', false),
    ('finance.reconciliation.read', 'finance', '', false),
    ('finance.reconciliation.execute', 'finance', '', false),
    ('finance.payout.read', 'finance', '', false),
    ('finance.payout.write', 'finance', '', false),
    ('finance.adjustment.write', 'finance', '', false),
    ('finance.ledger.read', 'finance', '', false),
    ('finance.insurance.read', 'finance', '', false),
    ('finance.fleet.read', 'finance', '', false),
    ('finance.invoice.read', 'finance', '', false),
    ('finance.tds_reimbursement.read', 'finance', '', false),
    ('finance.tds_reimbursement.write', 'finance', '', false),
    ('analytics.core.read', 'analytics', '', false),
    ('analytics.performance.read', 'analytics', '', false),
    ('analytics.public_transport.read', 'analytics', '', false),
    ('analytics.pricing.read', 'analytics', '', false),
    ('analytics.pt_stats.read', 'analytics', '', false),
    ('analytics.sla.read', 'analytics', '', false),
    ('analytics.sla.write', 'analytics', '', false),
    ('analytics.ai.execute', 'analytics', '', false),
    ('analytics.revenue.read', 'analytics', '', false),
    ('admin.user.read', 'admin', '', false),
    ('admin.user.write', 'admin', '', false),
    ('admin.role.read', 'admin', '', false),
    ('admin.role.write', 'admin', '', false),
    ('admin.capability.read', 'admin', '', false),
    ('admin.capability.grant', 'admin', '', false),
    ('admin.tier.write', 'admin', '', true),
    ('admin.entity.read', 'admin', '', false),
    ('admin.entity.write', 'admin', '', false),
    ('admin.audit.read', 'admin', '', false),
    ('admin.merchant.write', 'admin', '', false),
    ('admin.query.execute', 'admin', '', true),
    ('admin.crypto.execute', 'admin', '', true)
ON CONFLICT (id) DO NOTHING;
-- endpoint_id -> capability shim. Generated by generate_capability_seed.py;
-- do not hand-edit (regenerate instead).
INSERT INTO atlas_dashboard.capability_endpoint (capability_id, server_name, endpoint_id) VALUES
    ('city-config.geo.write', 'DASHBOARD', 'LEGACY/SPECIAL_ZONES/SPECIAL_ZONE_CREATE'),
    ('city-config.geo.write', 'DASHBOARD', 'LEGACY/SPECIAL_ZONES/SPECIAL_ZONE_DELETE'),
    ('city-config.geo.read', 'DASHBOARD', 'LEGACY/SPECIAL_ZONES/SPECIAL_ZONE_LOOKUP'),
    ('city-config.geo.write', 'DASHBOARD', 'LEGACY/SPECIAL_ZONES/SPECIAL_ZONE_UPDATE'),
    ('fleet.trip.read', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/DRIVER/GET_DRIVER_FLEET_LIST_RIDES'),
    ('city-operations.subscription.write', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/DRIVER_SUBSCRIPTION/POST_DRIVER_SUBSCRIPTION_SEND_SMS'),
    ('city-operations.subscription.write', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/DRIVER_SUBSCRIPTION/POST_DRIVER_SUBSCRIPTION_UPDATE_DRIVER_FEE_AND_INVOICE_INFO'),
    ('city-operations.wallet.read', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/DRIVER_WALLET/GET_DRIVER_WALLET_WALLET_BALANCE'),
    ('city-operations.wallet.read', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/DRIVER_WALLET/GET_DRIVER_WALLET_WALLET_PAYOUT_HISTORY'),
    ('city-operations.wallet.read', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/DRIVER_WALLET/GET_DRIVER_WALLET_WALLET_TRANSACTIONS'),
    ('city-operations.wallet.read', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/DRIVER_WALLET/GET_DRIVER_WALLET_WALLET_TRANSACTION_HISTORY'),
    ('city-operations.wallet.write', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/DRIVER_WALLET/POST_DRIVER_WALLET_WALLET_AIRPORT_CASH_RECHARGE'),
    ('city-operations.wallet.write', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/DRIVER_WALLET/POST_DRIVER_WALLET_WALLET_PAYOUT'),
    ('city-operations.wallet.write', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/DRIVER_WALLET/POST_DRIVER_WALLET_WALLET_TOPUP'),
    ('communication.message.read', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/OVERLAY/GET_OVERLAY_INFO'),
    ('communication.message.read', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/OVERLAY/GET_OVERLAY_LIST'),
    ('communication.message.write', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/OVERLAY/POST_OVERLAY_CREATE'),
    ('communication.message.write', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/OVERLAY/POST_OVERLAY_DELETE'),
    ('communication.message.write', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/OVERLAY/POST_OVERLAY_SCHEDULE'),
    ('system-config.scheduler.execute', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/PENALTY/POST_PENALTY_TRIGGER_JOB_CANCELLATION_PENALTY_SERVICE_NAME'),
    ('city-operations.subscription.read', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_CURRENT_PLAN'),
    ('city-operations.subscription.read', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_CURRENT_PLAN_V2'),
    ('city-operations.subscription.read', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_DRIVER_PAYMENT_HISTORY_API_V2'),
    ('city-operations.subscription.read', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_DRIVER_PAYMENT_HISTORY_ENTITY_DETAILS_V2'),
    ('city-operations.subscription.read', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_LIST_PLAN'),
    ('city-operations.subscription.read', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_LIST_PLAN_V2'),
    ('city-operations.subscription.read', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_ORDER_STATUS'),
    ('city-operations.subscription.read', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/GET_SUBSCRIPTION_PURCHASE_LIST'),
    ('city-operations.subscription.write', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_COLLECT_MANUAL_PAYMENTS'),
    ('finance.adjustment.write', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_FEE_WAIVE_OFF'),
    ('city-operations.subscription.write', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_SUBSCRIBE_PLAN'),
    ('city-operations.subscription.write', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/POST_SUBSCRIPTION_SUBSCRIBE_PLAN_V2'),
    ('city-operations.subscription.write', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SELECT_PLAN'),
    ('city-operations.subscription.write', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SELECT_PLAN_V2'),
    ('city-operations.subscription.write', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SUSPEND_PLAN'),
    ('city-operations.subscription.write', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION/PUT_SUBSCRIPTION_SUSPEND_PLAN_V2'),
    ('city-operations.subscription.read', 'DASHBOARD', 'PROVIDER_APP_MANAGEMENT/SUBSCRIPTION_TRANSACTION/GET_SUBSCRIPTION_TRANSACTION_SUBSCRIPTION_TRANSACTIONS'),
    ('fleet.live.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_DASHBOARD_FLEET_TRIP_WAYPOINTS'),
    ('fleet.driver.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_ACCESS_LIST'),
    ('fleet.trip.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_ASSIGNMENTS'),
    ('fleet.trip.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_BOOKINGS'),
    ('fleet.earnings.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DASHBOARD_ANALYTICS'),
    ('fleet.earnings.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DASHBOARD_ANALYTICS_ALL_TIME'),
    ('fleet.driver.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DRIVER_ASSOCIATION'),
    ('fleet.driver.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DRIVER_DETAILS'),
    ('fleet.earnings.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DRIVER_EARNING'),
    ('fleet.driver.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DRIVER_LIST_STATS'),
    ('fleet.vehicle.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DRIVER_ONBOARDED_DRIVERS_AND_UNLINKED_VEHICLES'),
    ('fleet.vehicle.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_DRIVER_VEHICLE_ASSOCIATION'),
    ('fleet.vehicle.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_GET_ALL_BADGE'),
    ('fleet.driver.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_GET_ALL_DRIVER'),
    ('fleet.vehicle.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_GET_ALL_VEHICLE'),
    ('fleet.driver.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_GET_DRIVER_REQUESTS'),
    ('fleet.profile.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_OPERATOR_INFO'),
    ('fleet.profile.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_OWNER_INFO'),
    ('fleet.driver.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_OWNER_LIST'),
    ('fleet.trip.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_POSSIBLE_ROUTES'),
    ('fleet.trip.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_ROUTES'),
    ('fleet.trip.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_SCHEDULED_BOOKING_LIST'),
    ('fleet.live.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_STATUS'),
    ('fleet.live.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_STATUS_SUMMARY'),
    ('fleet.earnings.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_TOTAL_EARNING'),
    ('fleet.trip.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_TRIP_TRANSACTIONS'),
    ('fleet.vehicle.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_VEHICLE_ASSOCIATION'),
    ('fleet.earnings.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_VEHICLE_EARNING'),
    ('fleet.vehicle.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_VEHICLE_LIST_STATS'),
    ('fleet.trip.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_FLEET_WMB_ROUTE_DETAILS'),
    ('fleet.vehicle.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/GET_DRIVER_VEHICLE_INFO'),
    ('fleet.onboarding.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_ADD_RIDE_PAYOUT_ACCOUNT_NUMBER'),
    ('fleet.trip.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_DASHBOARD_FLEET_ESTIMATE_ROUTE'),
    ('fleet.live.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_DASHBOARD_FLEET_TRACK_DRIVER'),
    ('fleet.trip.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_DASHBOARD_FLEET_WMB_TRIP_END'),
    ('fleet.driver.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ACCESS_SELECT'),
    ('fleet.driver.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ADD_DRIVERS'),
    ('fleet.trip.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ADD_DRIVER_BUS_ROUTE_MAPPING'),
    ('fleet.vehicle.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ADD_RC_WITHOUT_DRIVER'),
    ('fleet.vehicle.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ADD_VEHICLE'),
    ('fleet.vehicle.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_ADD_VEHICLES'),
    ('fleet.driver.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_APPROVE_DRIVER'),
    ('fleet.earnings.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_DASHBOARD_ANALYTICS_CACHE'),
    ('fleet.driver.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_DRIVER_CHANGE_FLEET_OWNER'),
    ('fleet.driver.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_DRIVER_UPDATE'),
    ('fleet.driver.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_GET_DRIVER_DETAILS'),
    ('fleet.live.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_GET_NEARBY_DRIVERS'),
    ('fleet.live.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_GET_NEARBY_DRIVERS_V2'),
    ('fleet.vehicle.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_LINK_RC_WITH_DRIVER'),
    ('fleet.live.read', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_LOCATION_LIST'),
    ('fleet.driver.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_REMOVE_DRIVER'),
    ('fleet.vehicle.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_REMOVE_VEHICLE'),
    ('fleet.driver.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_RESPOND_DRIVER_REQUEST'),
    ('fleet.trip.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_SCHEDULED_BOOKING_ASSIGN'),
    ('fleet.trip.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_SCHEDULED_BOOKING_CANCEL'),
    ('fleet.trip.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_SCHEDULED_BOOKING_REASSIGN'),
    ('fleet.driver.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_SEND_JOINING_OTP'),
    ('fleet.trip.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_TRIP_PLANNER'),
    ('fleet.trip.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_TRIP_TRANSACTIONS_V2'),
    ('fleet.driver.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_UNLINK'),
    ('fleet.driver.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_V2_ACCESS_MULTI_OWNER_ID_SELECT'),
    ('fleet.driver.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_V2_ACCESS_SELECT'),
    ('fleet.vehicle.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_VEHICLE_DRIVER_RC_STATUS'),
    ('fleet.vehicle.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_VEHICLE_EDIT'),
    ('fleet.driver.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_FLEET_VERIFY_JOINING_OTP'),
    ('fleet.profile.write', 'DASHBOARD', 'PROVIDER_FLEET/DRIVER/POST_DRIVER_UPDATE_FLEET_OWNER_INFO'),
    ('fleet.live.read', 'DASHBOARD', 'PROVIDER_FLEET/LIVE_MAP/GET_LIVE_MAP_DRIVERS'),
    ('fleet.onboarding.read', 'DASHBOARD', 'PROVIDER_FLEET/ONBOARDING/GET_ONBOARDING_DOCUMENT_CONFIGS'),
    ('fleet.onboarding.read', 'DASHBOARD', 'PROVIDER_FLEET/ONBOARDING/GET_ONBOARDING_GET_REFERRAL_DETAILS'),
    ('fleet.onboarding.read', 'DASHBOARD', 'PROVIDER_FLEET/ONBOARDING/GET_ONBOARDING_REGISTER_STATUS'),
    ('fleet.onboarding.read', 'DASHBOARD', 'PROVIDER_FLEET/ONBOARDING/GET_ONBOARDING_REGISTER_VEHICLE_STATUS'),
    ('fleet.onboarding.read', 'DASHBOARD', 'PROVIDER_FLEET/ONBOARDING/GET_ONBOARDING_VEHICLE_DOCUMENTS'),
    ('fleet.onboarding.write', 'DASHBOARD', 'PROVIDER_FLEET/ONBOARDING/POST_ONBOARDING_VERIFY'),
    ('fleet.onboarding.write', 'DASHBOARD', 'PROVIDER_FLEET/PAYOUT_ACCOUNT/POST_PAYOUT_ACCOUNT'),
    ('fleet.onboarding.write', 'DASHBOARD', 'PROVIDER_FLEET/PAYOUT_ACCOUNT/POST_PAYOUT_ACCOUNT_STATUS'),
    ('fleet.profile.read', 'DASHBOARD', 'PROVIDER_FLEET/REGISTRATION_V2/GET_REGISTRATION_V2_PROFILE_LANGUAGE'),
    ('fleet.onboarding.read', 'DASHBOARD', 'PROVIDER_FLEET/REGISTRATION_V2/GET_REGISTRATION_V2_REGISTER_BANK_ACCOUNT_STATUS'),
    ('fleet.onboarding.write', 'DASHBOARD', 'PROVIDER_FLEET/REGISTRATION_V2/POST_REGISTRATION_V2_REGISTER'),
    ('fleet.onboarding.write', 'DASHBOARD', 'PROVIDER_FLEET/REGISTRATION_V2/POST_REGISTRATION_V2_REGISTER_BANK_ACCOUNT_LINK'),
    ('fleet.profile.write', 'DASHBOARD', 'PROVIDER_FLEET/REGISTRATION_V2/PUT_REGISTRATION_V2_PROFILE_LANGUAGE'),
    ('system-config.driver_issue_config.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/DELETE_ISSUE_CATEGORY'),
    ('system-config.driver_issue_config.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/DELETE_ISSUE_MESSAGE'),
    ('system-config.driver_issue_config.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/DELETE_ISSUE_OPTION'),
    ('system-config.driver_issue_config.read', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CATEGORY_DETAIL'),
    ('system-config.driver_issue_config.read', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CATEGORY_FLOW_PREVIEW'),
    ('system-config.driver_issue_config.read', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CATEGORY_LIST'),
    ('city-operations.driver_issue.read', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CHAT_MESSAGES'),
    ('system-config.driver_issue_config.read', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CONFIG'),
    ('city-operations.driver_issue.read', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_INFO'),
    ('city-operations.driver_issue.read', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_INFO_V2'),
    ('city-operations.driver_issue.read', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_LIST'),
    ('city-operations.driver_issue.read', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_MEDIA'),
    ('system-config.driver_issue_config.read', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_MESSAGE_DETAIL'),
    ('system-config.driver_issue_config.read', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_MESSAGE_LIST'),
    ('system-config.driver_issue_config.read', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_OPTION_DETAIL'),
    ('system-config.driver_issue_config.read', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_OPTION_LIST'),
    ('system-config.driver_issue_config.read', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_TRANSLATIONS'),
    ('system-config.driver_issue_config.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_BULK_UPSERT_TRANSLATIONS'),
    ('system-config.driver_issue_config.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_ALL_COPY'),
    ('system-config.driver_issue_config.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_COPY'),
    ('system-config.driver_issue_config.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_CREATE'),
    ('system-config.driver_issue_config.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_DEFAULT_COPY'),
    ('system-config.driver_issue_config.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_REORDER'),
    ('system-config.driver_issue_config.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_UPDATE'),
    ('city-operations.driver_issue.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CHAT_MESSAGE'),
    ('city-operations.driver_issue.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CHAT_READ'),
    ('city-operations.driver_issue.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CHAT_UPLOAD'),
    ('city-operations.driver_issue.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_COMMENT'),
    ('system-config.driver_issue_config.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CONFIG_UPDATE'),
    ('system-config.driver_issue_config.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_MESSAGE_REORDER'),
    ('system-config.driver_issue_config.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_MESSAGE_UPSERT'),
    ('system-config.driver_issue_config.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_CREATE'),
    ('system-config.driver_issue_config.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_REORDER'),
    ('system-config.driver_issue_config.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_UPDATE'),
    ('city-operations.driver_issue.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_TICKET_STATUS_CALL_BACK'),
    ('city-operations.driver_issue.write', 'DASHBOARD', 'PROVIDER_ISSUE_MANAGEMENT/ISSUE/PUT_ISSUE_UPDATE'),
    ('city-operations.onboarding.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/ACCOUNT/GET_ACCOUNT_FETCH_UNVERIFIED_ACCOUNTS'),
    ('city-operations.onboarding.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/ACCOUNT/POST_ACCOUNT_VERIFY_ACCOUNT'),
    ('admin.user.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/ACCOUNT/PUT_ACCOUNT_UPDATE_ROLE'),
    ('city-operations.ride.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/BOOKING/POST_BOOKING_CANCEL_ALL_STUCK'),
    ('city-operations.ride.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/BOOKING/POST_BOOKING_SYNC_MULTIPLE'),
    ('system-config.coins.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/COINS_CONFIG/GET_COINS_CONFIG_LIST'),
    ('system-config.coins.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/COINS_CONFIG/POST_COINS_CONFIG_CREATE'),
    ('system-config.coins.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/COINS_CONFIG/PUT_COINS_CONFIG_UPDATE'),
    ('communication.message.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/COMMUNICATION/DELETE_COMMUNICATION_DELETE'),
    ('communication.message.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/COMMUNICATION/GET_COMMUNICATION_DELIVERY_STATUS'),
    ('communication.message.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/COMMUNICATION/GET_COMMUNICATION_INFO'),
    ('communication.message.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/COMMUNICATION/GET_COMMUNICATION_LIST'),
    ('communication.message.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/COMMUNICATION/GET_COMMUNICATION_RECIPIENTS'),
    ('communication.message.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/COMMUNICATION/GET_COMMUNICATION_TEMPLATE'),
    ('communication.message.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/COMMUNICATION/POST_COMMUNICATION_CREATE'),
    ('communication.message.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/COMMUNICATION/POST_COMMUNICATION_SEND'),
    ('communication.message.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/COMMUNICATION/PUT_COMMUNICATION_EDIT'),
    ('city-config.offer.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DOMAIN_DISCOUNT_CONFIG/DELETE_DOMAIN_DISCOUNT_CONFIG_DELETE'),
    ('city-config.offer.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DOMAIN_DISCOUNT_CONFIG/GET_DOMAIN_DISCOUNT_CONFIG_LIST'),
    ('city-config.offer.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DOMAIN_DISCOUNT_CONFIG/POST_DOMAIN_DISCOUNT_CONFIG_CREATE'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/DELETE_DRIVER_PERMANENTLY_DELETE'),
    ('city-operations.pii.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_AADHAAR_INFO'),
    ('city-operations.pii.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_AADHAAR_INFOBY_MOBILE_NUMBER'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_ACTIVITY'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_AIRPORT_PREFERENCE'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_BLOCK_REASON_LIST'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_CLEAR_STUCK_ON_RIDE'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_DOCUMENTS_INFO'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_EARNINGS'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_GET_OPERATING_CITY'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_IDENTITY_INFO'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_LIST'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_LOCATION'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_PAN_AADHAR_SELFIE_DETAILS'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_PAN_AADHAR_SELFIE_DETAILS_LIST'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_SEARCH_REQUEST_STATS'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_SECURITY_DEPOSIT_STATUS'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/GET_DRIVER_STATS'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_AC_RESTRICTION_UPDATE'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_AIRPORT_PREFERENCE'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_ASSOCIATION_CHANGE'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BLOCK'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BLOCK_WITH_REASON'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BULK_REVIEW_RC_VARIANT'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_BULK_SUBSCRIPTION_SERVICE_UPDATE'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_CHANGE_OPERATING_CITY'),
    ('finance.adjustment.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_CLEAR_FEE'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_DELETE_RC'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_DISABLE'),
    ('city-operations.pii.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_DRIVER_DATA_DECRYPTION'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_IDENTITY_INFO_UPDATE'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_PAUSE_OR_RESUME_SERVICE_CHARGES'),
    ('city-operations.pii.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_PERSON_ID'),
    ('city-operations.pii.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_PERSON_NUMBERS'),
    ('finance.adjustment.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_REFUND_BY_PAYOUT'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_SEND_DUMMY_NOTIFICATION'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_SYNC_DOC_AADHAR_PAN'),
    ('finance.adjustment.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_TDS_RATE_UPDATE'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UNBLOCK'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UNLINK_AADHAAR'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UNLINK_DL'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_BY_PHONE_NUMBER'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_DRIVER_TAG'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_MERCHANT'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_NAME'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_PHONE_NUMBER'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_RC_INVALID_STATUS'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_RC_INVALID_STATUS_BY_RC_NUMBER'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_SPECIAL_LOC_WARRIOR'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_TAG_BULK'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_VEHICLE_MANUFACTURING'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_UPDATE_VEHICLE_VARIANT'),
    ('city-operations.vehicle.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_VEHICLE_APPEND_SELECTED_SERVICE_TIERS'),
    ('city-operations.vehicle.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER/POST_DRIVER_VEHICLE_UPSERT_SELECTED_SERVICE_TIERS'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_COINS/GET_DRIVER_COINS_COIN_HISTORY'),
    ('system-config.coins.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_COINS/POST_DRIVER_COINS_BLACKLISTED_EVENTS_UPDATE'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_COINS/POST_DRIVER_COINS_BULK_UPLOAD_COINS'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_COINS/POST_DRIVER_COINS_BULK_UPLOAD_COINS_V2'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_GO_HOME/GET_DRIVER_GO_HOME_GET_GO_HOME_INFO'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_GO_HOME/GET_DRIVER_GO_HOME_GET_HOME_LOCATION'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_GO_HOME/POST_DRIVER_GO_HOME_INCREMENT_GO_TO_COUNT'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_GO_HOME/POST_DRIVER_GO_HOME_UPDATE_HOME_LOCATION'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REFERRAL/POST_DRIVER_REFERRAL_LINK_REFERRAL'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REFERRAL/POST_DRIVER_REFERRAL_REFERRAL_OPS_PASSWORD'),
    ('city-operations.onboarding.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_DOCUMENTS_COMMON_LIST'),
    ('city-operations.onboarding.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_DOCUMENTS_INFO'),
    ('city-operations.onboarding.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_DOCUMENTS_LIST'),
    ('city-operations.onboarding.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_GET_DOCUMENT'),
    ('city-operations.onboarding.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_INFO_BANK_ACCOUNT'),
    ('city-operations.onboarding.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_PAYOUT_ORDER_STATUS'),
    ('city-operations.onboarding.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_PAYOUT_REGISTRATION'),
    ('city-operations.onboarding.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_UNDER_REVIEW_DRIVERS'),
    ('city-operations.onboarding.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/GET_DRIVER_REGISTRATION_VERIFICATION_STATUS'),
    ('city-operations.onboarding.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_DELETE_BANK_ACCOUNT'),
    ('city-operations.onboarding.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_DOCUMENTS_COMMON'),
    ('city-operations.onboarding.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_DOCUMENTS_UPDATE'),
    ('city-operations.onboarding.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_DOCUMENT_REGISTER'),
    ('city-operations.onboarding.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_DOCUMENT_UPLOAD'),
    ('city-operations.onboarding.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_AADHAAR'),
    ('city-operations.onboarding.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_DL'),
    ('city-operations.onboarding.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_GENERATE_AADHAAR_OTP'),
    ('city-operations.onboarding.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_RC'),
    ('city-operations.onboarding.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_REGISTER_VERIFY_AADHAAR_OTP'),
    ('city-operations.onboarding.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_TRIGGER_REMINDER'),
    ('city-operations.onboarding.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_UNLINK_DOCUMENT'),
    ('city-operations.onboarding.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_VERIFY_BANK_ACCOUNT'),
    ('city-operations.vehicle.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_VEHICLE_QUALITY/GET_DRIVER_VEHICLE_QUALITY_LIST'),
    ('city-operations.vehicle.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_VEHICLE_QUALITY/GET_DRIVER_VEHICLE_QUALITY_SEARCH'),
    ('city-operations.vehicle.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/DRIVER_VEHICLE_QUALITY/POST_DRIVER_VEHICLE_QUALITY_UPDATE_VEHICLE_RATING'),
    ('city-operations.vehicle.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/ENTITY_INFO/GET_ENTITY_INFO_LIST'),
    ('city-operations.vehicle.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/ENTITY_INFO/POST_ENTITY_INFO_UPDATE'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FEEDBACK_FORM/DELETE_FEEDBACK_FORM_DELETE'),
    ('system-config.merchant.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FEEDBACK_FORM/GET_FEEDBACK_FORM'),
    ('system-config.merchant.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FEEDBACK_FORM/GET_FEEDBACK_FORM_LIST'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FEEDBACK_FORM/POST_FEEDBACK_FORM_CREATE'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FEEDBACK_FORM/PUT_FEEDBACK_FORM_UPDATE'),
    ('finance.report.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_AUDIT_LIST'),
    ('finance.report.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_INVOICE_LIST'),
    ('finance.report.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_INVOICE_PDF'),
    ('finance.report.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_PAYMENT_GATEWAY_TRANSACTION_LIST'),
    ('finance.report.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_PAYMENT_SETTLEMENT_LIST'),
    ('finance.reconciliation.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_RECONCILIATION'),
    ('finance.report.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_SAP_JOURNALS'),
    ('finance.report.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_SAP_JOURNALS_TRANSACTIONS'),
    ('finance.ledger.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_FINANCE_WALLET_LEDGER'),
    ('finance.report.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/GET_FINANCE_MANAGEMENT_SUBSCRIPTION_PURCHASE_LIST'),
    ('finance.reconciliation.execute', 'DASHBOARD', 'PROVIDER_MANAGEMENT/FINANCE_MANAGEMENT/POST_FINANCE_MANAGEMENT_RECONCILIATION_TRIGGER'),
    ('system-config.knowledge.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/KNOWLEDGE_CENTER/DELETE_KNOWLEDGE_CENTER_SOP_DOCUMENT'),
    ('system-config.knowledge.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/KNOWLEDGE_CENTER/DELETE_KNOWLEDGE_CENTER_SOP_TYPE'),
    ('system-config.knowledge.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/KNOWLEDGE_CENTER/GET_KNOWLEDGE_CENTER_GET_DOCUMENT'),
    ('system-config.knowledge.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/KNOWLEDGE_CENTER/GET_KNOWLEDGE_CENTER_SOP_LIST'),
    ('system-config.knowledge.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/KNOWLEDGE_CENTER/POST_KNOWLEDGE_CENTER_SOP_UPLOAD'),
    ('system-config.knowledge.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/KNOWLEDGE_CENTER/PUT_KNOWLEDGE_CENTER_SOP_TYPE_RENAME'),
    ('city-operations.pii.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MEDIA/GET_MEDIA_MEDIA_IMAGE'),
    ('city-operations.pii.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MEDIA_FILE_DOCUMENT/GET_MEDIA_FILE_DOCUMENT_DOWNLOAD_LINK'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MEDIA_FILE_DOCUMENT/POST_MEDIA_FILE_DOCUMENT_CONFIRM'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MEDIA_FILE_DOCUMENT/POST_MEDIA_FILE_DOCUMENT_DELETE'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MEDIA_FILE_DOCUMENT/POST_MEDIA_FILE_DOCUMENT_UPLOAD_LINK'),
    ('communication.message.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_MERCHANT_MESSAGE'),
    ('city-config.geo.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_DELETE'),
    ('city-config.geo.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_GATES_DELETE'),
    ('city-config.geo.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_TOLL_DELETE'),
    ('system-config.merchant.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_COMMON'),
    ('system-config.merchant.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_DRIVER_INTELLIGENT_POOL'),
    ('system-config.merchant.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_DRIVER_POOL'),
    ('system-config.merchant.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_DRIVER_POOL_LIST'),
    ('system-config.fare_policy.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_FARE_POLICY_DETAILS'),
    ('system-config.fare_policy.export', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_FARE_POLICY_EXPORT'),
    ('system-config.fare_policy.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_FARE_PRODUCT_LIST'),
    ('city-config.geo.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_GEOMETRY_LIST'),
    ('system-config.merchant.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_ONBOARDING_DOCUMENT'),
    ('city-config.geo.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_SPECIAL_LOCATION_LIST'),
    ('system-config.merchant.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_SUBSCRIPTION_CONFIG_LIST'),
    ('city-config.geo.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_TOLL_LIST'),
    ('city-config.service_tier.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_VEHICLE_SERVICE_TIER'),
    ('city-config.service_tier.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_VEHICLE_SERVICE_TIER_LIST'),
    ('system-config.merchant.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_VENDOR_SPLIT_DETAILS_LIST'),
    ('system-config.merchant.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_MERCHANT_DOCUMENT_LIST'),
    ('communication.message.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_MERCHANT_MESSAGE_CATALOG'),
    ('system-config.merchant.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_SERVICE_USAGE_CONFIG'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_CLEAR_CACHE_SUBSCRIPTION'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_COMMON_UPDATE'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DEBUG_LOG_UPDATE'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DRIVER_INTELLIGENT_POOL_UPDATE'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DRIVER_POOL_CREATE'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DRIVER_POOL_UPDATE'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DRIVER_POOL_UPSERT'),
    ('system-config.failover.execute', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FAILOVER'),
    ('system-config.fare_policy.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_DRIVER_EXTRA_FEE_BOUNDS_CREATE'),
    ('system-config.fare_policy.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_DRIVER_EXTRA_FEE_BOUNDS_UPDATE'),
    ('system-config.fare_policy.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_PER_EXTRA_KM_RATE_UPDATE'),
    ('system-config.fare_policy.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_UPDATE'),
    ('system-config.fare_policy.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_POLICY_UPSERT'),
    ('system-config.fare_policy.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FARE_PRODUCT_SET_ENABLED'),
    ('city-config.launch.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_MERCHANT_CREATE'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_ONBOARDING_DOCUMENT_CREATE'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_ONBOARDING_DOCUMENT_UPDATE'),
    ('city-config.launch.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_OPERATING_CITY_CREATE'),
    ('city-config.launch.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_OPERATING_CITY_WHITE_LIST'),
    ('city-config.geo.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_SPECIAL_LOCATION_UPSERT'),
    ('city-config.geo.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_TOLL_UPSERT'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_UPSERT_PLAN_AND_CONFIG_SUBSCRIPTION'),
    ('city-config.service_tier.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_VEHICLE_SERVICE_TIER_CREATE'),
    ('city-config.service_tier.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_VEHICLE_SERVICE_TIER_UPDATE'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_MERCHANT_DOCUMENT_CREATE'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_MERCHANT_DOCUMENT_DELETE'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_MERCHANT_DOCUMENT_UPDATE'),
    ('communication.message.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_MERCHANT_MESSAGE_UPSERT'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_PAYOUT_CONFIG_UPDATE'),
    ('system-config.scheduler.execute', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SCHEDULER_TRIGGER'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_MAPS_UPDATE'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_SMS_UPDATE'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_VERIFICATION_UPDATE'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_MAPS_UPDATE'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_SMS_UPDATE'),
    ('city-config.geo.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_GATES_UPSERT'),
    ('city-config.geo.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_UPSERT'),
    ('city-config.geo.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_TOLL_UPSERT'),
    ('system-config.merchant.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_UPDATE'),
    ('city-config.launch.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_UPDATE_ONBOARDING_VEHICLE_VARIANT_MAPPING'),
    ('city-config.geo.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MERCHANT/PUT_MERCHANT_CONFIG_GEOMETRY_UPDATE'),
    ('communication.message.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MESSAGE/GET_MESSAGE_DELIVERY_INFO'),
    ('communication.message.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MESSAGE/GET_MESSAGE_INFO'),
    ('communication.message.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MESSAGE/GET_MESSAGE_LIST'),
    ('communication.message.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MESSAGE/GET_MESSAGE_RECEIVER_LIST'),
    ('communication.message.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_ADD'),
    ('communication.message.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_ADD_LINK'),
    ('communication.message.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_EDIT'),
    ('communication.message.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_SEND'),
    ('communication.message.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/MESSAGE/POST_MESSAGE_UPLOAD_FILE'),
    ('system-config.namma_tag.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/DELETE_NAMMA_TAG_QUERY_DELETE'),
    ('system-config.namma_tag.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/DELETE_NAMMA_TAG_TAG_DELETE'),
    ('system-config.namma_tag.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/DELETE_NAMMA_TAG_TIME_BOUNDS_DELETE'),
    ('system-config.dynamic_logic.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC'),
    ('system-config.dynamic_logic.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_DOMAINS'),
    ('system-config.dynamic_logic.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_DOMAINS_AND_EVENTS'),
    ('system-config.dynamic_logic.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_GET_DOMAIN_SCHEMA'),
    ('system-config.dynamic_logic.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_GET_LOGIC_ROLLOUT'),
    ('system-config.dynamic_logic.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_VERSIONS'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_BEHAVIOR_VISIBILITY'),
    ('system-config.config_pilot.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_ALL_CONFIGS'),
    ('system-config.config_pilot.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_ALL_UI_CONFIGS'),
    ('system-config.config_pilot.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_ALWAYS_ON_LIST'),
    ('system-config.config_pilot.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_CONFIG_DETAILS'),
    ('system-config.config_pilot.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_GET_DIMENSION_SCHEMA'),
    ('system-config.config_pilot.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_GET_TABLE_DATA'),
    ('system-config.config_pilot.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_GET_UI_TABLE_DATA'),
    ('system-config.config_pilot.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_UI_CONFIG_DETAILS'),
    ('system-config.namma_tag.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_QUERY_ALL'),
    ('system-config.namma_tag.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_QUERY_DETAILS'),
    ('system-config.namma_tag.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_TAG_ALL'),
    ('system-config.namma_tag.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_TAG_DETAILS'),
    ('system-config.namma_tag.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_TIME_BOUNDS'),
    ('system-config.dynamic_logic.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_UPSERT_LOGIC_ROLLOUT'),
    ('system-config.dynamic_logic.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_VERIFY'),
    ('system-config.config_pilot.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_ACTION_CHANGE'),
    ('system-config.config_pilot.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_CREATE_ROW'),
    ('system-config.config_pilot.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_CREATE_UI_CONFIG'),
    ('system-config.config_pilot.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_GET_CONFIG'),
    ('system-config.config_pilot.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_GET_CONFIG_WITH_DIMENSIONS'),
    ('system-config.config_pilot.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_GET_PATCHED_ELEMENT'),
    ('system-config.config_pilot.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_GET_VERSION'),
    ('system-config.namma_tag.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_QUERY_CREATE'),
    ('system-config.namma_tag.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_QUERY_UPDATE'),
    ('system-config.dynamic_logic.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_RUN_JOB'),
    ('system-config.namma_tag.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TAG_CREATE'),
    ('system-config.namma_tag.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TAG_UPDATE'),
    ('system-config.namma_tag.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TAG_VERIFY'),
    ('system-config.namma_tag.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TIME_BOUNDS_CREATE'),
    ('finance.payout.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_PAYOUT'),
    ('finance.payout.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_PAYOUT_HISTORY'),
    ('finance.payout.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_PAYOUT_ORDER'),
    ('finance.payout.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_PAYOUT_REFERRAL_HISTORY'),
    ('finance.payout.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_CANCEL'),
    ('finance.payout.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_CASH'),
    ('finance.payout.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_RETRY'),
    ('finance.payout.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_SCHEDULED_PAYOUT_CONFIG_UPSERT'),
    ('finance.payout.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_VPA_DELETE'),
    ('finance.payout.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_VPA_REFUND_REGISTRATION'),
    ('finance.payout.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PAYOUT/POST_PAYOUT_PAYOUT_VPA_UPDATE'),
    ('city-config.plan.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PLAN_MANAGEMENT/GET_PLAN_MANAGEMENT_LIST_PLANS'),
    ('city-config.plan.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PLAN_MANAGEMENT/GET_PLAN_MANAGEMENT_PLAN_TRANSLATIONS'),
    ('city-config.plan.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PLAN_MANAGEMENT/POST_PLAN_MANAGEMENT_ACTIVATE_PLAN'),
    ('city-config.plan.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PLAN_MANAGEMENT/POST_PLAN_MANAGEMENT_CREATE'),
    ('city-config.plan.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/PLAN_MANAGEMENT/POST_PLAN_MANAGEMENT_DELETE_PLAN'),
    ('finance.ledger.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/REVENUE/GET_REVENUE_ALL_FEE_HISTORY'),
    ('finance.ledger.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/REVENUE/GET_REVENUE_COLLECTION_HISTORY'),
    ('city-operations.ride.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_AGENT_LIST'),
    ('city-operations.ride.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_CALL_COUNT'),
    ('city-operations.ride.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_FARE_BREAK_UP'),
    ('city-operations.ride.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_FLOW_DEBUG'),
    ('city-operations.ride.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_INFO'),
    ('city-operations.ride.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_KAPTURE_LIST'),
    ('city-operations.ride.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_LIST'),
    ('city-operations.ride.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_LIST_V2'),
    ('city-operations.ride.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE/GET_RIDE_NEARBY'),
    ('city-operations.ride.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_CANCEL_MULTIPLE'),
    ('city-operations.ride.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_END_MULTIPLE'),
    ('city-operations.ride.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_ROUTE'),
    ('city-operations.ride.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC'),
    ('city-operations.ride.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC_MULTIPLE'),
    ('finance.adjustment.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/RIDE/POST_RIDE_WAIVER_RIDE_CANCELLATION_PENALTY'),
    ('city-operations.ride.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/SEARCH_TRY/POST_SEARCH_TRY_RECENT'),
    ('city-operations.sos.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/SOS_MEDIA/GET_SOS_MEDIA_SOS_MEDIA'),
    ('city-operations.airport_queue.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/SPECIAL_ZONE_QUEUE/GET_SPECIAL_ZONE_QUEUE_DRIVER_QUEUE_HISTORY'),
    ('city-operations.airport_queue.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/SPECIAL_ZONE_QUEUE/GET_SPECIAL_ZONE_QUEUE_DRIVER_QUEUE_POSITION'),
    ('city-operations.airport_queue.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/SPECIAL_ZONE_QUEUE/GET_SPECIAL_ZONE_QUEUE_QUEUE_STATS'),
    ('city-operations.airport_queue.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/SPECIAL_ZONE_QUEUE/GET_SPECIAL_ZONE_QUEUE_TRIGGER_NOTIFY_STATUS'),
    ('city-operations.airport_queue.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/SPECIAL_ZONE_QUEUE/POST_SPECIAL_ZONE_QUEUE_MANUAL_QUEUE_ADD'),
    ('city-operations.airport_queue.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/SPECIAL_ZONE_QUEUE/POST_SPECIAL_ZONE_QUEUE_MANUAL_QUEUE_REMOVE'),
    ('city-operations.airport_queue.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/SPECIAL_ZONE_QUEUE/POST_SPECIAL_ZONE_QUEUE_TRIGGER_NOTIFY'),
    ('admin.query.execute', 'DASHBOARD', 'PROVIDER_MANAGEMENT/SYSTEM/POST_SYSTEM_RUN_QUERY'),
    ('city-operations.vehicle.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/VEHICLE/GET_VEHICLE_LIST'),
    ('city-operations.vehicle.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/VEHICLE_DETAILS/GET_VEHICLE_DETAILS_VEHICLE_MODELS'),
    ('city-operations.vehicle.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/VEHICLE_INFO/GET_VEHICLE_INFO_LIST'),
    ('city-operations.vehicle.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/VEHICLE_INFO/POST_VEHICLE_INFO_UPDATE'),
    ('city-operations.volunteer.read', 'DASHBOARD', 'PROVIDER_MANAGEMENT/VOLUNTEER/GET_VOLUNTEER_LIST'),
    ('city-operations.volunteer.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/VOLUNTEER/POST_VOLUNTEER_CREATE'),
    ('city-operations.volunteer.write', 'DASHBOARD', 'PROVIDER_MANAGEMENT/VOLUNTEER/POST_VOLUNTEER_UPDATE'),
    ('fleet.operator.read', 'DASHBOARD', 'PROVIDER_OPERATOR/DRIVER/GET_DRIVER_OPERATION_GET_ALL_HUBS'),
    ('fleet.operator.read', 'DASHBOARD', 'PROVIDER_OPERATOR/DRIVER/GET_DRIVER_OPERATOR_DASHBOARD_ANALYTICS'),
    ('fleet.operator.read', 'DASHBOARD', 'PROVIDER_OPERATOR/DRIVER/GET_DRIVER_OPERATOR_DASHBOARD_ANALYTICS_ALL_TIME'),
    ('fleet.operator.read', 'DASHBOARD', 'PROVIDER_OPERATOR/DRIVER/GET_DRIVER_OPERATOR_FETCH_HUB_REQUESTS'),
    ('fleet.operator.read', 'DASHBOARD', 'PROVIDER_OPERATOR/DRIVER/GET_DRIVER_OPERATOR_LIST'),
    ('fleet.operator.read', 'DASHBOARD', 'PROVIDER_OPERATOR/DRIVER/GET_DRIVER_REQUEST_REVIEW_HISTORY'),
    ('fleet.operator.read', 'DASHBOARD', 'PROVIDER_OPERATOR/DRIVER/GET_DRIVER_REVIEW_QUEUE_REQUEST'),
    ('fleet.operator.write', 'DASHBOARD', 'PROVIDER_OPERATOR/DRIVER/POST_DRIVER_OPERATOR_CREATE_REQUEST'),
    ('fleet.operator.write', 'DASHBOARD', 'PROVIDER_OPERATOR/DRIVER/POST_DRIVER_OPERATOR_RESPOND_HUB_REQUEST'),
    ('fleet.operator.write', 'DASHBOARD', 'PROVIDER_OPERATOR/DRIVER/POST_DRIVER_OPERATOR_SEND_JOINING_OTP'),
    ('fleet.operator.write', 'DASHBOARD', 'PROVIDER_OPERATOR/DRIVER/POST_DRIVER_OPERATOR_VERIFY_JOINING_OTP'),
    ('fleet.operator.write', 'DASHBOARD', 'PROVIDER_OPERATOR/DRIVER/POST_DRIVER_SUBMIT_REVIEW_REQUEST'),
    ('fleet.operator.read', 'DASHBOARD', 'PROVIDER_OPERATOR/FLEET_MANAGEMENT/GET_FLEET_MANAGEMENT_FLEETS'),
    ('fleet.operator.write', 'DASHBOARD', 'PROVIDER_OPERATOR/FLEET_MANAGEMENT/POST_FLEET_MANAGEMENT_FLEET_CREATE'),
    ('fleet.operator.write', 'DASHBOARD', 'PROVIDER_OPERATOR/FLEET_MANAGEMENT/POST_FLEET_MANAGEMENT_FLEET_LINK_SEND_OTP'),
    ('fleet.operator.write', 'DASHBOARD', 'PROVIDER_OPERATOR/FLEET_MANAGEMENT/POST_FLEET_MANAGEMENT_FLEET_LINK_VERIFY_OTP'),
    ('fleet.operator.write', 'DASHBOARD', 'PROVIDER_OPERATOR/FLEET_MANAGEMENT/POST_FLEET_MANAGEMENT_FLEET_MEMBER_ASSOCIATION_CREATE'),
    ('fleet.operator.write', 'DASHBOARD', 'PROVIDER_OPERATOR/FLEET_MANAGEMENT/POST_FLEET_MANAGEMENT_FLEET_REGISTER'),
    ('fleet.operator.write', 'DASHBOARD', 'PROVIDER_OPERATOR/FLEET_MANAGEMENT/POST_FLEET_MANAGEMENT_FLEET_UNLINK'),
    ('fleet.operator.write', 'DASHBOARD', 'PROVIDER_OPERATOR/REGISTRATION/POST_OPERATOR_REGISTER'),
    ('fleet.operator.write', 'DASHBOARD', 'PROVIDER_OPERATOR/REGISTRATION/POST_REGISTRATION_DASHBOARD_REGISTER'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/DRIVER/GET_DRIVER_FEEDBACK_LIST'),
    ('city-operations.driver.read', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/DRIVER/GET_DRIVER_INFO'),
    ('city-operations.subscription.write', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/DRIVER/GET_DRIVER_PAYMENT_DUE'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_ADD_VEHICLE'),
    ('city-operations.subscription.write', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_COLLECT_CASH'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_DELETE_AADHAAR'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_DELETE_PAN_CARD'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_ENABLE'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_END_RC_ASSOCIATION'),
    ('finance.adjustment.write', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_EXEMPT_CASH'),
    ('finance.adjustment.write', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_EXEMPT_DRIVER_FEE'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_SET_RC_STATUS'),
    ('city-operations.driver.write', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_UNLINK_VEHICLE'),
    ('city-operations.subscription.write', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_V2_COLLECT_CASH'),
    ('finance.adjustment.write', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/DRIVER/POST_DRIVER_V2_EXEMPT_CASH'),
    ('city-operations.booth_customer.write', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_AUTH'),
    ('city-operations.booth_customer.write', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/DRIVER_REGISTRATION/POST_DRIVER_REGISTRATION_VERIFY'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/MAPS/POST_MAPS_AUTO_COMPLETE'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/MAPS/POST_MAPS_GET_PLACE_NAME'),
    ('city-operations.ride.read', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/RIDE/GET_RIDE_CURRENT_ACTIVE_RIDE'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_BOOKING_WITH_VEHICLE_NUMBER_AND_PHONE'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_CANCEL'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_END'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/RIDE/POST_RIDE_START'),
    ('city-operations.ride.read', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/SEARCH_REQUEST/GET_SEARCH_REQUEST_INFO'),
    ('city-operations.ride.read', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/SEARCH_REQUEST/GET_SEARCH_REQUEST_LIST'),
    ('city-operations.ride.read', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/SEARCH_REQUEST/POST_SEARCH_REQUEST_SEARCHREQUESTS'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/VOLUNTEER/GET_VOLUNTEER_BOOKING'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'PROVIDER_RIDE_BOOKING/VOLUNTEER/POST_VOLUNTEER_ASSIGN_START_OTP_RIDE'),
    ('city-operations.customer.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/CUSTOMER/DELETE_CUSTOMER_SAVED_LOCATIONS'),
    ('city-operations.customer.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/CUSTOMER/GET_CUSTOMER_SAVED_LOCATIONS'),
    ('city-operations.customer.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/CUSTOMER/POST_CUSTOMER_DELETED_PERSON'),
    ('city-operations.customer.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/CUSTOMER/POST_CUSTOMER_SAVED_LOCATIONS'),
    ('city-operations.customer.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/CUSTOMER/POST_CUSTOMER_SOS_CREATE'),
    ('city-operations.edc.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EDC_MACHINE/ASSIGN_EDC_MACHINE'),
    ('city-operations.edc.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EDC_MACHINE/DELETE_EDC_MACHINE'),
    ('city-operations.edc.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EDC_MACHINE/LIST_EDC_MACHINE'),
    ('city-operations.edc.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EDC_MACHINE/UPDATE_EDC_MACHINE'),
    ('city-operations.ticket_place.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/GET_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_DEF'),
    ('city-operations.ticket_place.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/GET_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_DRAFTS'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_CANCEL_SUBMIT_DRAFT'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_CLEARDRAFT'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_CREATE'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_DEL_SERVICE'),
    ('city-operations.ticket_place.approve', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_RECOMMEND'),
    ('city-operations.ticket_place.approve', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_REVIEW_DRAFT'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_SERVICE_DEL_CATEGORY'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_SERVICE_UPDATE_CATEGORY'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_SUBMIT_DRAFT'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_UPDATE_BASIC_INFO'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKETPLACE_UPDATE_SERVICE'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKET_PLACE_CATEGORY_DEL_PEOPLE'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/EVENT_MANAGEMENT/POST_EVENT_MANAGEMENT_TICKETDASHBOARD_TICKET_PLACE_CATEGORY_UPDATE_PEOPLE'),
    ('city-operations.frfs.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_AUTOCOMPLETE'),
    ('city-operations.frfs.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_BOOKING_PAYMENT_ATTEMPTS'),
    ('city-operations.frfs.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_BOOKING_STATUS'),
    ('city-operations.frfs.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_CONFIG'),
    ('city-operations.frfs.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_PAYMENT_ATTEMPTS'),
    ('city-operations.frfs.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_ROUTE'),
    ('city-operations.frfs.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_ROUTES'),
    ('city-operations.frfs.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_ROUTE_SEAT_LAYOUT'),
    ('city-operations.frfs.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_SEARCH_QUOTE'),
    ('city-operations.frfs.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_STATIONS'),
    ('city-operations.frfs.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/GET_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_TRIP_ROUTE_SEATS'),
    ('transit-operations.trip.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_FLEET_OPERATOR_CURRENT_OPERATION'),
    ('transit-operations.trip.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_FLEET_OPERATOR_TRIP_ACTION'),
    ('city-operations.frfs.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_QUOTE_V2_CONFIRM'),
    ('city-operations.frfs.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_ROUTE_SERVICEABILITY'),
    ('city-operations.frfs.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_SEARCH'),
    ('city-operations.frfs.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/FRFS_TICKET_SERVICE/POST_FRFS_TICKET_SERVICE_CUSTOMER_FRFS_STATIONS_POSSIBLE_STOPS'),
    ('city-config.merchant_onboarding.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOADING_LIST_ALL'),
    ('city-config.merchant_onboarding.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_CANCEL'),
    ('city-config.merchant_onboarding.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_GET_FILE'),
    ('city-config.merchant_onboarding.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_INFO'),
    ('city-config.merchant_onboarding.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_LIST'),
    ('city-config.merchant_onboarding.approve', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_REJECT'),
    ('city-config.merchant_onboarding.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_START'),
    ('city-config.merchant_onboarding.approve', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_APPROVE'),
    ('city-config.merchant_onboarding.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_LIST'),
    ('city-config.merchant_onboarding.approve', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_REJECT'),
    ('city-config.merchant_onboarding.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_SUBMIT'),
    ('city-config.merchant_onboarding.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_UPDATE_PAYLOAD'),
    ('city-config.merchant_onboarding.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/MERCHANT_ONBOARDING/MERCHANT_ONBOARDING_STEP_UPLOAD_FILE'),
    ('city-operations.pass.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/GET_PASS_CUSTOMER_AVAILABLE_PASSES'),
    ('city-operations.pass.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/GET_PASS_CUSTOMER_PASS_PHOTO'),
    ('city-operations.pass.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/GET_PASS_CUSTOMER_PAYMENT_STATUS'),
    ('city-operations.pass.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/GET_PASS_CUSTOMER_PURCHASED_PASSES'),
    ('city-operations.pass.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/GET_PASS_CUSTOMER_TRANSACTIONS'),
    ('city-operations.pass.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_ACTIVATE_TODAY'),
    ('city-operations.pass.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_PASS_RESET_DEVICE_SWITCH_COUNT'),
    ('city-operations.pass.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_PASS_RESTORE'),
    ('city-operations.pass.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_PASS_SELECT'),
    ('city-operations.pass.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS/POST_PASS_CUSTOMER_PASS_UPDATE_PROFILE_PICTURE'),
    ('admin.crypto.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASSETTO/POST_PASSETTO_DECRYPT'),
    ('admin.crypto.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASSETTO/POST_PASSETTO_ENCRYPT'),
    ('city-operations.pass_org.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS_ORGANIZATION/GET_PASS_ORGANIZATION_GET_ORGANIZATIONS'),
    ('city-operations.pass_org.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS_ORGANIZATION/GET_PASS_ORGANIZATION_GET_PASS_ORGANIZATION'),
    ('city-operations.pass_org.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS_ORGANIZATION/GET_PASS_ORGANIZATION_PASS_DETAILS'),
    ('city-operations.pass_org.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS_ORGANIZATION/GET_PASS_ORGANIZATION_PASS_DETAILS_DEPOT'),
    ('city-operations.pass_org.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS_ORGANIZATION/GET_PASS_ORGANIZATION_PASS_DETAILS_DOCUMENT'),
    ('city-operations.pass_org.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS_ORGANIZATION/POST_PASS_ORGANIZATION_ASSIGN_DEPOT'),
    ('city-operations.pass_org.approve', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS_ORGANIZATION/POST_PASS_ORGANIZATION_PASS_DETAILS_VERIFY'),
    ('city-operations.pass_org.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PASS_ORGANIZATION/POST_PASS_ORGANIZATION_UPDATE'),
    ('city-operations.ride.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PAYMENT/GET_PAYMENT_FARE_BREAKUP'),
    ('city-operations.payment.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PAYMENT/GET_PAYMENT_REFUND_REQUEST_INFO'),
    ('city-operations.payment.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PAYMENT/GET_PAYMENT_REFUND_REQUEST_LIST'),
    ('city-operations.payment.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PAYMENT/POST_PAYMENT_REFUND_REQUEST_INITIATE'),
    ('city-operations.payment.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/PAYMENT/POST_PAYMENT_REFUND_REQUEST_RESPOND'),
    ('transit-config.seat_layout.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/SEAT_LAYOUT/DELETE_SEAT_LAYOUT'),
    ('transit-config.seat_layout.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/SEAT_LAYOUT/GET_SEAT_LAYOUT'),
    ('transit-config.seat_layout.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/SEAT_LAYOUT/LIST_SEAT_LAYOUT'),
    ('transit-config.seat_layout.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/SEAT_LAYOUT/UPSERT_SEAT_LAYOUT'),
    ('transit-config.stops.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/STOP_ROUTE_DETAILS/STOP_ROUTE_DETAILS_GET_ROUTE_STOP_MAPPING_BY_ROUTE'),
    ('transit-config.stops.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/STOP_ROUTE_DETAILS/STOP_ROUTE_DETAILS_GET_ROUTE_STOP_MAPPING_BY_STOP'),
    ('transit-config.stops.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/STOP_ROUTE_DETAILS/STOP_ROUTE_DETAILS_GET_STOP'),
    ('transit-config.stops.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/STOP_ROUTE_DETAILS/STOP_ROUTE_DETAILS_GET_STOPS'),
    ('city-operations.ticket_booking.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/GET_ALL_TICKET_BOOKINGS'),
    ('city-operations.ticket_booking.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_BOOKING_DETAILS'),
    ('city-operations.ticket_booking.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_DASHBOARD_BOOKING_STATUS'),
    ('city-operations.ticket.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_PLACES'),
    ('city-operations.ticket_place.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_AGREEMENT'),
    ('city-operations.ticket_place.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_FILE'),
    ('city-operations.ticket_place.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_TICKETPLACES'),
    ('city-operations.ticket_place.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_TICKETPLACE_INFO'),
    ('city-operations.ticket_place.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_TICKETPLACE_SUB_PLACES'),
    ('city-operations.ticket_place.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKETS_TICKETDASHBOARD_USER_INFO'),
    ('city-operations.ticket_booking.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKET_BOOKING_DETAILS'),
    ('city-operations.ticket.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKET_FLEET_VEHICLES'),
    ('city-operations.ticket.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKET_FLEET_VEHICLES_V2'),
    ('city-operations.ticket.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKET_PLACES'),
    ('city-operations.ticket_booking.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKET_PLACE_BOOKINGS'),
    ('city-operations.ticket.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/GET_TICKET_PLACE_SERVICES'),
    ('city-operations.ticket.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_BOOKINGS_CANCEL'),
    ('city-operations.ticket.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_SERVICES'),
    ('city-operations.ticket.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_SERVICE_CANCEL'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_TICKETPLACE_UPDATE'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_TICKETDASHBOARD_TICKETPLACE_UPDATE_SUB_PLACES'),
    ('city-operations.ticket.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_UPDATE'),
    ('city-operations.ticket.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKETS_VERIFY'),
    ('city-operations.ticket.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKET_BOOKINGS_VERIFY_V2'),
    ('city-operations.ticket.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKET_BOOKING_CASH_COLLECT'),
    ('city-operations.ticket.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKET_PLACES_BOOK'),
    ('city-operations.ticket.execute', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKETS/POST_TICKET_PLACES_DIRECT_BOOK'),
    ('city-operations.ticket_place.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_CURRENT_SEAT_STATUS'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_DELETE_ASSET'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_SEAT_MANAGEMENT'),
    ('city-operations.ticket_place.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TICKET_DASHBOARD/TICKET_DASHBOARD_UPLOAD_ASSET'),
    ('transit-config.stops.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_BULK_REPLACE_STOPS'),
    ('transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_DELETE_ROW'),
    ('transit-config.gtfs.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_EXPORT_ROUTE_STOP_MAPPING'),
    ('transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_ALL_ROWS'),
    ('transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_BREAK_TYPES'),
    ('transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_CONDUCTOR'),
    ('transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_DAY_TYPES'),
    ('transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_DEPOTS'),
    ('transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_DEVICE_IDS'),
    ('transit-operations.device.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_DEVICE_VEHICLE_MAPPING_LIST'),
    ('transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_DRIVER'),
    ('transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_FLEETS'),
    ('transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_OPERATORS'),
    ('transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_ROUTES'),
    ('transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_ROW'),
    ('transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_SCHEDULE_NUMBERS'),
    ('transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_SERVICE_TYPES'),
    ('transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_SHIFT_TYPES'),
    ('transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_TABLET_IDS'),
    ('transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_TRIP_DETAILS'),
    ('transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_TRIP_TYPES'),
    ('transit-operations.waybill.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_GET_WAYBILLS'),
    ('transit-config.stops.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_INSERT_ROUTE_STOP'),
    ('transit-config.stops.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_NEARBY_STOPS'),
    ('transit-operations.master.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_QUERY_ROWS'),
    ('transit-config.stops.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_REPROCESS_ROUTES'),
    ('transit-config.stops.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_ROUTE_STOPS'),
    ('transit-config.stops.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_SEARCH_STOPS'),
    ('transit-config.stops.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UNBLOCK_BUS'),
    ('transit-operations.waybill.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPDATE_WAYBILL_DETAILS'),
    ('transit-operations.waybill.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPDATE_WAYBILL_FLEET'),
    ('transit-operations.waybill.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPDATE_WAYBILL_STATUS'),
    ('transit-operations.waybill.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPDATE_WAYBILL_TABLET'),
    ('transit-operations.device.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPSERT_DEVICE_VEHICLE_MAPPING'),
    ('transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPSERT_ROW'),
    ('transit-operations.master.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/TRANSIT_OPERATOR/TRANSIT_OPERATOR_UPSERT_ROWS'),
    ('transit-config.seat_layout.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/VEHICLE_SEAT_LAYOUT_MAPPING/DELETE_VEHICLE_SEAT_LAYOUT_MAPPING'),
    ('transit-config.seat_layout.read', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/VEHICLE_SEAT_LAYOUT_MAPPING/LIST_VEHICLE_SEAT_LAYOUT_MAPPING'),
    ('transit-config.seat_layout.write', 'DASHBOARD', 'RIDER_APP_MANAGEMENT/VEHICLE_SEAT_LAYOUT_MAPPING/UPSERT_VEHICLE_SEAT_LAYOUT_MAPPING'),
    ('system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/DELETE_ISSUE_CATEGORY'),
    ('system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/DELETE_ISSUE_MESSAGE'),
    ('system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/DELETE_ISSUE_OPTION'),
    ('system-config.customer_issue_config.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CATEGORY_DETAIL'),
    ('system-config.customer_issue_config.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CATEGORY_FLOW_PREVIEW'),
    ('system-config.customer_issue_config.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CATEGORY_LIST'),
    ('city-operations.customer_issue.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CHAT_MESSAGES'),
    ('system-config.customer_issue_config.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_CONFIG'),
    ('city-operations.customer_issue.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_INFO'),
    ('city-operations.customer_issue.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_INFO_V2'),
    ('city-operations.customer_issue.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_LIST'),
    ('city-operations.customer_issue.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_MEDIA'),
    ('system-config.customer_issue_config.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_MESSAGE_DETAIL'),
    ('system-config.customer_issue_config.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_MESSAGE_LIST'),
    ('system-config.customer_issue_config.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_OPTION_DETAIL'),
    ('system-config.customer_issue_config.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_OPTION_LIST'),
    ('system-config.customer_issue_config.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/GET_ISSUE_TRANSLATIONS'),
    ('system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_BULK_UPSERT_TRANSLATIONS'),
    ('system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_ALL_COPY'),
    ('system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_COPY'),
    ('system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_CREATE'),
    ('system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_DEFAULT_COPY'),
    ('system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_REORDER'),
    ('system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CATEGORY_UPDATE'),
    ('city-operations.customer_issue.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CHAT_MESSAGE'),
    ('city-operations.customer_issue.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CHAT_READ'),
    ('city-operations.customer_issue.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CHAT_UPLOAD'),
    ('city-operations.customer_issue.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_COMMENT'),
    ('system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_CONFIG_UPDATE'),
    ('city-operations.customer_issue.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_KAPTURE_CREATE'),
    ('system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_MESSAGE_REORDER'),
    ('system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_MESSAGE_UPSERT'),
    ('system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_CREATE'),
    ('system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_REORDER'),
    ('system-config.customer_issue_config.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_OPTION_UPDATE'),
    ('city-operations.customer_issue.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/POST_ISSUE_TICKET_STATUS_CALL_BACK'),
    ('city-operations.customer_issue.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE/PUT_ISSUE_UPDATE'),
    ('city-operations.customer_issue.read', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE_LIST/GET_ISSUE_LIST_V1'),
    ('city-operations.customer_issue.write', 'DASHBOARD', 'RIDER_ISSUE_MANAGEMENT/ISSUE_LIST/POST_ISSUE_LIST_TICKET_STATUS_CALL_BACK'),
    ('city-operations.sos.read', 'DASHBOARD', 'RIDER_MANAGEMENT/ALERT_INCIDENT/GET_ALERT_INCIDENT_ALERTS_INCIDENTS'),
    ('city-operations.ride.write', 'DASHBOARD', 'RIDER_MANAGEMENT/BOOKING/POST_BOOKING_CANCEL_ALL_STUCK'),
    ('city-operations.ride.write', 'DASHBOARD', 'RIDER_MANAGEMENT/BOOKING/POST_BOOKING_SYNC_MULTIPLE'),
    ('city-operations.customer.write', 'DASHBOARD', 'RIDER_MANAGEMENT/CUSTOMER/DELETE_CUSTOMER_DELETE'),
    ('city-operations.customer.read', 'DASHBOARD', 'RIDER_MANAGEMENT/CUSTOMER/GET_CUSTOMER_CANCELLATION_DUES_DETAILS'),
    ('city-operations.customer.read', 'DASHBOARD', 'RIDER_MANAGEMENT/CUSTOMER/GET_CUSTOMER_INFO'),
    ('city-operations.customer.read', 'DASHBOARD', 'RIDER_MANAGEMENT/CUSTOMER/GET_CUSTOMER_LIST'),
    ('city-operations.customer.write', 'DASHBOARD', 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_APPLY_OFFER'),
    ('city-operations.customer.write', 'DASHBOARD', 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_BLOCK'),
    ('city-operations.customer.write', 'DASHBOARD', 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_BULK_APPLY_OFFER'),
    ('city-operations.customer.write', 'DASHBOARD', 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_ENSURE_EXISTS'),
    ('city-operations.customer.write', 'DASHBOARD', 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_OFFERS_LIST'),
    ('city-operations.pii.read', 'DASHBOARD', 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_PERSON_ID'),
    ('city-operations.pii.read', 'DASHBOARD', 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_PERSON_NUMBERS'),
    ('city-operations.customer.write', 'DASHBOARD', 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_UNBLOCK'),
    ('city-operations.customer.write', 'DASHBOARD', 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_UPDATE_PAYMENT_MODE'),
    ('city-operations.customer.write', 'DASHBOARD', 'RIDER_MANAGEMENT/CUSTOMER/POST_CUSTOMER_UPDATE_SAFETY_CENTER_BLOCKING'),
    ('transit-operations.device.read', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_ALERTS/GET_FRFS_ALERTS_FRFS_LIVE_METRICS'),
    ('transit-config.gtfs.read', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_TICKET/GET_FRFS_TICKET_FRFS_GTFS'),
    ('transit-config.gtfs.read', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_TICKET/GET_FRFS_TICKET_FRFS_ROUTES'),
    ('transit-config.fare.read', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_TICKET/GET_FRFS_TICKET_FRFS_ROUTE_FARE_LIST'),
    ('transit-config.gtfs.read', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_TICKET/GET_FRFS_TICKET_FRFS_ROUTE_STATIONS'),
    ('transit-config.fare.write', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_TICKET/POST_FRFS_TICKET_FRFS_STATUS_UPDATE'),
    ('transit-config.fare.write', 'DASHBOARD', 'RIDER_MANAGEMENT/FRFS_TICKET/PUT_FRFS_TICKET_FRFS_ROUTE_FARE_UPSERT'),
    ('finance.invoice.read', 'DASHBOARD', 'RIDER_MANAGEMENT/INVOICE/GET_INVOICE_INVOICE'),
    ('city-operations.pii.read', 'DASHBOARD', 'RIDER_MANAGEMENT/MEDIA/GET_MEDIA_FILE'),
    ('communication.message.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_MERCHANT_MESSAGE'),
    ('city-config.geo.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_DELETE'),
    ('city-config.geo.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_SPECIAL_LOCATION_GATES_DELETE'),
    ('city-config.geo.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/DELETE_MERCHANT_TOLL_DELETE'),
    ('city-config.geo.read', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_GEOMETRY_LIST'),
    ('city-config.geo.read', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_SPECIAL_LOCATION_LIST'),
    ('city-config.geo.read', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_CONFIG_TOLL_LIST'),
    ('communication.message.read', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_MERCHANT_MESSAGE_CATALOG'),
    ('system-config.merchant.read', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_RIDER_CONFIG_ESTIMATES_ORDER'),
    ('system-config.merchant.read', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/GET_MERCHANT_SERVICE_USAGE_CONFIG'),
    ('system-config.merchant.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_DEBUG_LOG_UPDATE'),
    ('system-config.failover.execute', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_FAILOVER'),
    ('city-config.launch.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_MERCHANT_CREATE'),
    ('city-config.launch.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_OPERATING_CITY_CREATE'),
    ('city-config.launch.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_OPERATING_CITY_WHITE_LIST'),
    ('city-config.geo.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_SPECIAL_LOCATION_UPSERT'),
    ('city-config.geo.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_CONFIG_TOLL_UPSERT'),
    ('communication.message.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_MERCHANT_MESSAGE_UPSERT'),
    ('system-config.merchant.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_RIDER_CONFIG_ESTIMATES_ORDER_UPDATE'),
    ('system-config.scheduler.execute', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SCHEDULER_TRIGGER'),
    ('system-config.merchant.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_MAPS_UPDATE'),
    ('system-config.merchant.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_CONFIG_SMS_UPDATE'),
    ('system-config.merchant.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_MAPS_UPDATE'),
    ('system-config.merchant.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SERVICE_USAGE_CONFIG_SMS_UPDATE'),
    ('city-config.geo.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_GATES_UPSERT'),
    ('city-config.geo.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_SPECIAL_LOCATION_UPSERT'),
    ('system-config.merchant.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_TICKET_CONFIG_UPSERT'),
    ('city-config.geo.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_TOLL_UPSERT'),
    ('system-config.merchant.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/POST_MERCHANT_UPDATE'),
    ('city-config.geo.write', 'DASHBOARD', 'RIDER_MANAGEMENT/MERCHANT/PUT_MERCHANT_CONFIG_GEOMETRY_UPDATE'),
    ('system-config.namma_tag.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/DELETE_NAMMA_TAG_QUERY_DELETE'),
    ('system-config.namma_tag.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/DELETE_NAMMA_TAG_TAG_DELETE'),
    ('system-config.namma_tag.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/DELETE_NAMMA_TAG_TIME_BOUNDS_DELETE'),
    ('system-config.dynamic_logic.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC'),
    ('system-config.dynamic_logic.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_DOMAINS'),
    ('system-config.dynamic_logic.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_DOMAINS_AND_EVENTS'),
    ('system-config.dynamic_logic.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_GET_DOMAIN_SCHEMA'),
    ('system-config.dynamic_logic.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_GET_LOGIC_ROLLOUT'),
    ('system-config.dynamic_logic.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_APP_DYNAMIC_LOGIC_VERSIONS'),
    ('system-config.config_pilot.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_ALL_CONFIGS'),
    ('system-config.config_pilot.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_ALL_UI_CONFIGS'),
    ('system-config.config_pilot.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_ALWAYS_ON_LIST'),
    ('system-config.config_pilot.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_CONFIG_DETAILS'),
    ('system-config.config_pilot.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_GET_DIMENSION_SCHEMA'),
    ('system-config.config_pilot.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_GET_TABLE_DATA'),
    ('system-config.config_pilot.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_GET_UI_TABLE_DATA'),
    ('system-config.config_pilot.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_CONFIG_PILOT_UI_CONFIG_DETAILS'),
    ('system-config.namma_tag.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_QUERY_ALL'),
    ('system-config.namma_tag.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_QUERY_DETAILS'),
    ('system-config.namma_tag.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_TAG_ALL'),
    ('system-config.namma_tag.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_TAG_DETAILS'),
    ('system-config.namma_tag.read', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/GET_NAMMA_TAG_TIME_BOUNDS'),
    ('system-config.dynamic_logic.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_UPSERT_LOGIC_ROLLOUT'),
    ('system-config.dynamic_logic.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_APP_DYNAMIC_LOGIC_VERIFY'),
    ('system-config.config_pilot.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_ACTION_CHANGE'),
    ('system-config.config_pilot.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_CREATE_ROW'),
    ('system-config.config_pilot.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_CREATE_UI_CONFIG'),
    ('system-config.config_pilot.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_GET_CONFIG'),
    ('system-config.config_pilot.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_GET_CONFIG_WITH_DIMENSIONS'),
    ('system-config.config_pilot.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_CONFIG_PILOT_GET_VERSION'),
    ('system-config.namma_tag.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_QUERY_CREATE'),
    ('system-config.namma_tag.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_QUERY_UPDATE'),
    ('system-config.dynamic_logic.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_RUN_JOB'),
    ('system-config.namma_tag.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TAG_CREATE'),
    ('system-config.namma_tag.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TAG_UPDATE'),
    ('system-config.namma_tag.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TAG_VERIFY'),
    ('system-config.namma_tag.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_TIME_BOUNDS_CREATE'),
    ('system-config.namma_tag.write', 'DASHBOARD', 'RIDER_MANAGEMENT/NAMMA_TAG/POST_NAMMA_TAG_UPDATE_CUSTOMER_TAG'),
    ('city-config.offer.read', 'DASHBOARD', 'RIDER_MANAGEMENT/OFFER/GET_OFFER_ELIGIBILITY_SCHEMA'),
    ('city-config.offer.read', 'DASHBOARD', 'RIDER_MANAGEMENT/OFFER/GET_OFFER_LIST'),
    ('city-config.offer.write', 'DASHBOARD', 'RIDER_MANAGEMENT/OFFER/POST_OFFER_CREATE'),
    ('city-config.offer.write', 'DASHBOARD', 'RIDER_MANAGEMENT/OFFER/POST_OFFER_TOGGLE'),
    ('city-config.offer.write', 'DASHBOARD', 'RIDER_MANAGEMENT/OFFER/POST_OFFER_UPDATE'),
    ('city-config.offer.write', 'DASHBOARD', 'RIDER_MANAGEMENT/OFFER/POST_OFFER_VALIDATE_ELIGIBILITY'),
    ('finance.payout.read', 'DASHBOARD', 'RIDER_MANAGEMENT/PAYOUT/GET_PAYOUT_PAYOUT_ORDER'),
    ('system-config.rewards.read', 'DASHBOARD', 'RIDER_MANAGEMENT/REWARDS/GET_REWARDS_CAMPAIGN'),
    ('system-config.rewards.read', 'DASHBOARD', 'RIDER_MANAGEMENT/REWARDS/GET_REWARDS_CAMPAIGNS'),
    ('system-config.rewards.read', 'DASHBOARD', 'RIDER_MANAGEMENT/REWARDS/GET_REWARDS_CAMPAIGN_STATS'),
    ('system-config.rewards.write', 'DASHBOARD', 'RIDER_MANAGEMENT/REWARDS/POST_REWARDS_CAMPAIGN'),
    ('system-config.rewards.write', 'DASHBOARD', 'RIDER_MANAGEMENT/REWARDS/POST_REWARDS_CAMPAIGN_COHORT'),
    ('system-config.rewards.write', 'DASHBOARD', 'RIDER_MANAGEMENT/REWARDS/POST_REWARDS_CAMPAIGN_COHORT_CODES'),
    ('system-config.rewards.write', 'DASHBOARD', 'RIDER_MANAGEMENT/REWARDS/POST_REWARDS_CAMPAIGN_STATUS'),
    ('system-config.rewards.write', 'DASHBOARD', 'RIDER_MANAGEMENT/REWARDS/POST_REWARDS_COHORT_VALIDATE_ELIGIBILITY'),
    ('system-config.rewards.write', 'DASHBOARD', 'RIDER_MANAGEMENT/REWARDS/POST_REWARDS_TRIGGER_EVAL'),
    ('system-config.rewards.write', 'DASHBOARD', 'RIDER_MANAGEMENT/REWARDS/PUT_REWARDS_CAMPAIGN'),
    ('system-config.rewards.write', 'DASHBOARD', 'RIDER_MANAGEMENT/REWARDS/PUT_REWARDS_CAMPAIGN_COHORT'),
    ('finance.adjustment.write', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE/CANCELLATION_CHARGES_WAIVE_OFF'),
    ('city-operations.ride.read', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE/GET_RIDE_FLOW_DEBUG_BAP'),
    ('city-operations.ride.read', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE/GET_RIDE_INFO'),
    ('city-operations.ride.read', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE/GET_RIDE_KAPTURE_LIST'),
    ('city-operations.ride.read', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE/GET_RIDE_LIST'),
    ('city-operations.ride.write', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE/POST_RIDE_CANCEL_MULTIPLE'),
    ('city-operations.ride.write', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE/POST_RIDE_PAYOUT_OFFER_SYNC'),
    ('city-operations.ride.write', 'DASHBOARD', 'RIDER_MANAGEMENT/RIDE/POST_RIDE_SYNC_MULTIPLE'),
    ('city-operations.ride.read', 'DASHBOARD', 'RIDER_MANAGEMENT/SEARCH_TRY/POST_SEARCH_TRY_RECENT_SEARCH_TRIES'),
    ('city-operations.sos.write', 'DASHBOARD', 'RIDER_MANAGEMENT/SOS/POST_SOS_CALL_EXTERNAL_SOS'),
    ('city-operations.sos.write', 'DASHBOARD', 'RIDER_MANAGEMENT/SOS/POST_SOS_ERSS_STATUS_UPDATE'),
    ('city-operations.sos.read', 'DASHBOARD', 'RIDER_MANAGEMENT/SOS_MEDIA/GET_SOS_MEDIA_SOS_MEDIA'),
    ('admin.query.execute', 'DASHBOARD', 'RIDER_MANAGEMENT/SYSTEM/POST_SYSTEM_RUN_QUERY'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/ADD_BAGGAGE/POST_ADD_BAGGAGE_CONFIRM'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/BOOKING/GET_BOOKING_AGENT_L1_LIST'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/BOOKING/GET_BOOKING_AGENT_L2_LIST'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/BOOKING/GET_BOOKING_LIST'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/BOOKING/POST_BOOKING_STATUS'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/CANCEL/POST_CANCEL_BOOKING'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/CHANGE_SERVICE_TIER/GET_CHANGE_SERVICE_TIER_QUOTES'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/CHANGE_SERVICE_TIER/POST_CHANGE_SERVICE_TIER_CONFIRM'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/CONFIRM/POST_CONFIRM_RIDE_SEARCH_QUOTES'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/FRONTEND/GET_FRONTEND_FLOW_STATUS'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/FRONTEND/POST_FRONTEND_NOTIFY_EVENT'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/MAPS/POST_MAPS_AUTO_COMPLETE'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/MAPS/POST_MAPS_GET_PLACE_DETAILS'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/MAPS/POST_MAPS_GET_PLACE_NAME'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/MULTI_MODAL/GET_MULTI_MODAL_GET_COMMENTS'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/MULTI_MODAL/GET_MULTI_MODAL_LIST'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/MULTI_MODAL/POST_MULTI_MODAL_ADD_COMMENT'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/MULTI_MODAL/POST_MULTI_MODAL_SEND_DIRECT_MESSAGE'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/MULTI_MODAL/POST_MULTI_MODAL_SEND_MESSAGE'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/NOTIFY_RIDE_INFO/POST_NOTIFY_RIDE_INFO_NOTIFY_RIDE_INFO'),
    ('city-operations.booth_customer.write', 'DASHBOARD', 'RIDER_RIDE_BOOKING/PROFILE/GET_PROFILE_DETAIL'),
    ('city-operations.booth_customer.write', 'DASHBOARD', 'RIDER_RIDE_BOOKING/PROFILE/POST_PROFILE_UPDATE'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/QUOTE/GET_QUOTE_RESULT'),
    ('city-operations.booth_customer.write', 'DASHBOARD', 'RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_AUTH'),
    ('city-operations.booth_customer.write', 'DASHBOARD', 'RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_LOGOUT'),
    ('city-operations.booth_customer.write', 'DASHBOARD', 'RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_OTP_RESEND'),
    ('city-operations.booth_customer.write', 'DASHBOARD', 'RIDER_RIDE_BOOKING/REGISTRATION/POST_REGISTRATION_VERIFY'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/SEARCH/POST_SEARCH_RIDE'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/SELECT/GET_SELECT_QUOTES'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/SELECT/GET_SELECT_RESULT'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/SELECT/POST_SELECT_CANCEL_SEARCH'),
    ('city-operations.booth_booking.execute', 'DASHBOARD', 'RIDER_RIDE_BOOKING/SELECT/POST_SELECT_ESTIMATE')
ON CONFLICT (server_name, endpoint_id, capability_id) DO NOTHING;
-- Create curated roles that exist on NEITHER dashboard (idempotent, by name).
-- dashboard_access_type DASHBOARD_USER: none of these are admin tiers;
-- admin tiering now lives in person.admin_tier, not this column.
-- Safe to run on both pre-merge schemas: ids are uuid5(name), so the
-- two sides create the SAME id and the Phase 1 name-dedupe is a no-op.
INSERT INTO atlas_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at)
SELECT 'e188362a-54e0-509a-9771-13ab5364e619', 'MSIL_ADMIN', 'DASHBOARD_USER', 'MSIL_ADMIN (created by capability seed)', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.role WHERE name = 'MSIL_ADMIN');
INSERT INTO atlas_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at)
SELECT 'f12c44f3-4f0d-5aa7-810f-7a48ffb9b4ea', 'BOT', 'DASHBOARD_USER', 'BOT (created by capability seed)', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.role WHERE name = 'BOT');
INSERT INTO atlas_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at)
SELECT 'fe280046-ddc5-5a97-9e9c-124bd39975cf', 'SUBURBAN', 'DASHBOARD_USER', 'SUBURBAN (created by capability seed)', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.role WHERE name = 'SUBURBAN');
INSERT INTO atlas_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at)
SELECT 'bac8ab59-0ed8-5494-b612-2ce48113f0b3', 'MtcFleetOps', 'DASHBOARD_USER', 'MtcFleetOps (created by capability seed)', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.role WHERE name = 'MtcFleetOps');
INSERT INTO atlas_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at)
SELECT 'f9055b62-ac49-529e-9d95-2124dba4a32c', 'MTC_ADMIN', 'DASHBOARD_USER', 'MTC_ADMIN (created by capability seed)', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.role WHERE name = 'MTC_ADMIN');
INSERT INTO atlas_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at)
SELECT '94279c7e-65dc-5885-883f-d867acfd174a', 'CLG_ADMIN', 'DASHBOARD_USER', 'CLG_ADMIN (created by capability seed)', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.role WHERE name = 'CLG_ADMIN');
INSERT INTO atlas_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at)
SELECT '523755ef-89c1-51a9-b9f8-b8a04011b0ee', 'STUDENT_PASS_DEPOT', 'DASHBOARD_USER', 'STUDENT_PASS_DEPOT (created by capability seed)', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.role WHERE name = 'STUDENT_PASS_DEPOT');
INSERT INTO atlas_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at)
SELECT '96f0598c-c596-536a-8842-31a964ac4e01', 'PT_CONDUCTOR', 'DASHBOARD_USER', 'PT_CONDUCTOR (created by capability seed)', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.role WHERE name = 'PT_CONDUCTOR');
INSERT INTO atlas_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at)
SELECT '94777b19-7e37-5d8c-9077-0bb8c1f7c0e6', 'PT_DEPOT_MANAGER', 'DASHBOARD_USER', 'PT_DEPOT_MANAGER (created by capability seed)', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.role WHERE name = 'PT_DEPOT_MANAGER');
INSERT INTO atlas_dashboard.role (id, name, dashboard_access_type, description, created_at, updated_at)
SELECT 'e89f1f07-9184-56ba-8aef-be470cc665de', 'TICKET_VALIDATOR', 'DASHBOARD_USER', 'TICKET_VALIDATOR (created by capability seed)', now(), now()
WHERE NOT EXISTS (SELECT 1 FROM atlas_dashboard.role WHERE name = 'TICKET_VALIDATOR');
-- Curated role bundles (capability-seed.md §4), applied per role NAME.
-- Unioned with the threshold derivation below; a role may receive both.
-- Roles absent on a side are skipped harmlessly.

-- JUSPAY_ADMIN (157 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('admin.audit.read'), ('admin.capability.grant'), ('admin.capability.read'), ('admin.crypto.execute'), ('admin.entity.read'), ('admin.entity.write'), ('admin.merchant.write'), ('admin.query.execute'), ('admin.role.read'), ('admin.role.write'), ('admin.user.read'), ('admin.user.write'), ('analytics.ai.execute'), ('analytics.core.read'), ('analytics.performance.read'), ('analytics.pricing.read'), ('analytics.pt_stats.read'), ('analytics.public_transport.read'), ('analytics.revenue.read'), ('analytics.sla.read'), ('analytics.sla.write'), ('city-config.geo.read'), ('city-config.geo.write'), ('city-config.launch.read'), ('city-config.launch.write'), ('city-config.merchant_onboarding.approve'), ('city-config.merchant_onboarding.read'), ('city-config.merchant_onboarding.write'), ('city-config.offer.read'), ('city-config.offer.write'), ('city-config.plan.read'), ('city-config.plan.write'), ('city-config.service_tier.read'), ('city-config.service_tier.write'), ('city-operations.airport_queue.read'), ('city-operations.airport_queue.write'), ('city-operations.booth_booking.execute'), ('city-operations.booth_customer.write'), ('city-operations.customer.read'), ('city-operations.customer.write'), ('city-operations.customer_issue.read'), ('city-operations.customer_issue.write'), ('city-operations.driver.read'), ('city-operations.driver.write'), ('city-operations.driver_issue.read'), ('city-operations.driver_issue.write'), ('city-operations.edc.read'), ('city-operations.edc.write'), ('city-operations.fleet_owner.read'), ('city-operations.fleet_owner.write'), ('city-operations.frfs.execute'), ('city-operations.frfs.read'), ('city-operations.grievance.read'), ('city-operations.grievance.write'), ('city-operations.incentive.read'), ('city-operations.membership.read'), ('city-operations.onboarding.read'), ('city-operations.onboarding.write'), ('city-operations.pass.execute'), ('city-operations.pass.read'), ('city-operations.pass_org.approve'), ('city-operations.pass_org.read'), ('city-operations.pass_org.write'), ('city-operations.payment.read'), ('city-operations.payment.write'), ('city-operations.pii.read'), ('city-operations.ride.read'), ('city-operations.ride.write'), ('city-operations.sos.read'), ('city-operations.sos.write'), ('city-operations.subscription.read'), ('city-operations.subscription.write'), ('city-operations.ticket.execute'), ('city-operations.ticket.read'), ('city-operations.ticket_booking.read'), ('city-operations.ticket_place.approve'), ('city-operations.ticket_place.read'), ('city-operations.ticket_place.write'), ('city-operations.training.read'), ('city-operations.vehicle.read'), ('city-operations.vehicle.write'), ('city-operations.volunteer.read'), ('city-operations.volunteer.write'), ('city-operations.wallet.read'), ('city-operations.wallet.write'), ('communication.message.read'), ('communication.message.write'), ('communication.shortener.execute'), ('finance.adjustment.write'), ('finance.fleet.read'), ('finance.insurance.read'), ('finance.invoice.read'), ('finance.ledger.read'), ('finance.payout.read'), ('finance.payout.write'), ('finance.reconciliation.execute'), ('finance.reconciliation.read'), ('finance.report.read'), ('finance.settlement.export'), ('finance.settlement.read'), ('fleet.driver.read'), ('fleet.driver.write'), ('fleet.earnings.read'), ('fleet.live.read'), ('fleet.onboarding.read'), ('fleet.onboarding.write'), ('fleet.operator.read'), ('fleet.operator.write'), ('fleet.profile.read'), ('fleet.profile.write'), ('fleet.trip.read'), ('fleet.trip.write'), ('fleet.vehicle.read'), ('fleet.vehicle.write'), ('system-config.coins.read'), ('system-config.coins.write'), ('system-config.config_pilot.read'), ('system-config.config_pilot.write'), ('system-config.customer_issue_config.read'), ('system-config.customer_issue_config.write'), ('system-config.driver_issue_config.read'), ('system-config.driver_issue_config.write'), ('system-config.dynamic_logic.read'), ('system-config.dynamic_logic.write'), ('system-config.failover.execute'), ('system-config.fare_policy.export'), ('system-config.fare_policy.read'), ('system-config.fare_policy.write'), ('system-config.firebase.read'), ('system-config.firebase.write'), ('system-config.knowledge.read'), ('system-config.knowledge.write'), ('system-config.merchant.read'), ('system-config.merchant.write'), ('system-config.namma_tag.read'), ('system-config.namma_tag.write'), ('system-config.registry.read'), ('system-config.registry.write'), ('system-config.release.read'), ('system-config.release.write'), ('system-config.rewards.read'), ('system-config.rewards.write'), ('system-config.scheduler.execute'), ('transit-config.fare.read'), ('transit-config.fare.write'), ('transit-config.gtfs.read'), ('transit-config.gtfs.write'), ('transit-config.seat_layout.read'), ('transit-config.seat_layout.write'), ('transit-config.stops.read'), ('transit-config.stops.write'), ('transit-operations.device.read'), ('transit-operations.device.write'), ('transit-operations.master.read'), ('transit-operations.master.write'), ('transit-operations.trip.execute'), ('transit-operations.waybill.write')) AS c(cap)
WHERE r.name = 'JUSPAY_ADMIN' ON CONFLICT DO NOTHING;

-- JUSPAY_OPS (72 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('analytics.ai.execute'), ('analytics.core.read'), ('analytics.performance.read'), ('analytics.public_transport.read'), ('analytics.sla.read'), ('city-operations.airport_queue.read'), ('city-operations.airport_queue.write'), ('city-operations.booth_booking.execute'), ('city-operations.booth_customer.write'), ('city-operations.customer.read'), ('city-operations.customer.write'), ('city-operations.customer_issue.read'), ('city-operations.customer_issue.write'), ('city-operations.driver.read'), ('city-operations.driver.write'), ('city-operations.driver_issue.read'), ('city-operations.driver_issue.write'), ('city-operations.edc.read'), ('city-operations.edc.write'), ('city-operations.fleet_owner.read'), ('city-operations.fleet_owner.write'), ('city-operations.frfs.execute'), ('city-operations.frfs.read'), ('city-operations.grievance.read'), ('city-operations.grievance.write'), ('city-operations.incentive.read'), ('city-operations.membership.read'), ('city-operations.onboarding.read'), ('city-operations.onboarding.write'), ('city-operations.pass.execute'), ('city-operations.pass.read'), ('city-operations.pass_org.approve'), ('city-operations.pass_org.read'), ('city-operations.pass_org.write'), ('city-operations.payment.read'), ('city-operations.payment.write'), ('city-operations.pii.read'), ('city-operations.ride.read'), ('city-operations.ride.write'), ('city-operations.sos.read'), ('city-operations.sos.write'), ('city-operations.subscription.read'), ('city-operations.subscription.write'), ('city-operations.ticket.execute'), ('city-operations.ticket.read'), ('city-operations.training.read'), ('city-operations.vehicle.read'), ('city-operations.vehicle.write'), ('city-operations.volunteer.read'), ('city-operations.volunteer.write'), ('city-operations.wallet.read'), ('city-operations.wallet.write'), ('communication.message.read'), ('communication.message.write'), ('communication.shortener.execute'), ('finance.reconciliation.read'), ('finance.report.read'), ('finance.settlement.read'), ('system-config.customer_issue_config.read'), ('system-config.customer_issue_config.write'), ('system-config.driver_issue_config.read'), ('system-config.driver_issue_config.write'), ('system-config.fare_policy.read'), ('system-config.fare_policy.write'), ('system-config.firebase.read'), ('system-config.firebase.write'), ('system-config.knowledge.read'), ('system-config.registry.read'), ('system-config.registry.write'), ('system-config.release.read'), ('system-config.rewards.read'), ('system-config.rewards.write')) AS c(cap)
WHERE r.name = 'JUSPAY_OPS' ON CONFLICT DO NOTHING;

-- CUSTOMER_SERVICE (8 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('city-operations.customer.read'), ('city-operations.ride.read'), ('city-operations.customer_issue.read'), ('city-operations.customer_issue.write'), ('city-operations.driver_issue.read'), ('city-operations.driver_issue.write'), ('city-operations.payment.read'), ('city-operations.payment.write')) AS c(cap)
WHERE r.name = 'CUSTOMER_SERVICE' ON CONFLICT DO NOTHING;

-- NY_DATA_TEAM (5 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('analytics.core.read'), ('analytics.performance.read'), ('analytics.public_transport.read'), ('analytics.ai.execute'), ('city-operations.pii.read')) AS c(cap)
WHERE r.name = 'NY_DATA_TEAM' ON CONFLICT DO NOTHING;

-- NY_MANAGER (9 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('analytics.core.read'), ('analytics.performance.read'), ('analytics.public_transport.read'), ('analytics.ai.execute'), ('city-operations.pii.read'), ('system-config.rewards.read'), ('system-config.rewards.write'), ('city-operations.membership.read'), ('admin.audit.read')) AS c(cap)
WHERE r.name = 'NY_MANAGER' ON CONFLICT DO NOTHING;

-- MSIL_ADMIN (4 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('analytics.core.read'), ('analytics.performance.read'), ('analytics.public_transport.read'), ('analytics.ai.execute')) AS c(cap)
WHERE r.name = 'MSIL_ADMIN' ON CONFLICT DO NOTHING;

-- ASSOCIATE (5 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('analytics.core.read'), ('analytics.performance.read'), ('analytics.public_transport.read'), ('analytics.ai.execute'), ('city-operations.membership.read')) AS c(cap)
WHERE r.name = 'ASSOCIATE' ON CONFLICT DO NOTHING;

-- EXECUTIVE (6 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('analytics.core.read'), ('analytics.performance.read'), ('analytics.public_transport.read'), ('analytics.ai.execute'), ('city-operations.pii.read'), ('city-operations.membership.read')) AS c(cap)
WHERE r.name = 'EXECUTIVE' ON CONFLICT DO NOTHING;

-- CITY_HEAD (9 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('analytics.core.read'), ('analytics.performance.read'), ('analytics.public_transport.read'), ('analytics.ai.execute'), ('city-operations.pii.read'), ('finance.adjustment.write'), ('city-operations.incentive.read'), ('system-config.fare_policy.read'), ('city-operations.membership.read')) AS c(cap)
WHERE r.name = 'CITY_HEAD' ON CONFLICT DO NOTHING;

-- YATRI_SATHI_ADMIN (1 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('finance.adjustment.write')) AS c(cap)
WHERE r.name = 'YATRI_SATHI_ADMIN' ON CONFLICT DO NOTHING;

-- ANALYTICS (1 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('analytics.core.read')) AS c(cap)
WHERE r.name = 'ANALYTICS' ON CONFLICT DO NOTHING;

-- FLEET (15 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('fleet.driver.read'), ('fleet.driver.write'), ('fleet.vehicle.read'), ('fleet.vehicle.write'), ('fleet.trip.read'), ('fleet.trip.write'), ('fleet.earnings.read'), ('fleet.live.read'), ('fleet.onboarding.read'), ('fleet.onboarding.write'), ('fleet.profile.read'), ('fleet.profile.write'), ('analytics.core.read'), ('analytics.performance.read'), ('finance.fleet.read')) AS c(cap)
WHERE r.name = 'FLEET' ON CONFLICT DO NOTHING;

-- RENTAL_FLEET (16 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('fleet.driver.read'), ('fleet.driver.write'), ('fleet.vehicle.read'), ('fleet.vehicle.write'), ('fleet.trip.read'), ('fleet.trip.write'), ('fleet.earnings.read'), ('fleet.live.read'), ('fleet.onboarding.read'), ('fleet.onboarding.write'), ('fleet.profile.read'), ('fleet.profile.write'), ('analytics.core.read'), ('analytics.performance.read'), ('finance.fleet.read'), ('finance.adjustment.write')) AS c(cap)
WHERE r.name = 'RENTAL_FLEET' ON CONFLICT DO NOTHING;

-- RENTAL_FLEET_OWNER (1 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('finance.adjustment.write')) AS c(cap)
WHERE r.name = 'RENTAL_FLEET_OWNER' ON CONFLICT DO NOTHING;

-- DASHBOARD_OPERATOR (8 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('fleet.driver.read'), ('fleet.driver.write'), ('fleet.vehicle.read'), ('fleet.vehicle.write'), ('fleet.trip.read'), ('fleet.trip.write'), ('fleet.live.read'), ('city-operations.training.read')) AS c(cap)
WHERE r.name = 'DASHBOARD_OPERATOR' ON CONFLICT DO NOTHING;

-- OPERATOR (4 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('fleet.operator.read'), ('fleet.operator.write'), ('fleet.driver.read'), ('fleet.vehicle.read')) AS c(cap)
WHERE r.name = 'OPERATOR' ON CONFLICT DO NOTHING;

-- BOT (2 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('city-operations.onboarding.read'), ('city-operations.onboarding.write')) AS c(cap)
WHERE r.name = 'BOT' ON CONFLICT DO NOTHING;

-- BTFinance (4 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('finance.reconciliation.read'), ('finance.reconciliation.execute'), ('finance.settlement.export'), ('finance.report.read')) AS c(cap)
WHERE r.name = 'BTFinance' ON CONFLICT DO NOTHING;

-- FINANCE_ADMIN (5 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('finance.report.read'), ('finance.settlement.read'), ('finance.settlement.export'), ('finance.ledger.read'), ('finance.invoice.read')) AS c(cap)
WHERE r.name = 'FINANCE_ADMIN' ON CONFLICT DO NOTHING;

-- CUMTA (4 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('analytics.sla.read'), ('analytics.public_transport.read'), ('finance.settlement.read'), ('transit-operations.device.read')) AS c(cap)
WHERE r.name = 'CUMTA' ON CONFLICT DO NOTHING;

-- MTC (3 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('analytics.public_transport.read'), ('finance.settlement.read'), ('transit-operations.device.read')) AS c(cap)
WHERE r.name = 'MTC' ON CONFLICT DO NOTHING;

-- CMRL (3 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('analytics.public_transport.read'), ('finance.settlement.read'), ('transit-operations.device.read')) AS c(cap)
WHERE r.name = 'CMRL' ON CONFLICT DO NOTHING;

-- SUBURBAN (3 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('analytics.public_transport.read'), ('finance.settlement.read'), ('transit-operations.device.read')) AS c(cap)
WHERE r.name = 'SUBURBAN' ON CONFLICT DO NOTHING;

-- MtcFleetOps (15 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('transit-config.stops.read'), ('transit-config.stops.write'), ('transit-config.gtfs.read'), ('transit-config.gtfs.write'), ('transit-config.fare.read'), ('transit-config.fare.write'), ('transit-config.seat_layout.read'), ('transit-config.seat_layout.write'), ('transit-operations.master.read'), ('transit-operations.master.write'), ('transit-operations.waybill.write'), ('transit-operations.device.read'), ('transit-operations.device.write'), ('transit-operations.trip.execute'), ('admin.audit.read')) AS c(cap)
WHERE r.name = 'MtcFleetOps' ON CONFLICT DO NOTHING;

-- MTC OPS (15 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('transit-config.stops.read'), ('transit-config.stops.write'), ('transit-config.gtfs.read'), ('transit-config.gtfs.write'), ('transit-config.fare.read'), ('transit-config.fare.write'), ('transit-config.seat_layout.read'), ('transit-config.seat_layout.write'), ('transit-operations.master.read'), ('transit-operations.master.write'), ('transit-operations.waybill.write'), ('transit-operations.device.read'), ('transit-operations.device.write'), ('transit-operations.trip.execute'), ('admin.audit.read')) AS c(cap)
WHERE r.name = 'MTC OPS' ON CONFLICT DO NOTHING;

-- MTC_ADMIN (3 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('city-operations.pass_org.read'), ('city-operations.pass_org.write'), ('city-operations.pass_org.approve')) AS c(cap)
WHERE r.name = 'MTC_ADMIN' ON CONFLICT DO NOTHING;

-- CLG_ADMIN (2 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('city-operations.pass_org.read'), ('city-operations.pass_org.write')) AS c(cap)
WHERE r.name = 'CLG_ADMIN' ON CONFLICT DO NOTHING;

-- STUDENT_PASS_DEPOT (2 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('city-operations.pass_org.read'), ('city-operations.pass_org.approve')) AS c(cap)
WHERE r.name = 'STUDENT_PASS_DEPOT' ON CONFLICT DO NOTHING;

-- PT_CONDUCTOR (1 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('analytics.pt_stats.read')) AS c(cap)
WHERE r.name = 'PT_CONDUCTOR' ON CONFLICT DO NOTHING;

-- PT_DEPOT_MANAGER (1 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('analytics.pt_stats.read')) AS c(cap)
WHERE r.name = 'PT_DEPOT_MANAGER' ON CONFLICT DO NOTHING;

-- SlaMonitoring (3 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('analytics.sla.read'), ('city-operations.grievance.read'), ('city-operations.grievance.write')) AS c(cap)
WHERE r.name = 'SlaMonitoring' ON CONFLICT DO NOTHING;

-- FIREBASE_CONTROL_CENTER (4 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('system-config.firebase.read'), ('system-config.firebase.write'), ('city-config.launch.read'), ('city-config.launch.write')) AS c(cap)
WHERE r.name = 'FIREBASE_CONTROL_CENTER' ON CONFLICT DO NOTHING;

-- IffcoOPS (1 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('finance.insurance.read')) AS c(cap)
WHERE r.name = 'IffcoOPS' ON CONFLICT DO NOTHING;

-- AIRPORT_OPS (9 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('city-operations.airport_queue.read'), ('city-operations.airport_queue.write'), ('city-operations.wallet.write'), ('city-operations.vehicle.read'), ('city-operations.vehicle.write'), ('city-operations.driver.write'), ('system-config.fare_policy.write'), ('communication.shortener.execute'), ('system-config.knowledge.read')) AS c(cap)
WHERE r.name = 'AIRPORT_OPS' ON CONFLICT DO NOTHING;

-- AirportAgent (3 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('city-operations.booth_booking.execute'), ('city-operations.booth_customer.write'), ('city-operations.wallet.write')) AS c(cap)
WHERE r.name = 'AirportAgent' ON CONFLICT DO NOTHING;

-- AirportKPAgent (3 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('city-operations.booth_booking.execute'), ('city-operations.booth_customer.write'), ('city-operations.wallet.write')) AS c(cap)
WHERE r.name = 'AirportKPAgent' ON CONFLICT DO NOTHING;

-- AirportKPSupervisor (5 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('city-operations.booth_booking.execute'), ('city-operations.booth_customer.write'), ('city-operations.wallet.write'), ('city-operations.airport_queue.read'), ('city-operations.airport_queue.write')) AS c(cap)
WHERE r.name = 'AirportKPSupervisor' ON CONFLICT DO NOTHING;

-- TICKET_VALIDATOR (2 capabilities)
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, c.cap FROM atlas_dashboard.role r,
     (VALUES ('city-operations.ticket.read'), ('city-operations.ticket.execute')) AS c(cap)
WHERE r.name = 'TICKET_VALIDATOR' ON CONFLICT DO NOTHING;
-- Threshold derivation for long-tail roles: a role inherits capability C only
-- if it already holds >= 50%% of C's endpoints. Nothing is withheld by
-- sensitivity — sub-threshold holdings stay reachable via the transitional
-- `capability OR legacy matrix` check at enforcement time (PLAN.md). Run AFTER
-- the curated seed.
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT held.role_id, held.capability_id
FROM (
  SELECT m.role_id, ce.capability_id, count(DISTINCT ce.endpoint_id) AS held_endpoints
  FROM atlas_dashboard.access_matrix m
  JOIN atlas_dashboard.capability_endpoint ce
    ON ce.endpoint_id = CASE WHEN m.api_entity = 'SPECIAL_ZONES'
       THEN 'LEGACY/SPECIAL_ZONES/' || m.user_action_type
       ELSE m.user_action_type END
  WHERE m.user_access_type = 'USER_FULL_ACCESS'
  GROUP BY m.role_id, ce.capability_id
) held
JOIN (
  SELECT capability_id, count(*) AS total_endpoints
  FROM atlas_dashboard.capability_endpoint GROUP BY capability_id
) sized ON sized.capability_id = held.capability_id
WHERE held.held_endpoints::numeric / sized.total_endpoints >= 0.5
ON CONFLICT DO NOTHING;

-- Coverage report: what each role holds vs the capability size. Rows below the
-- threshold did NOT derive and rely on the legacy fallback — they are the
-- worklist for manual curation (highest member counts first).
-- SELECT r.name, held.capability_id, held.held_endpoints, sized.total_endpoints,
--        round(100.0 * held.held_endpoints / sized.total_endpoints) AS pct
-- FROM ( ...held... ) held JOIN ( ...sized... ) sized USING (capability_id)
-- JOIN atlas_dashboard.role r ON r.id = held.role_id
-- WHERE held.held_endpoints::numeric / sized.total_endpoints < 0.5
-- ORDER BY pct DESC;

-- ---------------------------------------------------------------------------
-- Corrective grants/denies (2026-08-08). MUST run AFTER derivation — a DELETE
-- placed before it would be re-inserted by the derivation INSERT above.
--
-- 1) PT dashboard roles: analytics.pt_stats.read is a CONTROL_CENTER-only
--    capability (no NY endpoints), so threshold derivation can never produce
--    it, and the curated bundle is skipped for roles holding legacy grants.
--    The frontend gates /public-transport/* on this id — grant unconditionally.
INSERT INTO atlas_dashboard.role_capability (role_id, capability_id)
SELECT r.id, 'analytics.pt_stats.read'
FROM atlas_dashboard.role r
WHERE r.name IN ('PT_CONDUCTOR', 'PT_DEPOT_MANAGER')
ON CONFLICT DO NOTHING;

-- 2) DASHBOARD_OPERATOR: the curated bundle deliberately excludes earnings and
--    onboarding (operators shared the fleet login but never saw Finances or
--    Settings — the old excludedRoles rule). Matrix inheritance can widen the
--    role into those ids; revoke them so the UI hiding holds. API access is
--    unaffected during the transition (`capability OR legacy` floor).
DELETE FROM atlas_dashboard.role_capability rc
USING atlas_dashboard.role r
WHERE rc.role_id = r.id
  AND r.name = 'DASHBOARD_OPERATOR'
  AND rc.capability_id IN ('fleet.earnings.read', 'fleet.onboarding.read', 'fleet.onboarding.write');
