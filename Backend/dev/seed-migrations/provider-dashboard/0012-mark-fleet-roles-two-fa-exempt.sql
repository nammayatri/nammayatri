-- A phone OTP is the only login a fleet owner or an operator has. While a merchant is in ENFORCE it is
-- refused for anyone who holds a role that is not exempt from two-factor, so the fleet-side roles are
-- exempt as data. Run this once per environment before switching any merchant to ENFORCE.
UPDATE atlas_dashboard.role
   SET two_fa_exempt = true
 WHERE dashboard_access_type IN ('FLEET_OWNER', 'RENTAL_FLEET_OWNER', 'DASHBOARD_OPERATOR');
