-- Enable auto-verification of fleet owners on login for local dev.
-- Without this, fleet owners are created with verified=false and "Add Vehicle for Driver"
-- fails with INVALID_REQUEST: "Fleet owner is not verified".
-- Applies to all merchants that run fleet dashboard tests (NY, YS, BT partners).

UPDATE atlas_bpp_dashboard.merchant
SET verify_fleet_while_login = true
WHERE verify_fleet_while_login = false
  AND short_id IN ('NAMMA_YATRI_PARTNER', 'JATRI_SAATHI_PARTNER', 'BHARAT_TAXI_PARTNER', 'YATRI_PARTNER', 'BRIDGE_FINLAND_PARTNER', 'MSIL_PARTNER');
