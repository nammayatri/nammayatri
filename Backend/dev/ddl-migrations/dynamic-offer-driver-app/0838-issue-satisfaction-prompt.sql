-- Mirrors the rider-app migration: backs the satisfaction-prompt flow on the driver
-- side of the shared IssueManagement library. See the rider-app counterpart for
-- column-by-column rationale.
ALTER TABLE atlas_driver_offer_bpp.issue_config
  ADD COLUMN on_customer_not_satisfied_msgs text[] NOT NULL DEFAULT '{}';

ALTER TABLE atlas_driver_offer_bpp.issue_report
  ADD COLUMN customer_response text;
