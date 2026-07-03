-- The shared IssueManagement library gained an `additional_ticket_ids` column
-- to persist secondary-provider ticketIds for merchants that fan out ticket
-- writes to multiple third parties. Driver-app does not fan out today (only
-- rider-app dispatches through the primary+secondary code path), so this
-- column will stay NULL here — but the shared Beam schema requires it to be
-- present on every table backing `IssueReport`.
ALTER TABLE atlas_driver_offer_bpp.issue_report
  ADD COLUMN additional_ticket_ids text;
