-- Add SLA tracking fields to issue_report for response/resolution deadline enforcement
ALTER TABLE atlas_driver_offer_bpp.issue_report
  ADD COLUMN IF NOT EXISTS first_response_at TIMESTAMPTZ,
  ADD COLUMN IF NOT EXISTS resolved_at TIMESTAMPTZ,
  ADD COLUMN IF NOT EXISTS sla_deadline TIMESTAMPTZ,
  ADD COLUMN IF NOT EXISTS priority TEXT,
  ADD COLUMN IF NOT EXISTS escalation_level INT DEFAULT 0;

-- Add SLA configuration fields to issue_config
ALTER TABLE atlas_driver_offer_bpp.issue_config
  ADD COLUMN IF NOT EXISTS sla_first_response_duration_hours DOUBLE PRECISION DEFAULT 24,
  ADD COLUMN IF NOT EXISTS sla_resolution_duration_hours DOUBLE PRECISION DEFAULT 72,
  ADD COLUMN IF NOT EXISTS enable_sla_tracking BOOLEAN DEFAULT FALSE;

-- Index for SLA breach detection queries
CREATE INDEX IF NOT EXISTS idx_issue_report_sla_breach
  ON atlas_driver_offer_bpp.issue_report (created_at, first_response_at, resolved_at, status)
  WHERE status NOT IN ('RESOLVED', 'CLOSED', 'NOT_APPLICABLE');
