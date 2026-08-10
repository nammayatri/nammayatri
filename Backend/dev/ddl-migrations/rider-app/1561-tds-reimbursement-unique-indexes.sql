-- Enforces at most one PENDING/APPROVED TDS reimbursement request per fleet

CREATE UNIQUE INDEX IF NOT EXISTS idx_finance_tds_reimbursement_request_period_unique
  ON atlas_app.finance_tds_reimbursement_request (fleet_owner_id, quarter, assessment_year, merchant_operating_city_id)
  WHERE status IN ('PENDING', 'APPROVED');

-- Prevents the same invoice from being credited twice under the same
-- reimbursement request.
CREATE UNIQUE INDEX IF NOT EXISTS idx_finance_tds_reimbursement_invoice_mapping_unique
  ON atlas_app.finance_tds_reimbursement_invoice_mapping (request_id, invoice_id);
