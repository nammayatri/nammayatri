-- Fleet owner's TDS reimbursement history (findAllByFleetOwnerIdAndMerchantOperatingCityId). The unique
-- index from 0857 is partial on status IN ('PENDING', 'APPROVED'), so a lookup without a status filter
-- can't use it and the DB fallback would scan the table.
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_finance_tds_reimbursement_request_fleet_owner
  ON atlas_driver_offer_bpp.finance_tds_reimbursement_request USING btree (fleet_owner_id, merchant_operating_city_id);
