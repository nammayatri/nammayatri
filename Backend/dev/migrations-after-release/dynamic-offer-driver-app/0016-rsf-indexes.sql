-- RSF access paths, hand-written because the NammaDSL generator sorts index columns alphabetically.

-- Phase 1 (E3): a receiver_recon message_id is received once per merchant.
CREATE UNIQUE INDEX CONCURRENTLY IF NOT EXISTS rsf_recon_ledger_entry_message_received_uniq
  ON atlas_driver_offer_bpp.rsf_recon_ledger_entry (merchant_id, message_id)
  WHERE entry_type = 'MESSAGE_RECEIVED';

-- A message's rows (ingest, send).
CREATE INDEX CONCURRENTLY IF NOT EXISTS rsf_recon_ledger_entry_idx_merchant_id_message_id
  ON atlas_driver_offer_bpp.rsf_recon_ledger_entry (merchant_id, message_id);

-- A day's rows of one type (envelopes for send, accepted claims for allocation).
CREATE INDEX CONCURRENTLY IF NOT EXISTS rsf_recon_ledger_entry_idx_merchant_id_entry_type_created_at
  ON atlas_driver_offer_bpp.rsf_recon_ledger_entry (merchant_id, entry_type, created_at);

-- Dashboard paging and counts by the day of the latest claim.
CREATE INDEX CONCURRENTLY IF NOT EXISTS rsf_order_state_idx_merchant_id_last_claimed_at
  ON atlas_driver_offer_bpp.rsf_order_state (merchant_id, last_claimed_at);
CREATE INDEX CONCURRENTLY IF NOT EXISTS rsf_utr_state_idx_merchant_id_last_claimed_at
  ON atlas_driver_offer_bpp.rsf_utr_state (merchant_id, last_claimed_at);

-- Completed rides of a merchant in a time range (unclaimed-rides list, its count, and the missing-rides send).
CREATE INDEX CONCURRENTLY IF NOT EXISTS booking_idx_provider_id_created_at_completed
  ON atlas_driver_offer_bpp.booking (provider_id, created_at)
  WHERE status = 'COMPLETED';
