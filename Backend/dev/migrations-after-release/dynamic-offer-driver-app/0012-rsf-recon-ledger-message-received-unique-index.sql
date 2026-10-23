-- RSF Phase 1 (E3): a receiver_recon message_id is used once per merchant.
CREATE UNIQUE INDEX IF NOT EXISTS rsf_recon_ledger_entry_message_received_uniq
  ON atlas_driver_offer_bpp.rsf_recon_ledger_entry (merchant_id, message_id)
  WHERE entry_type = 'MESSAGE_RECEIVED';
