CREATE INDEX idx_jet_merchant_type_created
  ON atlas_driver_offer_bpp.journal_entry_transaction
  (merchant_id, merchant_operating_city_id, transaction_type, created_at DESC);
