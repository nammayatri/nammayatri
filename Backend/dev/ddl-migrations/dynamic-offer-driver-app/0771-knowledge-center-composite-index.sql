CREATE INDEX IF NOT EXISTS idx_knowledge_center_moc_sop_type_created_at
  ON atlas_driver_offer_bpp.knowledge_center (merchant_operating_city_id, sop_type, created_at DESC NULLS LAST);
