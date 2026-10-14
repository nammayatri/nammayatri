CREATE INDEX IF NOT EXISTS idx_ride_evidence_ride_id
  ON atlas_driver_offer_bpp.ride_evidence (ride_id);

CREATE INDEX IF NOT EXISTS idx_ride_evidence_media_file_id
  ON atlas_driver_offer_bpp.ride_evidence (media_file_id);
