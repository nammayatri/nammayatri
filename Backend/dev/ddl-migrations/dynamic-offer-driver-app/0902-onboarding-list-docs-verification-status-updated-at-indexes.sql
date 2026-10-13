-- The onboarding driver / vehicle / fleet lists are per city, ordered by docs_verification_status_updated_at
-- DESC NULLS LAST, created_at DESC, with a small LIMIT; matching that order and direction lets the scan stop
-- early with no sort. The _approval variant serves the status tabs (approved = true / false / IS NULL).
CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_driver_information_onboarding_list
  ON atlas_driver_offer_bpp.driver_information USING btree (merchant_operating_city_id, docs_verification_status_updated_at DESC NULLS LAST, created_at DESC);

CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_driver_information_onboarding_list_approval
  ON atlas_driver_offer_bpp.driver_information USING btree (merchant_operating_city_id, approved, docs_verification_status_updated_at DESC NULLS LAST, created_at DESC);

CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_fleet_owner_information_onboarding_list
  ON atlas_driver_offer_bpp.fleet_owner_information USING btree (merchant_operating_city_id, docs_verification_status_updated_at DESC NULLS LAST, created_at DESC);

CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_fleet_owner_information_onboarding_list_approval
  ON atlas_driver_offer_bpp.fleet_owner_information USING btree (merchant_operating_city_id, approved, docs_verification_status_updated_at DESC NULLS LAST, created_at DESC);

CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_vehicle_registration_certificate_onboarding_list
  ON atlas_driver_offer_bpp.vehicle_registration_certificate USING btree (merchant_operating_city_id, docs_verification_status_updated_at DESC NULLS LAST, created_at DESC);

CREATE INDEX CONCURRENTLY IF NOT EXISTS idx_vehicle_registration_certificate_onboarding_list_approval
  ON atlas_driver_offer_bpp.vehicle_registration_certificate USING btree (merchant_operating_city_id, approved, docs_verification_status_updated_at DESC NULLS LAST, created_at DESC);
