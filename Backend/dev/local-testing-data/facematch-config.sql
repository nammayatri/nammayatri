-- Self-seed for the FaceMatchOnboardingFlow integration suite.
--
-- Creates a DEDICATED test opCity (NAMMA_YATRI / Ahmedabad) configured for the NON-SDK IDfy
-- document path so the selfie<->document face match actually runs, WITHOUT touching the real
-- Bangalore opCity (which uses the HyperVerge SDK path and is relied on by other suites).
--
-- It clones the Bangalore/NAMMA config into a new opCity and flips:
--   * merchant_service_usage_config.pan_verification_service -> Idfy   (route PAN through callIdfy -> verifyPanFlow gate)
--   * merchant_service_usage_config.face_match_service        -> Idfy   (server-side /compare/face)
--   * document_verification_config.face_match_source_doc      -> ProfilePhoto (the feature toggle)
--   * document_verification_config.is_image_validation_required -> true (so uploaded doc images reach VALID)
-- The cloned Verification_Idfy service keeps the mock URL (.../gridline); gridline.py delegates the
-- /extract_image, /validate_image and /compare endpoints to the idfy mock (which has the face-compare handler).
--
-- Idempotent (skips if the test opCity already exists). Applied automatically by config-sync
-- (context-api apply_local_testing_data) and by `./run-tests.sh face-match`.
-- NOTE: the BE caches merchant_service_config in-process, so after a fresh sync the BE must be
-- (re)started for the new opCity's IDfy routing to take effect.

DO $$
DECLARE
  src text := 'f067bccf-5b34-fb51-a5a3-9d6fa6baac26';   -- Bangalore / NAMMA opCity (clone source)
  dst text := 'fa11ec00-0000-0000-0000-0facematch00';   -- dedicated face-match test opCity
  newcity text := 'Ahmedabad';                          -- a valid Context.City NAMMA does not already use
BEGIN
  IF EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.merchant_operating_city WHERE id = dst) THEN
    RAISE NOTICE 'face-match test opCity already present (%); skipping', dst;
    RETURN;
  END IF;
  IF NOT EXISTS (SELECT 1 FROM atlas_driver_offer_bpp.merchant_operating_city WHERE id = src) THEN
    RAISE NOTICE 'clone source opCity % not found (run config-sync first); skipping', src;
    RETURN;
  END IF;

  CREATE TEMP TABLE _moc ON COMMIT DROP AS SELECT * FROM atlas_driver_offer_bpp.merchant_operating_city WHERE id = src;
  UPDATE _moc SET id = dst, city = newcity;
  INSERT INTO atlas_driver_offer_bpp.merchant_operating_city SELECT * FROM _moc;

  CREATE TEMP TABLE _tc ON COMMIT DROP AS SELECT * FROM atlas_driver_offer_bpp.transporter_config WHERE merchant_operating_city_id = src;
  UPDATE _tc SET merchant_operating_city_id = dst;
  INSERT INTO atlas_driver_offer_bpp.transporter_config SELECT * FROM _tc;

  CREATE TEMP TABLE _msc ON COMMIT DROP AS SELECT * FROM atlas_driver_offer_bpp.merchant_service_config WHERE merchant_operating_city_id = src;
  UPDATE _msc SET merchant_operating_city_id = dst;
  INSERT INTO atlas_driver_offer_bpp.merchant_service_config SELECT * FROM _msc;

  CREATE TEMP TABLE _msuc ON COMMIT DROP AS SELECT * FROM atlas_driver_offer_bpp.merchant_service_usage_config WHERE merchant_operating_city_id = src;
  UPDATE _msuc SET merchant_operating_city_id = dst, pan_verification_service = 'Idfy', face_match_service = 'Idfy';
  INSERT INTO atlas_driver_offer_bpp.merchant_service_usage_config SELECT * FROM _msuc;

  CREATE TEMP TABLE _dvc ON COMMIT DROP AS SELECT * FROM atlas_driver_offer_bpp.document_verification_config WHERE merchant_operating_city_id = src;
  UPDATE _dvc SET merchant_operating_city_id = dst;
  UPDATE _dvc SET face_match_source_doc = 'ProfilePhoto' WHERE document_type IN ('DriverLicense','PanCard','AadhaarCard');
  UPDATE _dvc SET is_image_validation_required = true WHERE document_type IN ('PanCard','AadhaarCard');
  INSERT INTO atlas_driver_offer_bpp.document_verification_config SELECT * FROM _dvc;

  RAISE NOTICE 'created face-match test opCity % (% / NAMMA)', dst, newcity;
END $$;
