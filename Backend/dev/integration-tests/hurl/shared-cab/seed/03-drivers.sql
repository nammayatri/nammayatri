-- Needs the local-testing seed driver (Backend/dev/local-testing-data/dynamic-offer-driver-app.sql) applied first.
-- two shared-cab drivers cloned from the seeded NAMMA_YATRI_PARTNER driver
DO $$
DECLARE
  src text := '8256a5d6-b1f2-cb8c-dcd0-a691ea24aaa0';
  moc text := 'f8e9db0a-96c8-49e4-942a-3e3f7265d2da';
  n int; pid text; plate text; ph text; salt text := 'How wonderful it is that nobody need wait a single moment before starting to improve the world';
BEGIN
  FOR n IN 1..2 LOOP
    pid := md5('e2e-sc-driver-'||n)::uuid::text;
    plate := CASE n WHEN 1 THEN 'ML05A9999' ELSE 'ML05B8888' END;
    ph := '99999000' || (10+n);
    INSERT INTO atlas_driver_offer_bpp.person SELECT (jsonb_populate_record(NULL::atlas_driver_offer_bpp.person, to_jsonb(r) || jsonb_build_object('id',pid,'first_name','sc_driver_'||n,'merchant_operating_city_id',moc,'unencrypted_mobile_number',ph,'mobile_number_hash', to_jsonb(sha256(convert_to(salt||ph,'UTF8')))))).* FROM atlas_driver_offer_bpp.person r WHERE id=src ON CONFLICT DO NOTHING;
    INSERT INTO atlas_driver_offer_bpp.registration_token SELECT (jsonb_populate_record(NULL::atlas_driver_offer_bpp.registration_token, to_jsonb(r) || jsonb_build_object('id',md5('e2e-sc-tok-'||n)::uuid::text,'entity_id',pid,'token','e2e-sc-driver-token-'||n))).* FROM atlas_driver_offer_bpp.registration_token r WHERE entity_id=src ON CONFLICT DO NOTHING;
    INSERT INTO atlas_driver_offer_bpp.driver_information SELECT (jsonb_populate_record(NULL::atlas_driver_offer_bpp.driver_information, to_jsonb(r) || jsonb_build_object('driver_id',pid))).* FROM atlas_driver_offer_bpp.driver_information r WHERE driver_id=src ON CONFLICT DO NOTHING;
    INSERT INTO atlas_driver_offer_bpp.driver_stats SELECT (jsonb_populate_record(NULL::atlas_driver_offer_bpp.driver_stats, to_jsonb(r) || jsonb_build_object('driver_id',pid))).* FROM atlas_driver_offer_bpp.driver_stats r WHERE driver_id=src ON CONFLICT DO NOTHING;
    INSERT INTO atlas_driver_offer_bpp.vehicle SELECT (jsonb_populate_record(NULL::atlas_driver_offer_bpp.vehicle, to_jsonb(r) || jsonb_build_object('driver_id',pid,'registration_no',plate,'variant','SHARED_CAB','capacity',4,'merchant_operating_city_id',moc,'selected_service_tiers',ARRAY['SHARED_CAB']))).* FROM atlas_driver_offer_bpp.vehicle r WHERE driver_id=src ON CONFLICT DO NOTHING;
  END LOOP;
END $$;
UPDATE atlas_driver_offer_bpp.registration_token SET merchant_operating_city_id='f8e9db0a-96c8-49e4-942a-3e3f7265d2da' WHERE token LIKE 'e2e-sc-driver-token-%';
UPDATE atlas_driver_offer_bpp.driver_information SET merchant_operating_city_id='f8e9db0a-96c8-49e4-942a-3e3f7265d2da'
 WHERE driver_id IN (SELECT id FROM atlas_driver_offer_bpp.person WHERE first_name LIKE 'sc_driver_%');
