UPDATE atlas_driver_offer_bpp.document_verification_config
SET title = 'Back Seat Image'
WHERE document_type = 'VehicleBack';

-- Hide DriverInspectionHub from document verification config
UPDATE atlas_driver_offer_bpp.document_verification_config
SET is_hidden = true
WHERE document_type in ('DriverInspectionHub','BankingDetails');

-- Add missing Hindi translations for document type titles
INSERT INTO atlas_driver_offer_bpp.translations (id, message_key, language, message, created_at, updated_at)
VALUES
  (atlas_driver_offer_bpp.uuid_generate_v4(), 'DriverInspectionHub_Title', 'HINDI', 'ड्राइवर ऑपरेशन हब फ्लो', now(), now()),
  (atlas_driver_offer_bpp.uuid_generate_v4(), 'InspectionHub_Title', 'HINDI', 'वाहन ऑपरेशन हब फ्लो', now(), now()),
  (atlas_driver_offer_bpp.uuid_generate_v4(), 'LocalResidenceProof_Title', 'HINDI', 'स्थानीय निवास प्रमाण', now(), now()),
  (atlas_driver_offer_bpp.uuid_generate_v4(), 'PoliceVerificationCertificate_Title', 'HINDI', 'पुलिस सत्यापन प्रमाणपत्र', now(), now()),
  (atlas_driver_offer_bpp.uuid_generate_v4(), 'BankingDetails_Title', 'HINDI', 'भुगतान के लिए बैंक खाता जानकारी (वैकल्पिक)', now(), now());

-- Add rc_number_prefix_list {DL,KA,HR,TS,TG} for MSIL_PARTNER merchant operating cities
UPDATE atlas_driver_offer_bpp.document_verification_config
SET rc_number_prefix_list = '{DL,KA,HR,TS,TG,AP}'
WHERE merchant_operating_city_id IN (
  SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
  WHERE merchant_short_id = 'MSIL_PARTNER'
);

-- Add dependency_document_type for MSIL_PARTNER
UPDATE atlas_driver_offer_bpp.document_verification_config
SET dependency_document_type = '{ProfilePhoto}'
WHERE document_type = 'DriverLicense'
  AND merchant_operating_city_id IN (
    SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
  );

UPDATE atlas_driver_offer_bpp.document_verification_config
SET dependency_document_type = '{ProfilePhoto,DriverLicense}'
WHERE document_type IN ('PanCard', 'AadhaarCard')
  AND merchant_operating_city_id IN (
    SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
  );

-- Remove lmv entries from supported_vehicle_classes_json for MSIL RC config
UPDATE atlas_driver_offer_bpp.document_verification_config
SET supported_vehicle_classes_json = (
  SELECT jsonb_agg(elem)
  FROM jsonb_array_elements(supported_vehicle_classes_json::jsonb) AS elem
  WHERE elem->>'vehicleClass' != 'lmv'
)::json
WHERE document_type = 'VehicleRegistrationCertificate'
  AND merchant_operating_city_id IN (
    SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
  );

-- Update rc_validation_rules vehicle_class for MSIL (all classes except lmv)
UPDATE atlas_driver_offer_bpp.rc_validation_rules
SET vehicle_class = '{cab,pcv,lpv,passenger commercial vehicle,light passenger vehicle}'
WHERE merchant_operating_city_id IN (
    SELECT id FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
  );

INSERT INTO atlas_driver_offer_bpp.merchant_message (merchant_id, message_key, message, merchant_operating_city_id)
SELECT
    m.merchant_id,
    'SEND_TOTP',
    '{#otp#} is your OTP for 2FA verification. Please do not share it with anyone.',
    m.id
FROM atlas_driver_offer_bpp.merchant_operating_city AS m
ON CONFLICT (merchant_operating_city_id, message_key) DO NOTHING;