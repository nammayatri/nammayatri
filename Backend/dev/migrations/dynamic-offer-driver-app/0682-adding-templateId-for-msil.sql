UPDATE atlas_driver_offer_bpp.merchant_message
SET template_id = '1007589967455591716'
WHERE message_key = 'OPERATOR_JOINING_MESSAGE'
  AND merchant_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant
    WHERE name = 'MSIL'
);

UPDATE atlas_driver_offer_bpp.merchant_message
SET template_id = '1007212660954089760'
WHERE message_key = 'FLEET_JOIN_AND_DOWNLOAD_APP_MESSAGE'
  AND merchant_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant
    WHERE name = 'MSIL'
);


UPDATE atlas_driver_offer_bpp.merchant_message
SET template_id = '1007490949489488751'
WHERE message_key = 'FLEET_JOINING_MESSAGE'
  AND merchant_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant
    WHERE name = 'MSIL'
);


UPDATE atlas_driver_offer_bpp.merchant_message
SET template_id = '1007355245551500755'
WHERE message_key = 'OPERATOR_JOIN_AND_DOWNLOAD_APP_MESSAGE'
  AND merchant_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant
    WHERE name = 'MSIL'
);

UPDATE atlas_driver_offer_bpp.merchant_message
SET template_id = '1007755201316229984'
WHERE message_key = 'OPERATOR_CONSENT_DEEPLINK_MESSAGE'
  AND merchant_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant
    WHERE name = 'MSIL'
);


UPDATE atlas_driver_offer_bpp.merchant_message
SET template_id = '1007072640657451118'
WHERE message_key = 'FLEET_UNLINK_SUCCESS_MESSAGE'
  AND merchant_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant
    WHERE name = 'MSIL'
);

UPDATE atlas_driver_offer_bpp.merchant_message
SET template_id = '1007221910736355623'
WHERE message_key = 'SEND_OTP'
  AND merchant_id IN (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant
    WHERE name = 'MSIL'
);