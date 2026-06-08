UPDATE atlas_driver_offer_bpp.transporter_config
SET enable_face_match = true,
    face_match_score_threshold = 80.0
WHERE merchant_operating_city_id IN (select id from atlas_driver_offer_bpp.merchant_operating_city where merchant_short_id = 'MSIL_PARTNER');
