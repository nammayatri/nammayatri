INSERT INTO
  atlas_driver_offer_bpp.igm_config (
    id,
    merchant_id,
    expected_resolution_time,
    expected_response_time,
    gro_name,
    gro_phone,
    gro_email
  )
VALUES
  (
    atlas_driver_offer_bpp.uuid_generate_v4(),
    (select id from atlas_driver_offer_bpp.merchant where short_id = 'NAMMA_YATRI_PARTNER'),
    86400,
    3600,
    'Namma GRO',
    '1234567890',
    'gro@nammayatri.in'
  );
