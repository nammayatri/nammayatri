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
    'nearest-drivers-testing-organization',
    86400,
    3600,
    'Namma GRO',
    '1234567890',
    'gro@nammayatri.in'
  );

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
    'favorit0-0000-0000-0000-00000favorit',
    86400,
    3600,
    'Namma GRO',
    '1234567890',
    'gro@nammayatri.in'
  );