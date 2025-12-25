INSERT INTO
  atlas_app.igm_config (
    id,
    merchant_id,
    expected_resolution_time,
    expected_response_time,
    gro_name,
    gro_phone,
    gro_email
  )
VALUES
  ( atlas_app.uuid_generate_v4(),
    (select id from atlas_app.merchant where short_id = 'NAMMA_YATRI'),
    86400,
    3600,
    'Namma GRO',
    '1234567890',
    'gro@nammayatri.in'
  );
