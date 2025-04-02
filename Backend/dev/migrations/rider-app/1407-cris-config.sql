-- ONLY FOR LOCAL TESTING --

INSERT INTO
  atlas_app.integrated_bpp_config(
    id,
    domain,
    merchant_id,
    merchant_operating_city_id,
    vehicle_category,
    platform_type,
    config_json,
    created_at,
    updated_at
  )
VALUES
  (
    'vk59oufg-t64j-1754-v43z-d7c5mfyawo07',
    'FRFS',
    'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
    'namma-yatri-0-0000-0000-00000000city',
    'SUBWAY',
    'MULTIMODAL',
    '{"tag": "CRIS", "contents": {"clientKey": "2F4kflvIX4PRQYMxEcB9E69vA8R3tzqk", "clientSecret": "TUpqQ3NWb1o1M0lZTlVmZWxWQ3BXdldXQWtnYTpfREdIMlM1OWxRU2pRb2ZvQmRBSDN0eW9aT3Nh", "baseUrl": "https://gw.crisapis.indianrail.gov.in"}}',
    now(),
    now()
  );