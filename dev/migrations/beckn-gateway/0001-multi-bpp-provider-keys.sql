-- The transporter now gets its own Beckn BPP endpoint
UPDATE atlas_gateway.organization
  SET
    id = 'a30193df-4f7c-440f-bada-4d46c396d7d0',
    name = '[G] Transporter #1',
    callback_url = 'http://localhost:8014/v1/7f7896dd-787e-4a0b-8675-e9e6fe93bb8f',
    callback_api_key = 'bpp-1-key'
  WHERE
    id = 'mobility-provider';

-- Add a second transporter
INSERT INTO atlas_gateway.organization
  (id, name, type, status, verified, enabled, callback_url, callback_api_key, created_at, updated_at)
  VALUES
    ('83dde90a-81d2-404b-ada5-20aac58005e6', '[G] Transporter #2', 'PROVIDER', 'APPROVED', true, true,
      'http://localhost:8014/v1/e1f37274-f0aa-4bb3-93a0-2476349487b7','bpp-2-key',
      '2020-09-28 16:05:57.92753+00', '2020-09-28 16:05:57.92753+00');
