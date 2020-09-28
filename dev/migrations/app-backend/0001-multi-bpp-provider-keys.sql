-- With per-transporter BPP endpoints, we need per-transporter API keys
INSERT INTO atlas_app.organization
  (id, name, type, status, verified, enabled, callback_url, callback_api_key, created_at, updated_at)
  VALUES
    ('70c76e36-f035-46fd-98a7-572dc8934323', '[A] Transporter #1', 'PROVIDER',
      'APPROVED', true, true,
      'http://localhost:8014/v1/7f7896dd-787e-4a0b-8675-e9e6fe93bb8f','bpp-1-key',
      '2020-09-28 16:05:57.92753+00', '2020-09-28 16:05:57.92753+00');

INSERT INTO atlas_app.organization
  (id, name, type, status, verified, enabled, callback_url, callback_api_key, created_at, updated_at)
  VALUES
    ('1257a139-6039-40b8-8752-96a77311f645', '[A] Transporter #2', 'PROVIDER',
      'APPROVED', true, true,
      'http://localhost:8014/v1/e1f37274-f0aa-4bb3-93a0-2476349487b7','bpp-2-key',
      '2020-09-28 16:05:57.92753+00', '2020-09-28 16:05:57.92753+00');
