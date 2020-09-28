-- Add an API key for the app/gateway to provide and identify against this
-- transporter's Beckn BPP API
UPDATE atlas_transporter.organization
  SET
    api_key = 'bpp-1-key'
  WHERE
    id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f';

-- The admin was incorrectly assigned to the gateway instead of the provider
-- organisation
UPDATE atlas_transporter.person
  SET
    organization_id = '7f7896dd-787e-4a0b-8675-e9e6fe93bb8f'
  WHERE
    id = 'ec34eede-5a3e-4a41-89d4-7290a0d7a629';

-- Add a second transporter
INSERT INTO atlas_transporter.organization
  (id, name, status, type, domain, verified, enabled, location_id, mobile_number, mobile_country_code, api_key, created_at, updated_at)
  VALUES
  ('e1f37274-f0aa-4bb3-93a0-2476349487b7', 'Another Test Cabs', 'APPROVED',
    'PROVIDER','MOBILITY', true, true, 'e95d2f36-a455-4625-bfb4-22807fefa1eb',
    '9777777777', '+91', 'bpp-2-key', '2020-07-28 16:05:57.92753+00',
    '2020-07-28 16:05:57.92753+00');

INSERT INTO atlas_transporter.person
  (id, role, gender, identifier_type, mobile_country_code, identifier, verified, status, organization_id, created_at, updated_at) VALUES
  ('a30193df-4f7c-440f-bada-4d46c396d7d0', 'ADMIN', 'UNKNOWN', 'MOBILENUMBER',
    '91', '9999988888', true, 'INACTIVE',
    'e1f37274-f0aa-4bb3-93a0-2476349487b7', '2020-06-08 18:37:00+00',
    '2020-06-08 18:37:00+00');
