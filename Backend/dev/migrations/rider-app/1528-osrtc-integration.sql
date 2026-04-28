-- OSRTC Integration: Add journeyDate to frfs_search
ALTER TABLE atlas_app.frfs_search ADD COLUMN IF NOT EXISTS journey_date timestamp with time zone;

-- ONLY FOR LOCAL
-- secretKey must be the OSRTC UAT secret encrypted via Passetto (format: "0.1.0|0|<base64>").
-- Run: encrypt the raw OSRTC secretKey with your local Passetto instance and paste it below.
INSERT INTO atlas_app.integrated_bpp_config
  (id, domain, feed_key, agency_key, vehicle_category, merchant_id, merchant_operating_city_id,
   platform_type, config_json, provider_name, is_ticket_valid_on_multiple_routes,
   created_at, updated_at)
VALUES (
  'osrtc-cfg0-0000-0000-000000000001',
  'FRFS',
  'odisha_osrtc',
  'osrtc',
  'BUS',
  'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
  'namma-yatri-0-0000-0000-00000000city',
  'PARTNERORG',
  '{"tag":"OSRTC","contents":{"baseUrl":"https://osrtcuatthirdpartyapi.amnex.com/api","userName":"TP_LETRAVENUES","secretKey":"0.1.0|0|A/w<encrypted_secret>","platformId":7,"intPaymentModeId":1,"strPGType":"CC"}}',
  'OSRTC',
  false,
  now(),
  now()
)
ON CONFLICT (id) DO NOTHING;
