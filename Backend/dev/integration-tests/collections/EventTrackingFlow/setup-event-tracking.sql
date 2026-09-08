-- Seed for the EventTrackingFlow suite: all three event providers enabled for every city, pointed at
-- the local mock server. Secrets are dev-key Passetto blobs (values irrelevant). Both tables are
-- ConfigPilot-cached: flush Redis and restart rider-app after applying. See Rules.md.

INSERT INTO atlas_app.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json, created_at, updated_at)
SELECT
  moc.merchant_id,
  moc.id,
  svc.service_name,
  svc.config_json,
  now(),
  now()
FROM atlas_app.merchant_operating_city moc
CROSS JOIN (
  VALUES
    (
      'EventTracking_Moengage',
      '{
        "baseUrl": "http://localhost:8080/moengage",
        "appId": "test-moengage-app",
        "apiSecret": "0.1.0|0|oJOzop+9gdchzwbhz/EyxkSZ7s4z/irFEpsQrsNmSXbKnfe96m+P9xkFqy8/BFU1sGUhgszM1JKsuJNXBQ==",
        "enabled": true
      }'::jsonb
    ),
    (
      'EventTracking_Clevertap',
      '{
        "baseUrl": "http://localhost:8080/clevertap",
        "accountId": "test-clevertap-account",
        "passcode": "0.1.0|0|oJOzop+9gdchzwbhz/EyxkSZ7s4z/irFEpsQrsNmSXbKnfe96m+P9xkFqy8/BFU1sGUhgszM1JKsuJNXBQ==",
        "enabled": true
      }'::jsonb
    ),
    (
      'EventTracking_FirebaseAnalytics',
      '{
        "baseUrl": "http://localhost:8080/firebase",
        "apps": [
          {
            "platform": "ANDROID",
            "firebaseAppId": "1:1:android:test",
            "apiSecret": "0.1.0|0|oJOzop+9gdchzwbhz/EyxkSZ7s4z/irFEpsQrsNmSXbKnfe96m+P9xkFqy8/BFU1sGUhgszM1JKsuJNXBQ=="
          },
          {
            "platform": "IOS",
            "firebaseAppId": "1:1:ios:test",
            "apiSecret": "0.1.0|0|oJOzop+9gdchzwbhz/EyxkSZ7s4z/irFEpsQrsNmSXbKnfe96m+P9xkFqy8/BFU1sGUhgszM1JKsuJNXBQ=="
          }
        ],
        "enabled": true,
        "debug": false
      }'::jsonb
    )
) AS svc (service_name, config_json)
ON CONFLICT (service_name, merchant_operating_city_id) DO UPDATE
SET config_json = EXCLUDED.config_json,
    updated_at = now();

UPDATE atlas_app.merchant_service_usage_config
SET event_tracking_providers = '{Moengage,Clevertap,FirebaseAnalytics}',
    event_tracking_overrides = NULL,
    updated_at = now();
