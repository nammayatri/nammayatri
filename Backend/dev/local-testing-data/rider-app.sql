-- Local-testing seed for rider-app (atlas_app).
--
-- Inserts ONE seed Person + ONE RegistrationToken per merchant present in
-- atlas_app.merchant. Iterates via CROSS JOIN so adding a new merchant via
-- config-sync automatically extends this seed on the next apply.
--
-- All seed riders share the same mobile number (mobile-uniqueness is not
-- enforced on atlas_app.person). The mobile is stored encrypted+hashed using
-- passetto + the codebase's standard hash salt.
--
-- Encryption: passetto /encrypt (http://127.0.0.1:8079/encrypt). The mobile
-- below decrypts to "9999900001".
-- Hash: sha256("How wonderful it is that nobody need wait a single moment
-- before starting to improve the world" || "9999900001"), hex-encoded.
--
-- Idempotent via ON CONFLICT DO NOTHING — safe to re-apply.

-- unencrypted: 9999900001
INSERT INTO atlas_app.person
  ( id
  , blocked
  , gender
  , has_taken_valid_ride
  , identifier_type
  , is_new
  , is_valid_rating
  , role
  , merchant_id
  , mobile_country_code
  , mobile_number_encrypted
  , mobile_number_hash
  , unencrypted_mobile_number
  , first_name
  , last_name
  , created_at
  , updated_at
  )
SELECT
    md5(m.id || ':seed-rider-person')::uuid::text
  , false
  , 'UNKNOWN'
  , false
  , 'MOBILENUMBER'
  , false
  , true
  , 'USER'
  , m.id
  , '+91'
    -- unencrypted: 9999900001
  , '0.1.0|1|pw8GaBhQDD7vibd+HR13eV1JNYGZ8WBb0w2b6yUC+FJVf3jkvDw+whhLUn78e37JcRT7NaXwQhJBVjRKQQ=='
  , decode('631c7fc20076835796866f0a319e6d7e2ffb08096495cce4c193a9dbe8d20199', 'hex')
  , '9999900001'
  , 'seed_rider'
  , m.short_id
  , now()
  , now()
FROM atlas_app.merchant m
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_app.person p
  WHERE p.id = md5(m.id || ':seed-rider-person')::uuid::text
);

-- One RegistrationToken per seed rider Person. Token value is deterministic
-- per merchant: 'seed-rider-token-<merchant_short_id>'.
INSERT INTO atlas_app.registration_token
  ( id
  , attempts
  , auth_expiry
  , auth_medium
  , auth_type
  , auth_value_hash
  , entity_id
  , entity_type
  , merchant_id
  , token
  , token_expiry
  , verified
  , created_at
  , updated_at
  )
SELECT
    md5(m.id || ':seed-rider-token')::uuid::text
  , 0
  , 365
  , 'SMS'
  , 'OTP'
  , '7891'
  , md5(m.id || ':seed-rider-person')::uuid::text
  , 'USER'
  , m.id
  , 'seed-rider-token-' || m.short_id
  , 365
  , true
  , now()
  , now()
FROM atlas_app.merchant m
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_app.registration_token rt
  WHERE rt.id = md5(m.id || ':seed-rider-token')::uuid::text
);

-- During-ride feedback: example questions (AC check + follow-up, overcharging). Which rides get them is decided
-- by the RIDE-FEEDBACK logic below (app_dynamic_logic_element), whose version app_dynamic_logic_rollout picks.
INSERT INTO atlas_app.ride_feedback_config
  (id, merchant_id, merchant_operating_city_id, question_key, version, question_type, title, options, acknowledgement, ui_config,
   is_follow_up_only, allowed_ride_statuses, display_trigger, action_rules, priority, max_shows_per_ride, cooldown_days, enabled)
SELECT md5(moc.id || ':rf-ac-on-check')::uuid::text, moc.merchant_id, moc.id, 'AC_ON_CHECK', 1, 'YES_NO',
  '[{"language":"ENGLISH","translation":"Is the AC switched on?"}]',
  '[{"key":"YES","label":[{"language":"ENGLISH","translation":"Yes"}]},{"key":"NO","label":[{"language":"ENGLISH","translation":"No"}],"nextQuestionKey":"AC_OFF_REASON"}]',
  '[{"language":"ENGLISH","translation":"Thanks! We''ll look into it."}]',
  '{"layout":"bottom_sheet"}',
  false, '{INPROGRESS}',
  '{"deliveryMode":"IN_APP","showAfterSecondsFromRideStart":180,"maxDistanceCoveredPct":80,"autoDismissAfterSeconds":60}',
  '[]', 10, 1, 7, true
FROM atlas_app.merchant_operating_city moc WHERE moc.city = 'Bangalore'
ON CONFLICT (merchant_operating_city_id, question_key) DO NOTHING;

INSERT INTO atlas_app.ride_feedback_config
  (id, merchant_id, merchant_operating_city_id, question_key, version, question_type, title, options, acknowledgement,
   is_follow_up_only, allowed_ride_statuses, display_trigger, action_rules, priority, max_shows_per_ride, enabled)
SELECT md5(moc.id || ':rf-ac-off-reason')::uuid::text, moc.merchant_id, moc.id, 'AC_OFF_REASON', 1, 'SINGLE_SELECT',
  '[{"language":"ENGLISH","translation":"What happened?"}]',
  '[{"key":"DRIVER_REFUSED","label":[{"language":"ENGLISH","translation":"Driver refused to switch on"}]},{"key":"NOT_WORKING","label":[{"language":"ENGLISH","translation":"AC not working"}]},{"key":"OTHER","label":[{"language":"ENGLISH","translation":"Other"}],"requiresText":true}]',
  '[{"language":"ENGLISH","translation":"Thanks! We''ll look into it."}]',
  true, '{INPROGRESS}', '{"deliveryMode":"IN_APP"}',
  '[{"ruleId":"ac_report","condition":{"in":[{"var":"answer.primaryOptionKey"},["DRIVER_REFUSED","NOT_WORKING"]]},"actions":[{"actionType":"REPORT_ISSUE_TO_BPP","params":{"issueType":"AC_RELATED_ISSUE"}},{"actionType":"TAG_RIDE","params":{"tag":"AC_COMPLAINT"}}]},{"ruleId":"ac_other_text","condition":{"!=":[{"var":"answer.text"},null]},"actions":[{"actionType":"L0_SENSITIVE_WORD_CHECK"}]}]',
  11, 1, true
FROM atlas_app.merchant_operating_city moc WHERE moc.city = 'Bangalore'
ON CONFLICT (merchant_operating_city_id, question_key) DO NOTHING;

INSERT INTO atlas_app.ride_feedback_config
  (id, merchant_id, merchant_operating_city_id, question_key, version, question_type, title, options, ui_config,
   is_follow_up_only, allowed_ride_statuses, display_trigger, action_rules, priority, max_shows_per_ride, cooldown_days, enabled)
SELECT md5(moc.id || ':rf-extra-fare-asked')::uuid::text, moc.merchant_id, moc.id, 'EXTRA_FARE_ASKED', 1, 'SINGLE_SELECT',
  '[{"language":"ENGLISH","translation":"Did the driver ask for more than the app fare?"}]',
  '[{"key":"NO","label":[{"language":"ENGLISH","translation":"No"}]},{"key":"ASKED_EXTRA","label":[{"language":"ENGLISH","translation":"Yes, asked extra"}]},{"key":"ASKED_TOLL","label":[{"language":"ENGLISH","translation":"Yes, for toll"}]}]',
  '{"layout":"poll"}',
  false, '{NEW,INPROGRESS}',
  '{"deliveryMode":"IN_APP","showAfterSecondsFromAssign":60,"showAfterSecondsFromRideStart":120}',
  '[{"ruleId":"extra_fare","condition":{"==":[{"var":"answer.primaryOptionKey"},"ASKED_EXTRA"]},"actions":[{"actionType":"REPORT_ISSUE_TO_BPP","params":{"issueType":"EXTRA_FARE_MITIGATION"}}]},{"ruleId":"toll","condition":{"==":[{"var":"answer.primaryOptionKey"},"ASKED_TOLL"]},"actions":[{"actionType":"REPORT_ISSUE_TO_BPP","params":{"issueType":"DRIVER_TOLL_RELATED_ISSUE"}}]}]',
  20, 1, 14, true
FROM atlas_app.merchant_operating_city moc WHERE moc.city = 'Chennai'
ON CONFLICT (merchant_operating_city_id, question_key) DO NOTHING;

-- RIDE-FEEDBACK logic v1: returns the question keys for the ride ({"questions": [...]}).
INSERT INTO atlas_app.app_dynamic_logic_element (domain, version, "order", description, logic)
VALUES ('RIDE-FEEDBACK', 1, 0, 'AC check for AC cabs; overcharging question in Chennai',
  '{"questions":{"merge":[{"if":[{"and":[{"==":[{"var":"booking.vehicleCategory"},"CAB"]},{"==":[{"var":"booking.isAirConditioned"},true]}]},["AC_ON_CHECK"],[]]},{"if":[{"==":[{"var":"city.cityName"},"Chennai"]},["EXTRA_FARE_ASKED"],[]]}]}}')
ON CONFLICT DO NOTHING;

-- v1 for every city without its own rollout row.
INSERT INTO atlas_app.app_dynamic_logic_rollout (domain, merchant_operating_city_id, version, percentage_rollout, time_bounds, version_description)
VALUES ('RIDE-FEEDBACK', 'default', 1, 100, 'Unbounded', 'During-ride feedback v1')
ON CONFLICT DO NOTHING;

-- ride_feedback_config is a config table edited from the dashboard: keep it out of KV so writes are
-- visible to reads immediately (as for merchant, rider_config, issue_option). Needed in every environment.
UPDATE atlas_app.system_configs
SET config_value = jsonb_set(config_value::jsonb, '{disableForKV}', (config_value::jsonb->'disableForKV') || '["ride_feedback_config"]'::jsonb)::text
WHERE id = 'kv_configs' AND NOT (config_value::jsonb->'disableForKV' ? 'ride_feedback_config');
