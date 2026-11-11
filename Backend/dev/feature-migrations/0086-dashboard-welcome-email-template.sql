-- Seed the Control Centre welcome-email template (merchant_message key
-- DASHBOARD_WELCOME_EMAIL, channel EMAIL) for Bridge Amsterdam and Bridge
-- Finland (Helsinki), and make sure their operating cities have a support email.
--
-- Columns added by the NammaDSL migration for this change: merchant_message.title
-- (email subject) and merchant_message.sender_email (From address).
--
-- Placeholders substituted by the handler: {#role#}, {#email#}, {#password#},
-- {#supportEmail#}. The Control Centre link is literal text in the template and
-- is the INTEG URL: change it to that environment's real URL before running
-- this migration anywhere else (e.g. production).
--
-- Operating-city ids differ between environments, so rows are resolved by
-- merchant short id + city name.
--
-- Idempotent: support_emails is only filled when empty; the template insert
-- is ON CONFLICT DO NOTHING on the primary key.

------------------------------------------------------------
-- support_emails (first entry is used as the support contact in the email)
------------------------------------------------------------
UPDATE atlas_driver_offer_bpp.merchant_operating_city
SET support_emails = ARRAY['support@bridge.cab']
WHERE merchant_short_id = 'BRIDGE_CABS_PARTNER'
  AND city = 'Amsterdam'
  AND (support_emails IS NULL OR cardinality(support_emails) = 0);

UPDATE atlas_driver_offer_bpp.merchant_operating_city
SET support_emails = ARRAY['support@lynx.taxi']
WHERE merchant_short_id = 'BRIDGE_FINLAND_PARTNER'
  AND city = 'Helsinki'
  AND (support_emails IS NULL OR cardinality(support_emails) = 0);

------------------------------------------------------------
-- Welcome email template rows
------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.merchant_message (
  merchant_id, merchant_operating_city_id, message_key, message,
  title, sender_email, channel, domain, contains_url_button,
  created_at, updated_at
)
SELECT
  moc.merchant_id,
  moc.id,
  'DASHBOARD_WELCOME_EMAIL',
  $body$Hi! Welcome to Control Centre

Your role is {#role#}

Your user email Login is: {#email#}
Your password is: {#password#}

Change password from here: https://control-center.integ.moving.tech/dashboard

In case of any queries, please email us at {#supportEmail#}$body$,
  'Welcome to Control Centre',
  CASE moc.merchant_short_id
    WHEN 'BRIDGE_CABS_PARTNER' THEN 'noreply@bridge.cab'
    WHEN 'BRIDGE_FINLAND_PARTNER' THEN 'noreply@lynx.taxi'
  END,
  'EMAIL',
  'GENERAL',
  false,
  now(),
  now()
FROM atlas_driver_offer_bpp.merchant_operating_city moc
WHERE (moc.merchant_short_id = 'BRIDGE_CABS_PARTNER' AND moc.city = 'Amsterdam')
   OR (moc.merchant_short_id = 'BRIDGE_FINLAND_PARTNER' AND moc.city = 'Helsinki')
ON CONFLICT (merchant_operating_city_id, message_key) DO NOTHING;
