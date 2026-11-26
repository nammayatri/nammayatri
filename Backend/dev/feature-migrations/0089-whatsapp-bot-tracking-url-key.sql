-- WhatsApp bot: the ride tracking link's bot_config key is renamed from
-- nyTrackingUrl (India-specific) to trackingUrl, so other countries'
-- merchants can set their own link (Domain.Types.Extra.MetaWebhookConfig).
--
-- Run BEFORE deploying the rider-app build that reads trackingUrl: until it
-- runs, the new build shows the default link for every merchant.
--
-- Copies the value; the old key stays, because pods still running the old
-- build require it. Once every pod runs the new build, it can be removed:
--   UPDATE atlas_app.meta_config SET bot_config = bot_config - 'nyTrackingUrl';
UPDATE atlas_app.meta_config
SET bot_config = bot_config || jsonb_build_object('trackingUrl', bot_config -> 'nyTrackingUrl')
WHERE bot_config ? 'nyTrackingUrl'
  AND NOT bot_config ? 'trackingUrl';
