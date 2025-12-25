--- for local testing only --
-- empty array to support all tiers
-- nothing to support none
--- for local testing only --
UPDATE atlas_driver_offer_bpp.transporter_config
SET issue_breach_config = (json_build_array (json_build_object(
  'ibCountWindowSizeInDays', 7,
  'ibDailyCooldownTimeInHours', 1,
  'ibDailyMinRidesforBlocking', 2,
  'ibDailyOffenceSuspensionTimeInHours', 1,
  'ibRateThresholdDaily', 50,
  'ibRateThresholdWeekly', 50,
  'ibWeeklyCooldownTimeInHours', 1,
  'ibWeeklyMinRidesforBlocking', 10,
  'ibWeeklyOffenceSuspensionTimeInHours', 4,
  'ibAllowedServiceTiers', json_build_array (),
  'ibIssueBreachType', 'EXTRA_FARE_MITIGATION',
  'ibBlockType', 'IBSoft',
  'ibNotifyInMins', 5
)))
WHERE merchant_id = 'favorit0-0000-0000-0000-00000favorit';


INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'ISSUE_BREACH_EXTRA_FARE_MITIGATION',
    'ISSUE_BREACH_EXTRA_FARE_MITIGATION',
    moc.merchant_id,
    moc.id,
    'You are soft blocked!',
    'Soft blocked from {#blockedSTiers#} till {#blockExpirationTime#} due to extra fare asked by you many times. Please contact support for more details.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_driver_offer_bpp.merchant_operating_city moc;
