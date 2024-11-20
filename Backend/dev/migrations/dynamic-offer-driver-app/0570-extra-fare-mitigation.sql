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
  'ibIssueBreachType', 'EXTRA_FARE_MITIGATION'
)))
WHERE merchant_id = 'favorit0-0000-0000-0000-00000favorit';