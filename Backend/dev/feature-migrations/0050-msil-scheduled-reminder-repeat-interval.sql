-- SWS-5 recurring-reminder cadence: feeds ride_related_notification_config.repeat_interval (column added by 0855).
-- Non-NULL = reminder recurs every N sec until the broadcast lead; must stay < max scheduleTryTimes
-- (validated at config-write in Tools/ConfigPilot). Only recurring nudges get a cadence; point-in-time
-- reminders stay one-shot (NULL, untouched).
-- TODO: confirm MSIL's recurring set + cadence + city.

UPDATE atlas_driver_offer_bpp.ride_related_notification_config
SET repeat_interval = 900
WHERE on_scheduled_booking = true
  AND notification_key IN (
    'GO_ONLINE_REMINDER',
    'SCHEDULED_RIDE_REMINDER',
    'SMS_TO_GO_ONLINE_IN_SCHEDULED_RIDE'
  )
  AND merchant_operating_city_id = (
    SELECT id
    FROM atlas_driver_offer_bpp.merchant_operating_city
    WHERE merchant_short_id = 'MSIL_PARTNER'
  );
