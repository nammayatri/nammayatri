-- ============================================================================
-- MSIL driver cancellation reasons
-- ============================================================================
-- Served by GET /ui/ride/{rideId}/getCancellationReasons, which scopes on
-- message_type + merchant_id + merchant_operating_city_id and translates via the
-- driver's person.language (defaulting to ENGLISH, not the city default).
--
-- MSIL is resolved by short_id and its cities by join, so this runs correctly on
-- any stack and is a no-op where MSIL is not provisioned. Ids derive from
-- md5(city, key), so a re-run collides with itself instead of duplicating.
--
-- Naming follows the production convention (DRIVER_CANCEL_* SCREAMING_SNAKE).
-- All keys are new: `translations` is keyed globally on (message_key, language)
-- with no merchant column, so reusing an existing key would bind MSIL's wording
-- to every other merchant sharing it.
-- ============================================================================

-- 1. Drop reasons that are not MSIL's.
--    Delhi currently carries NY's six (pickup_too_far, traffic_jam,
--    customer_was_rude, ...), none of which appear in MSIL's requirement.
--    Scoped to MSIL, so no other merchant is touched.
DELETE FROM atlas_driver_offer_bpp.message_dictionary md
USING  atlas_driver_offer_bpp.merchant m
WHERE  md.merchant_id  = m.id
  AND  m.short_id      = 'MSIL_PARTNER'
  AND  md.message_type = 'CancellationReason'
  AND  md.message_key NOT IN (
         'DRIVER_CANCEL_SAFETY_OR_MISCONDUCT',
         'DRIVER_CANCEL_UNSAFE_RIDE_REQUEST',
         'DRIVER_CANCEL_EMERGENCY_OR_UNFORESEEN',
         'DRIVER_CANCEL_VEHICLE_BREAKDOWN',
         'DRIVER_CANCEL_INVALID_BOOKING',
         'DRIVER_CANCEL_PASSENGER_UNREACHABLE',
         'DRIVER_CANCEL_LOCATION_INACCESSIBLE',
         'DRIVER_CANCEL_CUSTOMER_NO_SHOW'
       );

-- 2. Seed the eight reasons for every MSIL operating city.
--
--    Seven come from the requirement's driver-cancellation list.
--    DRIVER_CANCEL_CUSTOMER_NO_SHOW is the eighth and is NOT in that list: the
--    requirement specifies a no-show fee when the customer fails to arrive and
--    the driver cancels as a result, but names no reason by which the driver can
--    declare it. Without this key that fee is unreachable.
--
--    eligibility_logic stays NULL (always visible) — no stage gating is asked for
--    on driver reasons.
INSERT INTO atlas_driver_offer_bpp.message_dictionary
  (id, merchant_id, merchant_operating_city_id, message_key, message_type)
SELECT
  substr(h,1,8) ||'-'|| substr(h,9,4) ||'-'|| substr(h,13,4) ||'-'|| substr(h,17,4) ||'-'|| substr(h,21,12),
  s.merchant_id,
  s.merchant_operating_city_id,
  s.message_key,
  'CancellationReason'
FROM (
  SELECT m.id   AS merchant_id,
         moc.id AS merchant_operating_city_id,
         k.message_key,
         md5('msil-cancel-reason:' || moc.id || ':' || k.message_key) AS h
  FROM   atlas_driver_offer_bpp.merchant m
  JOIN   atlas_driver_offer_bpp.merchant_operating_city moc ON moc.merchant_id = m.id
  CROSS  JOIN (VALUES
           ('DRIVER_CANCEL_SAFETY_OR_MISCONDUCT'),
           ('DRIVER_CANCEL_UNSAFE_RIDE_REQUEST'),
           ('DRIVER_CANCEL_EMERGENCY_OR_UNFORESEEN'),
           ('DRIVER_CANCEL_VEHICLE_BREAKDOWN'),
           ('DRIVER_CANCEL_INVALID_BOOKING'),
           ('DRIVER_CANCEL_PASSENGER_UNREACHABLE'),
           ('DRIVER_CANCEL_LOCATION_INACCESSIBLE'),
           ('DRIVER_CANCEL_CUSTOMER_NO_SHOW')
         ) AS k(message_key)
  WHERE  m.short_id = 'MSIL_PARTNER'
) s
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_driver_offer_bpp.message_dictionary e
  WHERE  e.merchant_operating_city_id = s.merchant_operating_city_id
    AND  e.message_key                = s.message_key
    AND  e.message_type               = 'CancellationReason'
);

-- 3. Translations.
--    This table has no merchant or city dimension, so rows go in unconditionally
--    rather than gated on MSIL existing: unused keys are inert, and this way the
--    strings are present if MSIL is provisioned after this migration runs.
--
--    Both languages are needed. merchant_operating_city.language is HINDI for
--    MSIL's cities, but the lookup defaults a driver with person.language = NULL
--    to ENGLISH rather than to the city default.
INSERT INTO atlas_driver_offer_bpp.translations
  (id, message_key, language, message)
SELECT
  substr(h,1,8) ||'-'|| substr(h,9,4) ||'-'|| substr(h,13,4) ||'-'|| substr(h,17,4) ||'-'|| substr(h,21,12),
  t.message_key,
  t.language,
  t.message
FROM (
  SELECT v.message_key, v.language, v.message,
         md5('msil-cancel-translation:' || v.message_key || ':' || v.language) AS h
  FROM (VALUES
    ('DRIVER_CANCEL_SAFETY_OR_MISCONDUCT',    'ENGLISH', 'Safety concern or passenger misconduct'),
    ('DRIVER_CANCEL_SAFETY_OR_MISCONDUCT',    'HINDI',   'सुरक्षा चिंता या यात्री का दुर्व्यवहार'),
    ('DRIVER_CANCEL_UNSAFE_RIDE_REQUEST',     'ENGLISH', 'Unsafe or non-compliant ride request'),
    ('DRIVER_CANCEL_UNSAFE_RIDE_REQUEST',     'HINDI',   'असुरक्षित या नियमों के विरुद्ध राइड अनुरोध'),
    ('DRIVER_CANCEL_EMERGENCY_OR_UNFORESEEN', 'ENGLISH', 'Accident, medical emergency or unforeseen event'),
    ('DRIVER_CANCEL_EMERGENCY_OR_UNFORESEEN', 'HINDI',   'दुर्घटना, चिकित्सा आपातकाल या अप्रत्याशित घटना'),
    ('DRIVER_CANCEL_VEHICLE_BREAKDOWN',       'ENGLISH', 'Vehicle breakdown or mechanical issue'),
    ('DRIVER_CANCEL_VEHICLE_BREAKDOWN',       'HINDI',   'वाहन खराब होना या यांत्रिक समस्या'),
    ('DRIVER_CANCEL_INVALID_BOOKING',         'ENGLISH', 'Incorrect, duplicate or cancelled booking'),
    ('DRIVER_CANCEL_INVALID_BOOKING',         'HINDI',   'गलत, डुप्लिकेट या रद्द बुकिंग'),
    ('DRIVER_CANCEL_PASSENGER_UNREACHABLE',   'ENGLISH', 'Passenger unreachable after multiple attempts'),
    ('DRIVER_CANCEL_PASSENGER_UNREACHABLE',   'HINDI',   'कई बार संपर्क करने पर भी यात्री उपलब्ध नहीं'),
    ('DRIVER_CANCEL_LOCATION_INACCESSIBLE',   'ENGLISH', 'Pickup or drop location inaccessible'),
    ('DRIVER_CANCEL_LOCATION_INACCESSIBLE',   'HINDI',   'पिकअप या ड्रॉप स्थान तक पहुँच नहीं'),
    ('DRIVER_CANCEL_CUSTOMER_NO_SHOW',        'ENGLISH', 'Customer did not show up'),
    ('DRIVER_CANCEL_CUSTOMER_NO_SHOW',        'HINDI',   'ग्राहक नहीं आया')
  ) AS v(message_key, language, message)
) t
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_driver_offer_bpp.translations e
  WHERE  e.message_key = t.message_key
    AND  e.language    = t.language
);

-- Verify — expect 8 rows per MSIL operating city:
--
-- SELECT moc.city, count(*)
-- FROM   atlas_driver_offer_bpp.message_dictionary md
-- JOIN   atlas_driver_offer_bpp.merchant_operating_city moc ON moc.id = md.merchant_operating_city_id
-- JOIN   atlas_driver_offer_bpp.merchant m                  ON m.id  = md.merchant_id
-- WHERE  md.message_type = 'CancellationReason' AND m.short_id = 'MSIL_PARTNER'
-- GROUP  BY moc.city;
