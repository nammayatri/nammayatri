-- ============================================================================
-- Rider cancellation reason translations
-- ============================================================================
-- When a rider cancels, Ride/Common.hs resolveLabel renders the reason back to
-- the driver by looking up translations on message_key. Without these rows the
-- driver sees the raw key.
--
-- Unlike the driver set (0047) these are NOT seeded into message_dictionary:
-- the rider picks them in the Buyer App, not in ours, so we never present the
-- list. We only ever render one back.
--
-- Two groups, matching the two halves of the resolution ladder:
--
--   ONDC_*         the eight buyer-side codes ONDC defines (000-007). Wording
--                  follows ONDC's own definitions, not MSIL's matrix labels —
--                  the code means what ONDC says it means, and the fee matrix
--                  decides separately whether it charges.
--
--   RIDER_CANCEL_* the five reasons in MSIL's matrix with no ONDC equivalent.
--                  Provisional; see SharedLogic.RiderCancellationReason.
--
-- translations is keyed globally on (message_key, language) with no merchant
-- column, so both prefixes keep these clear of other merchants' vocabularies.
-- Ids derive from md5(key, language), so a re-run collides with itself.
-- ============================================================================

INSERT INTO atlas_driver_offer_bpp.translations
  (id, message_key, language, message)
SELECT
  substr(h,1,8) ||'-'|| substr(h,9,4) ||'-'|| substr(h,13,4) ||'-'|| substr(h,17,4) ||'-'|| substr(h,21,12),
  t.message_key,
  t.language,
  t.message
FROM (
  SELECT v.message_key, v.language, v.message,
         md5('rider-cancel-translation:' || v.message_key || ':' || v.language) AS h
  FROM (VALUES
    -- ONDC buyer-side codes 000-007
    ('ONDC_TECHNICAL_CANCELLATION',             'ENGLISH', 'Technical cancellation'),
    ('ONDC_TECHNICAL_CANCELLATION',             'HINDI',   'तकनीकी कारण से रद्द'),
    ('ONDC_DRIVER_NOT_MOVING',                  'ENGLISH', 'Driver was not moving'),
    ('ONDC_DRIVER_NOT_MOVING',                  'HINDI',   'चालक आगे नहीं बढ़ा'),
    ('ONDC_DRIVER_NOT_REACHABLE',               'ENGLISH', 'Customer was unable to contact the driver'),
    ('ONDC_DRIVER_NOT_REACHABLE',               'HINDI',   'ग्राहक चालक से संपर्क नहीं कर सका'),
    ('ONDC_DRIVER_ASKED_TO_CANCEL',             'ENGLISH', 'Driver asked the customer to cancel'),
    ('ONDC_DRIVER_ASKED_TO_CANCEL',             'HINDI',   'चालक ने ग्राहक से रद्द करने को कहा'),
    ('ONDC_INCORRECT_PICKUP_LOCATION',          'ENGLISH', 'Pickup location was incorrect'),
    ('ONDC_INCORRECT_PICKUP_LOCATION',          'HINDI',   'पिकअप स्थान गलत था'),
    ('ONDC_BOOKED_BY_MISTAKE',                  'ENGLISH', 'Customer booked the ride by mistake'),
    ('ONDC_BOOKED_BY_MISTAKE',                  'HINDI',   'ग्राहक ने गलती से राइड बुक की'),
    ('ONDC_SAFETY_CONCERN_WITH_DRIVER_OR_RIDE', 'ENGLISH', 'Safety concern with the driver or ride'),
    ('ONDC_SAFETY_CONCERN_WITH_DRIVER_OR_RIDE', 'HINDI',   'चालक या राइड को लेकर सुरक्षा चिंता'),
    ('ONDC_VEHICLE_UNSAFE_OR_NON_COMPLIANT',    'ENGLISH', 'Vehicle appeared unsafe or non-compliant'),
    ('ONDC_VEHICLE_UNSAFE_OR_NON_COMPLIANT',    'HINDI',   'वाहन असुरक्षित या नियमों के विरुद्ध लगा'),

    -- MSIL matrix reasons with no ONDC code (provisional)
    ('RIDER_CANCEL_MEDICAL_EMERGENCY',          'ENGLISH', 'Medical emergency or accident'),
    ('RIDER_CANCEL_MEDICAL_EMERGENCY',          'HINDI',   'चिकित्सा आपातकाल या दुर्घटना'),
    ('RIDER_CANCEL_UNEXPECTED_EVENT',           'ENGLISH', 'Unexpected event prevented the ride'),
    ('RIDER_CANCEL_UNEXPECTED_EVENT',           'HINDI',   'अप्रत्याशित घटना के कारण राइड संभव नहीं'),
    ('RIDER_CANCEL_NO_LONGER_REQUIRED',         'ENGLISH', 'Ride was no longer required'),
    ('RIDER_CANCEL_NO_LONGER_REQUIRED',         'HINDI',   'अब राइड की आवश्यकता नहीं थी'),
    ('RIDER_CANCEL_FOUND_ANOTHER_RIDE',         'ENGLISH', 'Customer found another ride'),
    ('RIDER_CANCEL_FOUND_ANOTHER_RIDE',         'HINDI',   'ग्राहक को दूसरी राइड मिल गई'),
    ('RIDER_CANCEL_OTHER',                      'ENGLISH', 'Other reason'),
    ('RIDER_CANCEL_OTHER',                      'HINDI',   'अन्य कारण')
  ) AS v(message_key, language, message)
) t
WHERE NOT EXISTS (
  SELECT 1 FROM atlas_driver_offer_bpp.translations e
  WHERE  e.message_key = t.message_key
    AND  e.language    = t.language
);
