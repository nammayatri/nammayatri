-- The accounts the day-1 clean-up removes: the twelve simulated cars, the four
-- test passengers (two until 2026-11-18), the review accounts and the owner's
-- two Algerian test accounts (2026-11-22), and NOBODY else. Reads only.
--
--   psql -At -F'|' -f launch-cleanup-targets.sql
--
-- It lives beside the script and NOT in db/, on purpose: a release applies every
-- new db/*.sql to the live database, and this is a read of people's numbers.
--
-- It names them by (country code, number) exactly as the people who made them
-- stored them -- simulate-driver.py for the cars (Mauritania 22100001..06,
-- Algeria 0555100001..06 with the trunk zero) and the ride tests' passengers
-- (+222 22778899, +213 0555000199) and, since 2026-11-18, the virtual
-- passenger's second one per country (+222 22778898, +213 0555000198:
-- scenario 8, two requests at once). A pattern ("anything starting 2210") would
-- be shorter and one real number away from erasing a real person, so the list
-- is the list. The Algerian numbers appear with and without the trunk zero
-- because the backend accepts either and the stored form has been seen both
-- ways; the country code is part of the key, so a Mauritanian number can never
-- match an Algerian row.
--
-- One row per account: side | person_id | cc | number | first_name
WITH wanted (side, cc, num) AS (VALUES
  ('driver', '+222', '22100001'), ('driver', '+222', '22100002'), ('driver', '+222', '22100003'),
  ('driver', '+222', '22100004'), ('driver', '+222', '22100005'), ('driver', '+222', '22100006'),
  ('driver', '+213', '0555100001'), ('driver', '+213', '0555100002'), ('driver', '+213', '0555100003'),
  ('driver', '+213', '0555100004'), ('driver', '+213', '0555100005'), ('driver', '+213', '0555100006'),
  ('driver', '+213', '555100001'), ('driver', '+213', '555100002'), ('driver', '+213', '555100003'),
  ('driver', '+213', '555100004'), ('driver', '+213', '555100005'), ('driver', '+213', '555100006'),
  ('rider',  '+222', '22778899'),
  -- The two REVIEW accounts made for Google Play's reviewers (2026-10-25): +222 22000701
  -- (passenger) and 22000702 (driver). Each number may exist on either side.
  ('driver', '+222', '22000701'), ('driver', '+222', '22000702'),
  ('rider',  '+222', '22000701'), ('rider',  '+222', '22000702'),
  ('rider',  '+213', '0555000199'), ('rider', '+213', '555000199'),
  -- The virtual passenger's second test passenger per country (2026-11-18).
  ('rider',  '+222', '22778898'),
  ('rider',  '+213', '0555000198'), ('rider', '+213', '555000198'),
  -- The owner's Algerian test accounts (2026-11-22, WhatsApp banned and the
  -- office SIM silent): +213 0555000197 (passenger) and 0555000196 (driver),
  -- in SMS_BYPASS and the app's DZ_TEST_NUMBERS. Either number on either side.
  ('driver', '+213', '0555000197'), ('driver', '+213', '555000197'),
  ('driver', '+213', '0555000196'), ('driver', '+213', '555000196'),
  ('rider',  '+213', '0555000197'), ('rider', '+213', '555000197'),
  ('rider',  '+213', '0555000196'), ('rider', '+213', '555000196'),
  -- Two more test passengers (2026-11-25), to test two requests at once.
  ('rider',  '+213', '0555000195'), ('rider', '+213', '555000195'),
  ('rider',  '+213', '0555000194'), ('rider', '+213', '555000194')
)
SELECT 'driver', p.id::text, p.mobile_country_code, p.unencrypted_mobile_number, coalesce(p.first_name, '')
  FROM atlas_driver_offer_bpp.person p
  JOIN wanted w ON w.side = 'driver'
               AND w.cc = p.mobile_country_code
               AND w.num = p.unencrypted_mobile_number
 WHERE p.role = 'DRIVER'
UNION ALL
SELECT 'rider', p.id::text, p.mobile_country_code, p.unencrypted_mobile_number, coalesce(p.first_name, '')
  FROM atlas_app.person p
  JOIN wanted w ON w.side = 'rider'
               AND w.cc = p.mobile_country_code
               AND w.num = p.unencrypted_mobile_number
ORDER BY 1, 3, 4;
