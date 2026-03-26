-- NOTE: ONLY FOR LOCAL, NEED TO BE UPDATED PROPERLY FOR MASTER / PRODUCTION
-- Only set default std:0484 for rows that have city names (like 'Kochi') or NULL,
-- but skip rows that already have proper STD/country codes (e.g. fin:009, std:011).
UPDATE atlas_registry.subscriber
SET city = '{std:0484}'
WHERE city IS NULL
   OR city = '{}'
   OR city = '{Kochi}'
   OR NOT (city::text LIKE '{std:%' OR city::text LIKE '{fin:%' OR city::text LIKE '{ind:%');
