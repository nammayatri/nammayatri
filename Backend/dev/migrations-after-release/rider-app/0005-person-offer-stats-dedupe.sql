-- Collapse duplicate person_offer_stats rows.
--
-- Duplicates arose while enableKVPG carried an empty secondary-key list for
-- this table: the (offer_id, person_id, entity_type) lookup in upsertOfferStats
-- could come back empty from KV, so each apply inserted a fresh row instead of
-- incrementing the existing one. The counts fragmented across rows, which makes
-- countUses under-report and lets a rider exceed maxApplyCount.
--
-- Step 1 sums each group into its earliest row; step 2 deletes the rest.
-- The order matters: swapping them loses the fragmented counts.
--
-- MUST run before the generated unique index on
-- (entity_type, offer_id, person_id), which fails while duplicates remain.

WITH ranked AS (
  SELECT
    id,
    ROW_NUMBER() OVER (
      PARTITION BY offer_id, person_id, entity_type
      ORDER BY created_at, id
    ) AS rn,
    COUNT(*) OVER (
      PARTITION BY offer_id, person_id, entity_type
    ) AS grp_size,
    SUM(offer_applied_count) OVER (
      PARTITION BY offer_id, person_id, entity_type
    ) AS total_applied
  FROM atlas_app.person_offer_stats
)
UPDATE atlas_app.person_offer_stats s
SET offer_applied_count = r.total_applied,
    updated_at = NOW()
FROM ranked r
WHERE s.id = r.id
  AND r.rn = 1
  AND r.grp_size > 1;

DELETE FROM atlas_app.person_offer_stats s
USING (
  SELECT
    id,
    ROW_NUMBER() OVER (
      PARTITION BY offer_id, person_id, entity_type
      ORDER BY created_at, id
    ) AS rn
  FROM atlas_app.person_offer_stats
) r
WHERE s.id = r.id
  AND r.rn > 1;
