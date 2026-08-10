-- ============================================================================
-- Phase 0 preflight: BAP vs BPP dashboard identity parity
-- ============================================================================
-- Compares the staged BAP side (bap_*) with the staged BPP side (bpp_*)
-- (provider-dashboard) ahead of the merge into atlas_dashboard.
--
-- Run read-only against atlas_dashboard AFTER 000a/000b/000c have staged both
-- sides as bap_*/bpp_* tables. Prod runs the dashboards on separate databases,
-- so both sides are CSV imports rather than live schemas.
--
-- Matching keys: email_hash (deterministic DbHash) primary, mobile_number_hash
-- fallback for rows with NULL email. password_hash is a DbHash (bytea) —
-- direct equality comparison is valid.
-- ============================================================================

-- ---------------------------------------------------------------------------
-- 0. Headline counts per side
-- ---------------------------------------------------------------------------
SELECT 'bap' AS side,
       count(*)                                   AS persons,
       count(*) FILTER (WHERE email_hash IS NULL) AS null_email,
       count(*) FILTER (WHERE password_hash IS NULL) AS null_password,
       count(DISTINCT role_id)                    AS distinct_roles_in_use
FROM atlas_dashboard.bap_person
UNION ALL
SELECT 'bpp',
       count(*),
       count(*) FILTER (WHERE email_hash IS NULL),
       count(*) FILTER (WHERE password_hash IS NULL),
       count(DISTINCT role_id)
FROM atlas_dashboard.bpp_person;

--  side | persons | null_email | null_password | distinct_roles_in_use
-- ------+---------+------------+---------------+-----------------------
--  bap  |     894 |          3 |             3 |                    53
--  bpp  |    1629 |         73 |           434 |                    54
-- (2 rows)


-- ---------------------------------------------------------------------------
-- 1. Data quality gate: duplicate identity keys WITHIN a side.
-- Any rows here need manual resolution before the merge script can key on
-- email_hash / mobile_number_hash.
-- ---------------------------------------------------------------------------
SELECT 'bap dup email' AS issue, encode(email_hash, 'hex') AS key, count(*)
FROM atlas_dashboard.bap_person WHERE email_hash IS NOT NULL
GROUP BY email_hash HAVING count(*) > 1
UNION ALL
SELECT 'bpp dup email', encode(email_hash, 'hex'), count(*)
FROM atlas_dashboard.bpp_person WHERE email_hash IS NOT NULL
GROUP BY email_hash HAVING count(*) > 1
UNION ALL
SELECT 'bap dup mobile', encode(mobile_number_hash, 'hex'), count(*)
FROM atlas_dashboard.bap_person
GROUP BY mobile_number_hash HAVING count(*) > 1
UNION ALL
SELECT 'bpp dup mobile', encode(mobile_number_hash, 'hex'), count(*)
FROM atlas_dashboard.bpp_person
GROUP BY mobile_number_hash HAVING count(*) > 1;

--      issue      |                               key                                | count
-- ----------------+------------------------------------------------------------------+-------
--  bap dup mobile | d7aec708e8e72b58d51d1bde63edd326c2cf36495176ad733e4f56a95fe51a02 |     2
--  bap dup mobile | da7da5b855a1d8927a12dc8a1e4c2cd585ff58155603e14435a2a757b2dfa7ce |     2
--  bap dup mobile | af0e0344df296ebbfa1534857dbe72a20c1779863bfe75813f3b0622eb83b5d8 |     2
-- (3 rows)

-- ---------------------------------------------------------------------------
-- 2. Person overlap by email_hash: both / bap-only / bpp-only.
-- bap-only rows are the ones that must be INSERTed into the merged store;
-- both-sides rows keep the BPP id (authority) + legacy_bap_person_id mapping.
-- ---------------------------------------------------------------------------
WITH bap AS (SELECT email_hash FROM atlas_dashboard.bap_person WHERE email_hash IS NOT NULL),
     bpp AS (SELECT email_hash FROM atlas_dashboard.bpp_person WHERE email_hash IS NOT NULL)
SELECT
  (SELECT count(*) FROM bap JOIN bpp USING (email_hash)) AS both_sides,
  (SELECT count(*) FROM bap WHERE email_hash NOT IN (SELECT email_hash FROM bpp)) AS bap_only,
  (SELECT count(*) FROM bpp WHERE email_hash NOT IN (SELECT email_hash FROM bap)) AS bpp_only;

--  both_sides | bap_only | bpp_only
-- ------------+----------+----------
--         620 |      271 |      936
-- ---------------------------------------------------------------------------
-- 3. THE critical check — password parity for both-sides persons.
-- 'hash_mismatch' rows get a forced password reset at cutover (never pick a
-- side silently). Also surfaces role-name divergence for the same human.
-- ---------------------------------------------------------------------------
SELECT
  CASE
    WHEN a.password_hash IS NULL AND b.password_hash IS NULL THEN 'both_null'
    WHEN a.password_hash IS NULL OR  b.password_hash IS NULL THEN 'one_null'
    WHEN a.password_hash =  b.password_hash                  THEN 'match'
    ELSE 'hash_mismatch'
  END                                          AS password_status,
  count(*)                                     AS persons,
  count(*) FILTER (WHERE ra.name <> rb.name)   AS role_name_differs
FROM atlas_dashboard.bap_person a
JOIN atlas_dashboard.bpp_person b USING (email_hash)
LEFT JOIN atlas_dashboard.bap_role ra ON ra.id = a.role_id
LEFT JOIN atlas_dashboard.bpp_role rb ON rb.id = b.role_id
WHERE a.email_hash IS NOT NULL
GROUP BY 1 ORDER BY 1;

--  password_status | persons | role_name_differs
-- -----------------+---------+-------------------
--  hash_mismatch   |      77 |                36
--  match           |     540 |               184
--  one_null        |       3 |                 3
-- (3 rows)


-- Detail list for the reset campaign (emails are encrypted; export ids and
-- decrypt out-of-band with the service key):
SELECT a.id AS bap_person_id, b.id AS bpp_person_id,
       a.first_name, a.last_name, ra.name AS bap_role, rb.name AS bpp_role
FROM atlas_dashboard.bap_person a
JOIN atlas_dashboard.bpp_person b USING (email_hash)
LEFT JOIN atlas_dashboard.bap_role ra ON ra.id = a.role_id
LEFT JOIN atlas_dashboard.bpp_role rb ON rb.id = b.role_id
WHERE a.email_hash IS NOT NULL
  AND a.password_hash IS DISTINCT FROM b.password_hash;

-- ---------------------------------------------------------------------------
-- 4. Fallback matching: NULL-email persons, matched by mobile_number_hash.
-- (Mostly fleet/OTP users on the BPP side; expect bap side to be small.)
-- ---------------------------------------------------------------------------
WITH bap AS (SELECT mobile_number_hash FROM atlas_dashboard.bap_person WHERE email_hash IS NULL),
     bpp AS (SELECT mobile_number_hash FROM atlas_dashboard.bpp_person WHERE email_hash IS NULL)
SELECT
  (SELECT count(*) FROM bap JOIN bpp USING (mobile_number_hash)) AS both_sides_by_mobile,
  (SELECT count(*) FROM bap) AS bap_null_email,
  (SELECT count(*) FROM bpp) AS bpp_null_email;

--  both_sides_by_mobile | bap_null_email | bpp_null_email
-- ----------------------+----------------+----------------
--                     0 |              3 |             73


-- ---------------------------------------------------------------------------
-- 5. Role landscape: names on both sides vs one side, with member counts.
-- Same-name roles merge to one row; one-side roles carry over as-is. Watch
-- for same name + different dashboard_access_type — needs a human decision.
-- ---------------------------------------------------------------------------
SELECT coalesce(a.name, b.name) AS role_name,
       a.dashboard_access_type  AS bap_access_type,
       b.dashboard_access_type  AS bpp_access_type,
       (SELECT count(*) FROM atlas_dashboard.bap_person p WHERE p.role_id = a.id) AS bap_members,
       (SELECT count(*) FROM atlas_dashboard.bpp_person p WHERE p.role_id = b.id) AS bpp_members,
       CASE WHEN a.id IS NULL THEN 'bpp_only'
            WHEN b.id IS NULL THEN 'bap_only'
            WHEN a.dashboard_access_type <> b.dashboard_access_type THEN 'CONFLICT'
            ELSE 'both' END AS status
FROM atlas_dashboard.bap_role a
FULL OUTER JOIN atlas_dashboard.bpp_role b ON a.name = b.name
ORDER BY status, role_name;

-- ---------------------------------------------------------------------------
-- 6. Access matrix size per side (keyed by role NAME since ids differ).
-- Feeds the capability-seed union; the seed must cover every distinct
-- (role_name, user_action_type) surviving here.
-- ---------------------------------------------------------------------------
SELECT 'bap' AS side, r.name AS role_name, count(*) AS matrix_rows
FROM atlas_dashboard.bap_access_matrix m JOIN atlas_dashboard.bap_role r ON r.id = m.role_id
GROUP BY r.name
UNION ALL
SELECT 'bpp', r.name, count(*)
FROM atlas_dashboard.bpp_access_matrix m JOIN atlas_dashboard.bpp_role r ON r.id = m.role_id
GROUP BY r.name
ORDER BY role_name, side;

-- ---------------------------------------------------------------------------
-- 7. Merchant + merchant_access union preview.
-- merchant_access rows are unioned in the merge (person may gain the union
-- of their BAP-side and BPP-side city grants — intended behavior).
-- ---------------------------------------------------------------------------
SELECT 'bap' AS side, count(DISTINCT merchant_short_id) AS merchants,
       count(*) AS access_rows, count(DISTINCT person_id) AS persons_with_access
FROM atlas_dashboard.bap_merchant_access
UNION ALL
SELECT 'bpp', count(DISTINCT merchant_short_id), count(*), count(DISTINCT person_id)
FROM atlas_dashboard.bpp_merchant_access;

SELECT coalesce(a.short_id, b.short_id) AS merchant_short_id,
       CASE WHEN a.id IS NULL THEN 'bpp_only'
            WHEN b.id IS NULL THEN 'bap_only' ELSE 'both' END AS presence
FROM atlas_dashboard.bap_merchant a
FULL OUTER JOIN atlas_dashboard.bpp_merchant b ON a.short_id = b.short_id
ORDER BY presence, merchant_short_id;
