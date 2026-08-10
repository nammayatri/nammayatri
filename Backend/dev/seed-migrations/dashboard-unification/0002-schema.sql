-- ============================================================================
-- Phase 1 / 0002: assert the atlas_dashboard tables exist, and create the
-- merge's own bookkeeping tables.
-- COPY-ONLY MIGRATION INVARIANT: source schemas are never modified; rollback
-- at any point = point servers back at the old schemas.
-- This file does NOT create the dashboard tables — atlas_dashboard is built by
-- the normal ddl-migrations before the merge runs, so here we only raise if any
-- expected table is missing. It does create legacy_bap_person,
-- legacy_bap_merchant and merchant_pair, which exist only for the merge.
-- ============================================================================


-- Adjust to the deployment's role model; the servers connect with this user.
-- CREATE USER atlas_dashboard_user WITH PASSWORD '<from-vault>';
-- GRANT USAGE ON SCHEMA atlas_dashboard TO atlas_dashboard_user;

-- The merged tables already exist in atlas_dashboard (created out-of-band by
-- someone with schema rights), so nothing is created here. Verify instead.
DO $$
DECLARE missing text;
BEGIN
  SELECT string_agg(t, ', ') INTO missing
  FROM (VALUES ('role'),('person'),('merchant'),('merchant_access'),('access_matrix'),
               ('entity'),('transaction'),('registration_token'),('capability'),
               ('merchant_operating_city'),
               ('capability_endpoint'),('role_capability'),('person_capability'),
               ('access_audit')) AS want(t)
  WHERE to_regclass('atlas_dashboard.' || t) IS NULL;
  IF missing IS NOT NULL THEN
    RAISE EXCEPTION 'atlas_dashboard is missing tables the merge writes to: %', missing;
  END IF;
END $$;

-- Traceability: merged person rows keep the BPP id; this maps retired BAP ids.
CREATE TABLE IF NOT EXISTS atlas_dashboard.legacy_bap_person (
bap_person_id character(36) NOT NULL,
person_id character(36) NOT NULL REFERENCES atlas_dashboard.person (id),
email_hash bytea,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT legacy_bap_person_pkey PRIMARY KEY (bap_person_id)
);

-- The two sides reuse merchant UUIDs (BRIDGE_CABS/BRIDGE_CABS_PARTNER share an
-- id; so do JATRI_SAATHI and YATRI_PARTNER, which are DIFFERENT merchants).
-- Both rows must survive — the dashboard resolves a merchant by the short_id in
-- the URL — so colliding BAP merchants are re-ided on the way in and every BAP
-- reference is remapped through this table.
CREATE TABLE IF NOT EXISTS atlas_dashboard.legacy_bap_merchant (
bap_merchant_id character(36) NOT NULL,
merchant_id character(36) NOT NULL,
re_ided boolean NOT NULL DEFAULT false,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT legacy_bap_merchant_pkey PRIMARY KEY (bap_merchant_id)
);

-- One UI-selected "logical merchant" resolves to a per-platform merchant id
-- (BAP/BPP short ids never overlap: NAMMA_YATRI vs NAMMA_YATRI_PARTNER).
CREATE TABLE IF NOT EXISTS atlas_dashboard.merchant_pair (
logical_short_id character varying(255) NOT NULL,
bap_merchant_id character(36),
bpp_merchant_id character(36),
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT merchant_pair_pkey PRIMARY KEY (logical_short_id)
);

-- NEXT: 0003 only if the capability tables are not already present (the check
-- above will have failed loudly if they are missing).
