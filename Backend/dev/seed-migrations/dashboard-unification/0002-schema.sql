-- ============================================================================
-- Phase 1 / 0002: create atlas_dashboard schema + table shells.
-- COPY-ONLY MIGRATION INVARIANT: source schemas are never modified; rollback
-- at any point = point servers back at the old schemas (and optionally
-- DROP SCHEMA atlas_dashboard CASCADE).
-- Table shells are cloned from atlas_bpp_dashboard (the authority side) via
-- LIKE INCLUDING ALL: columns, defaults, CHECK constraints, indexes, PK/unique
-- come along; FOREIGN KEYS do NOT (re-added below where they matter).
-- ============================================================================

CREATE SCHEMA atlas_dashboard;
-- Adjust to the deployment's role model; the servers connect with this user.
-- CREATE USER atlas_dashboard_user WITH PASSWORD '<from-vault>';
-- GRANT USAGE ON SCHEMA atlas_dashboard TO atlas_dashboard_user;

CREATE TABLE atlas_dashboard.role               (LIKE atlas_bpp_dashboard.role INCLUDING ALL);
CREATE TABLE atlas_dashboard.person             (LIKE atlas_bpp_dashboard.person INCLUDING ALL);
CREATE TABLE atlas_dashboard.merchant           (LIKE atlas_bpp_dashboard.merchant INCLUDING ALL);
CREATE TABLE atlas_dashboard.merchant_access    (LIKE atlas_bpp_dashboard.merchant_access INCLUDING ALL);
CREATE TABLE atlas_dashboard.access_matrix      (LIKE atlas_bpp_dashboard.access_matrix INCLUDING ALL);
CREATE TABLE atlas_dashboard.transaction        (LIKE atlas_bpp_dashboard.transaction INCLUDING ALL);
CREATE TABLE atlas_dashboard.registration_token (LIKE atlas_bpp_dashboard.registration_token INCLUDING ALL);
-- Entity lives on the BAP side in practice; clone its shape from there.
CREATE TABLE atlas_dashboard.entity             (LIKE atlas_bap_dashboard.entity INCLUDING ALL);

-- Re-add the FKs that existed in the sources.
ALTER TABLE atlas_dashboard.person
  ADD CONSTRAINT person_role_fk FOREIGN KEY (role_id) REFERENCES atlas_dashboard.role (id);
ALTER TABLE atlas_dashboard.access_matrix
  ADD CONSTRAINT access_matrix_role_fk FOREIGN KEY (role_id) REFERENCES atlas_dashboard.role (id);
ALTER TABLE atlas_dashboard.merchant_access
  ADD CONSTRAINT merchant_access_person_fk FOREIGN KEY (person_id) REFERENCES atlas_dashboard.person (id);

-- Traceability: merged person rows keep the BPP id; this maps retired BAP ids.
CREATE TABLE atlas_dashboard.legacy_bap_person (
bap_person_id character(36) NOT NULL,
person_id character(36) NOT NULL REFERENCES atlas_dashboard.person (id),
email_hash bytea,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT legacy_bap_person_pkey PRIMARY KEY (bap_person_id)
);

-- One UI-selected "logical merchant" resolves to a per-platform merchant id
-- (BAP/BPP short ids never overlap: NAMMA_YATRI vs NAMMA_YATRI_PARTNER).
CREATE TABLE atlas_dashboard.merchant_pair (
logical_short_id character varying(255) NOT NULL,
bap_merchant_id character(36),
bpp_merchant_id character(36),
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT merchant_pair_pkey PRIMARY KEY (logical_short_id)
);

-- NEXT: run 0003-capability-ddl.sql (the capability-framework tables for
-- this schema; same generated DDL as the per-side 0091/0062 ddl-migrations).
