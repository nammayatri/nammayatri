-- Single-login across platforms: logical merchant -> per-platform merchant
-- rows (see Domain.Types.MerchantPair). Empty on this pre-merge schema by
-- design — Tools.Auth.Api falls back to legacy single-merchant behavior when
-- no pair row exists. Seeded in atlas_dashboard by the Phase 1 merge.
CREATE TABLE atlas_bap_dashboard.merchant_pair (
logical_short_id character varying(255) NOT NULL,
bap_merchant_id character(36),
bpp_merchant_id character(36),
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT merchant_pair_pkey PRIMARY KEY (logical_short_id)
);
ALTER TABLE atlas_bap_dashboard.merchant_pair OWNER TO atlas_bap_dashboard_user;
