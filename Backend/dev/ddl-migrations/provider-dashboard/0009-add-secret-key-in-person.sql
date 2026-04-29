ALTER TABLE atlas_bpp_dashboard.merchant_access ADD COLUMN secret_key character varying(255);
ALTER TABLE atlas_bpp_dashboard.merchant_access ADD COLUMN is2fa_enabled Boolean NOT NULL DEFAULT false;
ALTER TABLE atlas_bpp_dashboard.merchant ADD COLUMN is2fa_mandatory Boolean NOT NULL DEFAULT false;