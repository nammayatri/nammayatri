
ALTER table atlas_bpp_dashboard.merchant ADD COLUMN is_strong_name_check_required boolean DEFAULT true;

UPDATE atlas_bpp_dashboard.merchant SET is_strong_name_check_required = false where short_id = 'JATRI_SAATHI_PARTNER';