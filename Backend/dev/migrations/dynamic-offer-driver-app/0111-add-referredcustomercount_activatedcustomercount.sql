ALTER TABLE atlas_driver_offer_bpp.driver_referral ADD COLUMN referred_customer_count integer not null default 0;
ALTER TABLE atlas_driver_offer_bpp.driver_referral ADD COLUMN activated_customer_count integer not null default 0;
