ALTER TABLE atlas_app.person
ADD COLUMN referred_by_customer text;

ALTER TABLE atlas_app.person
ADD COLUMN customer_referral_code text;

CREATE INDEX idx_customer_referral_code ON atlas_app.person USING btree (customer_referral_code);