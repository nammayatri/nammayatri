ALTER TABLE atlas_app.person ADD COLUMN referral_code character varying(15);
ALTER TABLE atlas_app.person ADD COLUMN referral_at timestamp with time zone;
ALTER TABLE atlas_app.person ADD COLUMN has_taken_ride bool DEFAULT False NOT NULL;


