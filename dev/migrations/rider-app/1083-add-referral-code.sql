ALTER TABLE atlas_app.person ADD COLUMN referral_code character varying(15);
ALTER TABLE atlas_app.person ADD COLUMN referred_at timestamp with time zone;
ALTER TABLE atlas_app.person ADD COLUMN has_taken_ride bool;
UPDATE atlas_app.person SET has_taken_ride = false;
UPDATE atlas_app.person SET has_taken_ride = true WHERE id in (SELECT rider_id FROM atlas_app.booking WHERE status = 'COMPLETED')