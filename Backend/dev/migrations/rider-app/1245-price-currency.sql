-- ALTER TABLE atlas_app.booking ADD COLUMN currency character varying(255);

ALTER TABLE atlas_app.estimate ADD COLUMN currency character varying(255);
ALTER TABLE atlas_app.estimate ADD COLUMN night_shift_charge_amount double precision;
ALTER TABLE atlas_app.estimate ADD COLUMN waiting_charge_per_min_amount double precision;

ALTER TABLE atlas_app.estimate_breakup ADD COLUMN currency character varying(255);

ALTER TABLE atlas_app.quote ADD COLUMN currency character varying(255);