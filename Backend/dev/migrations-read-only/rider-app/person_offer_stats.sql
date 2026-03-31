CREATE TABLE atlas_app.person_offer_stats ();

ALTER TABLE atlas_app.person_offer_stats ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.person_offer_stats ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.person_offer_stats ADD COLUMN offer_applied_count integer NOT NULL;
ALTER TABLE atlas_app.person_offer_stats ADD COLUMN offer_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.person_offer_stats ADD COLUMN person_id text NOT NULL;
ALTER TABLE atlas_app.person_offer_stats ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.person_offer_stats ADD PRIMARY KEY ( id);
