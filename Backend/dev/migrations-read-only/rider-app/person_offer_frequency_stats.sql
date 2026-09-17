CREATE TABLE atlas_app.person_offer_frequency_stats ();

ALTER TABLE atlas_app.person_offer_frequency_stats ADD COLUMN applied_count integer NOT NULL;
ALTER TABLE atlas_app.person_offer_frequency_stats ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.person_offer_frequency_stats ADD COLUMN currency text NOT NULL;
ALTER TABLE atlas_app.person_offer_frequency_stats ADD COLUMN entity_id text NOT NULL;
ALTER TABLE atlas_app.person_offer_frequency_stats ADD COLUMN entity_type text NOT NULL;
ALTER TABLE atlas_app.person_offer_frequency_stats ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.person_offer_frequency_stats ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.person_offer_frequency_stats ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_app.person_offer_frequency_stats ADD COLUMN offer_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.person_offer_frequency_stats ADD COLUMN period_start timestamp with time zone ;
ALTER TABLE atlas_app.person_offer_frequency_stats ADD COLUMN total_cashback_amount double precision NOT NULL;
ALTER TABLE atlas_app.person_offer_frequency_stats ADD COLUMN total_discount_amount double precision NOT NULL;
ALTER TABLE atlas_app.person_offer_frequency_stats ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.person_offer_frequency_stats ADD PRIMARY KEY ( id);



------- SQL updates -------

CREATE INDEX CONCURRENTLY person_offer_frequency_stats_idx_entity_id ON atlas_app.person_offer_frequency_stats USING btree (entity_id);
ALTER TABLE atlas_app.person_offer_frequency_stats ADD CONSTRAINT person_offer_frequency_stats_unique_idx_entity_id_entity_type_offer_id UNIQUE (entity_id, entity_type, offer_id);