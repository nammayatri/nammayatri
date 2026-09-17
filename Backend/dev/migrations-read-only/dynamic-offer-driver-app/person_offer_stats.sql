CREATE TABLE atlas_driver_offer_bpp.person_offer_stats ();

ALTER TABLE atlas_driver_offer_bpp.person_offer_stats ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.person_offer_stats ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_offer_stats ADD COLUMN offer_applied_count integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_offer_stats ADD COLUMN offer_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_offer_stats ADD COLUMN person_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_offer_stats ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.person_offer_stats ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.person_offer_stats ADD COLUMN entity_type text ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.person_offer_stats ALTER COLUMN entity_type SET DEFAULT 'Person';


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.person_offer_stats ADD COLUMN total_discount_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.person_offer_stats ADD COLUMN total_cashback_amount double precision ;
ALTER TABLE atlas_driver_offer_bpp.person_offer_stats ADD COLUMN currency text ;
CREATE INDEX CONCURRENTLY person_offer_stats_idx_entity_type_person_id ON atlas_driver_offer_bpp.person_offer_stats USING btree (entity_type, person_id);
CREATE INDEX CONCURRENTLY person_offer_stats_idx_person_id ON atlas_driver_offer_bpp.person_offer_stats USING btree (person_id);
ALTER TABLE atlas_driver_offer_bpp.person_offer_stats ADD CONSTRAINT person_offer_stats_unique_idx_entity_type_offer_id_person_id UNIQUE (entity_type, offer_id, person_id);