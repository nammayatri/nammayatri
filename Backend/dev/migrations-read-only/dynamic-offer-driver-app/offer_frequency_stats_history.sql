CREATE TABLE atlas_driver_offer_bpp.offer_frequency_stats_history ();

ALTER TABLE atlas_driver_offer_bpp.offer_frequency_stats_history ADD COLUMN applied_count integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer_frequency_stats_history ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.offer_frequency_stats_history ADD COLUMN currency text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer_frequency_stats_history ADD COLUMN entity_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer_frequency_stats_history ADD COLUMN entity_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer_frequency_stats_history ADD COLUMN frequency_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer_frequency_stats_history ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer_frequency_stats_history ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer_frequency_stats_history ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer_frequency_stats_history ADD COLUMN offer_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer_frequency_stats_history ADD COLUMN period_end timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer_frequency_stats_history ADD COLUMN period_start timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer_frequency_stats_history ADD COLUMN total_cashback_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer_frequency_stats_history ADD COLUMN total_discount_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.offer_frequency_stats_history ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.offer_frequency_stats_history ADD PRIMARY KEY ( id);



------- SQL updates -------

CREATE INDEX CONCURRENTLY offer_frequency_stats_history_idx_entity_id ON atlas_driver_offer_bpp.offer_frequency_stats_history USING btree (entity_id);
ALTER TABLE atlas_driver_offer_bpp.offer_frequency_stats_history ADD CONSTRAINT offer_frequency_stats_history_unique_idx_entity_id_entity_type_frequency_type_offer_id_period_start UNIQUE (entity_id, entity_type, frequency_type, offer_id, period_start);