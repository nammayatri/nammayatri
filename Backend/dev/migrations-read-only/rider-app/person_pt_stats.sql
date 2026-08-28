CREATE TABLE atlas_app.person_pt_stats ();

ALTER TABLE atlas_app.person_pt_stats ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.person_pt_stats ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.person_pt_stats ADD COLUMN last_purchased_at timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.person_pt_stats ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.person_pt_stats ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.person_pt_stats ADD COLUMN pass_type_id character varying(36) ;
ALTER TABLE atlas_app.person_pt_stats ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.person_pt_stats ADD COLUMN product_type text NOT NULL;
ALTER TABLE atlas_app.person_pt_stats ADD COLUMN purchase_count integer NOT NULL;
ALTER TABLE atlas_app.person_pt_stats ADD COLUMN static_person_id text NOT NULL;
ALTER TABLE atlas_app.person_pt_stats ADD COLUMN ticket_count integer ;
ALTER TABLE atlas_app.person_pt_stats ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.person_pt_stats ADD COLUMN vehicle_service_tier_type text ;
ALTER TABLE atlas_app.person_pt_stats ADD COLUMN vehicle_type text ;
ALTER TABLE atlas_app.person_pt_stats ADD PRIMARY KEY ( id);
CREATE INDEX CONCURRENTLY person_pt_stats_idx_pass_type_id_product_type_static_person_id_vehicle_service_tier_type_vehicle_type ON atlas_app.person_pt_stats USING btree (pass_type_id, product_type, static_person_id, vehicle_service_tier_type, vehicle_type);
CREATE INDEX CONCURRENTLY person_pt_stats_idx_static_person_id ON atlas_app.person_pt_stats USING btree (static_person_id);


------- SQL updates -------

CREATE INDEX CONCURRENTLY person_pt_stats_idx_person_id ON atlas_app.person_pt_stats USING btree (person_id);


------- SQL updates -------

ALTER TABLE atlas_app.person_pt_stats ADD COLUMN person_created_at timestamp with time zone ;