CREATE TABLE atlas_app.search_request_mapping (
   id CHARACTER(36) PRIMARY KEY NOT NULL,
   location_id CHARACTER VARYING(255) NOT NULL,
   tag CHARACTER VARYING(255) NOT NULL,
   entity_id CHARACTER VARYING(255) NOT NULL,
   "order" INTEGER NOT NULL,
   version CHARACTER VARYING(255) NOT NULL,
   created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
   updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
   merchant_id character(36) REFERENCES atlas_app.merchant (id),
   merchant_operating_city_id character(36) REFERENCES atlas_app.merchant_operating_city (id)
);

CREATE INDEX ON atlas_app.search_request_mapping USING btree (entity_id);