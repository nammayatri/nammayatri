CREATE TABLE atlas_app.dispatcher_history ();

ALTER TABLE atlas_app.dispatcher_history ADD COLUMN conductor_code character varying(255) ;
ALTER TABLE atlas_app.dispatcher_history ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.dispatcher_history ADD COLUMN current_vehicle character varying(255) NOT NULL;
ALTER TABLE atlas_app.dispatcher_history ADD COLUMN depot_id character varying(255) NOT NULL;
ALTER TABLE atlas_app.dispatcher_history ADD COLUMN dispatcher_id character varying(255) NOT NULL;
ALTER TABLE atlas_app.dispatcher_history ADD COLUMN driver_code character varying(255) ;
ALTER TABLE atlas_app.dispatcher_history ADD COLUMN id character varying(255) NOT NULL;
ALTER TABLE atlas_app.dispatcher_history ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.dispatcher_history ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.dispatcher_history ADD COLUMN reason_content character varying(1000) ;
ALTER TABLE atlas_app.dispatcher_history ADD COLUMN reason_tag character varying(255) NOT NULL;
ALTER TABLE atlas_app.dispatcher_history ADD COLUMN replaced_vehicle character varying(255) NOT NULL;
ALTER TABLE atlas_app.dispatcher_history ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.dispatcher_history ADD COLUMN waybill_no character varying(255) ;
ALTER TABLE atlas_app.dispatcher_history ADD PRIMARY KEY ( id);
CREATE INDEX dispatcher_history_idx_dispatcher_id ON atlas_app.dispatcher_history USING btree (dispatcher_id);


------- SQL updates -------

CREATE INDEX dispatcher_history_idx_depot_id ON atlas_app.dispatcher_history USING btree (depot_id);