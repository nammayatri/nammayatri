CREATE TABLE atlas_app.pt_circuit_breaker_history ();

ALTER TABLE atlas_app.pt_circuit_breaker_history ADD COLUMN api_type character varying(20) NOT NULL;
ALTER TABLE atlas_app.pt_circuit_breaker_history ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pt_circuit_breaker_history ADD COLUMN failure_count integer ;
ALTER TABLE atlas_app.pt_circuit_breaker_history ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pt_circuit_breaker_history ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.pt_circuit_breaker_history ADD COLUMN new_state character varying(20) NOT NULL;
ALTER TABLE atlas_app.pt_circuit_breaker_history ADD COLUMN previous_state character varying(20) NOT NULL;
ALTER TABLE atlas_app.pt_circuit_breaker_history ADD COLUMN pt_mode character varying(20) NOT NULL;
ALTER TABLE atlas_app.pt_circuit_breaker_history ADD COLUMN reason character varying(500) ;
ALTER TABLE atlas_app.pt_circuit_breaker_history ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.pt_circuit_breaker_history ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.pt_circuit_breaker_history ADD PRIMARY KEY ( id);
