CREATE TABLE atlas_app.parking_transaction ();

ALTER TABLE atlas_app.parking_transaction ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_app.parking_transaction ADD COLUMN end_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.parking_transaction ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.parking_transaction ADD COLUMN parking_lot_id text NOT NULL;
ALTER TABLE atlas_app.parking_transaction ADD COLUMN payment_order_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.parking_transaction ADD COLUMN start_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.parking_transaction ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.parking_transaction ADD COLUMN vehicle_number text NOT NULL;
ALTER TABLE atlas_app.parking_transaction ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.parking_transaction ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.parking_transaction ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.parking_transaction ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.parking_transaction ADD PRIMARY KEY ( id);
