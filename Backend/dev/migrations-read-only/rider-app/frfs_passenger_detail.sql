CREATE TABLE atlas_app.frfs_passenger_detail ();

ALTER TABLE atlas_app.frfs_passenger_detail ADD COLUMN age integer ;
ALTER TABLE atlas_app.frfs_passenger_detail ADD COLUMN booking_id character varying(36) ;
ALTER TABLE atlas_app.frfs_passenger_detail ADD COLUMN drop_off_point_place_id text ;
ALTER TABLE atlas_app.frfs_passenger_detail ADD COLUMN gender text NOT NULL;
ALTER TABLE atlas_app.frfs_passenger_detail ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_passenger_detail ADD COLUMN is_child boolean NOT NULL;
ALTER TABLE atlas_app.frfs_passenger_detail ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_passenger_detail ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_passenger_detail ADD COLUMN name text ;
ALTER TABLE atlas_app.frfs_passenger_detail ADD COLUMN pickup_point_place_id text ;
ALTER TABLE atlas_app.frfs_passenger_detail ADD COLUMN quote_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_passenger_detail ADD COLUMN seat_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_passenger_detail ADD COLUMN seat_label text NOT NULL;
ALTER TABLE atlas_app.frfs_passenger_detail ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_passenger_detail ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_passenger_detail ADD PRIMARY KEY ( id);





------- SQL updates -------



------- SQL updates -------

ALTER TABLE atlas_app.frfs_passenger_detail ADD COLUMN id_proof_number text ;
ALTER TABLE atlas_app.frfs_passenger_detail ADD COLUMN id_proof_lookup_id text ;


------- SQL updates -------

CREATE INDEX CONCURRENTLY frfs_passenger_detail_idx_booking_id ON atlas_app.frfs_passenger_detail USING btree (booking_id);
CREATE INDEX CONCURRENTLY frfs_passenger_detail_idx_quote_id ON atlas_app.frfs_passenger_detail USING btree (quote_id);