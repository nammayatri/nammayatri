CREATE TABLE atlas_app.frfs_pass_ticket_statistics ();

ALTER TABLE atlas_app.frfs_pass_ticket_statistics ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_pass_ticket_statistics ADD COLUMN date date NOT NULL;
ALTER TABLE atlas_app.frfs_pass_ticket_statistics ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_pass_ticket_statistics ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_pass_ticket_statistics ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_pass_ticket_statistics ADD COLUMN purchased_pass_payment_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_pass_ticket_statistics ADD COLUMN ticket_count integer NOT NULL;
ALTER TABLE atlas_app.frfs_pass_ticket_statistics ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_pass_ticket_statistics ADD PRIMARY KEY ( date, purchased_pass_payment_id);



------- SQL updates -------

CREATE INDEX CONCURRENTLY frfs_pass_ticket_statistics_idx_purchased_pass_payment_id ON atlas_app.frfs_pass_ticket_statistics USING btree (purchased_pass_payment_id);


------- SQL updates -------

ALTER TABLE atlas_app.frfs_pass_ticket_statistics ADD COLUMN saved_amount double precision ;
ALTER TABLE atlas_app.frfs_pass_ticket_statistics ADD COLUMN fare_amount double precision ;