CREATE TABLE atlas_app.ticket_sub_place ();

ALTER TABLE atlas_app.ticket_sub_place ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_sub_place ADD COLUMN description text ;
ALTER TABLE atlas_app.ticket_sub_place ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_sub_place ADD COLUMN is_active boolean NOT NULL default true;
ALTER TABLE atlas_app.ticket_sub_place ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.ticket_sub_place ADD COLUMN rules json ;
ALTER TABLE atlas_app.ticket_sub_place ADD COLUMN sub_place_type text NOT NULL;
ALTER TABLE atlas_app.ticket_sub_place ADD COLUMN ticket_place_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_sub_place ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_sub_place ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.ticket_sub_place ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.ticket_sub_place ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.ticket_sub_place ADD COLUMN enforced_ticket_place_id character varying(36) ;