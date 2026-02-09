CREATE TABLE atlas_app.frfs_ticket ();

ALTER TABLE atlas_app.frfs_ticket ADD COLUMN frfs_ticket_booking_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket ADD COLUMN qr_data text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_ticket ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket ADD COLUMN ticket_number text NOT NULL;
ALTER TABLE atlas_app.frfs_ticket ADD COLUMN valid_till timestamp with time zone NOT NULL;
ALTER TABLE atlas_app.frfs_ticket ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.frfs_ticket ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.frfs_ticket ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_ticket ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_ticket ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket ADD COLUMN partner_org_transaction_id character varying(36) ;
ALTER TABLE atlas_app.frfs_ticket ADD COLUMN partner_org_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket ALTER COLUMN merchant_operating_city_id SET NOT NULL;
ALTER TABLE atlas_app.frfs_ticket ALTER COLUMN merchant_id SET NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket ADD COLUMN is_ticket_free boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket ADD COLUMN qr_refresh_at timestamp with time zone ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket ADD COLUMN description text ;



------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket ADD COLUMN scanned_by_vehicle_number text ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket ADD COLUMN commencing_hours integer ;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_ticket ADD COLUMN is_return_ticket boolean ;