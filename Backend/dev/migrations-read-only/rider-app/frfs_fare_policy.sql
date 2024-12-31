CREATE TABLE atlas_app.frfs_fare_policy ();

ALTER TABLE atlas_app.frfs_fare_policy ADD COLUMN type text NOT NULL;
ALTER TABLE atlas_app.frfs_fare_policy ADD COLUMN applicable_discount_ids text[] NOT NULL default '{}';
ALTER TABLE atlas_app.frfs_fare_policy ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_app.frfs_fare_policy ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_fare_policy ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_fare_policy ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.frfs_fare_policy ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_fare_policy ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.frfs_fare_policy ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.frfs_fare_policy ADD COLUMN applicable_pass_ids text[] ;


------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------




------- SQL updates -------

ALTER TABLE atlas_app.frfs_fare_policy ALTER COLUMN applicable_pass_ids SET DEFAULT '{}';


------- SQL updates -------

ALTER TABLE atlas_app.frfs_fare_policy ALTER COLUMN applicable_pass_ids DROP DEFAULT;


------- SQL updates -------

ALTER TABLE atlas_app.frfs_fare_policy ALTER COLUMN applicable_pass_ids SET DEFAULT '{}';