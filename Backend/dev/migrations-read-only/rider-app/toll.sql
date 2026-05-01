CREATE TABLE atlas_app.toll ();

ALTER TABLE atlas_app.toll ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.toll ADD COLUMN is_auto_rickshaw_allowed boolean NOT NULL default false;
ALTER TABLE atlas_app.toll ADD COLUMN is_two_wheeler_allowed boolean  default false;
ALTER TABLE atlas_app.toll ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.toll ADD COLUMN currency text ;
ALTER TABLE atlas_app.toll ADD COLUMN price double precision NOT NULL;
ALTER TABLE atlas_app.toll ADD COLUMN toll_end_gates text[] NOT NULL;
ALTER TABLE atlas_app.toll ADD COLUMN toll_start_gates text[] NOT NULL;
ALTER TABLE atlas_app.toll ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.toll ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.toll ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.toll ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.toll ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.toll ALTER COLUMN merchant_operating_city_id TYPE text;
ALTER TABLE atlas_app.toll ALTER COLUMN merchant_id TYPE text;