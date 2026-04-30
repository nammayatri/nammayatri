CREATE TABLE atlas_app.svp_journey ();

ALTER TABLE atlas_app.svp_journey ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.svp_journey ADD COLUMN currency text ;
ALTER TABLE atlas_app.svp_journey ADD COLUMN entry_station_code text ;
ALTER TABLE atlas_app.svp_journey ADD COLUMN entry_time timestamp with time zone ;
ALTER TABLE atlas_app.svp_journey ADD COLUMN exit_station_code text ;
ALTER TABLE atlas_app.svp_journey ADD COLUMN exit_time timestamp with time zone ;
ALTER TABLE atlas_app.svp_journey ADD COLUMN fare_charged double precision ;
ALTER TABLE atlas_app.svp_journey ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.svp_journey ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.svp_journey ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.svp_journey ADD COLUMN rider_id character varying(36) NOT NULL;
ALTER TABLE atlas_app.svp_journey ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.svp_journey ADD COLUMN tkt_sl_no text NOT NULL;
ALTER TABLE atlas_app.svp_journey ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.svp_journey ADD PRIMARY KEY ( id);
