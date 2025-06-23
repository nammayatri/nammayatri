CREATE TABLE atlas_driver_offer_bpp.trip_alert_request ();

ALTER TABLE atlas_driver_offer_bpp.trip_alert_request ADD COLUMN alert_request_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_alert_request ADD COLUMN alert_request_type text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_alert_request ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.trip_alert_request ADD COLUMN driver_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_alert_request ADD COLUMN fleet_owner_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_alert_request ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_alert_request ADD COLUMN is_violated boolean NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_alert_request ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_alert_request ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_alert_request ADD COLUMN route_code text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_alert_request ADD COLUMN trip_transaction_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.trip_alert_request ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.trip_alert_request ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.trip_alert_request ADD COLUMN fleet_badge_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.trip_alert_request ADD COLUMN conductor_fleet_badge_id character varying(36) ;


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.trip_alert_request ADD COLUMN alert_status text ;