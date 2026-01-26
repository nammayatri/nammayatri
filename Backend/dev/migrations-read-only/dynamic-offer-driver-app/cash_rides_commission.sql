CREATE TABLE atlas_driver_offer_bpp.cash_rides_commission ();

ALTER TABLE atlas_driver_offer_bpp.cash_rides_commission ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cash_rides_commission ADD COLUMN currency character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cash_rides_commission ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cash_rides_commission ADD COLUMN last_settlement_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cash_rides_commission ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cash_rides_commission ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cash_rides_commission ADD COLUMN next_settlement_time timestamp with time zone NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cash_rides_commission ADD COLUMN number_of_rides integer NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cash_rides_commission ADD COLUMN payment_mode text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cash_rides_commission ADD COLUMN person_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cash_rides_commission ADD COLUMN person_role text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cash_rides_commission ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.cash_rides_commission ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.cash_rides_commission ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.cash_rides_commission ADD PRIMARY KEY ( id);
