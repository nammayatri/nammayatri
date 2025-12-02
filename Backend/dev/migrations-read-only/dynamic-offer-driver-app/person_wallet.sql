CREATE TABLE atlas_driver_offer_bpp.person_wallet ();

ALTER TABLE atlas_driver_offer_bpp.person_wallet ADD COLUMN cash_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_wallet ADD COLUMN cash_from_points_redemption double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_wallet ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.person_wallet ADD COLUMN expired_balance double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_wallet ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_wallet ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_wallet ADD COLUMN merchant_operating_city_id text ;
ALTER TABLE atlas_driver_offer_bpp.person_wallet ADD COLUMN person_id text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_wallet ADD COLUMN points_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_wallet ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.person_wallet ADD COLUMN usable_cash_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_wallet ADD COLUMN usable_points_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.person_wallet ADD PRIMARY KEY ( id);
