CREATE TABLE atlas_app.offline_offer ();

ALTER TABLE atlas_app.offline_offer ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.offline_offer ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.offline_offer ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.offline_offer ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_app.offline_offer ADD COLUMN offer_code text NOT NULL;
ALTER TABLE atlas_app.offline_offer ADD COLUMN offer_id text NOT NULL;
ALTER TABLE atlas_app.offline_offer ADD COLUMN reference_id text NOT NULL;
ALTER TABLE atlas_app.offline_offer ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.offline_offer ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.offline_offer ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.offline_offer ADD COLUMN payout_amount double precision ;
ALTER TABLE atlas_app.offline_offer ADD COLUMN discount_amount double precision ;