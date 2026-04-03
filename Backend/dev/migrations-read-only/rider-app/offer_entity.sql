CREATE TABLE atlas_app.offer_entity ();

ALTER TABLE atlas_app.offer_entity ADD COLUMN amount_saved double precision NOT NULL;
ALTER TABLE atlas_app.offer_entity ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.offer_entity ADD COLUMN discount_amount double precision NOT NULL;
ALTER TABLE atlas_app.offer_entity ADD COLUMN entity_id text NOT NULL;
ALTER TABLE atlas_app.offer_entity ADD COLUMN entity_type text NOT NULL;
ALTER TABLE atlas_app.offer_entity ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.offer_entity ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.offer_entity ADD COLUMN merchant_operating_city_id text NOT NULL;
ALTER TABLE atlas_app.offer_entity ADD COLUMN offer_code text NOT NULL;
ALTER TABLE atlas_app.offer_entity ADD COLUMN offer_description text ;
ALTER TABLE atlas_app.offer_entity ADD COLUMN offer_id text NOT NULL;
ALTER TABLE atlas_app.offer_entity ADD COLUMN offer_sponsored_by text ;
ALTER TABLE atlas_app.offer_entity ADD COLUMN offer_title text ;
ALTER TABLE atlas_app.offer_entity ADD COLUMN offer_tnc text ;
ALTER TABLE atlas_app.offer_entity ADD COLUMN payout_amount double precision NOT NULL;
ALTER TABLE atlas_app.offer_entity ADD COLUMN post_offer_amount double precision NOT NULL;
ALTER TABLE atlas_app.offer_entity ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.offer_entity ADD PRIMARY KEY ( id);



------- SQL updates -------

ALTER TABLE atlas_app.offer_entity ALTER COLUMN merchant_operating_city_id TYPE character varying(36);
ALTER TABLE atlas_app.offer_entity ALTER COLUMN merchant_id TYPE character varying(36);