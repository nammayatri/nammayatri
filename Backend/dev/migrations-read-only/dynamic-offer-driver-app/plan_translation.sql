CREATE TABLE atlas_driver_offer_bpp.plan_translation ();

ALTER TABLE atlas_driver_offer_bpp.plan_translation ADD COLUMN description text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan_translation ADD COLUMN language text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan_translation ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan_translation ADD COLUMN plan_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.plan_translation ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.plan_translation ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.plan_translation ADD PRIMARY KEY ( plan_id, language);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.plan_translation DROP CONSTRAINT plan_translation_pkey;
ALTER TABLE atlas_driver_offer_bpp.plan_translation ADD PRIMARY KEY ( plan_id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.plan_translation DROP CONSTRAINT plan_translation_pkey;
ALTER TABLE atlas_driver_offer_bpp.plan_translation ADD PRIMARY KEY ( language, plan_id);