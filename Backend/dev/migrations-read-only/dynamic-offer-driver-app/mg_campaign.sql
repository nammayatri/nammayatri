CREATE TABLE atlas_driver_offer_bpp.mg_campaign ();

ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN created_by text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN currency character varying(255) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN description text ;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN end_date date NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN force_assign boolean NOT NULL default false;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN frequency text NOT NULL default 'DAILY';
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN guarantee_amount double precision NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN max_budget double precision ;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN merchant_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN merchant_operating_city_id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN on_fleet_change text NOT NULL default 'DISQUALIFY';
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN opt_in_cutoff_minutes integer NOT NULL default 60;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN settlement_grace_minutes integer NOT NULL default 120;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN start_date date NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN status text NOT NULL default 'DRAFT';
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN vehicle_categories text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.mg_campaign ADD PRIMARY KEY ( id);


