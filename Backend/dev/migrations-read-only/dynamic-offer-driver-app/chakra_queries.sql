CREATE TABLE atlas_driver_offer_bpp.chakra_queries ();

ALTER TABLE atlas_driver_offer_bpp.chakra_queries ADD COLUMN chakra text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.chakra_queries ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.chakra_queries ADD COLUMN query_results text[] NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.chakra_queries ADD COLUMN query_text text NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.chakra_queries ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.chakra_queries ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_driver_offer_bpp.chakra_queries ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_driver_offer_bpp.chakra_queries ADD COLUMN query_name text ;
ALTER TABLE atlas_driver_offer_bpp.chakra_queries DROP CONSTRAINT chakra_queries_pkey;
ALTER TABLE atlas_driver_offer_bpp.chakra_queries ADD PRIMARY KEY ( chakra, query_name);
ALTER TABLE atlas_driver_offer_bpp.chakra_queries ALTER COLUMN query_name SET NOT NULL;

--- Now DSL don't allow dropping tables instead we will drop not null constraint if any .Please be careful while running ---
ALTER TABLE atlas_driver_offer_bpp.chakra_queries ALTER COLUMN id DROP NOT NULL;
--- Drop section ends. Please check before running ---