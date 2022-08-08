ALTER TABLE
   atlas_app.search_request_location
   ADD COLUMN city character varying(255),
   ADD COLUMN state character varying(255),
   ADD COLUMN country character varying(255),
   ADD COLUMN street character varying(255),
   ADD COLUMN door character varying(255),
   ADD COLUMN building character varying(255),
   ADD COLUMN area_code character varying(255),
   ADD COLUMN area character varying(255);