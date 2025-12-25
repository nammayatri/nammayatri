CREATE TABLE atlas_app.ticket_place ();

ALTER TABLE atlas_app.ticket_place ADD COLUMN close_timings time without time zone ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN description text ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN gallery text[] NOT NULL;
ALTER TABLE atlas_app.ticket_place ADD COLUMN icon_url text ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.ticket_place ADD COLUMN lat double precision ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN lon double precision ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN map_image_url text ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN name text NOT NULL;
ALTER TABLE atlas_app.ticket_place ADD COLUMN open_timings time without time zone ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN place_type text NOT NULL default 'Other';
ALTER TABLE atlas_app.ticket_place ADD COLUMN short_desc text NOT NULL;
ALTER TABLE atlas_app.ticket_place ADD COLUMN terms_and_conditions text[] NOT NULL default '{}';
ALTER TABLE atlas_app.ticket_place ADD COLUMN merchant_id character varying(36) ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN merchant_operating_city_id character varying(36) ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_place ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.ticket_place ADD PRIMARY KEY ( id);


------- SQL updates -------

ALTER TABLE atlas_app.ticket_place ALTER COLUMN short_desc SET DEFAULT 'Short description not available';


------- SQL updates -------

ALTER TABLE atlas_app.ticket_place ALTER COLUMN merchant_operating_city_id SET NOT NULL;


------- SQL updates -------

ALTER TABLE atlas_app.ticket_place ADD COLUMN status text NOT NULL default 'Active';


------- SQL updates -------

ALTER TABLE atlas_app.ticket_place ADD COLUMN allow_same_day_booking boolean NOT NULL default true;


------- SQL updates -------

ALTER TABLE atlas_app.ticket_place ADD COLUMN terms_and_conditions_url text ;


------- SQL updates -------

ALTER TABLE atlas_app.ticket_place ADD COLUMN priority integer  default 0;


------- SQL updates -------

ALTER TABLE atlas_app.ticket_place ADD COLUMN ticket_merchant_id text ;


------- SQL updates -------

ALTER TABLE atlas_app.ticket_place ADD COLUMN custom_tabs json ;


------- SQL updates -------

ALTER TABLE atlas_app.ticket_place ADD COLUMN rules json ;


------- SQL updates -------

ALTER TABLE atlas_app.ticket_place ADD COLUMN recommend boolean ;


------- SQL updates -------

ALTER TABLE atlas_app.ticket_place ADD COLUMN pricing_onwards integer ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN platform_fee text ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN metadata json ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN is_recurring boolean  default true;
ALTER TABLE atlas_app.ticket_place ADD COLUMN faqs json ;


------- SQL updates -------

ALTER TABLE atlas_app.ticket_place ADD COLUMN platform_fee_vendor text ;


------- SQL updates -------

ALTER TABLE atlas_app.ticket_place ADD COLUMN venue text ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN start_date date ;
ALTER TABLE atlas_app.ticket_place ADD COLUMN is_closed boolean  default false;
ALTER TABLE atlas_app.ticket_place ADD COLUMN end_date date ;


------- SQL updates -------

ALTER TABLE atlas_app.ticket_place ADD COLUMN assign_ticket_to_bpp boolean  default false;


------- SQL updates -------

ALTER TABLE atlas_app.ticket_place ADD COLUMN enforced_as_sub_place boolean  default false;