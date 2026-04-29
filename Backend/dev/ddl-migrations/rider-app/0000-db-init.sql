-- tables

CREATE TABLE atlas_app.tag (
id character(36) NOT NULL,
created_by character(36) NOT NULL,
created_by_entity_type character varying(255) NOT NULL,
tag_type character varying(255) NOT NULL,
tag character varying(255) NOT NULL,
info text,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16475_primary PRIMARY KEY (id)
);
ALTER TABLE atlas_app.tag OWNER TO atlas_app_user;
CREATE INDEX idx_16475_tag ON atlas_app.tag USING btree (tag);
CREATE INDEX idx_16475_tag_type ON atlas_app.tag USING btree (tag_type);

CREATE TABLE atlas_app.geometry (
id integer NOT NULL,
region character varying(255) NOT NULL,
geom public.geometry(MultiPolygon)
,CONSTRAINT  geometry_pkey PRIMARY KEY (id)
);
ALTER TABLE atlas_app.geometry OWNER TO atlas_app_user;
CREATE SEQUENCE atlas_app.geometry_id_seq
AS integer
START WITH 1
INCREMENT BY 1
NO MINVALUE
NO MAXVALUE
CACHE 1;

ALTER TABLE atlas_app.geometry_id_seq OWNER TO atlas_app_user;
ALTER SEQUENCE atlas_app.geometry_id_seq OWNED BY atlas_app.geometry.id;
ALTER TABLE ONLY atlas_app.geometry ALTER COLUMN id SET DEFAULT nextval('atlas_app.geometry_id_seq'::regclass);

CREATE TABLE atlas_app.issues (
id character(36) NOT NULL,
customer_id character(36) NOT NULL,
ride_booking_id character varying(36) DEFAULT NULL::character varying,
contact_email character varying(100),
reason character varying(500) NOT NULL,
description character varying(1000) DEFAULT NULL::character varying NOT NULL,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
ALTER TABLE atlas_app.issues OWNER TO atlas_app_user;

CREATE TABLE atlas_app.location_backup (
id character(36),
location_type character varying(255),
lat double precision,
long double precision,
point public.geography(Point,4326),
ward character varying(255),
district character varying(255),
city character varying(255),
state character varying(255),
country character varying(255),
pincode character varying(255),
address character varying(255),
bound character varying(255),
info text,
created_at timestamp with time zone,
updated_at timestamp with time zone
);
ALTER TABLE atlas_app.location_backup OWNER TO atlas_app_user;

CREATE TABLE atlas_app.organization (
id character(36) NOT NULL,
name character varying(255),
short_id character varying(255) NOT NULL,
gstin character varying(255),
status character varying(255),
type character varying(255),
domain character varying(255),
verified boolean NOT NULL,
enabled boolean DEFAULT true NOT NULL,
description text,
mobile_number text,
mobile_country_code character varying(255),
from_time timestamp with time zone,
to_time timestamp with time zone,
api_key text,
callback_url text,
callback_api_key text,
head_count bigint,
info text,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_16442_primary PRIMARY KEY (id)
,CONSTRAINT  unique_api_key UNIQUE (api_key)
,CONSTRAINT  unique_short_id UNIQUE (short_id)
);
ALTER TABLE atlas_app.organization OWNER TO atlas_app_user;
CREATE INDEX idx_organization_short_id ON atlas_app.organization USING btree (short_id);

CREATE TABLE atlas_app.product_instance_backup (
id character(36),
case_id character varying(255),
product_id character varying(255),
person_id character varying(255),
person_updated_at timestamp with time zone,
short_id character varying(36),
entity_id character varying(255),
entity_type character varying(255),
quantity bigint,
price numeric(30,10),
type character varying(255),
status character varying(255),
start_time timestamp with time zone,
end_time timestamp with time zone,
valid_till timestamp with time zone,
from_location_id character varying(255),
to_location_id character varying(255),
organization_id character varying(255),
parent_id character varying(255),
info text,
udf1 character varying(255),
udf2 character varying(255),
udf3 character varying(255),
udf4 character varying(255),
udf5 character varying(255),
created_at timestamp with time zone,
updated_at timestamp with time zone,
actual_distance double precision,
actual_price double precision
);
ALTER TABLE atlas_app.product_instance_backup OWNER TO atlas_app_user;

ALTER TABLE atlas_app.search_request OWNER TO atlas_app_user;
CREATE INDEX idx_16386_requestor ON atlas_app.search_request USING btree (rider_id);

ALTER TABLE atlas_app.search_request_location OWNER TO atlas_app_user;
CREATE INDEX idx_16434_city ON atlas_app.search_request_location USING btree (city);
CREATE INDEX idx_16434_state ON atlas_app.search_request_location USING btree (state);

CREATE TABLE atlas_app.ride_cancellation_reason (
ride_booking_id character(36) NOT NULL,
source character varying(255) NOT NULL,
reason_code character varying(255),
additional_info character varying(255)
,CONSTRAINT  ride_cancellation_reason_pkey PRIMARY KEY (ride_booking_id)
,CONSTRAINT  ride_cancellation_reason_ride_booking_id_fkey FOREIGN KEY (ride_booking_id) REFERENCES atlas_app.booking(id)
);
ALTER TABLE atlas_app.ride_cancellation_reason OWNER TO atlas_app_user;

ALTER TABLE atlas_app.person
ADD COLUMN full_name character varying (255);

ALTER TABLE atlas_app.person
ADD COLUMN email character varying(255);

ALTER TABLE atlas_app.person
ADD COLUMN rating character varying(255);

ALTER TABLE atlas_app.person
ADD COLUMN udf1 character varying(255);

ALTER TABLE atlas_app.person
ADD COLUMN udf2 character varying(255);

-- Ensure uniqueness when both trip_category and fcm_sub_category are not null
CREATE UNIQUE INDEX unique_combination_dne_not_null
ON atlas_app.merchant_push_notification (
    fcm_notification_type,
    key,
    merchant_id,
    merchant_operating_city_id,
    language,
    trip_category,
    fcm_sub_category
)
WHERE trip_category IS NOT NULL AND fcm_sub_category IS NOT NULL;

-- In master and prod drop existing pkeys and make id pkey
-- Please drop all existing pkeys
ALTER TABLE atlas_app.merchant_push_notification DROP CONSTRAINT merchant_push_notification_pkey;
ALTER TABLE atlas_app.merchant_push_notification ADD PRIMARY KEY ( id);

-- Ensure uniqueness when trip_category is null and fcm_sub_category is not null
CREATE UNIQUE INDEX unique_combination_trip_null_fcm_sub_not_null
ON atlas_app.merchant_push_notification (
    fcm_notification_type,
    key,
    merchant_id,
    merchant_operating_city_id,
    language,
    fcm_sub_category
)
WHERE trip_category IS NULL AND fcm_sub_category IS NOT NULL;

-- Ensure uniqueness when trip_category is not null and fcm_sub_category is null
CREATE UNIQUE INDEX unique_combination_trip_not_null_fcm_sub_null
ON atlas_app.merchant_push_notification (
    fcm_notification_type,
    key,
    merchant_id,
    merchant_operating_city_id,
    language,
    trip_category
)
WHERE trip_category IS NOT NULL AND fcm_sub_category IS NULL;

-- Ensure uniqueness when both trip_category and fcm_sub_category are null
CREATE UNIQUE INDEX unique_combination_trip_null_fcm_sub_null
ON atlas_app.merchant_push_notification (
    fcm_notification_type,
    key,
    merchant_id,
    merchant_operating_city_id,
    language
)
WHERE trip_category IS NULL AND fcm_sub_category IS NULL;

-- thank you for getting here!
