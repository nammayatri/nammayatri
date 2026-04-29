CREATE TABLE atlas_bpp_dashboard.transaction (
id character(36) NOT NULL,
requestor_id character(36) REFERENCES atlas_bpp_dashboard.person (id) NOT NULL,
merchant_id character(36) REFERENCES atlas_bpp_dashboard.merchant (id),
common_driver_id character(36),
common_ride_id character(36),
endpoint character varying(255) NOT NULL,
request text,
response text,
response_error text,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  idx_transaction_primary PRIMARY KEY (id)
);
ALTER TABLE atlas_bpp_dashboard.transaction OWNER TO atlas_bpp_dashboard_user;
