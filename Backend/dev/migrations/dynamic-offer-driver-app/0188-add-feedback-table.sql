CREATE TABLE atlas_driver_offer_bpp.feedback (
id character(36) NOT NULL,
driver_id character(36) NOT NULL,
ride_id character varying(36) NOT NULL,
badge character varying(255) NOT NULL,
CONSTRAINT  idx_feedback_primary PRIMARY KEY (id)
);

ALTER TABLE atlas_driver_offer_bpp.feedback OWNER TO atlas_driver_offer_bpp_user;
