CREATE TABLE atlas_driver_offer_bpp.feedback_badge (
id character(36) NOT NULL,
driver_id character(36) NOT NULL,
badge character varying(255) NOT NULL,
badge_count int DEFAULT 0 NOT NULL,
created_at timestamp with time zone NOT NULL,
updated_at timestamp with time zone NOT NULL,
CONSTRAINT  idx_feedback_badge_primary PRIMARY KEY (id),
CONSTRAINT  feedback_badge_product_instance_id_key UNIQUE (id)
);

ALTER TABLE atlas_driver_offer_bpp.feedback_badge OWNER TO atlas_driver_offer_bpp_user;
CREATE INDEX idx_driver_feedback ON atlas_driver_offer_bpp.feedback_badge USING btree (driver_id);