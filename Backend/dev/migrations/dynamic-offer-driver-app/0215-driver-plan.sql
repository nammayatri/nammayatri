CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.driver_plan
(   id character(36) NOT NULL PRIMARY KEY,
    driver_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.person(id),
    plan_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.plan_details(id),
    max_amount integer NOT NULL,
    payment_method_type character varying(255) NOT NULL,
    payment_method character varying(255) NOT NULL,
    mandate_status character varying(255) NOT NULL,
    activated_at timestamp with time zone NOT NULL,
    auto_pay boolean NOT NULL
);

ALTER TABLE atlas_driver_offer_bpp.driver_plan OWNER TO atlas_driver_offer_bpp_user;