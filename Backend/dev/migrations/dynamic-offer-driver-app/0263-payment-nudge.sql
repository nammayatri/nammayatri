CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.merchant_push_notification
(   merchant_id character(36) NOT NULL,
    push_notification_key character varying(255) not null,
    language character varying(255) not null,
    udf1 character varying(255),
    notification_sub_type character varying(255) not null,
    icon character varying(255),
    title text not null,
    body text not null
);
ALTER TABLE atlas_driver_offer_bpp.payment_order ALTER COLUMN amount type numeric(30,2);