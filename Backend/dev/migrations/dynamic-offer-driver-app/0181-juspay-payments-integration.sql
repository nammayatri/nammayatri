CREATE TABLE atlas_driver_offer_bpp.payment_order (
    id character(36) PRIMARY KEY NOT NULL,
    short_id character varying (36) NOT NULL,
    person_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.person (id),
    merchant_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id),
    amount integer NOT NULL,
    currency character varying(30) NOT NULL,
    status character varying(100) NOT NULL,
    web_payment_link text,
    iframe_payment_link text,
    mobile_payment_link text,
    client_auth_token_encrypted character varying(255) NOT NULL,
    client_auth_token_hash bytea NOT NULL,
    client_auth_token_expiry timestamp with time zone NOT NULL,
    get_upi_deep_links_option boolean,
    environment character varying(100),
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_driver_offer_bpp.payment_transaction (
    id character(36) PRIMARY KEY NOT NULL,
    txn_uuid character varying (255) NOT NULL,
    payment_method_type character varying(100) NOT NULL,
    payment_method character varying(100) NOT NULL,
    resp_message character varying (255),
    resp_code character varying (255),
    gateway_reference_id character varying(100),
    order_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.payment_order (id),
    merchant_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id),
    amount numeric(30,2) NOT NULL,
    currency character varying(30) NOT NULL,
    date_created timestamp with time zone,
    status_id int NOT NULL,
    status character varying(100) NOT NULL,
    juspay_response text,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- local sync
INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT m.merchant_id, m.id, 'Payment_Juspay',
  json_build_object(
      'apiKey','0.1.0|2|aH69syF+qmjP8wpjdwy5KdrHqhsTd1s7lH6TupSUYwMAS5rpi4jGDsA6Nt1uqGPWZxdMshc18cDhmQ=='
    , 'returnUrl','dummyReturnUrl'
    , 'url','dummyUrl'
    , 'merchantId', 'dummyMerchantId'
    , 'username', 'dummyUsername'
    , 'password','0.1.0|0|MbGCmY0OMu39bi7dEokkZ4kvgN17S+whz29QJa+XXUy+mue72jMsAHfVGd4lM9AEWbCqRywCu2RTpA=='
  )
FROM atlas_driver_offer_bpp.merchant_operating_city m;
