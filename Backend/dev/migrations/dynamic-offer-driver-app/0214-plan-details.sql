CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.plan_details
(   id character(36) NOT NULL PRIMARY KEY,
    payment_mode character varying(255) NOT NULL,
    frequency character varying(255) NOT NULL,
    merchant_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id),
    plan_criteria_config_json json,
    city character varying(255) NOT NULL
);

ALTER TABLE atlas_driver_offer_bpp.plan_details OWNER TO atlas_driver_offer_bpp_user;

INSERT INTO atlas_driver_offer_bpp.plan_details ( id, merchant_id, payment_mode, frequency, plan_criteria_config_json, city) VALUES
  (atlas_driver_offer_bpp.uuid_generate_v4(), 'favorit0-0000-0000-0000-00000favorit', 'AUTOPAY', 'DAILY', '{}', 'Bengaluru');


update atlas_driver_offer_bpp.plan_details set plan_criteria_config_json=
    json_build_object(
        'rideCountBasedFeePolicy',
        json_build_array(
            json_build_object(
                'baseRideCount', 4,
                'platformFee', 20,
                'platformFeeCgst', 1.00,
                'platformFeeSgst', 1.00,
                'perRideFee', 20,
                'perRideCgst', 1.00,
                'perRideSgst', 1.00
            ),
            json_build_object(
                'baseRideCount', 8,
                'platformFee', 20,
                'platformFeeCgst', 1.00,
                'platformFeeSgst', 1.00,
                'perRideFee', 20,
                'perRideCgst', 1.00,
                'perRideSgst', 1.00
            ),
            json_build_object(
                'baseRideCount', 12,
                'platformFee', 20,
                'platformFeeCgst', 1.00,
                'platformFeeSgst', 1.00,
                'perRideFee', 20,
                'perRideCgst', 1.00,
                'perRideSgst', 1.00
            )
        )
    ) ;
