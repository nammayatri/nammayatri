CREATE TABLE atlas_driver_offer_bpp.coin_config (
    id VARCHAR(36) PRIMARY KEY,
    event_function VARCHAR(255),
    event_name VARCHAR(255),
    merchant_id VARCHAR(255) NOT NULL,
    merchant_opt_city_id VARCHAR(255) NOT NULL,
    coins INT NOT NULL,
    expiration_at INT,
    active BOOLEAN NOT NULL
);

INSERT INTO atlas_driver_offer_bpp.coin_config
    (id, event_function, event_name, merchant_id, merchant_opt_city_id, coins, expiration_at, active)
SELECT
    md5(random()::TEXT || clock_timestamp()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT)::UUID,
    event.event_function,
    event.event_name,
    city.merchant_id,
    city.id,
    event.coins,
    event.expiration_at,
    event.active
FROM
    (VALUES
        ('OneOrTwoStarRating', 'Rating', -1, null, True),
        ('RideCompleted', 'EndRide', 1, 7776000, True),
        ('FiveStarRating', 'Rating', 1, 7776000, True),
        ('BookingCancellation', 'Cancellation', -5, null, True),
        ('CustomerReferral', 'CustomerToDriverReferral', 200, 7776000, True),
        ('DriverReferral', 'DriverToCustomerReferral', 200, 7776000, True),
        ('EightPlusRidesInOneDay', 'EndRide', 10, 7776000, True),
        ('PurpleRideCompleted', 'EndRide', 5, 7776000, True),
        ('LeaderBoardTopFiveHundred', 'LeaderBoard', 50, 7776000, False),
        ('TrainingCompleted', 'Training', 20, 7776000, False)
    ) AS event(event_function, event_name, coins, expiration_at, active),
    atlas_driver_offer_bpp.merchant_operating_city AS city;

CREATE TABLE atlas_driver_offer_bpp.coin_history (
    id VARCHAR (36) PRIMARY KEY,
    event_function VARCHAR (255) NOT NULL,
    merchant_id VARCHAR (36) NOT NULL,
    merchant_opt_city_id VARCHAR (36) NOT NULL,
    driver_id VARCHAR (36) NOT NULL,
    coins INT NOT NULL,
    created_at timestamp with time zone NOT NULL,
    expiration_at timestamp with time zone,
    status TEXT NOT NULL,
    coins_used INT NOT NULL
);

CREATE TABLE atlas_driver_offer_bpp.coin_purchase_history (
    id VARCHAR (36) PRIMARY KEY,
    driver_id VARCHAR (36) NOT NULL,
    merchant_id VARCHAR (36) NOT NULL,
    merchant_opt_city_id VARCHAR (36) NOT NULL,
    num_coins INT NOT NULL,
    created_at timestamp with time zone,
    updated_at timestamp with time zone,
    cash double precision NOT NULL,
    title VARCHAR (36) NOT NULL
);

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN distance_to_pickup double precision;

ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN total_earned_coins INT NOT NULL DEFAULT 0;

ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN used_coins INT NOT NULL DEFAULT 0;

Alter Table atlas_driver_offer_bpp.transporter_config ADD COLUMN coin_feature BOOL NOT NULL DEFAULT True;

Alter Table atlas_driver_offer_bpp.transporter_config ADD COLUMN coin_expire_time INT NOT NULL DEFAULT 1296000;

Alter Table atlas_driver_offer_bpp.transporter_config ADD COLUMN coin_conversion_rate double precision NOT NULL DEFAULT 0.4;

Alter Table atlas_driver_offer_bpp.driver_plan ADD COLUMN coin_coverted_to_cash_left double precision NOT NULL DEFAULT 0;

Alter Table atlas_driver_offer_bpp.driver_plan ADD COLUMN total_coins_converted_cash double precision NOT NULL DEFAULT 0;

ALTER TABLE atlas_driver_offer_bpp.driver_fee ADD COLUMN amount_paid_by_coin double precision NOT NULL DEFAULT 0;

Alter Table atlas_driver_offer_bpp.transporter_config ADD COLUMN cancellation_time_diff INT NOT NULL DEFAULT 120;

Alter Table atlas_driver_offer_bpp.transporter_config ADD COLUMN cancellation_dist_diff INT NOT NULL DEFAULT 50;

ALTER TABLE atlas_driver_offer_bpp.coin_config
ALTER COLUMN event_function SET NOT NULL,
ALTER COLUMN event_name SET NOT NULL;