CREATE TABLE atlas_driver_offer_bpp.coin_config (
    id VARCHAR(36) PRIMARY KEY,
    fn VARCHAR(255),
    event_name VARCHAR(255),
    merchant_id VARCHAR(255) NOT NULL,
    coins INT NOT NULL,
    expiration_at INT,
    track_expiry INT NOT NULL,
    active BOOLEAN NOT NULL
);

INSERT INTO atlas_driver_offer_bpp.coin_config (id, fn, event_name, merchant_id, coins, expiration_at, track_expiry, active)
VALUES
  ('f3f83c1f-4cc4-4504-9586-3c564c9f86e6',    'OneOrTwoStarRating',        'Rating',        'favorit0-0000-0000-0000-00000favorit',  -1,   null,      1296000,   True),
  ('fa3ac1f6-8982-4f3b-ab2f-6ade7497b865',    'RideCompleted',             'EndRide',       'favorit0-0000-0000-0000-00000favorit',  1,    7776000,   1296000,   True),
  ('8d5f4a88-0a5e-4c08-9aa5-b7ec4d3f3c83',    'FiveStarRating',            'Rating',        'favorit0-0000-0000-0000-00000favorit',  1,    7776000,   1296000,   True),
  ('9a6d33f9-a558-48ff-9e37-741312c97ce9',    'BookingCancellation',       'Cancellation',  'favorit0-0000-0000-0000-00000favorit',  -5,   null,      1296000,   True),
  ('f26fe73a-0cb4-4d4b-9928-07380c3cae86',    'CustomerReferral',          'Referral',      'favorit0-0000-0000-0000-00000favorit',  200,  7776000,   1296000,   True),
  ('daa7f5cb-93c3-4034-b3a3-f307df98ff60',    'DriverReferral',            'Referral',      'favorit0-0000-0000-0000-00000favorit',  200,  7776000,   1296000,   True),
  ('b7ce863e-da4b-4b81-b2a5-905b06d0bd51',    'EightPlusRidesInOneDay',    'EndRide',       'favorit0-0000-0000-0000-00000favorit',  10,   7776000,   1296000,   True),
  ('2c99324e-8a9f-4000-9ab5-9cd0cf1cd36f',    'PurpleRideCompleted',       'EndRide',       'favorit0-0000-0000-0000-00000favorit',  5,    7776000,   1296000,   True),
  ('d05cd21f-9bda-4255-94e3-4264bd330073',    'LeaderBoardTopFiveHundred', 'LeaderBoard',   'favorit0-0000-0000-0000-00000favorit',  50,   7776000,   1296000,   True),
  ('a87a285e-2a85-4973-84ff-37dcce0d0369',    'TrainingCompleted',         'Training',      'favorit0-0000-0000-0000-00000favorit',  20,   7776000,   1296000,   True);

CREATE TABLE atlas_driver_offer_bpp.coin_history (
    id VARCHAR (36) PRIMARY KEY,
    fn VARCHAR (255),
    merchant_id VARCHAR (36) NOT NULL,
    driver_id VARCHAR (36) NOT NULL,
    coins INT NOT NULL,
    created_at timestamp with time zone,
    expiration_at timestamp with time zone,
    status TEXT NOT NULL,
    coins_used INT
);

CREATE TABLE atlas_driver_offer_bpp.coin_purchase_history (
    id VARCHAR (36) PRIMARY KEY,
    driver_id VARCHAR (36) NOT NULL,
    coin_plan_id  VARCHAR (36) NOT NULL,
    num_coins INT NOT NULL,
    created_at timestamp with time zone,
    quantity INT,
    quantity_left INT NOT NULL
);

CREATE TABLE atlas_driver_offer_bpp.coin_plan (
    id VARCHAR (36) PRIMARY KEY,
    merchant_id VARCHAR (36) NOT NULL,
    coin_plan_name TEXT NOT NULL,
    sub_plan_id TEXT NOT NULL,
    sub_plan_mode TEXT NOT NULL,
    required_coins INT,
    num_of_days INT
);

INSERT INTO atlas_driver_offer_bpp.coin_plan (id, merchant_id, coin_plan_name, sub_plan_id, sub_plan_mode, required_coins, num_of_days)
VALUES
  ('8879702d-96cb-4816-9dab-d2536bf9b9c4',   'favorit0-0000-0000-0000-00000favorit',  'Daily Unlimited Plan',   'a35ffc7c-de0d-4dcc-83a8-e36a5a29cc1d',  'AUTOPAY',    100,  1),
  ('c5af2b82-0ee4-44ce-a4f6-59ecffba2d65',   'favorit0-0000-0000-0000-00000favorit', 'Weekly Unlimited Plan',  'a35ffc7c-de0d-4dcc-83a8-e36a5a29cc1d',  'AUTOPAY',    600,  7);

ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN distance_to_pickup double precision;

ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN total_earned_coins INT NOT NULL DEFAULT 0;

ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN used_coins INT NOT NULL DEFAULT 0;

Alter Table atlas_driver_offer_bpp.transporter_config ADD COLUMN coin_feature BOOL DEFAULT True;