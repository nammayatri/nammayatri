UPDATE atlas_driver_offer_bpp.subscription_config SET is_vendor_split_enabled = true where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city in ('Kolkata', 'Asansol', 'Durgapur', 'Siliguri'));

-- Kolkata Open Market --
-- 2 Rs. for 1233333 (any variant)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 2.0, 'SUV', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 2.0, 'SEDAN', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 2.0, 'HATCHBACK', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 2.0, 'TAXI', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

-- 3 Rs. for TestAccount2 (any variant)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 3.0, 'SUV', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 3.0, 'SEDAN', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 3.0, 'HATCHBACK', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 3.0, 'TAXI', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';


-- Howrah --
-- 2 Rs. for 1233333 (any variant)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8111', id, 'FIXED', 2.0, 'SUV', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8111', id, 'FIXED', 2.0, 'SEDAN', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8111', id, 'FIXED', 2.0, 'HATCHBACK', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8111', id, 'FIXED', 2.0, 'TAXI', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

-- 3 Rs. for TestAccount2 (any variant)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8111', id, 'FIXED', 3.0, 'SUV', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8111', id, 'FIXED', 3.0, 'SEDAN', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8111', id, 'FIXED', 3.0, 'HATCHBACK', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8111', id, 'FIXED', 3.0, 'TAXI', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

-- Airport --
-- 2 Rs. for 1233333 (any variant)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8444', id, 'FIXED', 2.0, 'SUV', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8444', id, 'FIXED', 2.0, 'SEDAN', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8444', id, 'FIXED', 2.0, 'HATCHBACK', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8444', id, 'FIXED', 2.0, 'TAXI', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

-- 3 Rs. for TestAccount2 (any variant)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8444', id, 'FIXED', 3.0, 'SUV', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8444', id, 'FIXED', 3.0, 'SEDAN', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8444', id, 'FIXED', 3.0, 'HATCHBACK', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8444', id, 'FIXED', 3.0, 'TAXI', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

-- Asansol Open Market --
-- 1.75 Rs. for TestAccount2 (Auto)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 1.75, 'AUTO_RICKSHAW', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Asansol';

-- 1.75 Rs. for 1233333 (Auto)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 1.75, 'AUTO_RICKSHAW', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Asansol';

-- 0.5 Rs. for test 123 (Auto)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 0.5, 'AUTO_RICKSHAW', 'test 123', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Asansol';


-- 1 Re. for TestAccount2 (any variant)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 1.0, 'SUV', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Asansol';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 1.0, 'SEDAN', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Asansol';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 1.0, 'HATCHBACK', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Asansol';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 1.0, 'TAXI', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Asansol';

-- 2 Rs. for test 123 (any variant)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 2.0, 'SUV', 'test 123', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Asansol';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 2.0, 'SEDAN', 'test 123', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Asansol';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 2.0, 'HATCHBACK', 'test 123', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Asansol';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 2.0, 'TAXI', 'test 123', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Asansol';

-- 3 Rs. for testing 5
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 3.0, 'SUV', 'testing 5', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Asansol';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 3.0, 'SEDAN', 'testing 5', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Asansol';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 3.0, 'HATCHBACK', 'testing 5', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Asansol';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 3.0, 'TAXI', 'testing 5', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Asansol';

-- Siliguri Open Market --
-- 1 Re. for test 123 (Bike)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 1.0, 'BIKE', 'test 123', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri';

-- 2 Rs. for TestAccount2 (Bike)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 1.0, 'BIKE', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri';

-- Bagdogra Airport --
-- 3 Rs. for TestAccount2 (any variant)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_019b2a0a-54c0-4503-8057-8442dca1c2e9', id, 'FIXED', 3.0, 'SUV', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_019b2a0a-54c0-4503-8057-8442dca1c2e9', id, 'FIXED', 3.0, 'SEDAN', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_019b2a0a-54c0-4503-8057-8442dca1c2e9', id, 'FIXED', 3.0, 'HATCHBACK', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_019b2a0a-54c0-4503-8057-8442dca1c2e9', id, 'FIXED', 3.0, 'TAXI', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri';

-- 2 Rs. for testing 5 (any variant)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_019b2a0a-54c0-4503-8057-8442dca1c2e9', id, 'FIXED', 2.0, 'SUV', 'testing 5', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_019b2a0a-54c0-4503-8057-8442dca1c2e9', id, 'FIXED', 2.0, 'SEDAN', 'testing 5', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_019b2a0a-54c0-4503-8057-8442dca1c2e9', id, 'FIXED', 2.0, 'HATCHBACK', 'testing 5', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_variant, vendor_id, created_at, updated_at)
SELECT
'Pickup_019b2a0a-54c0-4503-8057-8442dca1c2e9', id, 'FIXED', 2.0, 'TAXI', 'testing 5', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Siliguri';

ALTER TABLE atlas_driver_offer_bpp.payment_transaction ADD COLUMN split_settlement_response json;