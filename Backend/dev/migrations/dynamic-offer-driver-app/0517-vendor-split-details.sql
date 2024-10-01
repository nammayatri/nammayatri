UPDATE atlas_driver_offer_bpp.transporter_config SET is_vendor_split_enabled = true where merchant_operating_city_id in (select id from atlas_driver_offer_bpp.merchant_operating_city where city in ('Kolkata', 'Asansol', 'Durgapur', 'Siliguri'));

-- Kolkata Open Market --
-- 2 Rs. for 1233333 (any variant)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 2.0, 'SUV', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 2.0, 'SEDAN', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 2.0, 'HATCHBACK', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 2.0, 'TAXI', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

-- 3 Rs. for TestAccount2 (any variant)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 3.0, 'SUV', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 3.0, 'SEDAN', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 3.0, 'HATCHBACK', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Default', id, 'FIXED', 3.0, 'TAXI', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';


-- Howrah --
-- 2 Rs. for 1233333 (any variant)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8111', id, 'FIXED', 2.0, 'SUV', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8111', id, 'FIXED', 2.0, 'SEDAN', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8111', id, 'FIXED', 2.0, 'HATCHBACK', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8111', id, 'FIXED', 2.0, 'TAXI', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

-- 3 Rs. for TestAccount2 (any variant)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8111', id, 'FIXED', 3.0, 'SUV', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8111', id, 'FIXED', 3.0, 'SEDAN', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8111', id, 'FIXED', 3.0, 'HATCHBACK', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8111', id, 'FIXED', 3.0, 'TAXI', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

-- Airport --
-- 2 Rs. for 1233333 (any variant)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8444', id, 'FIXED', 2.0, 'SUV', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8444', id, 'FIXED', 2.0, 'SEDAN', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8444', id, 'FIXED', 2.0, 'HATCHBACK', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8444', id, 'FIXED', 2.0, 'TAXI', '1233333', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

-- 3 Rs. for TestAccount2 (any variant)
INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8444', id, 'FIXED', 3.0, 'SUV', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8444', id, 'FIXED', 3.0, 'SEDAN', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8444', id, 'FIXED', 3.0, 'HATCHBACK', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';

INSERT INTO atlas_driver_offer_bpp.vendor_split_details
(area, merchant_operating_city_id, split_type, split_value, vehicle_category, vendor_id, created_at, updated_at)
SELECT
'Pickup_1h8016d0-f9cd-4f9f-886f-bc4cbh6a8444', id, 'FIXED', 3.0, 'TAXI', 'TestAccount2', now(), now() from atlas_driver_offer_bpp.merchant_operating_city where city = 'Kolkata';
