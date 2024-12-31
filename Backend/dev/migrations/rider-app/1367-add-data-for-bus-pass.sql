-- ONLY FOR LOCAL TESTING --
-- Insert Pass Categories
INSERT INTO atlas_app.pass_category (
    id,
    name,
    description,
    merchant_id,
    merchant_operating_city_id
)
VALUES
    (
        'c58b2f5d-2a91-4c1c-b6f5-1a2345678901',
        'Super Saver Plan',
        'Cheapest way to travel in the city',
        'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
        'namma-yatri-0-0000-0000-00000000city'
    ),
    (
        'c58b2f5d-2a91-4c1c-b6f5-1a2345678902',
        'Student Pass',
        'Discounted pass for students',
        'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
        'namma-yatri-0-0000-0000-00000000city'
    ),
    (
        'c58b2f5d-2a91-4c1c-b6f5-1a2345678903',
        'Unlimited Rides Pass',
        'Enjoy unlimited trips across the city',
        'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
        'namma-yatri-0-0000-0000-00000000city'
    ),
    (
        'c58b2f5d-2a91-4c1c-b6f5-1a2345678904',
        'Senior Citizen Super Saver Plan',
        'Discounted plans for senior citizens',
        'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
        'namma-yatri-0-0000-0000-00000000city'
    );

INSERT INTO atlas_app.pass_type (
    id,
    pass_category_id,
    name,
    catchline,
    title,
    description,
    "order",
    merchant_id,
    merchant_operating_city_id
)
VALUES
    -- Super Saver Plan
    ('d38b2f5d-3a91-4c1c-b6f5-1a2345678911', 'c58b2f5d-2a91-4c1c-b6f5-1a2345678901', 'Weekly', NULL, 'Plans for ticket upto Rs 6', 'This plan can be used for both AC and Non-AC buses', 1, 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'namma-yatri-0-0000-00000000city'),
    ('d38b2f5d-3a91-4c1c-b6f5-1a2345678912', 'c58b2f5d-2a91-4c1c-b6f5-1a2345678901', 'Monthly', NULL, 'Plans for ticket upto Rs 6', 'This plan can be used for both AC and Non-AC buses', 1, 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'namma-yatri-0-0000-00000000city'),

    -- Student Pass
    ('d38b2f5d-3a91-4c1c-b6f5-1a2345678913', 'c58b2f5d-2a91-4c1c-b6f5-1a2345678902', NULL, 'Travel for as low as 3 Rs per trip!', 'Choose your plan', 'School, College, and diploma students with age upto 26 years can apply for discounted student passes. These passes work for both AC and non-AC buses.', 1, 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'namma-yatri-0-0000-00000000city'),

    -- Unlimited Rides Pass
    ('d38b2f5d-3a91-4c1c-b6f5-1a2345678914', 'c58b2f5d-2a91-4c1c-b6f5-1a2345678903', 'General', NULL, 'Choose your plan', NULL, 1, 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'namma-yatri-0-0000-00000000city'),
    ('d38b2f5d-3a91-4c1c-b6f5-1a2345678915', 'c58b2f5d-2a91-4c1c-b6f5-1a2345678903', 'Senior Citizen', NULL, 'Choose your plan', NULL, 1, 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'namma-yatri-0-0000-00000000city'),

    -- Senior Citizen Super Saver Plan
    ('d38b2f5d-3a91-4c1c-b6f5-1a2345678916', 'c58b2f5d-2a91-4c1c-b6f5-1a2345678904', NULL, NULL, 'Plans for ticket upto Rs 6', 'This plan works on all tickets upto Rs 6 on both AC and Non-AC buses. Senior citizens with age 60 years and more than 60 years can avail these passes.', 1, 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'namma-yatri-0-0000-00000000city');

INSERT INTO atlas_app.pass (
    id,
    pass_type_id,
    code,
    name,
    amount,
    savings,
    benefit,
    vehicle_service_tier_type,
    purchase_eligibility_json_logic, -- aadhar (aadharData.isVerfied)
    redeem_eligibility_json_logic, -- days, trips
    trips,
    days,
    "order",
    merchant_id,
    merchant_operating_city_id
)
VALUES
    -- Super Saver Plan: Weekly Plan
    ('00000000-0000-0000-0000-000000000001', 'd38b2f5d-3a91-4c1c-b6f5-1a2345678911', 'SSP-WEEKLY-15', '15 trips', 1, 0, 'Discount', 'AC',
     ARRAY['{ ">": [ { "var": "orderQty" }, 0 ] }'], ARRAY['{ ">": [ { "var": "purchasedPassData.validTripsLeft" }, 0 ] }'], 15, 7, 1, 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'namma-yatri-0-0000-00000000city'),

    -- Super Saver Plan: Monthly Plan
    ('00000000-0000-0000-0000-000000000002', 'd38b2f5d-3a91-4c1c-b6f5-1a2345678912', 'SSP-MONTHLY-60', '60 trips', 1, 0, 'Discount', 'AC',
     ARRAY['{ ">": [ { "var": "orderQty" }, 0 ] }'], ARRAY['{ ">": [ { "var": "purchasedPassData.validTripsLeft" }, 0 ] }'], 60, 28, 1, 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'namma-yatri-0-0000-00000000city'),
    ('00000000-0000-0000-0000-000000000003', 'd38b2f5d-3a91-4c1c-b6f5-1a2345678912', 'SSP-MONTHLY-100', '100 trips', 1, 0, 'Discount', 'AC',
     ARRAY['{ ">": [ { "var": "orderQty" }, 0 ] }'], ARRAY['{ ">": [ { "var": "purchasedPassData.validTripsLeft" }, 0 ] }'], 100, 28, 1, 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'namma-yatri-0-0000-00000000city'),

    -- Student Plan
    ('00000000-0000-0000-0000-000000000005', 'd38b2f5d-3a91-4c1c-b6f5-1a2345678913', 'SP-60-30', 'Student Pass', 1, 0, 'Custom Text', 'AC',
     ARRAY['{ ">": [ { "var": "orderQty" }, 0 ] }'], ARRAY['{ ">": [ { "var": "purchasedPassData.validTripsLeft" }, 0 ] }'], 60, 30, 1, 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'namma-yatri-0-0000-00000000city'),

    -- Unlimited Rides Plan: General
    ('00000000-0000-0000-0000-000000000008', 'd38b2f5d-3a91-4c1c-b6f5-1a2345678914', 'URP-GENERAL', 'General Pass', 1, 0, 'Unlimited rides', 'AC',
     ARRAY['{ ">": [ { "var": "orderQty" }, 0 ] }'], ARRAY['{ ">": [ { "var": "purchasedPassData.validTripsLeft" }, 0 ] }'], NULL, 30, 1, 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'namma-yatri-0-0000-00000000city'),

    -- Unlimited Rides Plan: Senior Citizen
    ('00000000-0000-0000-0000-000000000009', 'd38b2f5d-3a91-4c1c-b6f5-1a2345678915', 'URP-SENIOR', 'Senior Citizen Pass', 1, 0, 'Unlimited rides', 'AC',
     ARRAY['{ "==": [ { "var": "aadhaarData.isValid" }, true ] }'], ARRAY['{ ">": [ { "var": "purchasedPassData.validTripsLeft" }, 0 ] }'], NULL, 30, 1, 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'namma-yatri-0-0000-00000000city');

INSERT INTO atlas_app.purchased_pass (
    id,
    short_id,
    person_id,
    pass_id,
    valid_trips_left,
    status,
    expiry_date,
    merchant_id,
    merchant_operating_city_id
)
VALUES
    ('1a2b3c4d-0000-0000-0000-000000000001', 'shortid-001', 'person-001', '00000000-0000-0000-0000-000000000001', 15, 'Pending', '2024-12-31', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'namma-yatri-0-0000-00000000city'),
    ('1a2b3c4d-0000-0000-0000-000000000002', 'shortid-002', 'person-002', '00000000-0000-0000-0000-000000000002', 60, 'Pending', '2024-12-31', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'namma-yatri-0-0000-00000000city'),
    ('1a2b3c4d-0000-0000-0000-000000000003', 'shortid-003', 'person-003', '00000000-0000-0000-0000-000000000003', 100, 'Pending', '2024-12-31', 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a52', 'namma-yatri-0-0000-00000000city');
