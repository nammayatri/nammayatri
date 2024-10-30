UPDATE atlas_driver_offer_bpp.coin_config SET active = False WHERE event_function = 'RidesCompleted 5';

INSERT INTO atlas_driver_offer_bpp.coin_config
    (id, event_function, event_name, merchant_id, merchant_opt_city_id, coins, expiration_at, active)
SELECT
    md5(random()::TEXT || clock_timestamp()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT)::UUID,
    'RidesCompleted 6',
    'EndRide',
    city.merchant_id,
    city.id,
    15,
    12960000,
    False,
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS city;

INSERT INTO atlas_driver_offer_bpp.translations (id, message_key, language, message, created_at, updated_at)
VALUES
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 6', 'ENGLISH', '6 rides in a day', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 6', 'TELUGU', 'ఒక రోజులో 6 రైడ్‌లు', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 6', 'HINDI', 'एक दिन में 6 सवारी', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 6', 'KANNADA', 'ಒಂದು ದಿನದಲ್ಲಿ 6 ರೈಡ್‌ಗಳು', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 6', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 6 യാത്രകൾ', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 6', 'BENGALI', 'দিনে ৬টি রাইড', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 6', 'TAMIL', 'ஒரு நாளில் 6 பயணங்கள்', now(), now());