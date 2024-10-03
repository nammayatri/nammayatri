INSERT INTO atlas_driver_offer_bpp.coin_config
    (id, event_function, event_name, merchant_id, merchant_opt_city_id, coins, expiration_at, active)
SELECT
    md5(random()::TEXT || clock_timestamp()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT)::UUID,
    'RidesCompleted 2',
    'EndRide',
    city.merchant_id,
    city.id,
    5,
    12960000,
    False
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS city;

INSERT INTO atlas_driver_offer_bpp.coin_config
    (id, event_function, event_name, merchant_id, merchant_opt_city_id, coins, expiration_at, active)
SELECT
    md5(random()::TEXT || clock_timestamp()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT)::UUID,
    'RidesCompleted 5',
    'EndRide',
    city.merchant_id,
    city.id,
    15,
    12960000,
    False
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS city;

INSERT INTO atlas_driver_offer_bpp.coin_config
    (id, event_function, event_name, merchant_id, merchant_opt_city_id, coins, expiration_at, active)
SELECT
    md5(random()::TEXT || clock_timestamp()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT)::UUID,
    'RidesCompleted 8',
    'EndRide',
    city.merchant_id,
    city.id,
    15,
    12960000,
    False
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS city;

INSERT INTO atlas_driver_offer_bpp.coin_config
    (id, event_function, event_name, merchant_id, merchant_opt_city_id, coins, expiration_at, active)
SELECT
    md5(random()::TEXT || clock_timestamp()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT || random()::TEXT)::UUID,
    'RidesCompleted 10',
    'EndRide',
    city.merchant_id,
    city.id,
    30,
    12960000,
    False
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS city;

INSERT INTO atlas_driver_offer_bpp.translations (id, message_key, language, message, created_at, updated_at)
  VALUES
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 1', 'ENGLISH', '1 ride in a day', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 2', 'ENGLISH', '2 rides in a day', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 5', 'ENGLISH', '5 rides in a day', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 8', 'ENGLISH', '8 rides in a day', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 10', 'ENGLISH', '10 rides in a day', now(), now());

INSERT INTO atlas_driver_offer_bpp.translations (id, message_key, language, message, created_at, updated_at)
  VALUES
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 1', 'TELUGU', 'రోజులో ఒక రైడ్', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 2', 'TELUGU', 'రోజులో రెండు రైడ్‌లు', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 5', 'TELUGU', 'రోజులో ఐదు రైడ్‌లు', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 8', 'TELUGU', 'రోజులో ఎనిమిది రైడ్‌లు', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 10', 'TELUGU', 'రోజులో పది రైడ్‌లు', now(), now());

INSERT INTO atlas_driver_offer_bpp.translations (id, message_key, language, message, created_at, updated_at)
  VALUES
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 1', 'HINDI', 'एक दिन में 1 सवारी', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 2', 'HINDI', 'एक दिन में 2 सवारी', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 5', 'HINDI', 'एक दिन में 5 सवारी', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 8', 'HINDI', 'एक दिन में 8 सवारी', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 10', 'HINDI', 'एक दिन में 10 सवारी', now(), now());

INSERT INTO atlas_driver_offer_bpp.translations ( id, message_key, language, message, created_at, updated_at)
  VALUES
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 1', 'KANNADA', 'ಒಂದು ದಿನದಲ್ಲಿ 1 ರೈಡ್', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 2', 'KANNADA', 'ಒಂದು ದಿನದಲ್ಲಿ 2 ರೈಡ್‌ಗಳು', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 5', 'KANNADA', 'ಒಂದು ದಿನದಲ್ಲಿ 5 ರೈಡ್‌ಗಳು', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 8', 'KANNADA', 'ಒಂದು ದಿನದಲ್ಲಿ 8 ರೈಡ್‌ಗಳು', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 10', 'KANNADA', 'ಒಂದು ದಿನದಲ್ಲಿ 10 ರೈಡ್‌ಗಳು', now(), now());

INSERT INTO atlas_driver_offer_bpp.translations ( id, message_key, language, message, created_at, updated_at)
  VALUES
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 1', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 1 യാത്ര', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 2', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 2 യാത്രകൾ', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 5', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 5 യാത്രകൾ', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 8', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 8 യാത്രകൾ', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 10', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 10 യാത്രകൾ', now(), now());

INSERT INTO atlas_driver_offer_bpp.translations ( id, message_key, language, message, created_at, updated_at)
  VALUES
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 1', 'BENGALI', 'এক দিনে ১টি রাইড', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 2', 'BENGALI', 'এক দিনে ২টি রাইড', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 5', 'BENGALI', 'এক দিনে ৫টি রাইড', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 8', 'BENGALI', 'এক দিনে ৮টি রাইড', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 10', 'BENGALI', 'এক দিনে ১০টি রাইড', now(), now());

INSERT INTO atlas_driver_offer_bpp.translations (id, message_key, language, message, created_at, updated_at)
  VALUES
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 1', 'TAMIL', 'ஒரு நாளில் 1 பயணம்', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 2', 'TAMIL', 'ஒரு நாளில் 2 பயணங்கள்', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 5', 'TAMIL', 'ஒரு நாளில் 5 பயணங்கள்', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 8', 'TAMIL', 'ஒரு நாளில் 8 பயணங்கள்', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RidesCompleted 10', 'TAMIL', 'ஒரு நாளில் 10 பயணங்கள்', now(), now());