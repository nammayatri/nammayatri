INSERT INTO atlas_driver_offer_bpp.call_feedback_options (created_at,updated_at, id, merchant_id, merchant_operating_city_id, category, message_key)
    SELECT
        now(),
        now(),
        atlas_driver_offer_bpp.uuid_generate_v4(),
        T1.merchant_id,
        T1.id,
        'EKD_LIVE_CALL_FEEDBACK',
        'FARE_TOO_LOW'
    FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.call_feedback_options (created_at,updated_at, id, merchant_id, merchant_operating_city_id, category, message_key)
    SELECT
        now(),
        now(),
        atlas_driver_offer_bpp.uuid_generate_v4(),
        T1.merchant_id,
        T1.id,
        'EKD_LIVE_CALL_FEEDBACK',
        'HIGH_TRAFFIC'
    FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.call_feedback_options (created_at,updated_at, id, merchant_id, merchant_operating_city_id, category, message_key)
    SELECT
        now(),
        now(),
        atlas_driver_offer_bpp.uuid_generate_v4(),
        T1.merchant_id,
        T1.id,
        'EKD_LIVE_CALL_FEEDBACK',
        'NO_RETURN_TRIP'
    FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.call_feedback_options (created_at,updated_at, id, merchant_id, merchant_operating_city_id, category, message_key)
    SELECT
        now(),
        now(),
        atlas_driver_offer_bpp.uuid_generate_v4(),
        T1.merchant_id,
        T1.id,
        'EKD_LIVE_CALL_FEEDBACK',
        'EXTRA_LUGGAGE'
    FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
    ON CONFLICT DO NOTHING;

INSERT INTO atlas_driver_offer_bpp.call_feedback_options (created_at,updated_at, id, merchant_id, merchant_operating_city_id, category, message_key)
    SELECT
        now(),
        now(),
        atlas_driver_offer_bpp.uuid_generate_v4(),
        T1.merchant_id,
        T1.id,
        'EKD_LIVE_CALL_FEEDBACK',
        'PICKUP_TOO_FAR'
    FROM atlas_driver_offer_bpp.merchant_operating_city AS T1
    ON CONFLICT DO NOTHING;


insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'ENGLISH', 'Fare too low', 'FARE_TOO_LOW',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'KANNADA', 'ಭಾಡೆ ತುಂಬಾ ಕಡಿಮೆಯಾಗಿದೆ', 'FARE_TOO_LOW',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'TAMIL', 'கட்டணம் மிகக்குறைவு', 'FARE_TOO_LOW',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'MALAYALM', 'കൂലി വളരെ കുറവാണ്', 'FARE_TOO_LOW',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'BENGALI', 'ভাড়া খুব কম', 'FARE_TOO_LOW',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'HINDI', 'किराया बहुत कम है', 'FARE_TOO_LOW',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'TELUGU', 'అద్దె చాలా తక్కువగా ఉంది', 'FARE_TOO_LOW',now());

insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'ENGLISH', 'High Traffic', 'HIGH_TRAFFIC',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'KANNADA', 'ಹೆಚ್ಚಿನ ಸಂಚಾರ', 'HIGH_TRAFFIC',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'TAMIL', 'போக்குவரத்து நெரிசல்', 'HIGH_TRAFFIC',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'MALAYALM', 'കൂടിയ ട്രാഫിക്', 'HIGH_TRAFFIC',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'BENGALI', 'ভিড় ট্র্যাফিক', 'HIGH_TRAFFIC',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'HINDI', 'अत्यधिक ट्रैफिक', 'HIGH_TRAFFIC',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'TELUGU', 'అధిక ట్రాఫిక్', 'HIGH_TRAFFIC',now());

insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'ENGLISH', 'No return trip', 'NO_RETURN_TRIP',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'KANNADA', 'ಮರುಯಾತ್ರೆ ಇಲ್ಲ', 'NO_RETURN_TRIP',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'TAMIL', 'திரும்பும் பயணம் இல்லை', 'NO_RETURN_TRIP',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'MALAYALM', 'മടങ്ങിയ യാത്ര ഇല്ല', 'NO_RETURN_TRIP',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'BENGALI', 'ফিরতি যাত্রা নেই', 'NO_RETURN_TRIP',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'HINDI', 'वापसी यात्रा नहीं है', 'NO_RETURN_TRIP',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'TELUGU', 'రీటర్న్ ట్రిప్ లేదు', 'NO_RETURN_TRIP',now());

insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'ENGLISH', 'Extra luggage', 'EXTRA_LUGGAGE',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'KANNADA', 'ಹೆಚ್ಚಿನ ಸಾಮಾನು', 'EXTRA_LUGGAGE',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'TAMIL', 'கூடுதல் சுமை', 'EXTRA_LUGGAGE',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'MALAYALM', 'കൂടുതൽ ചരക്ക്', 'EXTRA_LUGGAGE',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'BENGALI', 'অতিরিক্ত লাগেজ', 'EXTRA_LUGGAGE',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'HINDI', 'अतिरिक्त सामान', 'EXTRA_LUGGAGE',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'TELUGU', 'అతిరిక్త సామాను', 'EXTRA_LUGGAGE',now());

insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'ENGLISH', 'Pickup too far', 'PICKUP_TOO_FAR',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'KANNADA', 'ಪಿಕಪ್ ಬಹಳ ದೂರದಲ್ಲಿದೆ', 'PICKUP_TOO_FAR',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'TAMIL', 'பிக்அப் மிகத் தொலைவில் உள்ளது', 'PICKUP_TOO_FAR',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'MALAYALM', 'പിക്കപ്പ് വളരെ ദൂരെയുണ്ട്', 'PICKUP_TOO_FAR',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'BENGALI', 'পিকআপ অনেক দূরে', 'PICKUP_TOO_FAR',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'HINDI', 'पिकअप बहुत दूर है', 'PICKUP_TOO_FAR',now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4(),  'TELUGU', 'పికప్ చాలా దూరంగా ఉంది', 'PICKUP_TOO_FAR',now());




