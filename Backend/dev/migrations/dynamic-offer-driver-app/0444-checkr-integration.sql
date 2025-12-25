insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'Unauthorized', 'KANNADA', 'ವೇದಿಕೆಯಲ್ಲಿ ಅಧಿಕೃತವಾಗಿಲ್ಲ', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'Unauthorized', 'TAMIL', 'மேடையில் அங்கீகரிக்கப்படவில்லை', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'Unauthorized', 'MALAYALM', 'പ്ലാറ്റ്‌ഫോമിൽ അനുമതിയില്ല', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'Unauthorized', 'BENGALI', 'প্ল্যাটফর্মে অনুমোদিত নয়', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'Unauthorized', 'HINDI', 'प्लेटफ़ॉर्म पर अधिकृत नहीं है', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'Unauthorized', 'TELUGU', 'ప్లాట్‌ఫారమ్‌పై అధికారం లేదు', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'Unauthorized', 'ENGLISH', 'Not Authorized on the platform', now(), now());

INSERT INTO atlas_driver_offer_bpp.merchant_service_config (merchant_id, merchant_operating_city_id, service_name, config_json)
SELECT
  m.id,
  moc.id,
  'BackgroundVerification_Checkr',
  json_build_object(
    'url', 'https://api.checkr-staging.com/v1/',
    'returnUrl', 'https://api.checkr-staging.com/v1/',
    'apiKey', '0.1.0|1|ulhKfCaTGXPqbYx7S6mwbWS7hyS8UcaE3GnYxV3HcGeH8SUl6XdzprfqRiPZtl0ymuO+ZUhm4qcjaoPKtNpDzMrjIA7HY1rdiKP9feW+gAr1nj/zjSh+',
    'package', 'test_pro_criminal_and_mvr'
  )
FROM
  atlas_driver_offer_bpp.merchant m
JOIN
  atlas_driver_offer_bpp.merchant_operating_city moc
ON
  m.id = moc.merchant_id;