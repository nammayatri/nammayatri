INSERT INTO atlas_driver_offer_bpp.translations (id, message_key, language, message, created_at, updated_at)
VALUES
    (atlas_driver_offer_bpp.uuid_generate_v4(),'InvalidFuelType', 'ENGLISH', 'Invalid fuel type', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(),'InvalidVehicleClass', 'ENGLISH', 'Invalid vehicle class', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(),'InvalidOEM', 'ENGLISH', 'Invalid OEM', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(),'InvalidManufacturingYear', 'ENGLISH', 'Invalid manufacturing year', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(),'FitnessCertificateExpired', 'ENGLISH', 'Fitness Certificate expired', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(),'InsuranceExpired', 'ENGLISH', 'Insurance expired', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(),'PermitExpired', 'ENGLISH', 'Permit expired', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(),'PUCExpired', 'ENGLISH', 'PUC expired', now(), now()),

    (atlas_driver_offer_bpp.uuid_generate_v4(),'InvalidFuelType', 'HINDI', 'अमान्य ईंधन प्रकार', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(),'InvalidVehicleClass', 'HINDI', 'अमान्य वाहन वर्ग', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(),'InvalidOEM', 'HINDI', 'अमान्य ओईएम', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(),'InvalidManufacturingYear', 'HINDI', 'अमान्य निर्माण वर्ष', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(),'FitnessCertificateExpired', 'HINDI', 'फिटनेस प्रमाणपत्र समाप्त हो गया', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(),'InsuranceExpired', 'HINDI', 'बीमा समाप्त हो गया', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(),'PermitExpired', 'HINDI', 'परमिट समाप्त हो गया', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(),'PUCExpired', 'HINDI', 'पीयूसी समाप्त हो गया', now(), now()),
    (atlas_driver_offer_bpp.uuid_generate_v4(),'RCInvalid', 'HINDI', 'आपके वाहन का प्रकार वर्तमान में समर्थित नहीं है. कृपया सहायत टीम से संपर्क करें।', now(), now()); -- message can be changed in MSIL environment