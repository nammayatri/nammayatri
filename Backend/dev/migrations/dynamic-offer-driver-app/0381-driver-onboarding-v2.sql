update atlas_driver_offer_bpp.document_verification_config set supported_vehicle_classes_json = json_build_array() where document_type not in ('VehicleRegistrationCertificate', 'DriverLicense');

insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4() , 'UnderManualReview','ENGLISH' , 'Document under review..' , now() , now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4() , 'UnderManualReview','KANNADA' , 'ದಸ್ತಾವೇಜುಗಳು ಪರಿಶೀಲನೆಯಲ್ಲಿದೆ..', now() , now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4() , 'UnderManualReview','TAMIL' , 'ஆவணம் பரிசீலனையில் உள்ளது..' , now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4() , 'UnderManualReview','MALAYALM' , 'പ്രമാണം പരിശോധനയിലാണ്..', now() , now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4() , 'UnderManualReview','BENGALI' , 'দস্তাবেজ পর্যালোচনার অধীন..', now() , now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4() , 'UnderManualReview','HINDI' , 'दस्तावेज़ समीक्षा में..' , now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4() , 'UnderManualReview','TELUGU' , 'సమీక్షలో ఉన్న పత్రం..' , now(), now());