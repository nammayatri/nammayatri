update atlas_driver_offer_bpp.document_verification_config set supported_vehicle_classes_json = json_build_array() where document_type not in ('VehicleRegistrationCertificate', 'DriverLicense');

insert into atlas_driver_offer_bpp.translations VALUES (now() , atlas_driver_offer_bpp.uuid_generate_v4() ,'ENGLISH' , 'Document under review..' ,  'UnderManualReview', now());
insert into atlas_driver_offer_bpp.translations VALUES (now() , atlas_driver_offer_bpp.uuid_generate_v4() ,'KANNADA' , 'ದಸ್ತಾವೇಜುಗಳು ಪರಿಶೀಲನೆಯಲ್ಲಿದೆ..',  'UnderManualReview', now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4() ,'TAMIL' , 'ஆவணம் பரிசீலனையில் உள்ளது..' ,  'UnderManualReview', now());
insert into atlas_driver_offer_bpp.translations VALUES (now() , atlas_driver_offer_bpp.uuid_generate_v4() ,'MALAYALM' , 'പ്രമാണം പരിശോധനയിലാണ്..',  'UnderManualReview', now());
insert into atlas_driver_offer_bpp.translations VALUES (now() , atlas_driver_offer_bpp.uuid_generate_v4() ,'BENGALI' , 'দস্তাবেজ পর্যালোচনার অধীন..',  'UnderManualReview', now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4() ,'HINDI' , 'दस्तावेज़ समीक्षा में..' ,  'UnderManualReview', now());
insert into atlas_driver_offer_bpp.translations VALUES (now(), atlas_driver_offer_bpp.uuid_generate_v4() ,'TELUGU' , 'సమీక్షలో ఉన్న పత్రం..' ,  'UnderManualReview', now());