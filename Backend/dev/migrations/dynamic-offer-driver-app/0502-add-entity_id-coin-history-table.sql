

-- this queries are already ran in master
Alter Table atlas_driver_offer_bpp.coin_history ADD COLUMN entity_id varchar(36) ;


INSERT INTO atlas_driver_offer_bpp.translations (created_at, id, message_key, language, message, updated_at)
  VALUES
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RideCompleted', 'ENGLISH', '1 ride in a day', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TwoRidesCompleted', 'ENGLISH', '2 rides in a day', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_FiveRidesCompleted', 'ENGLISH', '5 rides in a day', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_EightPlusRidesInOneDay', 'ENGLISH', '8 rides in a day', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TenRidesCompleted', 'ENGLISH', '10 rides in a day', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_PurpleRideCompleted', 'ENGLISH', 'Purple ride', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_MetroRideCompleted', 'ENGLISH', 'Metro ride', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_OneOrTwoStarRating', 'ENGLISH', '1 or 2 star ride', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_FiveStarRating', 'ENGLISH', '5 star ride', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'DriverToCustomerReferral_DriverReferral', 'ENGLISH', 'Customer referral | *Customer should complete a valid ride', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Cancellation_BookingCancellation', 'ENGLISH', 'Cancel ride', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'CustomerToDriverReferral_CustomerReferral', 'ENGLISH', 'Driver referral | *Driver should complete a valid ride', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'BulkUploadEvent_BulkUploadFunction', 'ENGLISH', 'Bulk upload', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'LeaderBoard_LeaderBoardTopFiveHundred', 'ENGLISH', 'Leaderboard', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Training_TrainingCompleted', 'ENGLISH', 'Complete training', now());

INSERT INTO atlas_driver_offer_bpp.translations (created_at, id, message_key, language, message, updated_at)
  VALUES
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RideCompleted', 'TELUGU', 'రోజులో ఒక రైడ్', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TwoRidesCompleted', 'TELUGU', 'రోజులో రెండు రైడ్‌లు', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_FiveRidesCompleted', 'TELUGU', 'రోజులో ఐదు రైడ్‌లు', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_EightPlusRidesInOneDay', 'TELUGU', 'రోజులో ఎనిమిది రైడ్‌లు', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TenRidesCompleted', 'TELUGU', 'రోజులో పది రైడ్‌లు', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_PurpleRideCompleted', 'TELUGU', 'పర్పుల్ రైడ్', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_MetroRideCompleted', 'TELUGU', 'మెట్రో రైడ్', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_OneOrTwoStarRating', 'TELUGU', '1 లేదా 2 స్టార్ రైడ్', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_FiveStarRating', 'TELUGU', '5 స్టార్ రైడ్', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'DriverToCustomerReferral_DriverReferral', 'TELUGU', 'కస్టమర్ రిఫరల్ | *కస్టమర్ చెల్లుబాటు అయ్యే రైడ్‌ను పూర్తి చేయాలి', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Cancellation_BookingCancellation', 'TELUGU', 'రైడ్ రద్దు చేయండి', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'CustomerToDriverReferral_CustomerReferral', 'TELUGU', 'డ్రైవర్ రిఫరల్ | *డ్రైవర్ చెల్లుబాటు అయ్యే రైడ్‌ను పూర్తి చేయాలి', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'BulkUploadEvent_BulkUploadFunction', 'TELUGU', 'బల్క్ అప్‌లోడ్', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'LeaderBoard_LeaderBoardTopFiveHundred', 'TELUGU', 'లీడర్‌బోర్డ్', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Training_TrainingCompleted', 'TELUGU', 'శిక్షణను పూర్తి చేయండి', now());


INSERT INTO atlas_driver_offer_bpp.translations (created_at, id, message_key, language, message, updated_at)
  VALUES
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RideCompleted', 'HINDI', 'एक दिन में 1 सवारी', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TwoRidesCompleted', 'HINDI', 'एक दिन में 2 सवारी', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_FiveRidesCompleted', 'HINDI', 'एक दिन में 5 सवारी', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_EightPlusRidesInOneDay', 'HINDI', 'एक दिन में 8 सवारी', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TenRidesCompleted', 'HINDI', 'एक दिन में 10 सवारी', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_PurpleRideCompleted', 'HINDI', 'बैंगनी सवारी', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_MetroRideCompleted', 'HINDI', 'मेट्रो सवारी', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_OneOrTwoStarRating', 'HINDI', '1 या 2 स्टार सवारी', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_FiveStarRating', 'HINDI', '5 स्टार सवारी', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'DriverToCustomerReferral_DriverReferral', 'HINDI', 'ग्राहक रेफ़रल | *ग्राहक को एक मान्य सवारी पूरी करनी चाहिए', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Cancellation_BookingCancellation', 'HINDI', 'सवारी रद्द करें', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'CustomerToDriverReferral_CustomerReferral', 'HINDI', 'ड्राइवर रेफ़रल | *ड्राइवर को एक मान्य सवारी पूरी करनी चाहिए', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'BulkUploadEvent_BulkUploadFunction', 'HINDI', 'थोक अपलोड', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'LeaderBoard_LeaderBoardTopFiveHundred', 'HINDI', 'लीडरबोर्ड', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Training_TrainingCompleted', 'HINDI', 'प्रशिक्षण पूरा करें', now());


INSERT INTO atlas_driver_offer_bpp.translations (created_at, id, message_key, language, message, updated_at)
  VALUES
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RideCompleted', 'KANNADA', 'ಒಂದು ದಿನದಲ್ಲಿ 1 ರೈಡ್', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TwoRidesCompleted', 'KANNADA', 'ಒಂದು ದಿನದಲ್ಲಿ 2 ರೈಡ್‌ಗಳು', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_FiveRidesCompleted', 'KANNADA', 'ಒಂದು ದಿನದಲ್ಲಿ 5 ರೈಡ್‌ಗಳು', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_EightPlusRidesInOneDay', 'KANNADA', 'ಒಂದು ದಿನದಲ್ಲಿ 8 ರೈಡ್‌ಗಳು', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TenRidesCompleted', 'KANNADA', 'ಒಂದು ದಿನದಲ್ಲಿ 10 ರೈಡ್‌ಗಳು', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_PurpleRideCompleted', 'KANNADA', 'ಪರ್ಪಲ್ ರೈಡ್', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_MetroRideCompleted', 'KANNADA', 'ಮೆಟ್ರೋ ರೈಡ್', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_OneOrTwoStarRating', 'KANNADA', '1 ಅಥವಾ 2 ಸ್ಟಾರ್ ರೈಡ್', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_FiveStarRating', 'KANNADA', '5 ಸ್ಟಾರ್ ರೈಡ್', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'DriverToCustomerReferral_DriverReferral', 'KANNADA', 'ಗ್ರಾಹಕರ ರೆಫರಲ್ | *ಗ್ರಾಹಕರು ಮಾನ್ಯವಾದ ರೈಡ್ ಅನ್ನು ಪೂರ್ಣಗೊಳ್ಳಬೇಕು', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Cancellation_BookingCancellation', 'KANNADA', 'ರೈಡ್ ರದ್ದು ಮಾಡು', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'CustomerToDriverReferral_CustomerReferral', 'KANNADA', 'ಡ್ರೈವರ್ ರೆಫರಲ್ | *ಡ್ರೈವರ್ ಮಾನ್ಯವಾದ ರೈಡ್ ಅನ್ನು ಪೂರ್ಣಗೊಳ್ಳಬೇಕು', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'BulkUploadEvent_BulkUploadFunction', 'KANNADA', 'ಬಲ್ಕ್ ಅಪ್‌ಲೋಡ್', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'LeaderBoard_LeaderBoardTopFiveHundred', 'KANNADA', 'ಲೀಡರ್‌ಬೋರ್ಡ್', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Training_TrainingCompleted', 'KANNADA', 'ಪ್ರಶಿಕ್ಷಣವನ್ನು ಪೂರ್ಣಗೊಳಿಸಿ', now());





INSERT INTO atlas_driver_offer_bpp.translations (created_at, id, message_key, language, message, updated_at)
  VALUES
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RideCompleted', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 1 യാത്ര', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TwoRidesCompleted', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 2 യാത്രകൾ', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_FiveRidesCompleted', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 5 യാത്രകൾ', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_EightPlusRidesInOneDay', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 8 യാത്രകൾ', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TenRidesCompleted', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 10 യാത്രകൾ', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_PurpleRideCompleted', 'MALAYALAM', 'പർപ്പിൾ യാത്ര', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_MetroRideCompleted', 'MALAYALAM', 'മെട്രോ യാത്ര', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_OneOrTwoStarRating', 'MALAYALAM', '1 അല്ലെങ്കിൽ 2 സ്റ്റാർ യാത്ര', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_FiveStarRating', 'MALAYALAM', '5 സ്റ്റാർ യാത്ര', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'DriverToCustomerReferral_DriverReferral', 'MALAYALAM', 'ഉപഭോക്തൃ റഫറൽ | *ഉപഭോക്താവ് ഒരു സാധുവായ യാത്ര പൂർത്തിയാക്കണം', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Cancellation_BookingCancellation', 'MALAYALAM', 'യാത്ര റദ്ദാക്കുക', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'CustomerToDriverReferral_CustomerReferral', 'MALAYALAM', 'ഡ്രൈവർ റഫറൽ | *ഡ്രൈവർ ഒരു സാധുവായ യാത്ര പൂർത്തിയാക്കണം', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'BulkUploadEvent_BulkUploadFunction', 'MALAYALAM', 'ബൾക്ക് അപ്‌ലോഡ്', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'LeaderBoard_LeaderBoardTopFiveHundred', 'MALAYALAM', 'ലീഡർബോർഡ്', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Training_TrainingCompleted', 'MALAYALAM', 'പരിശീലനം പൂർത്തിയാക്കുക', now());

INSERT INTO atlas_driver_offer_bpp.translations (created_at, id, message_key, language, message, updated_at)
  VALUES
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RideCompleted', 'BENGALI', 'এক দিনে ১টি রাইড', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TwoRidesCompleted', 'BENGALI', 'এক দিনে ২টি রাইড', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_FiveRidesCompleted', 'BENGALI', 'এক দিনে ৫টি রাইড', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_EightPlusRidesInOneDay', 'BENGALI', 'এক দিনে ৮টি রাইড', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TenRidesCompleted', 'BENGALI', 'এক দিনে ১০টি রাইড', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_PurpleRideCompleted', 'BENGALI', 'পার্পল রাইড', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_MetroRideCompleted', 'BENGALI', 'মেট্রো রাইড', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_OneOrTwoStarRating', 'BENGALI', '১ বা ২ স্টার রাইড', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_FiveStarRating', 'BENGALI', '৫ স্টার রাইড', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'DriverToCustomerReferral_DriverReferral', 'BENGALI', 'গ্রাহক রেফারেল | *গ্রাহককে একটি বৈধ রাইড সম্পন্ন করতে হবে', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Cancellation_BookingCancellation', 'BENGALI', 'রাইড বাতিল করুন', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'CustomerToDriverReferral_CustomerReferral', 'BENGALI', 'ড্রাইভার রেফারেল | *ড্রাইভারকে একটি বৈধ রাইড সম্পন্ন করতে হবে', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'BulkUploadEvent_BulkUploadFunction', 'BENGALI', 'বাল্ক আপলোড', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'LeaderBoard_LeaderBoardTopFiveHundred', 'BENGALI', 'লিডারবোর্ড', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Training_TrainingCompleted', 'BENGALI', 'প্রশিক্ষণ সম্পন্ন করুন', now());

INSERT INTO atlas_driver_offer_bpp.translations (created_at, id, message_key, language, message, updated_at)
  VALUES
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RideCompleted', 'TAMIL', 'ஒரு நாளில் 1 பயணம்', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TwoRidesCompleted', 'TAMIL', 'ஒரு நாளில் 2 பயணங்கள்', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_FiveRidesCompleted', 'TAMIL', 'ஒரு நாளில் 5 பயணங்கள்', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_EightPlusRidesInOneDay', 'TAMIL', 'ஒரு நாளில் 8 பயணங்கள்', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TenRidesCompleted', 'TAMIL', 'ஒரு நாளில் 10 பயணங்கள்', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_PurpleRideCompleted', 'TAMIL', 'பர்பிள் பயணம்', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_MetroRideCompleted', 'TAMIL', 'மெட்ரோ பயணம்', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_OneOrTwoStarRating', 'TAMIL', '1 அல்லது 2 ஸ்டார் பயணம்', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_FiveStarRating', 'TAMIL', '5 ஸ்டார் பயணம்', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'DriverToCustomerReferral_DriverReferral', 'TAMIL', 'வாடிக்கையாளர் பரிந்துரை | *வாடிக்கையாளர் செல்லுபடியாகும் பயணத்தை முடிக்க வேண்டும்', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Cancellation_BookingCancellation', 'TAMIL', 'பயணத்தை ரத்து செய்யவும்', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'CustomerToDriverReferral_CustomerReferral', 'TAMIL', 'இயக்குனர் பரிந்துரை | *இயக்குனர் செல்லுபடியாகும் பயணத்தை முடிக்க வேண்டும்', now()),

    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'BulkUploadEvent_BulkUploadFunction', 'TAMIL', 'மொத்தப் பதிவேற்றம்', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'LeaderBoard_LeaderBoardTopFiveHundred', 'TAMIL', 'லீடர்போர்டு', now()),
    (now(), atlas_driver_offer_bpp.uuid_generate_v4(), 'Training_TrainingCompleted', 'TAMIL', 'பயிற்சியை முடிக்கவும்', now());



