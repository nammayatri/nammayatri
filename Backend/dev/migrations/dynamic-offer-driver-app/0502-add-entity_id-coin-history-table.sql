

Alter Table atlas_driver_offer_bpp.coin_history ADD COLUMN entity_id varchar(36) ;


INSERT INTO atlas_driver_offer_bpp.translations (id, message_key, language, message)
  VALUES
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RideCompleted', 'ENGLISH', '1 ride in a day'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TwoRidesCompleted', 'ENGLISH', '2 rides in a day'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_FiveRidesCompleted', 'ENGLISH', '5 rides in a day'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_EightPlusRidesInOneDay', 'ENGLISH', '8 rides in a day'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TenRidesCompleted', 'ENGLISH', '10 rides in a day'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_PurpleRideCompleted', 'ENGLISH', 'Purple ride'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_MetroRideCompleted', 'ENGLISH', 'Metro ride'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_OneOrTwoStarRating', 'ENGLISH', '1 or 2 star ride'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_FiveStarRating', 'ENGLISH', '5 star ride'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'DriverToCustomerReferral_DriverReferral', 'ENGLISH', 'Customer referral | *Customer should complete a valid ride'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Cancellation_BookingCancellation', 'ENGLISH', 'Cancel ride'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'CustomerToDriverReferral_CustomerReferral', 'ENGLISH', 'Driver referral | *Driver should complete a valid ride'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'BulkUploadEvent_BulkUploadFunction', 'ENGLISH', 'Bulk upload'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'LeaderBoard_LeaderBoardTopFiveHundred', 'ENGLISH', 'Leaderboard'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Training_TrainingCompleted', 'ENGLISH', 'Complete training');

INSERT INTO atlas_driver_offer_bpp.translations (id, message_key, language, message)
  VALUES
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RideCompleted', 'TELUGU', 'రోజులో ఒక రైడ్'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TwoRidesCompleted', 'TELUGU', 'రోజులో రెండు రైడ్‌లు'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_FiveRidesCompleted', 'TELUGU', 'రోజులో ఐదు రైడ్‌లు'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_EightPlusRidesInOneDay', 'TELUGU', 'రోజులో ఎనిమిది రైడ్‌లు'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TenRidesCompleted', 'TELUGU', 'రోజులో పది రైడ్‌లు'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_PurpleRideCompleted', 'TELUGU', 'పర్పుల్ రైడ్'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_MetroRideCompleted', 'TELUGU', 'మెట్రో రైడ్'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_OneOrTwoStarRating', 'TELUGU', '1 లేదా 2 స్టార్ రైడ్'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_FiveStarRating', 'TELUGU', '5 స్టార్ రైడ్'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'DriverToCustomerReferral_DriverReferral', 'TELUGU', 'కస్టమర్ రిఫరల్ | *కస్టమర్ చెల్లుబాటు అయ్యే రైడ్‌ను పూర్తి చేయాలి'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Cancellation_BookingCancellation', 'TELUGU', 'రైడ్ రద్దు చేయండి'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'CustomerToDriverReferral_CustomerReferral', 'TELUGU', 'డ్రైవర్ రిఫరల్ | *డ్రైవర్ చెల్లుబాటు అయ్యే రైడ్‌ను పూర్తి చేయాలి'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'BulkUploadEvent_BulkUploadFunction', 'TELUGU', 'బల్క్ అప్‌లోడ్'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'LeaderBoard_LeaderBoardTopFiveHundred', 'TELUGU', 'లీడర్‌బోర్డ్'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Training_TrainingCompleted', 'TELUGU', 'శిక్షణను పూర్తి చేయండి');


INSERT INTO atlas_driver_offer_bpp.translations (id, message_key, language, message)
  VALUES
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RideCompleted', 'HINDI', 'एक दिन में 1 सवारी'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TwoRidesCompleted', 'HINDI', 'एक दिन में 2 सवारी'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_FiveRidesCompleted', 'HINDI', 'एक दिन में 5 सवारी'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_EightPlusRidesInOneDay', 'HINDI', 'एक दिन में 8 सवारी'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TenRidesCompleted', 'HINDI', 'एक दिन में 10 सवारी'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_PurpleRideCompleted', 'HINDI', 'बैंगनी सवारी'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_MetroRideCompleted', 'HINDI', 'मेट्रो सवारी'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_OneOrTwoStarRating', 'HINDI', '1 या 2 स्टार सवारी'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_FiveStarRating', 'HINDI', '5 स्टार सवारी'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'DriverToCustomerReferral_DriverReferral', 'HINDI', 'ग्राहक रेफ़रल | *ग्राहक को एक मान्य सवारी पूरी करनी चाहिए'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Cancellation_BookingCancellation', 'HINDI', 'सवारी रद्द करें'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'CustomerToDriverReferral_CustomerReferral', 'HINDI', 'ड्राइवर रेफ़रल | *ड्राइवर को एक मान्य सवारी पूरी करनी चाहिए'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'BulkUploadEvent_BulkUploadFunction', 'HINDI', 'थोक अपलोड'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'LeaderBoard_LeaderBoardTopFiveHundred', 'HINDI', 'लीडरबोर्ड'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Training_TrainingCompleted', 'HINDI', 'प्रशिक्षण पूरा करें');


INSERT INTO atlas_driver_offer_bpp.translations ( id, message_key, language, message)
  VALUES
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RideCompleted', 'KANNADA', 'ಒಂದು ದಿನದಲ್ಲಿ 1 ರೈಡ್'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TwoRidesCompleted', 'KANNADA', 'ಒಂದು ದಿನದಲ್ಲಿ 2 ರೈಡ್‌ಗಳು'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_FiveRidesCompleted', 'KANNADA', 'ಒಂದು ದಿನದಲ್ಲಿ 5 ರೈಡ್‌ಗಳು'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_EightPlusRidesInOneDay', 'KANNADA', 'ಒಂದು ದಿನದಲ್ಲಿ 8 ರೈಡ್‌ಗಳು'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TenRidesCompleted', 'KANNADA', 'ಒಂದು ದಿನದಲ್ಲಿ 10 ರೈಡ್‌ಗಳು'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_PurpleRideCompleted', 'KANNADA', 'ಪರ್ಪಲ್ ರೈಡ್'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_MetroRideCompleted', 'KANNADA', 'ಮೆಟ್ರೋ ರೈಡ್'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_OneOrTwoStarRating', 'KANNADA', '1 ಅಥವಾ 2 ಸ್ಟಾರ್ ರೈಡ್'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_FiveStarRating', 'KANNADA', '5 ಸ್ಟಾರ್ ರೈಡ್'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'DriverToCustomerReferral_DriverReferral', 'KANNADA', 'ಗ್ರಾಹಕರ ರೆಫರಲ್ | *ಗ್ರಾಹಕರು ಮಾನ್ಯವಾದ ರೈಡ್ ಅನ್ನು ಪೂರ್ಣಗೊಳ್ಳಬೇಕು'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Cancellation_BookingCancellation', 'KANNADA', 'ರೈಡ್ ರದ್ದು ಮಾಡು'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'CustomerToDriverReferral_CustomerReferral', 'KANNADA', 'ಡ್ರೈವರ್ ರೆಫರಲ್ | *ಡ್ರೈವರ್ ಮಾನ್ಯವಾದ ರೈಡ್ ಅನ್ನು ಪೂರ್ಣಗೊಳ್ಳಬೇಕು'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'BulkUploadEvent_BulkUploadFunction', 'KANNADA', 'ಬಲ್ಕ್ ಅಪ್‌ಲೋಡ್'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'LeaderBoard_LeaderBoardTopFiveHundred', 'KANNADA', 'ಲೀಡರ್‌ಬೋರ್ಡ್'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Training_TrainingCompleted', 'KANNADA', 'ಪ್ರಶಿಕ್ಷಣವನ್ನು ಪೂರ್ಣಗೊಳಿಸಿ');

INSERT INTO atlas_driver_offer_bpp.translations ( id, message_key, language, message)
  VALUES
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RideCompleted', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 1 യാത്ര'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TwoRidesCompleted', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 2 യാത്രകൾ'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_FiveRidesCompleted', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 5 യാത്രകൾ'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_EightPlusRidesInOneDay', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 8 യാത്രകൾ'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TenRidesCompleted', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 10 യാത്രകൾ'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_PurpleRideCompleted', 'MALAYALAM', 'പർപ്പിൾ യാത്ര'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_MetroRideCompleted', 'MALAYALAM', 'മെട്രോ യാത്ര'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_OneOrTwoStarRating', 'MALAYALAM', '1 അല്ലെങ്കിൽ 2 സ്റ്റാർ യാത്ര'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_FiveStarRating', 'MALAYALAM', '5 സ്റ്റാർ യാത്ര'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'DriverToCustomerReferral_DriverReferral', 'MALAYALAM', 'ഉപഭോക്തൃ റഫറൽ | *ഉപഭോക്താവ് ഒരു സാധുവായ യാത്ര പൂർത്തിയാക്കണം'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Cancellation_BookingCancellation', 'MALAYALAM', 'യാത്ര റദ്ദാക്കുക'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'CustomerToDriverReferral_CustomerReferral', 'MALAYALAM', 'ഡ്രൈവർ റഫറൽ | *ഡ്രൈവർ ഒരു സാധുവായ യാത്ര പൂർത്തിയാക്കണം'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'BulkUploadEvent_BulkUploadFunction', 'MALAYALAM', 'ബൾക്ക് അപ്‌ലോഡ്'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'LeaderBoard_LeaderBoardTopFiveHundred', 'MALAYALAM', 'ലീഡർബോർഡ്'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Training_TrainingCompleted', 'MALAYALAM', 'പരിശീലനം പൂർത്തിയാക്കുക');



INSERT INTO atlas_driver_offer_bpp.translations ( id, message_key, language, message)
  VALUES
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RideCompleted', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 1 യാത്ര'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TwoRidesCompleted', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 2 യാത്രകൾ'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_FiveRidesCompleted', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 5 യാത്രകൾ'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_EightPlusRidesInOneDay', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 8 യാത്രകൾ'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TenRidesCompleted', 'MALAYALAM', 'ഒരു ദിവസത്തിൽ 10 യാത്രകൾ'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_PurpleRideCompleted', 'MALAYALAM', 'പർപ്പിൾ യാത്ര'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_MetroRideCompleted', 'MALAYALAM', 'മെട്രോ യാത്ര'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_OneOrTwoStarRating', 'MALAYALAM', '1 അല്ലെങ്കിൽ 2 സ്റ്റാർ യാത്ര'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_FiveStarRating', 'MALAYALAM', '5 സ്റ്റാർ യാത്ര'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'DriverToCustomerReferral_DriverReferral', 'MALAYALAM', 'ഉപഭോക്തൃ റഫറൽ | *ഉപഭോക്താവ് ഒരു സാധുവായ യാത്ര പൂർത്തിയാക്കണം'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Cancellation_BookingCancellation', 'MALAYALAM', 'യാത്ര റദ്ദാക്കുക'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'CustomerToDriverReferral_CustomerReferral', 'MALAYALAM', 'ഡ്രൈവർ റഫറൽ | *ഡ്രൈവർ ഒരു സാധുവായ യാത്ര പൂർത്തിയാക്കണം'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'BulkUploadEvent_BulkUploadFunction', 'MALAYALAM', 'ബൾക്ക് അപ്‌ലോഡ്'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'LeaderBoard_LeaderBoardTopFiveHundred', 'MALAYALAM', 'ലീഡർബോർഡ്'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Training_TrainingCompleted', 'MALAYALAM', 'പരിശീലനം പൂർത്തിയാക്കുക');

INSERT INTO atlas_driver_offer_bpp.translations ( id, message_key, language, message)
  VALUES
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RideCompleted', 'BENGALI', 'এক দিনে ১টি রাইড'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TwoRidesCompleted', 'BENGALI', 'এক দিনে ২টি রাইড'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_FiveRidesCompleted', 'BENGALI', 'এক দিনে ৫টি রাইড'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_EightPlusRidesInOneDay', 'BENGALI', 'এক দিনে ৮টি রাইড'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TenRidesCompleted', 'BENGALI', 'এক দিনে ১০টি রাইড'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_PurpleRideCompleted', 'BENGALI', 'পার্পল রাইড'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_MetroRideCompleted', 'BENGALI', 'মেট্রো রাইড'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_OneOrTwoStarRating', 'BENGALI', '১ বা ২ স্টার রাইড'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_FiveStarRating', 'BENGALI', '৫ স্টার রাইড'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'DriverToCustomerReferral_DriverReferral', 'BENGALI', 'গ্রাহক রেফারেল | *গ্রাহককে একটি বৈধ রাইড সম্পন্ন করতে হবে'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Cancellation_BookingCancellation', 'BENGALI', 'রাইড বাতিল করুন'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'CustomerToDriverReferral_CustomerReferral', 'BENGALI', 'ড্রাইভার রেফারেল | *ড্রাইভারকে একটি বৈধ রাইড সম্পন্ন করতে হবে'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'BulkUploadEvent_BulkUploadFunction', 'BENGALI', 'বাল্ক আপলোড'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'LeaderBoard_LeaderBoardTopFiveHundred', 'BENGALI', 'লিডারবোর্ড'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Training_TrainingCompleted', 'BENGALI', 'প্রশিক্ষণ সম্পন্ন করুন');

INSERT INTO atlas_driver_offer_bpp.translations (id, message_key, language, message)
  VALUES
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_RideCompleted', 'TAMIL', 'ஒரு நாளில் 1 பயணம்'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TwoRidesCompleted', 'TAMIL', 'ஒரு நாளில் 2 பயணங்கள்'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_FiveRidesCompleted', 'TAMIL', 'ஒரு நாளில் 5 பயணங்கள்'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_EightPlusRidesInOneDay', 'TAMIL', 'ஒரு நாளில் 8 பயணங்கள்'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_TenRidesCompleted', 'TAMIL', 'ஒரு நாளில் 10 பயணங்கள்'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_PurpleRideCompleted', 'TAMIL', 'பர்பிள் பயணம்'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'EndRide_MetroRideCompleted', 'TAMIL', 'மெட்ரோ பயணம்'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_OneOrTwoStarRating', 'TAMIL', '1 அல்லது 2 ஸ்டார் பயணம்'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Rating_FiveStarRating', 'TAMIL', '5 ஸ்டார் பயணம்'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'DriverToCustomerReferral_DriverReferral', 'TAMIL', 'வாடிக்கையாளர் பரிந்துரை | *வாடிக்கையாளர் செல்லுபடியாகும் பயணத்தை முடிக்க வேண்டும்'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Cancellation_BookingCancellation', 'TAMIL', 'பயணத்தை ரத்து செய்யவும்'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'CustomerToDriverReferral_CustomerReferral', 'TAMIL', 'இயக்குனர் பரிந்துரை | *இயக்குனர் செல்லுபடியாகும் பயணத்தை முடிக்க வேண்டும்'),

    (atlas_driver_offer_bpp.uuid_generate_v4(), 'BulkUploadEvent_BulkUploadFunction', 'TAMIL', 'மொத்தப் பதிவேற்றம்'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'LeaderBoard_LeaderBoardTopFiveHundred', 'TAMIL', 'லீடர்போர்டு'),
    (atlas_driver_offer_bpp.uuid_generate_v4(), 'Training_TrainingCompleted', 'TAMIL', 'பயிற்சியை முடிக்கவும்');



