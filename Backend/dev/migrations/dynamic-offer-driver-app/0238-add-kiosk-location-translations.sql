CREATE TABLE atlas_driver_offer_bpp.kiosk_location_translation (
  kiosk_location_id character(36),
  language character(36) NOT NULL,
  landmark character varying(255) NOT NULL,
  address text NOT NULL,
  PRIMARY KEY (kiosk_location_id, language)
);

INSERT INTO atlas_driver_offer_bpp.kiosk_location_translation (kiosk_location_id, language, landmark, address) VALUES
    ('f63ec21a-06b8-3fc4-4697-5fd671dfed26', 'ENGLISH', 'Hyderbad biryani, Arekere main road', 'Arekere Main Rd, Bengaluru, Karnataka 560076'),
    ('f63ec21a-06b8-3fc4-4697-5fd671dfed26', 'TAMIL', 'ஹைதராபாத் பிரியாணி, அரேகெரே பிரதான சாலை' , 'ரரேகெரே மெயின் ரோடு, பெங்களூர், கர்நாடகா 560076'),
    ('f63ec21a-06b8-3fc4-4697-5fd671dfed26', 'KANNADA', 'ಹೈದರಾಬಾದ್ ಬಿರಿಯಾನಿ, ಅರೆಕೆರೆ ಮುಖ್ಯರಸ್ತೆ' , 'ಅರೆಕೆರೆ ಮುಖ್ಯ ರಸ್ತೆ, ಬೆಂಗಳೂರು, ಕರ್ನಾಟಕ 560076'),
    ('f63ec21a-06b8-3fc4-4697-5fd671dfed26', 'HINDI', 'हैदराबाद बिरयानी, अरेकेरे मुख्य सड़क' , 'अरेकेरे मेन रोड, बैंगलोर, कर्नाटक 560076'),

    ('14dfd71a-6ea1-0df6-53d7-ced6186203c8', 'ENGLISH', 'GBJ park, Marathahalli bridge' , 'Jawahar Nagar, Marathahalli, Bengaluru, Karnataka 560037'),
    ('14dfd71a-6ea1-0df6-53d7-ced6186203c8', 'TAMIL', 'ஜிபிஜே பூங்கா, மாரத்தஹள்ளி பாலம்' , 'ஜவஹர் நகர், மாரத்தஹள்ளி, பெங்களூரு, கர்நாடகா 560037'),
    ('14dfd71a-6ea1-0df6-53d7-ced6186203c8', 'KANNADA', 'ಜಿಬಿಜೆ ಪಾರ್ಕ್, ಮಾರತಹಳ್ಳಿ ಸೇತುವೆ' , 'ಜವಾಹರ್ ನಗರ, ಮಾರತಹಳ್ಳಿ, ಬೆಂಗಳೂರು, ಕರ್ನಾಟಕ 560037'),
    ('14dfd71a-6ea1-0df6-53d7-ced6186203c8', 'HINDI', 'जीबीजे पार्क, मराठाहल्ली ब्रिज' , 'जवाहर नगर, मराठाहल्ली, बेंगलुरु, कर्नाटक 560037'),

    ('c64ed661-4821-37f0-c3eb-964d125a8e85', 'ENGLISH', 'Magadi underpass, Magadi toll' , 'Rajaji Nagar Industrial Town, Rajajinagar, Bengaluru, Karnataka'),
    ('c64ed661-4821-37f0-c3eb-964d125a8e85', 'TAMIL', 'மாகடி சுங்கச்சாவடி, மாகடி சுங்கச்சாவடி' , 'ராஜாஜி நகர் தொழில் நகரம், ராஜாஜிநகர், பெங்களூரு, கர்நாடகா'),
    ('c64ed661-4821-37f0-c3eb-964d125a8e85', 'KANNADA', 'ಮಾಗಡಿ ಅಂಡರ್ ಪಾಸ್, ಮಾಗಡಿ ಟೋಲ್' , 'ರಾಜಾಜಿ ನಗರ ಇಂಡಸ್ಟ್ರಿಯಲ್ ಟೌನ್, ರಾಜಾಜಿನಗರ, ಬೆಂಗಳೂರು, ಕರ್ನಾಟಕ'),
    ('c64ed661-4821-37f0-c3eb-964d125a8e85', 'HINDI', 'मगदी अंडरपास, मगदी टोल' , 'राजाजी नगर औद्योगिक शहर, राजाजीनगर, बेंगलुरु, कर्नाटक'),

    ('fa03e867-3d3f-9e81-a81f-67222b640602', 'ENGLISH', 'Sri shaneshwara temple, Hebbal' , 'lake, Bellary Rd, below fly over, near to hebbal, Subramani Nagar, Hebbal, Bengaluru, Karnataka 560024'),
    ('fa03e867-3d3f-9e81-a81f-67222b640602', 'TAMIL', 'ஸ்ரீ சனீஸ்வர கோவில், ஹெப்பல்' , 'ஏரி, பெல்லாரி சாலை, ஃப்ளை ஓவருக்கு கீழே, ஹெப்பலுக்கு அருகில், சுப்ரமணி நகர், ஹெப்பல், பெங்களூரு, கர்நாடகா 560024'),
    ('fa03e867-3d3f-9e81-a81f-67222b640602', 'KANNADA', 'ಶ್ರೀ ಶನೇಶ್ವರ ದೇವಸ್ಥಾನ, ಹೆಬ್ಬಾಳ' , 'ಕೆರೆ, ಬಳ್ಳಾರಿ ರಸ್ತೆ, ಫ್ಲೈ ಓವರ್ ಕೆಳಗೆ, ಹೆಬ್ಬಾಳದ ಹತ್ತಿರ, ಸುಬ್ರಮಣಿ ನಗರ, ಹೆಬ್ಬಾಳ, ಬೆಂಗಳೂರು, ಕರ್ನಾಟಕ 560024'),
    ('fa03e867-3d3f-9e81-a81f-67222b640602', 'HINDI', 'श्री शनेश्वर मंदिर, हेब्बल' , 'झील, बेल्लारी रोड, फ्लाई ओवर के नीचे, हेब्बल के पास, सुब्रमणि नगर, हेब्बल, बेंगलुरु, कर्नाटक 560024'),

    ('80820a44-48b7-8d25-d53d-13124249c45b', 'ENGLISH', 'Yelahanka police station' , 'SH 9, Ambedkar Colony, Yelahanka New Town, Bengaluru, Karnataka 560065'),
    ('80820a44-48b7-8d25-d53d-13124249c45b', 'TAMIL', 'யெலஹங்கா காவல் நிலையம்' , 'SH 9, அம்பேத்கர் காலனி, யெலஹங்கா புதிய நகரம், பெங்களூரு, கர்நாடகா 560065'),
    ('80820a44-48b7-8d25-d53d-13124249c45b', 'KANNADA', 'ಯಲಹಂಕ ಪೊಲೀಸ್ ಠಾಣೆ' , 'SH 9, ಅಂಬೇಡ್ಕರ್ ಕಾಲೋನಿ, ಯಲಹಂಕ ನ್ಯೂ ಟೌನ್, ಬೆಂಗಳೂರು, ಕರ್ನಾಟಕ 560065'),
    ('80820a44-48b7-8d25-d53d-13124249c45b', 'HINDI', 'येलहंका पुलिस स्टेशन' , 'एसएच 9, अंबेडकर कॉलोनी, येलहंका न्यू टाउन, बेंगलुरु, कर्नाटक 560065');

