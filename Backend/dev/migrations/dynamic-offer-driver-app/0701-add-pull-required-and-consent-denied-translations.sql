-- Add translations for PullRequired and ConsentDenied error messages
-- These are used when DigiLocker document pull is required or consent is denied

-- PullRequired translations
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4(), 'PullRequired', 'ENGLISH', 'Please pull the document from DigiLocker to complete verification', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4(), 'PullRequired', 'KANNADA', 'ದಯವಿಟ್ಟು ಪರಿಶೀಲನೆಯನ್ನು ಪೂರ್ಣಗೊಳಿಸಲು DigiLocker ನಿಂದ ದಾಖಲೆಯನ್ನು ಪುಲ್ ಮಾಡಿ', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4(), 'PullRequired', 'TAMIL', 'சரிபார்ப்பை முடிக்க DigiLocker இலிருந்து ஆவணத்தை இழுக்கவும்', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4(), 'PullRequired', 'MALAYALM', 'പരിശോധന പൂർത്തിയാക്കാൻ DigiLocker ൽ നിന്ന് ഡോക്യുമെന്റ് പുൾ ചെയ്യുക', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4(), 'PullRequired', 'BENGALI', 'যাচাইকরণ সম্পূর্ণ করতে অনুগ্রহ করে DigiLocker থেকে নথিটি টানুন', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4(), 'PullRequired', 'HINDI', 'कृपया सत्यापन पूरा करने के लिए DigiLocker से दस्तावेज़ खींचें', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4(), 'PullRequired', 'TELUGU', 'దయచేసి ధృవీకరణను పూర్తి చేయడానికి DigiLocker నుండి పత్రాన్ని పుల్ చేయండి', now(), now());

-- ConsentDenied translations
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4(), 'ConsentDenied', 'ENGLISH', 'You have denied consent for DigiLocker verification. Please grant consent to proceed', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4(), 'ConsentDenied', 'KANNADA', 'ನೀವು DigiLocker ಪರಿಶೀಲನೆಗೆ ಸಮ್ಮತಿಯನ್ನು ನಿರಾಕರಿಸಿದ್ದೀರಿ. ಮುಂದುವರೆಯಲು ದಯವಿಟ್ಟು ಸಮ್ಮತಿಯನ್ನು ನೀಡಿ', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4(), 'ConsentDenied', 'TAMIL', 'DigiLocker சரிபார்ப்புக்கான சம்மதத்தை நீங்கள் மறுத்துள்ளீர்கள். தொடர சம்மதத்தை வழங்கவும்', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4(), 'ConsentDenied', 'MALAYALM', 'DigiLocker പരിശോധനയ്ക്കുള്ള സമ്മതം നിങ്ങൾ നിരസിച്ചു. തുടരാൻ ദയവായി സമ്മതം നൽകുക', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4(), 'ConsentDenied', 'BENGALI', 'আপনি DigiLocker যাচাইকরণের জন্য সম্মতি অস্বীকার করেছেন। এগিয়ে যেতে অনুগ্রহ করে সম্মতি দিন', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4(), 'ConsentDenied', 'HINDI', 'आपने DigiLocker सत्यापन के लिए सहमति से इनकार कर दिया है। आगे बढ़ने के लिए कृपया सहमति दें', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4(), 'ConsentDenied', 'TELUGU', 'మీరు DigiLocker ధృవీకరణకు సమ్మతిని తిరస్కరించారు. కొనసాగించడానికి దయచేసి సమ్మతిని మంజూరు చేయండి', now(), now());

