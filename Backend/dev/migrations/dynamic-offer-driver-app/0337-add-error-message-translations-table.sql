CREATE TABLE atlas_driver_offer_bpp.error_messages_translations (
    id VARCHAR(36) PRIMARY KEY,
    error_type text NOT NULL,
    language text NOT NULL,
    error_message text NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);


insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4() , 'InvalidDocumentNumber','ENGLISH' , 'Invalid document number. Please enter a valid number and try again' , now() , now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4() , 'InvalidDocumentNumber','KANNADA','ಅಮಾನ್ಯ ಡಾಕ್ಯುಮೆಂಟ್ ಸಂಖ್ಯೆ. ದಯವಿಟ್ಟು ಮಾನ್ಯವಾದ ಸಂಖ್ಯೆಯನ್ನು ನಮೂದಿಸಿ ಮತ್ತು ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ.' , now() , now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4() , 'InvalidDocumentNumber', 'TAMIL', 'ஐடி_கண்டுபிடிக்கப்படவில்லை - தவறான ஆவண எண். சரியான எண்ணை உள்ளிட்டு மீண்டும் முயற்சிக்கவும்' , now() , now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4() , 'InvalidDocumentNumber',  'MALAYALM', 'ഡോക്യുമെന്റ് നമ്പർ അസാധു ആണ്. ദയവായി സാധുവായ ഡോക്യുമെന്റ് നമ്പർ ഉപയോഗിച്ച് വീണ്ടും ശ്രമിക്കുക' , now() , now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4() , 'InvalidDocumentNumber', 'BENGALI', 'অবৈধ নথি নম্বর। অনুগ্রহ করে একটি বৈধ নম্বর লিখে আবার চেষ্টা করুন।' , now() , now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4() , 'InvalidDocumentNumber', 'HINDI', 'अमान्य दस्तावेज़ नंबर। कृपया एक वैध नंबर दर्ज करें और पुनः प्रयास करें' , now() , now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES (atlas_driver_offer_bpp.uuid_generate_v4() , 'InvalidDocumentNumber',  'TELUGU', 'చెల్లని డాక్యుమెంట్ నంబర్. దయచేసి చెల్లుబాటు అయ్యే నంబర్‌ని నమోదు చేసి, మళ్లీ ప్రయత్నించండి.' , now() , now());



insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'VerificationFailed', 'KANNADA', 'ಡಾಕ್ಯುಮೆಂಟ್ ಪರಿಶೀಲನೆ ವಿಫಲವಾಗಿದೆ. ದಯವಿಟ್ಟು ಮತ್ತೊಮ್ಮೆ ಅಪ್‌ಲೋಡ್ ಮಾಡಿ.', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'VerificationFailed', 'TAMIL', 'ஆவணச் சரிபார்ப்பு தோல்வியடைந்தது. மீண்டும் பதிவேற்றவும்', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'VerificationFailed', 'MALAYALM', 'താങ്കളുടെ വാഹന തരം നിലവിൽ സ്വീകാര്യമല്ല. ദയവായി സപ്പോർട്ട് ടീമുമായി ബന്ധപ്പെടുക', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'VerificationFailed', 'BENGALI', 'নথি যাচাইকরণ ব্যর্থ হয়েছে। অনুগ্রহ করে আবার আপলোড করুন।', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'VerificationFailed', 'HINDI', 'दस्तावेज़ सत्यापन विफल हो गया है. कृपया पुनः अपलोड करें.', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'VerificationFailed', 'TELUGU', 'పత్ర ధృవీకరణ విఫలమైంది. దయచేసి మళ్లీ అప్‌లోడ్ చేయండి.', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'VerificationFailed', 'ENGLISH', 'Document verification has failed. Please upload again.', now(), now());


insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'NoDcoumentFound', 'KANNADA', 'ಯಾವುದೇ ಡಾಕ್ಯುಮೆಂಟ್ ಕಂಡುಬಂದಿಲ್ಲ', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'NoDcoumentFound', 'TAMIL', 'ஆவணம் எதுவும் கிடைக்கவில்லை', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'NoDcoumentFound', 'MALAYALM', 'ഡോക്യുമെന്റ് ലഭ്യമല്ല', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'NoDcoumentFound', 'BENGALI', 'কোনও ডকুমেন্ট পাওয়া যায়নি', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'NoDcoumentFound', 'HINDI', 'कोई दस्तावेज़ नहीं मिला', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'NoDcoumentFound', 'TELUGU', 'ఏమిటిలాగినా పత్రం కనపడలేదు', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'NoDcoumentFound', 'ENGLISH', 'No document found', now(), now());


insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'LimitExceed', 'KANNADA', 'ನೀವು ಹಲವಾರು ಬಾರಿ ಅಪ್‌ಲೋಡ್ ಮಾಡಿರುವಿರಿ. ದಯವಿಟ್ಟು ಬೆಂಬಲ ತಂಡವನ್ನು ಸಂಪರ್ಕಿಸಿ.', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'LimitExceed', 'TAMIL', 'நீங்கள் பல முறை பதிவேற்றியுள்ளீர்கள். ஆதரவைத் தொடர்பு கொள்ளவும்', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'LimitExceed', 'MALAYALM', 'താങ്കളുടെ അപ്‌ലോഡ് പരിധി കഴിഞ്ഞു. ദയവായി സപ്പോർട്ട് ടീമുമായി ബന്ധപ്പെടുക', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'LimitExceed', 'BENGALI', 'আপনি অনেকবার আপলোড করেছেন। অনুগ্রহ করে চালক সেবার সাথে যোগাযোগ করুন।', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'LimitExceed', 'HINDI', 'आपने बहुत अधिक बार अपलोड किया है. कृपया सहायता टीम से संपर्क करें.', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'LimitExceed', 'TELUGU', 'మీరు చాలా సార్లు అప్‌లోడ్ చేసారు. దయచేసి మద్దతు బృందాన్ని సంప్రదించండి.', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'LimitExceed', 'ENGLISH', 'Your have uploaded too many times. Please contact support.', now(), now());


insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'DLInvalid', 'KANNADA', 'ನೀವು ಚಾಲಕ ಬೆಂಬಲಿತ ವಾಹನಕ್ಕೆ ಪರವಾನಗಿ ಹೊಂದಿಲ್ಲ. ದಯವಿಟ್ಟು ಬೆಂಬಲವನ್ನು ಸಂಪರ್ಕಿಸಿ.', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'DLInvalid', 'TAMIL', 'ஓட்டுனர் ஆதரவு வாகனத்திற்கான உரிமம் உங்களிடம் இல்லை. ஆதரவைத் தொடர்பு கொள்ளவும்.', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'DLInvalid', 'MALAYALM', 'ഡ്രൈവർ പിന്തുണയ്ക്കുന്ന വാഹനത്തിനുള്ള ലൈസൻസ് നിങ്ങൾക്കില്ല. പിന്തുണയുമായി ബന്ധപ്പെടുക.', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'DLInvalid', 'BENGALI', 'আপনার ড্রাইভার সমর্থিত গাড়ির লাইসেন্স নেই। অনুগ্রহ করে চালক সেবার সাথে যোগাযোগ করুন।', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'DLInvalid', 'HINDI', 'आपके पास समर्थित वाहन चालन के लिए लाइसेंस नहीं है। कृपया सहायता टीम से संपर्क करें.', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'DLInvalid', 'TELUGU', 'మీకు డ్రైవింగ్ సపోర్ట్ చేసే వాహనానికి లైసెన్స్ లేదు. దయచేసి మద్దతును సంప్రదించండి.', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'DLInvalid', 'ENGLISH', 'Your dont have license to driver supported vehicle. Please contact support.', now(), now());


insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'RCInvalid', 'KANNADA', 'ನಿಮ್ಮ ವಾಹನದ ಪ್ರಕಾರವು ಪ್ರಸ್ತುತ ಬೆಂಬಲಿತವಾಗಿಲ್ಲ. ದಯವಿಟ್ಟು ಬೆಂಬಲ ತಂಡವನ್ನು ಸಂಪರ್ಕಿಸಿ.', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'RCInvalid', 'TAMIL', 'உங்கள் வாகன வகை தற்போது ஆதரிக்கப்படவில்லை. ஆதரவைத் தொடர்பு கொள்ளவும்', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'RCInvalid', 'MALAYALM', 'താങ്കളുടെ വാഹന തരം നിലവിൽ സ്വീകാര്യമല്ല. ദയവായി സപ്പോർട്ട് ടീമുമായി ബന്ധപ്പെടുക', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'RCInvalid', 'BENGALI', 'আপনার গাড়ি বর্তমানে সমর্থিত নয়। অনুগ্রহ করে চালক সেবার সাথে যোগাযোগ করুন।', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'RCInvalid', 'HINDI', 'आपके वाहन का प्रकार वर्तमान में समर्थित नहीं है. कृपया सहायत टीम से संपर्क करें।', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'RCInvalid', 'TELUGU', 'మీ వాహనం రకానికి ప్రస్తుతం మద్దతు లేదు. దయచేసి మద్దతు బృందాన్ని సంప్రదించండి.', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'RCInvalid', 'ENGLISH', 'Your vehicle is currently not supported. Please contact support.', now(), now());


insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'DocumentValid', 'KANNADA', 'ದಾಖಲೆ ಪರಿಶೀಲನೆ ಯಶಸ್ವಿಯಾಗಿದೆ', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'DocumentValid', 'TAMIL', 'ஆவணம் வெற்றிகரமாக சரிபார்க்கப்பட்டது', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'DocumentValid', 'MALAYALM', 'ഡോക്യുമെന്റ് വെരിഫിക്കേഷൻ പൂർത്തിയായി', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'DocumentValid', 'BENGALI', 'দস্তাবেজ সফলভাবে যাচাই করা হয়েছে।', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'DocumentValid', 'HINDI', 'दस्तावेज़ सत्यापन सफल', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'DocumentValid', 'TELUGU', 'పత్ర ధృవీకరణ విజయవంతమైంది', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'DocumentValid', 'ENGLISH', 'Document verification successful', now(), now());


insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'VerificationInProgress', 'KANNADA', 'ದಾಖಲೆ ಪರಿಶೀಲನೆ ಪ್ರಗತಿಯಲ್ಲಿದೆ. ದಯವಿಟ್ಟು ನಂತರ ಸ್ಥಿತಿಯನ್ನು ಪರಿಶೀಲಿಸಿ.', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'VerificationInProgress', 'TAMIL', 'ஆவண சரிபார்ப்பு நடந்து வருகிறது. பின்னர் நிலையைச் சரிபார்க்கவும்', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'VerificationInProgress', 'MALAYALM', 'ഡോക്യുമെന്റ് വെരിഫിക്കേഷൻ പുരോഗതിയിലാണ്. അല്പസമയം കഴിഞ്ഞു വീണ്ടും ശ്രമിക്കുക', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'VerificationInProgress', 'BENGALI', 'দস্তাবেজ যাচাইকরণ চলছে। অনুগ্রহ করে কিছুক্ষুণ পরে স্ট্যাটাস চেক করুন।', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'VerificationInProgress', 'HINDI', 'दस्तावेज़ सत्यापन चल रहा है. कृपया बाद में स्थिति जांचें.', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'VerificationInProgress', 'TELUGU', 'డాక్యుమెంట్ వెరిఫికేషన్ ప్రోగ్రెస్‌లో ఉంది. దయచేసి తర్వాత స్థితిని తనిఖీ చేయండి.', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'VerificationInProgress', 'ENGLISH', 'Document verification is in progress. Please check the status later.', now(), now());


insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'Other', 'KANNADA', 'ಏನೋ ತಪ್ಪಾಗಿದೆ. ದಯವಿಟ್ಟು ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ ಅಥವಾ ಸಹಾಯಕ್ಕಾಗಿ ಬೆಂಬಲವನ್ನು ಸಂಪರ್ಕಿಸಿ.', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'Other', 'TAMIL', 'ஏதோ தவறு நடந்துவிட்டது. மீண்டும் முயற்சிக்கவும் அல்லது உதவிக்கு ஆதரவைத் தொடர்பு கொள்ளவும்', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'Other', 'MALAYALM', 'സാങ്കേതിക പിഴവ് സംഭവിച്ചു. വീണ്ടും ശ്രമിക്കുക അല്ലെങ്കിൽ സഹായത്തിനായി സപ്പോർട്ട് ടീമുമായി ബന്ധപ്പെടുക', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'Other', 'BENGALI', 'কিছু ভুল হয়েছে। অনুগ্রহ করে আবার চেষ্টা করুন বা সহায়তার জন্য চালক সেবার সাথে যোগাযোগ করুন।', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'Other', 'HINDI', 'कुछ गलत हो गया। कृपया पुनः प्रयास करें या सहायता के लिए सहायता टीम से संपर्क करें।', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'Other', 'TELUGU', 'ఎక్కడో తేడ జరిగింది. దయచేసి మళ్లీ ప్రయత్నించండి లేదా సహాయం కోసం మద్దతును సంప్రదించండి.', now(), now());
insert into atlas_driver_offer_bpp.error_messages_translations VALUES  (atlas_driver_offer_bpp.uuid_generate_v4(), 'Other', 'ENGLISH', 'Something went wrong. Please try again or contact support for assistance.', now(), now());




