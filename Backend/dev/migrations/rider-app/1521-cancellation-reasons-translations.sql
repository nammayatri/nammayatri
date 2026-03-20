INSERT INTO atlas_app.translations (
  message_key,
  language,
  message,
  merchant_operating_city_id,
  id,
  created_at,
  updated_at
)
VALUES
  -- ENGLISH
  ('WAIT_TIME_TOO_LONG', 'ENGLISH', 'Wait time too long', NULL, gen_random_uuid(), now(), now()),
  ('GOT_ANOTHER_RIDE', 'ENGLISH', 'Got another ride', NULL, gen_random_uuid(), now(), now()),
  ('WRONG_PICKUP_LOCATION', 'ENGLISH', 'Wrong pickup location', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_NOT_MOVING', 'ENGLISH', 'Driver not moving towards pickup', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_UNAVAILABLE', 'ENGLISH', 'Driver unavailable', NULL, gen_random_uuid(), now(), now()),
  ('AC_NOT_TURNED_ON', 'ENGLISH', 'AC not turned on', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_DEMANDED_EXTRA', 'ENGLISH', 'Driver demanded extra money', NULL, gen_random_uuid(), now(), now()),
  ('OTHER', 'ENGLISH', 'My issue is not listed here', NULL, gen_random_uuid(), now(), now()),

  -- HINDI
  ('WAIT_TIME_TOO_LONG', 'HINDI', 'प्रतीक्षा समय बहुत लंबा', NULL, gen_random_uuid(), now(), now()),
  ('GOT_ANOTHER_RIDE', 'HINDI', 'दूसरी सवारी मिल गई', NULL, gen_random_uuid(), now(), now()),
  ('WRONG_PICKUP_LOCATION', 'HINDI', 'गलत पिकअप स्थान', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_NOT_MOVING', 'HINDI', 'ड्राइवर पिकअप की ओर नहीं बढ़ रहा', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_UNAVAILABLE', 'HINDI', 'ड्राइवर उपलब्ध नहीं', NULL, gen_random_uuid(), now(), now()),
  ('AC_NOT_TURNED_ON', 'HINDI', 'एसी चालू नहीं', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_DEMANDED_EXTRA', 'HINDI', 'ड्राइवर ने अतिरिक्त पैसे मांगे', NULL, gen_random_uuid(), now(), now()),
  ('OTHER', 'HINDI', 'मेरी समस्या यहां सूचीबद्ध नहीं है', NULL, gen_random_uuid(), now(), now()),

  -- BENGALI
  ('WAIT_TIME_TOO_LONG', 'BENGALI', 'অপেক্ষার সময় খুব বেশি', NULL, gen_random_uuid(), now(), now()),
  ('GOT_ANOTHER_RIDE', 'BENGALI', 'অন্য একটি রাইড পেয়ে গেছি', NULL, gen_random_uuid(), now(), now()),
  ('WRONG_PICKUP_LOCATION', 'BENGALI', 'ভুল পিকআপ লোকেশন', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_NOT_MOVING', 'BENGALI', 'ড্রাইভার পিকআপের দিকে এগোচ্ছে না', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_UNAVAILABLE', 'BENGALI', 'ড্রাইভার উপলব্ধ নন', NULL, gen_random_uuid(), now(), now()),
  ('AC_NOT_TURNED_ON', 'BENGALI', 'এসি চালু করা হয়নি', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_DEMANDED_EXTRA', 'BENGALI', 'ড্রাইভার অতিরিক্ত টাকা চেয়েছেন', NULL, gen_random_uuid(), now(), now()),
  ('OTHER', 'BENGALI', 'আমার সমস্যাটি এখানে তালিকাভুক্ত নয়', NULL, gen_random_uuid(), now(), now()),

  -- DUTCH
  ('WAIT_TIME_TOO_LONG', 'DUTCH', 'De wachttijd is te lang', NULL, gen_random_uuid(), now(), now()),
  ('GOT_ANOTHER_RIDE', 'DUTCH', 'Ik heb een andere rit gevonden', NULL, gen_random_uuid(), now(), now()),
  ('WRONG_PICKUP_LOCATION', 'DUTCH', 'Verkeerde ophaallocatie', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_NOT_MOVING', 'DUTCH', 'De chauffeur rijdt niet naar de ophaallocatie', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_UNAVAILABLE', 'DUTCH', 'Chauffeur niet beschikbaar', NULL, gen_random_uuid(), now(), now()),
  ('AC_NOT_TURNED_ON', 'DUTCH', 'Airconditioning staat niet aan', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_DEMANDED_EXTRA', 'DUTCH', 'De chauffeur vroeg om extra geld', NULL, gen_random_uuid(), now(), now()),
  ('OTHER', 'DUTCH', 'Mijn probleem staat hier niet vermeld', NULL, gen_random_uuid(), now(), now()),

  -- FINNISH
  ('WAIT_TIME_TOO_LONG', 'FINNISH', 'Odotusaika on liian pitkä', NULL, gen_random_uuid(), now(), now()),
  ('GOT_ANOTHER_RIDE', 'FINNISH', 'Sain toisen kyydin', NULL, gen_random_uuid(), now(), now()),
  ('WRONG_PICKUP_LOCATION', 'FINNISH', 'Väärä noutopaikka', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_NOT_MOVING', 'FINNISH', 'Kuljettaja ei ole liikkumassa kohti noutopaikkaa', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_UNAVAILABLE', 'FINNISH', 'Kuljettaja ei ole saatavilla', NULL, gen_random_uuid(), now(), now()),
  ('AC_NOT_TURNED_ON', 'FINNISH', 'Ilmastointia ei ole kytketty päälle', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_DEMANDED_EXTRA', 'FINNISH', 'Kuljettaja vaati ylimääräistä rahaa', NULL, gen_random_uuid(), now(), now()),
  ('OTHER', 'FINNISH', 'Ongelmaani ei ole listattu täällä', NULL, gen_random_uuid(), now(), now()),

  -- FRENCH
  ('WAIT_TIME_TOO_LONG', 'FRENCH', 'Temps d’attente trop long', NULL, gen_random_uuid(), now(), now()),
  ('GOT_ANOTHER_RIDE', 'FRENCH', 'J’ai trouvé une autre course', NULL, gen_random_uuid(), now(), now()),
  ('WRONG_PICKUP_LOCATION', 'FRENCH', 'Mauvais lieu de prise en charge', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_NOT_MOVING', 'FRENCH', 'Le chauffeur ne se dirige pas vers le point de prise en charge', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_UNAVAILABLE', 'FRENCH', 'Chauffeur indisponible', NULL, gen_random_uuid(), now(), now()),
  ('AC_NOT_TURNED_ON', 'FRENCH', 'La climatisation n’est pas allumée', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_DEMANDED_EXTRA', 'FRENCH', 'Le chauffeur a demandé un supplément', NULL, gen_random_uuid(), now(), now()),
  ('OTHER', 'FRENCH', 'Mon problème n’est pas listé ici', NULL, gen_random_uuid(), now(), now()),

  -- GERMAN
  ('WAIT_TIME_TOO_LONG', 'GERMAN', 'Die Wartezeit ist zu lang', NULL, gen_random_uuid(), now(), now()),
  ('GOT_ANOTHER_RIDE', 'GERMAN', 'Ich habe eine andere Fahrt gefunden', NULL, gen_random_uuid(), now(), now()),
  ('WRONG_PICKUP_LOCATION', 'GERMAN', 'Falscher Abholort', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_NOT_MOVING', 'GERMAN', 'Der Fahrer bewegt sich nicht zum Abholort', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_UNAVAILABLE', 'GERMAN', 'Fahrer nicht verfügbar', NULL, gen_random_uuid(), now(), now()),
  ('AC_NOT_TURNED_ON', 'GERMAN', 'Klimaanlage ist nicht eingeschaltet', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_DEMANDED_EXTRA', 'GERMAN', 'Der Fahrer hat zusätzliches Geld verlangt', NULL, gen_random_uuid(), now(), now()),
  ('OTHER', 'GERMAN', 'Mein Problem ist hier nicht aufgeführt', NULL, gen_random_uuid(), now(), now()),

  -- GUJARATI
  ('WAIT_TIME_TOO_LONG', 'GUJARATI', 'રાહ જોવાનો સમય બહુ લાંબો છે', NULL, gen_random_uuid(), now(), now()),
  ('GOT_ANOTHER_RIDE', 'GUJARATI', 'મને બીજી રાઈડ મળી ગઈ', NULL, gen_random_uuid(), now(), now()),
  ('WRONG_PICKUP_LOCATION', 'GUJARATI', 'ખોટું પિકઅપ સ્થાન', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_NOT_MOVING', 'GUJARATI', 'ડ્રાઈવર પિકઅપ તરફ આગળ વધી રહ્યો નથી', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_UNAVAILABLE', 'GUJARATI', 'ડ્રાઈવર ઉપલબ્ધ નથી', NULL, gen_random_uuid(), now(), now()),
  ('AC_NOT_TURNED_ON', 'GUJARATI', 'એસી ચાલુ કરવામાં આવ્યો નથી', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_DEMANDED_EXTRA', 'GUJARATI', 'ડ્રાઈવરે વધારાના પૈસા માંગ્યા', NULL, gen_random_uuid(), now(), now()),
  ('OTHER', 'GUJARATI', 'મારી સમસ્યા અહીં સૂચિબદ્ધ નથી', NULL, gen_random_uuid(), now(), now()),

  -- KANNADA
  ('WAIT_TIME_TOO_LONG', 'KANNADA', 'ಕಾಯುವ ಸಮಯ ತುಂಬಾ ಉದ್ದವಾಗಿದೆ', NULL, gen_random_uuid(), now(), now()),
  ('GOT_ANOTHER_RIDE', 'KANNADA', 'ನನಗೆ ಮತ್ತೊಂದು ರೈಡ್ ಸಿಕ್ಕಿದೆ', NULL, gen_random_uuid(), now(), now()),
  ('WRONG_PICKUP_LOCATION', 'KANNADA', 'ತಪ್ಪು ಪಿಕಪ್ ಸ್ಥಳ', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_NOT_MOVING', 'KANNADA', 'ಚಾಲಕ ಪಿಕಪ್ ಕಡೆಗೆ ಬರುತ್ತಿಲ್ಲ', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_UNAVAILABLE', 'KANNADA', 'ಚಾಲಕ ಲಭ್ಯವಿಲ್ಲ', NULL, gen_random_uuid(), now(), now()),
  ('AC_NOT_TURNED_ON', 'KANNADA', 'ಎಸಿ ಆನ್ ಮಾಡಲಾಗಿಲ್ಲ', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_DEMANDED_EXTRA', 'KANNADA', 'ಚಾಲಕ ಹೆಚ್ಚುವರಿ ಹಣ ಕೇಳಿದರು', NULL, gen_random_uuid(), now(), now()),
  ('OTHER', 'KANNADA', 'ನನ್ನ ಸಮಸ್ಯೆ ಇಲ್ಲಿ ಪಟ್ಟಿ ಮಾಡಲಾಗಿಲ್ಲ', NULL, gen_random_uuid(), now(), now()),

  -- MALAYALAM
  ('WAIT_TIME_TOO_LONG', 'MALAYALAM', 'കാത്തിരിപ്പ് സമയം വളരെ കൂടുതലാണ്', NULL, gen_random_uuid(), now(), now()),
  ('GOT_ANOTHER_RIDE', 'MALAYALAM', 'എനിക്ക് മറ്റൊരു റൈഡ് കിട്ടി', NULL, gen_random_uuid(), now(), now()),
  ('WRONG_PICKUP_LOCATION', 'MALAYALAM', 'തെറ്റായ പിക്കപ്പ് സ്ഥലം', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_NOT_MOVING', 'MALAYALAM', 'ഡ്രൈവർ പിക്കപ്പ് സ്ഥലത്തേക്ക് വരുന്നതില്ല', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_UNAVAILABLE', 'MALAYALAM', 'ഡ്രൈവർ ലഭ്യമല്ല', NULL, gen_random_uuid(), now(), now()),
  ('AC_NOT_TURNED_ON', 'MALAYALAM', 'എസി ഓണാക്കിയിട്ടില്ല', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_DEMANDED_EXTRA', 'MALAYALAM', 'ഡ്രൈവർ അധിക പണം ആവശ്യപ്പെട്ടു', NULL, gen_random_uuid(), now(), now()),
  ('OTHER', 'MALAYALAM', 'എന്റെ പ്രശ്നം ഇവിടെ പട്ടികപ്പെടുത്തിയിട്ടില്ല', NULL, gen_random_uuid(), now(), now()),

  -- ODIA
  ('WAIT_TIME_TOO_LONG', 'ODIA', 'ଅପେକ୍ଷା ସମୟ ବହୁତ ଦୀର୍ଘ', NULL, gen_random_uuid(), now(), now()),
  ('GOT_ANOTHER_RIDE', 'ODIA', 'ମୁଁ ଅନ୍ୟ ଗୋଟିଏ ରାଇଡ୍ ପାଇଲି', NULL, gen_random_uuid(), now(), now()),
  ('WRONG_PICKUP_LOCATION', 'ODIA', 'ଭୁଲ ପିକଅପ୍ ସ୍ଥାନ', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_NOT_MOVING', 'ODIA', 'ଡ୍ରାଇଭର ପିକଅପ୍ ଦିଗକୁ ଆସୁନାହାନ୍ତି', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_UNAVAILABLE', 'ODIA', 'ଡ୍ରାଇଭର ଉପଲବ୍ଧ ନାହାନ୍ତି', NULL, gen_random_uuid(), now(), now()),
  ('AC_NOT_TURNED_ON', 'ODIA', 'ଏସି ଚାଲୁ କରାଯାଇନି', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_DEMANDED_EXTRA', 'ODIA', 'ଡ୍ରାଇଭର ଅତିରିକ୍ତ ଟଙ୍କା ଚାହିଲେ', NULL, gen_random_uuid(), now(), now()),
  ('OTHER', 'ODIA', 'ମୋ ସମସ୍ୟା ଏଠାରେ ତାଲିକାଭୁକ୍ତ ନୁହେଁ', NULL, gen_random_uuid(), now(), now()),

  -- SWEDISH
  ('WAIT_TIME_TOO_LONG', 'SWEDISH', 'Väntetiden är för lång', NULL, gen_random_uuid(), now(), now()),
  ('GOT_ANOTHER_RIDE', 'SWEDISH', 'Jag fick en annan resa', NULL, gen_random_uuid(), now(), now()),
  ('WRONG_PICKUP_LOCATION', 'SWEDISH', 'Fel upphämtningsplats', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_NOT_MOVING', 'SWEDISH', 'Föraren rör sig inte mot upphämtningsplatsen', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_UNAVAILABLE', 'SWEDISH', 'Föraren är inte tillgänglig', NULL, gen_random_uuid(), now(), now()),
  ('AC_NOT_TURNED_ON', 'SWEDISH', 'AC:n är inte påslagen', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_DEMANDED_EXTRA', 'SWEDISH', 'Föraren krävde extra pengar', NULL, gen_random_uuid(), now(), now()),
  ('OTHER', 'SWEDISH', 'Mitt problem finns inte listat här', NULL, gen_random_uuid(), now(), now()),

  -- TAMIL
  ('WAIT_TIME_TOO_LONG', 'TAMIL', 'காத்திருக்கும் நேரம் மிகவும் நீண்டது', NULL, gen_random_uuid(), now(), now()),
  ('GOT_ANOTHER_RIDE', 'TAMIL', 'எனக்கு மற்றொரு ரைடு கிடைத்தது', NULL, gen_random_uuid(), now(), now()),
  ('WRONG_PICKUP_LOCATION', 'TAMIL', 'தவறான பிக்கப் இடம்', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_NOT_MOVING', 'TAMIL', 'ஓட்டுநர் பிக்கப் இடத்தை நோக்கி வரவில்லை', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_UNAVAILABLE', 'TAMIL', 'ஓட்டுநர் கிடைக்கவில்லை', NULL, gen_random_uuid(), now(), now()),
  ('AC_NOT_TURNED_ON', 'TAMIL', 'ஏசி இயக்கப்படவில்லை', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_DEMANDED_EXTRA', 'TAMIL', 'ஓட்டுநர் கூடுதல் பணம் கேட்டார்', NULL, gen_random_uuid(), now(), now()),
  ('OTHER', 'TAMIL', 'என் பிரச்சனை இங்கே பட்டியலிடப்படவில்லை', NULL, gen_random_uuid(), now(), now()),

  -- TELUGU
  ('WAIT_TIME_TOO_LONG', 'TELUGU', 'వేచి ఉండే సమయం చాలా ఎక్కువగా ఉంది', NULL, gen_random_uuid(), now(), now()),
  ('GOT_ANOTHER_RIDE', 'TELUGU', 'నాకు మరో రైడ్ దొరికింది', NULL, gen_random_uuid(), now(), now()),
  ('WRONG_PICKUP_LOCATION', 'TELUGU', 'తప్పు పికప్ స్థానం', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_NOT_MOVING', 'TELUGU', 'డ్రైవర్ పికప్ వైపు కదలడం లేదు', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_UNAVAILABLE', 'TELUGU', 'డ్రైవర్ అందుబాటులో లేరు', NULL, gen_random_uuid(), now(), now()),
  ('AC_NOT_TURNED_ON', 'TELUGU', 'ఏసీ ఆన్ చేయలేదు', NULL, gen_random_uuid(), now(), now()),
  ('DRIVER_DEMANDED_EXTRA', 'TELUGU', 'డ్రైవర్ అదనపు డబ్బు అడిగారు', NULL, gen_random_uuid(), now(), now()),
  ('OTHER', 'TELUGU', 'నా సమస్య ఇక్కడ జాబితాలో లేదు', NULL, gen_random_uuid(), now(), now())