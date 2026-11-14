INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, should_trigger, created_at, updated_at
)
SELECT
    'DRIVER_HAS_REACHED',
    'DRIVER_HAS_REACHED_OTP_LESS',
    moc.merchant_id,
    moc.id,
    copy.title,
    copy.body,
    copy.language,
    true,
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM atlas_app.merchant_operating_city moc
CROSS JOIN (VALUES
    ('ENGLISH',   'Driver Has Reached!',          'Your driver has arrived in Vehicle No. {#vehicleNumber#}. No OTP is needed for this ride, you can hop in.'),
    ('HINDI',     'ड्राइवर पहुँच गया है!',            'आपका ड्राइवर वाहन नं. {#vehicleNumber#} में पहुँच गया है। इस राइड के लिए OTP की ज़रूरत नहीं है, आप बैठ सकते हैं।'),
    ('KANNADA',   'ಚಾಲಕರು ಬಂದಿದ್ದಾರೆ!',             'ನಿಮ್ಮ ಚಾಲಕರು ವಾಹನ ಸಂಖ್ಯೆ {#vehicleNumber#} ನಲ್ಲಿ ಬಂದಿದ್ದಾರೆ. ಈ ರೈಡ್‌ಗೆ OTP ಅಗತ್ಯವಿಲ್ಲ, ನೀವು ಹತ್ತಬಹುದು.'),
    ('TAMIL',     'ஓட்டுநர் வந்துவிட்டார்!',          'உங்கள் ஓட்டுநர் வாகன எண் {#vehicleNumber#} இல் வந்துள்ளார். இந்த பயணத்திற்கு OTP தேவையில்லை, நீங்கள் ஏறலாம்.'),
    ('MALAYALAM', 'ഡ്രൈവർ എത്തി!',                  'നിങ്ങളുടെ ഡ്രൈവർ വാഹന നമ്പർ {#vehicleNumber#} ൽ എത്തിയിട്ടുണ്ട്. ഈ റൈഡിന് OTP ആവശ്യമില്ല, നിങ്ങൾക്ക് കയറാം.'),
    ('BENGALI',   'ড্রাইভার পৌঁছে গেছেন!',           'আপনার ড্রাইভার গাড়ি নং {#vehicleNumber#} নিয়ে পৌঁছে গেছেন। এই রাইডের জন্য OTP লাগবে না, আপনি উঠে পড়তে পারেন।'),
    ('FRENCH',    'Chauffeur Arrivé !',           'Votre chauffeur est arrivé avec le Véhicule n° {#vehicleNumber#}. Aucun OTP n''est requis pour cette course, vous pouvez monter.'),
    ('TELUGU',    'డ్రైవర్ చేరుకున్నారు!',            'మీ డ్రైవర్ వాహనం నం. {#vehicleNumber#} లో చేరుకున్నారు. ఈ రైడ్‌కు OTP అవసరం లేదు, మీరు ఎక్కవచ్చు.'),
    ('ODIA',      'ଡ୍ରାଇଭର ପହଞ୍ଚିଗଲେ!',             'ଆପଣଙ୍କ ଡ୍ରାଇଭର ଗାଡ଼ି ନମ୍ବର {#vehicleNumber#} ରେ ପହଞ୍ଚିଛନ୍ତି। ଏହି ରାଇଡ୍ ପାଇଁ OTP ଆବଶ୍ୟକ ନାହିଁ, ଆପଣ ବସିପାରିବେ।'),
    ('DUTCH',     'Chauffeur is Aangekomen!',     'Uw chauffeur is aangekomen met Voertuig Nr. {#vehicleNumber#}. Voor deze rit is geen OTP nodig, u kunt instappen.'),
    ('GERMAN',    'Fahrer ist da!',               'Dein Fahrer ist mit Fahrzeug-Nr. {#vehicleNumber#} angekommen. Für diese Fahrt ist kein OTP nötig, du kannst einsteigen.'),
    ('FINNISH',   'Kuljettaja on saapunut!',      'Kuljettajasi on saapunut ajoneuvolla nro {#vehicleNumber#}. Tähän matkaan ei tarvita OTP-koodia, voit nousta kyytiin.'),
    ('SWEDISH',   'Föraren har anlänt!',          'Din förare har anlänt med fordon nr {#vehicleNumber#}. Ingen OTP behövs för den här resan, du kan kliva in.'),
    ('GUJARATI',  'ડ્રાઇવર પહોંચી ગયા છે!',          'તમારા ડ્રાઇવર વાહન નં. {#vehicleNumber#} માં પહોંચી ગયા છે. આ રાઇડ માટે OTP ની જરૂર નથી, તમે બેસી શકો છો.'),
    ('MARATHI',   'ड्रायव्हर पोहोचले आहेत!',         'तुमचे ड्रायव्हर वाहन क्र. {#vehicleNumber#} मध्ये पोहोचले आहेत. या राईडसाठी OTP ची गरज नाही, तुम्ही बसू शकता.'),
    ('ARABIC',    'وصل السائق!',                  'وصل سائقك بالمركبة رقم {#vehicleNumber#}. لا حاجة إلى OTP لهذه الرحلة، يمكنك الصعود.'),
    ('RUSSIAN',   'Водитель прибыл!',             'Ваш водитель прибыл на автомобиле № {#vehicleNumber#}. Для этой поездки OTP не требуется, можете садиться.'),
    ('SOMALI',    'Darawalku wuu yimid!',         'Darawalkaagu wuxuu ku yimid gaari lambarkiisu yahay {#vehicleNumber#}. Safarkan OTP looma baahna, waad fuuli kartaa.'),
    ('KHASI',     'U driver u la poi!',           'U driver jong phi u la poi ha ka kali No. {#vehicleNumber#}. Ym donkam OTP na ka bynta kane ka ride, phi lah ban kiew.')
) AS copy(language, title, body)
ON CONFLICT DO NOTHING;
