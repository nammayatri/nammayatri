CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.merchant_push_notification
(   merchant_id character(36) NOT NULL,
    push_notification_key character varying(255) not null,
    language character varying(255) not null,
    udf1 character varying(255),
    notification_sub_type character varying(255) not null,
    icon character varying(255),
    title text not null,
    body text not null
);

INSERT INTO atlas_driver_offer_bpp.merchant_push_notification (merchant_id, push_notification_key, language, udf1, notification_sub_type, icon, title, body) VALUES
    (('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'ENGLISH', 'AUTOPAY_PAYMENT_FAILED', 'Z9', 'LOW_ACCOUNT_BALANCE', 'Low Account Balance', 'Your Bank Account balance is low.<br>Add <b> \\u20B9 {#dueAmount#} </b> to enjoy uninterrupted rides.')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'ENGLISH', 'MANUAL_PAYMENT_FAILED', 'Z9', 'PAYMENT_FAILED_LOW_ACCOUNT_BALANCE', 'Payment Failed', '<b>Low Account Balance.</b><br>Please add  <b> \\u20B9 {#dueAmount#} </b> in your bank account and retry payment.')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'ENGLISH', 'SWITCH_PLAN', null, 'SWITCH_PLAN', 'Save more with\n DAILY UNLIMITED', 'You have completed {#numberOfRides#} rides today. Save up to {#saveUpto#} by switching to the DAILY UNLIMITED plan')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'ENGLISH', 'PAYMENT_PENDING', 'AUTOPAY', 'PAYMENT_PENDING_AUTOPAY_SET', 'Payment Pending', 'To continue taking rides on Namma Yatri, clear your payment dues')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'ENGLISH', 'PAYMENT_PENDING', 'MANUAL', 'PAYMENT_PENDING_AUTOPAY_NOT_SET', 'Payment Pending', 'To continue taking rides on Namma Yatri, clear your payment dues')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'ENGLISH', 'AUTOPAY_PAYMENT_FAILED', null, 'AUTOPAY_PAYMENT_FAILED', 'Autopay Payment Failed', 'Clear your Manual dues for hassle free rides!')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'ENGLISH', 'MANUAL_PAYMENT_FAILED', null, 'MANUAL_PAYMENT_FAILED', 'Payment Failed', 'Clear your Manual dues for hassle free rides!')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'KANNADA', 'AUTOPAY_PAYMENT_FAILED', 'Z9', 'LOW_ACCOUNT_BALANCE', 'ಕಡಿಮೆ ಖಾತೆ ಬ್ಯಾಲೆನ್ಸ್', 'ನಿಮ್ಮ ಬ್ಯಾಂಕ್ ಖಾತೆಯ ಬ್ಯಾಲೆನ್ಸ್ ಕಡಿಮೆಯಾಗಿದೆ.<br> <b> \\u20B9 {#dueAmount#} </b> ಅಡೆತಡೆಯಿಲ್ಲದ ಸವಾರಿಗಳನ್ನು ಆನಂದಿಸಲು.')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'KANNADA', 'MANUAL_PAYMENT_FAILED', 'Z9', 'PAYMENT_FAILED_LOW_ACCOUNT_BALANCE', 'ಪಾವತಿ ವಿಫಲವಾಗಿದೆ', '<b>ಕಡಿಮೆ ಖಾತೆ ಬಾಕಿ.</b><br>ದಯವಿಟ್ಟು <b> \\u20B9 {#dueAmount#} </b> ಅನ್ನು ನಿಮ್ಮ ಬ್ಯಾಂಕ್ ಖಾತೆಯಲ್ಲಿ ಸೇರಿಸಿ ಮತ್ತು ಪಾವತಿಯನ್ನು ಮರುಪ್ರಯತ್ನಿಸಿ.')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'KANNADA', 'SWITCH_PLAN', null, 'SWITCH_PLAN', 'ಪ್ರತಿದಿನ ಅನ್ಲಿಮಿಟೆಡ್\n ಜೊತೆಗೆ ಇನ್ನಷ್ಟು ಉಳಿಸಿ', 'ನೀವು ಇಂದು {#numberOfRides#} ರೈಡ್‌ಗಳನ್ನು ಪೂರ್ಣಗೊಳಿಸಿದ್ದೀರಿ. ಡೆಯ್ಲಿ ಆನ್ಲಿಮಿಡೆಟ್ ಜೊತೆಗೆ ಪ್ಲಾನ್ ಗೆ ಬದಲಾಯಿಸುವ ಮೂಲಕ {#saveUpto#} ವರೆಗೆ ಉಳಿಸಿ')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'KANNADA', 'PAYMENT_PENDING', 'AUTOPAY', 'PAYMENT_PENDING_AUTOPAY_SET', 'ಪಾವತಿ ಬಾಕಿ ಇದೆ', 'ನಮ್ಮ ಯಾತ್ರಿಯಲ್ಲಿ ಸವಾರಿ ಮಾಡುವುದನ್ನು ಮುಂದುವರಿಸಲು, ನಿಮ್ಮ ಪಾವತಿ ಬಾಕಿಗಳನ್ನು ತೆರವುಗೊಳಿಸಿ')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'KANNADA', 'PAYMENT_PENDING', 'MANUAL', 'PAYMENT_PENDING_AUTOPAY_NOT_SET', 'ಪಾವತಿ ಬಾಕಿ ಇದೆ', 'ನಮ್ಮ ಯಾತ್ರಿಯಲ್ಲಿ ಸವಾರಿ ಮಾಡುವುದನ್ನು ಮುಂದುವರಿಸಲು, ನಿಮ್ಮ ಪಾವತಿ ಬಾಕಿಗಳನ್ನು ತೆರವುಗೊಳಿಸಿ')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'KANNADA', 'AUTOPAY_PAYMENT_FAILED', null, 'AUTOPAY_PAYMENT_FAILED', 'ಆಟೋಪೆ ಪಾವತಿ ವಿಫಲವಾಗಿದೆ', 'ಜಗಳ ಮುಕ್ತ ಸವಾರಿಗಳಿಗಾಗಿ ನಿಮ್ಮ ಮನುಲ ದುಸ್ ಅನ್ನು ತೆರವುಗೊಳಿಸಿ!')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'KANNADA', 'MANUAL_PAYMENT_FAILED', null, 'MANUAL_PAYMENT_FAILED', 'ಪಾವತಿ ವಿಫಲವಾಗಿದೆ', 'ಜಗಳ ಮುಕ್ತ ಸವಾರಿಗಳಿಗಾಗಿ ನಿಮ್ಮ ಮನುಲ ದುಸ್ ಅನ್ನು ತೆರವುಗೊಳಿಸಿ!')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'HINDI', 'AUTOPAY_PAYMENT_FAILED', 'Z9', 'LOW_ACCOUNT_BALANCE', 'कम खाता शेष', 'आपके बैंक खाते का बैलेंस कम है। निर्बाध सवारी का आनंद लेने के लिए<br> <b> \\u20B9 {#dueAmount#} </b> जोड़ें।')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'HINDI', 'MANUAL_PAYMENT_FAILED', 'Z9', 'PAYMENT_FAILED_LOW_ACCOUNT_BALANCE', 'भुगतान विफल', '<b>कम खाता शेष।</b><br>कृपया अपने बैंक खाते में <b> \\u20B9 {#dueAmount#} </b> जोड़ें और भुगतान का पुनः प्रयास करें।')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'HINDI', 'SWITCH_PLAN', null, 'SWITCH_PLAN', 'डेली अनलिमिटेड\n के साथ अधिक बचत करें', 'आपने आज {#numberOfRides#} सवारी पूरी कर ली है। डेली अनलिमिटेड प्लान पर स्विच करके {#saveUpto#} तक की बचत करें')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'HINDI', 'PAYMENT_PENDING', 'AUTOPAY', 'PAYMENT_PENDING_AUTOPAY_SET', 'भुगतान लंबित', 'नम्मा यात्री पर यात्रा जारी रखने के लिए, अपना बकाया भुगतान चुकाएं')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'HINDI', 'PAYMENT_PENDING', 'MANUAL', 'PAYMENT_PENDING_AUTOPAY_NOT_SET', 'भुगतान लंबित', 'नम्मा यात्री पर यात्रा जारी रखने के लिए, अपना बकाया भुगतान चुकाएं')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'HINDI', 'AUTOPAY_PAYMENT_FAILED', null, 'AUTOPAY_PAYMENT_FAILED', 'ऑटोपे भुगतान विफल', 'परेशानी मुक्त सवारी के लिए अपना मैन्युअल बकाया चुकाएं!')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'HINDI', 'MANUAL_PAYMENT_FAILED', null, 'MANUAL_PAYMENT_FAILED', 'भुगतान विफल', 'परेशानी मुक्त सवारी के लिए अपना मैन्युअल बकाया चुकाएं!')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'TAMIL', 'AUTOPAY_PAYMENT_FAILED', 'Z9', 'LOW_ACCOUNT_BALANCE', 'குறைந்த கணக்கு இருப்பு', 'உங்கள் பேங்க் அக்கவுண்ட் பேலன்ஸ் குறைவாக உள்ளது.<br>தடையின்றி சவாரி செய்ய <b> \\u20B9 {#dueAmount#} </b> ஐ சேர்க்கவும்.')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'TAMIL', 'MANUAL_PAYMENT_FAILED', 'Z9', 'PAYMENT_FAILED_LOW_ACCOUNT_BALANCE', 'பணம் செலுத்த முடியவில்லை', '<b>குறைந்த கணக்கு இருப்பு.</b><br>உங்கள் வங்கிக் கணக்கில் <b> \\u20B9 {#dueAmount#} </b> சேர்த்து மீண்டும் பணம் செலுத்த முயற்சிக்கவும்.')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'TAMIL', 'SWITCH_PLAN', null, 'SWITCH_PLAN', 'டெய்லி அன்லிமிடெட் உடன்\n மேலும் சேமிக்கவும்', 'இன்று {#numberOfRides#} சவாரிகளை முடித்துவிட்டீர்கள். டெய்லி அன்லிமிடெட் உடன் திட்டத்திற்கு மாறுவதன் மூலம் {#saveUpto#} வரை சேமிக்கவும்')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'TAMIL', 'PAYMENT_PENDING', 'AUTOPAY', 'PAYMENT_PENDING_AUTOPAY_SET', 'கட்டணம் நிலுவையில் உள்ளது', 'நம்ம யாத்ரியில் தொடர்ந்து சவாரி செய்ய, உங்கள் கட்டண நிலுவைத் தொகையைச் செலுத்துங்கள்')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'TAMIL', 'PAYMENT_PENDING', 'MANUAL', 'PAYMENT_PENDING_AUTOPAY_NOT_SET', 'கட்டணம் நிலுவையில் உள்ளது', 'நம்ம யாத்ரியில் தொடர்ந்து சவாரி செய்ய, உங்கள் கட்டண நிலுவைத் தொகையைச் செலுத்துங்கள்')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'TAMIL', 'AUTOPAY_PAYMENT_FAILED', null, 'AUTOPAY_PAYMENT_FAILED', 'ஆக்டோபய கட்டணம் செலுத்த முடியவில்லை', 'தொந்தரவு இல்லாத சவாரிகளுக்கு உங்கள் கைமுறை நிலுவைத் தொகையை அழிக்கவும்!')
    ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'TAMIL', 'MANUAL_PAYMENT_FAILED', null, 'MANUAL_PAYMENT_FAILED', 'பணம் செலுத்த முடியவில்லை', 'தொந்தரவு இல்லாத சவாரிகளுக்கு உங்கள் கைமுறை நிலுவைத் தொகையை அழிக்கவும்!'));
