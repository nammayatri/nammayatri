CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.merchant_overlay
(   id CHARACTER(36) PRIMARY KEY NOT NULL,
    merchant_id character(36) NOT NULL,
    overlay_key character varying(255) not null,
    language character varying(255) not null,
    udf1 character varying(255),
    title text,
    description text,
    image_url text,
    ok_button_text text,
    cancel_button_text text,
    actions text[] not null default '{}',
    link text
);

CREATE INDEX idx_merchant_overlay_key ON merchant_overlay (merchant_id,overlay_key);

-- INSERT INTO atlas_driver_offer_bpp.merchant_overlay (merchant_id, language, overlay_key, udf1, notification_sub_type, icon, title, body) VALUES
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'ENGLISH', 'PAYMENT_FAILED_AUTOPAY', 'Z9', 'LOW_ACCOUNT_BALANCE', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_save_more', 'Low Account Balance', 'Your Bank Account balance is low.<br>Add <b> \\u20B9 {#dueAmount#} </b> to enjoy uninterrupted rides.'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'ENGLISH', 'PAYMENT_FAILED_MANUAL', 'Z9', 'PAYMENT_FAILED_LOW_ACCOUNT_BALANCE', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_save_more', 'Payment Failed', '<b>Low Account Balance.</b><br>Please add  <b> \\u20B9 {#dueAmount#} </b> in your bank account and retry payment.'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'ENGLISH', 'PLAN_SWITCH', null, 'SWITCH_PLAN', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_save_more', 'Save more with\n DAILY UNLIMITED', 'You have completed {#numberOfRides#} rides today. Save up to {#saveUpto#} by switching to the DAILY UNLIMITED plan'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'ENGLISH', 'PAYMENT_PENDING', 'AUTOPAY', 'PAYMENT_PENDING_AUTOPAY_SET', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_payment_pending', 'Payment Pending', 'To continue taking rides on Namma Yatri, clear your payment dues'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'ENGLISH', 'PAYMENT_PENDING', 'MANUAL', 'PAYMENT_PENDING_AUTOPAY_NOT_SET', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_payment_pending', 'Payment Pending', 'To continue taking rides on Namma Yatri, clear your payment dues'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'ENGLISH', 'PAYMENT_FAILED_AUTOPAY', null, 'AUTOPAY_PAYMENT_FAILED', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_autopay_payment_failed', 'Autopay Payment Failed', 'Clear your Manual dues for hassle free rides!'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'ENGLISH', 'PAYMENT_FAILED_MANUAL', null, 'MANUAL_PAYMENT_FAILED', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_failed', 'Payment Failed', 'Clear your Manual dues for hassle free rides!'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'KANNADA', 'PAYMENT_FAILED_AUTOPAY', 'Z9', 'LOW_ACCOUNT_BALANCE', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_save_more', 'ಕಡಿಮೆ ಖಾತೆ ಬ್ಯಾಲೆನ್ಸ್', 'ನಿಮ್ಮ ಬ್ಯಾಂಕ್ ಖಾತೆಯ ಬ್ಯಾಲೆನ್ಸ್ ಕಡಿಮೆಯಾಗಿದೆ.<br> <b> \\u20B9 {#dueAmount#} </b> ಅಡೆತಡೆಯಿಲ್ಲದ ಸವಾರಿಗಳನ್ನು ಆನಂದಿಸಲು.'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'KANNADA', 'PAYMENT_FAILED_MANUAL', 'Z9', 'PAYMENT_FAILED_LOW_ACCOUNT_BALANCE', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_save_more', 'ಪಾವತಿ ವಿಫಲವಾಗಿದೆ', '<b>ಕಡಿಮೆ ಖಾತೆ ಬಾಕಿ.</b><br>ದಯವಿಟ್ಟು <b> \\u20B9 {#dueAmount#} </b> ಅನ್ನು ನಿಮ್ಮ ಬ್ಯಾಂಕ್ ಖಾತೆಯಲ್ಲಿ ಸೇರಿಸಿ ಮತ್ತು ಪಾವತಿಯನ್ನು ಮರುಪ್ರಯತ್ನಿಸಿ.'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'KANNADA', 'PLAN_SWITCH', null, 'SWITCH_PLAN', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_save_more', 'ಪ್ರತಿದಿನ ಅನ್ಲಿಮಿಟೆಡ್\n ಜೊತೆಗೆ ಇನ್ನಷ್ಟು ಉಳಿಸಿ', 'ನೀವು ಇಂದು {#numberOfRides#} ರೈಡ್‌ಗಳನ್ನು ಪೂರ್ಣಗೊಳಿಸಿದ್ದೀರಿ. ಡೆಯ್ಲಿ ಆನ್ಲಿಮಿಡೆಟ್ ಜೊತೆಗೆ ಪ್ಲಾನ್ ಗೆ ಬದಲಾಯಿಸುವ ಮೂಲಕ {#saveUpto#} ವರೆಗೆ ಉಳಿಸಿ'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'KANNADA', 'PAYMENT_PENDING', 'AUTOPAY', 'PAYMENT_PENDING_AUTOPAY_SET', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_payment_pending', 'ಪಾವತಿ ಬಾಕಿ ಇದೆ', 'ನಮ್ಮ ಯಾತ್ರಿಯಲ್ಲಿ ಸವಾರಿ ಮಾಡುವುದನ್ನು ಮುಂದುವರಿಸಲು, ನಿಮ್ಮ ಪಾವತಿ ಬಾಕಿಗಳನ್ನು ತೆರವುಗೊಳಿಸಿ'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'KANNADA', 'PAYMENT_PENDING', 'MANUAL', 'PAYMENT_PENDING_AUTOPAY_NOT_SET', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_payment_pending', 'ಪಾವತಿ ಬಾಕಿ ಇದೆ', 'ನಮ್ಮ ಯಾತ್ರಿಯಲ್ಲಿ ಸವಾರಿ ಮಾಡುವುದನ್ನು ಮುಂದುವರಿಸಲು, ನಿಮ್ಮ ಪಾವತಿ ಬಾಕಿಗಳನ್ನು ತೆರವುಗೊಳಿಸಿ'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'KANNADA', 'PAYMENT_FAILED_AUTOPAY', null, 'AUTOPAY_PAYMENT_FAILED', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_autopay_payment_failed', 'ಆಟೋಪೆ ಪಾವತಿ ವಿಫಲವಾಗಿದೆ', 'ಜಗಳ ಮುಕ್ತ ಸವಾರಿಗಳಿಗಾಗಿ ನಿಮ್ಮ ಮನುಲ ದುಸ್ ಅನ್ನು ತೆರವುಗೊಳಿಸಿ!'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'KANNADA', 'PAYMENT_FAILED_MANUAL', null, 'MANUAL_PAYMENT_FAILED', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_failed', 'ಪಾವತಿ ವಿಫಲವಾಗಿದೆ', 'ಜಗಳ ಮುಕ್ತ ಸವಾರಿಗಳಿಗಾಗಿ ನಿಮ್ಮ ಮನುಲ ದುಸ್ ಅನ್ನು ತೆರವುಗೊಳಿಸಿ!'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'HINDI', 'PAYMENT_FAILED_AUTOPAY', 'Z9', 'LOW_ACCOUNT_BALANCE', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_save_more', 'कम खाता शेष', 'आपके बैंक खाते का बैलेंस कम है। निर्बाध सवारी का आनंद लेने के लिए<br> <b> \\u20B9 {#dueAmount#} </b> जोड़ें।'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'HINDI', 'PAYMENT_FAILED_MANUAL', 'Z9', 'PAYMENT_FAILED_LOW_ACCOUNT_BALANCE', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_save_more', 'भुगतान विफल', '<b>कम खाता शेष।</b><br>कृपया अपने बैंक खाते में <b> \\u20B9 {#dueAmount#} </b> जोड़ें और भुगतान का पुनः प्रयास करें।'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'HINDI', 'PLAN_SWITCH', null, 'SWITCH_PLAN', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_save_more', 'डेली अनलिमिटेड\n के साथ अधिक बचत करें', 'आपने आज {#numberOfRides#} सवारी पूरी कर ली है। डेली अनलिमिटेड प्लान पर स्विच करके {#saveUpto#} तक की बचत करें'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'HINDI', 'PAYMENT_PENDING', 'AUTOPAY', 'PAYMENT_PENDING_AUTOPAY_SET', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_payment_pending', 'भुगतान लंबित', 'नम्मा यात्री पर यात्रा जारी रखने के लिए, अपना बकाया भुगतान चुकाएं'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'HINDI', 'PAYMENT_PENDING', 'MANUAL', 'PAYMENT_PENDING_AUTOPAY_NOT_SET', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_payment_pending', 'भुगतान लंबित', 'नम्मा यात्री पर यात्रा जारी रखने के लिए, अपना बकाया भुगतान चुकाएं'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'HINDI', 'PAYMENT_FAILED_AUTOPAY', null, 'AUTOPAY_PAYMENT_FAILED', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_autopay_payment_failed', 'ऑटोपे भुगतान विफल', 'परेशानी मुक्त सवारी के लिए अपना मैन्युअल बकाया चुकाएं!'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'HINDI', 'PAYMENT_FAILED_MANUAL', null, 'MANUAL_PAYMENT_FAILED', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_failed', 'भुगतान विफल', 'परेशानी मुक्त सवारी के लिए अपना मैन्युअल बकाया चुकाएं!'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'TAMIL', 'PAYMENT_FAILED_AUTOPAY', 'Z9', 'LOW_ACCOUNT_BALANCE', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_save_more', 'குறைந்த கணக்கு இருப்பு', 'உங்கள் பேங்க் அக்கவுண்ட் பேலன்ஸ் குறைவாக உள்ளது.<br>தடையின்றி சவாரி செய்ய <b> \\u20B9 {#dueAmount#} </b> ஐ சேர்க்கவும்.'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'TAMIL', 'PAYMENT_FAILED_MANUAL', 'Z9', 'PAYMENT_FAILED_LOW_ACCOUNT_BALANCE', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_save_more', 'பணம் செலுத்த முடியவில்லை', '<b>குறைந்த கணக்கு இருப்பு.</b><br>உங்கள் வங்கிக் கணக்கில் <b> \\u20B9 {#dueAmount#} </b> சேர்த்து மீண்டும் பணம் செலுத்த முயற்சிக்கவும்.'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'TAMIL', 'PLAN_SWITCH', null, 'SWITCH_PLAN', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_save_more', 'டெய்லி அன்லிமிடெட் உடன்\n மேலும் சேமிக்கவும்', 'இன்று {#numberOfRides#} சவாரிகளை முடித்துவிட்டீர்கள். டெய்லி அன்லிமிடெட் உடன் திட்டத்திற்கு மாறுவதன் மூலம் {#saveUpto#} வரை சேமிக்கவும்'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'TAMIL', 'PAYMENT_PENDING', 'AUTOPAY', 'PAYMENT_PENDING_AUTOPAY_SET', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_payment_pending', 'கட்டணம் நிலுவையில் உள்ளது', 'நம்ம யாத்ரியில் தொடர்ந்து சவாரி செய்ய, உங்கள் கட்டண நிலுவைத் தொகையைச் செலுத்துங்கள்'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'TAMIL', 'PAYMENT_PENDING', 'MANUAL', 'PAYMENT_PENDING_AUTOPAY_NOT_SET', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_payment_pending', 'கட்டணம் நிலுவையில் உள்ளது', 'நம்ம யாத்ரியில் தொடர்ந்து சவாரி செய்ய, உங்கள் கட்டண நிலுவைத் தொகையைச் செலுத்துங்கள்'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'TAMIL', 'PAYMENT_FAILED_AUTOPAY', null, 'AUTOPAY_PAYMENT_FAILED', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_autopay_payment_failed', 'ஆக்டோபய கட்டணம் செலுத்த முடியவில்லை', 'தொந்தரவு இல்லாத சவாரிகளுக்கு உங்கள் கைமுறை நிலுவைத் தொகையை அழிக்கவும்!'),
--     ('7f7896dd-787e-4a0b-8675-e9e6fe93bb8f', 'TAMIL', 'PAYMENT_FAILED_MANUAL', null, 'MANUAL_PAYMENT_FAILED', 'https://assets.juspay.in/beckn/nammayatri/driver/images/ny_failed', 'பணம் செலுத்த முடியவில்லை', 'தொந்தரவு இல்லாத சவாரிகளுக்கு உங்கள் கைமுறை நிலுவைத் தொகையை அழிக்கவும்!');

ALTER TABLE atlas_driver_offer_bpp.payment_order ALTER COLUMN amount type numeric(30,2);

ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN cache_offer_list_by_driver_id type boolean default false not null;
ALTER TABLE atlas_driver_offer_bpp.transporter_config ADD COLUMN use_offer_list_cache type boolean default true not null;
