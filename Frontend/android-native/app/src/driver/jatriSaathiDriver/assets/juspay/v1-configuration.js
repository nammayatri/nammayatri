window.version = window.version || {};
let version = "1.0.0";
if (typeof __VERSION__ !== "undefined") {
  version = __VERSION__
}
window.version["configuration"]= version;
window.getMerchantConfig = function () {
  return JSON.stringify({
    "APP_LINK": "https://play.google.com/store/apps/details?id=in.juspay.jatrisaathidriver",
    "USER_APP_LINK" : "https://nammayatri.in/link/rider/kTZ1",
    "PRIVACY_POLICY_LINK": "https://docs.google.com/document/d/1-bcjLOZ_gR0Rda2BNmkKnqVds8Pm23v1e7JbSDdM70E",
    "SPECIAL_ZONE_OTP_VIEW": "true",
    "StringKeys": [
      "NEED_IT_TO_ENABLE_LOCATION",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM",
      "EARNED_ON_APP",
      "TRAVELLED_ON_APP",
      "REPORT_ISSUE_CHAT_PLACEHOLDER",
      "MY_PLAN_TITLE",
      "CHOOSE_YOUR_PLAN",
      "OFFER_CARD_BANNER_TITLE",
      "TO_CONTINUE_USING_YATRI_SATHI",
      "YATRI_SATHI_FEE_PAYABLE_FOR_DATE",
      "PAYMENT_FAILED_DESC",
      "AADHAAR_LINKING_REQUIRED_DESCRIPTION",
      "COMPLETE_PAYMENT_TO_CONTINUE",
      "GET_READY_FOR_YS_SUBSCRIPTION",
      "SUBSCRIPTION_PLAN_STR",
      "FIND_HELP_CENTRE",
      "HOW_IT_WORKS",
      "GET_SPECIAL_OFFERS",
      "PAYMENT_PENDING_ALERT_DESC",
      "NO_OPEN_MARKET_RIDES",
      "DOWNLOAD_NAMMA_YATRI",
      "START_TAKING_RIDES_AND_REFER",
      "REFERRED_DRIVERS_INFO",
      "REFERRED_CUSTOMERS_INFO",
      "SHARE_NAMMA_YATRI",
      "YATRI_POINTS_FAQS_QUES1_ANS1",
      "YATRI_POINTS_FAQS_QUES1_ANS2",
      "YATRI_POINTS_FAQS_QUES1_ANS3",
      "YATRI_POINTS_FAQS_QUES5_ANS1",
      "YATRI_POINTS_FAQS_QUES3_ANS1",
      "YATRI_POINTS_FAQS_QUES3_ANS2",
      "YATRI_POINTS_FAQS_QUES4_ANS1",
      "YATRI_POINTS_USAGE_POPUP",
      "EARN_POINTS_BY_TAKING_RIDES_AND_REFERRING_THE_APP_TO_OTHERS",
      "NOW_EARN_POINTS_FOR_EVERY_RIDE_AND_REFERRAL_AND_USE_THEM_TO_GET_REWARDS",
      "REFER_NAMMA_YATRI_APP_TO_CUSTOMERS_AND_EARN_POINTS"
    ],
    "leaderBoard": {
      "isMaskedName": false
    },
    "gotoConfig" : {
      "maxGotoLocations" : 5,
      "enableGoto" : true
    },
    "fontType": "Assets",
    "currency": "₹",
    "isGradient" : "false",
    "BONUS_EARNED" : "false",
    "gradient": [],
    "addFavouriteScreenBackArrow" : "ny_ic_chevron_left_white,https://assets.juspay.in/nammayatri/images/user/ny_ic_chevron_left_white.png",
    "popupBackground" : "#FFFFFF",
    "apiLoaderLottie": "primary_button_loader.json",
    "primaryTextColor": "#FCC32C",
    "primaryBackground": "#2C2F3A",
    "showCorporateAddress" : false,
    "showRegisteredAddress" : false,
    "imageUploadOptional" : true,
    "clientName" : "Yatri Sathi",
    "languageList": [{
      "name": "English",
      "value": "EN_US",
      "subtitle": "ইংরেজি"
    },
    {
      "name": "বাংলা",
      "value": "BN_IN",
      "subtitle": "Bengali"
    },
    {
      "name": "हिंदी",
      "value": "HI_IN",
      "subtitle": "Hindi"
    }
    ],
    "engilshInNative" : "ইংরেজি",
    "englishStrings": {
      "MERCHANT_NAME" : "Yatri Sathi",
      "NEED_IT_TO_ENABLE_LOCATION": "Yatri Sathi Driver collect location data to enable share your location to monitor driver current location, even when the app is closed or not in use.",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "Currently,We allow only West Bengal registered number",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "You are about to place a call to the Yatri Sathi Support Team. Do you want to proceed?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "Your location helps our system to map down all the near by taxis and get you the quickest ride possible.",
      "EARNED_ON_APP" : "Earned on YS",
      "TRAVELLED_ON_APP" : "Travelled On Yatri Sathi",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "Describe your issue. Yatri Sathi will try to resolve it in under 24 hours.",
      "MY_PLAN_TITLE" : "Yatri Sathi Plans",
      "OFFER_CARD_BANNER_TITLE" : "Setup Autopay and pay only ₹1/ride from Jan 1-31",
      "TO_CONTINUE_USING_YATRI_SATHI" : "To continue using Yatri Sathi, please complete your payment for",
      "YATRI_SATHI_FEE_PAYABLE_FOR_DATE" : "Yatri Sathi fee payable for",
      "PAYMENT_FAILED_DESC" : "You may retry payment, or make the payment at your nearest Yatri Sathi booth",
      "AADHAAR_LINKING_REQUIRED_DESCRIPTION" : "To start driving for Yatri Sathi, please \n link your Aadhaar ID",
      "COMPLETE_PAYMENT_TO_CONTINUE" : "To continue using Yatri Sathi, please complete your payment",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "Get ready for\nYatri Sathi Plans!",
      "SUBSCRIPTION_PLAN_STR" : "Yatri Sathi Plan",
      "CHOOSE_YOUR_PLAN" : "Activate Plan Now!",
      "FIND_HELP_CENTRE" : "Find Help Desk",
      "HOW_IT_WORKS" : "How Autopay works?",
      "GET_SPECIAL_OFFERS" : "Guaranteed fixed price until\nJan 1, 2025",
      "PAYMENT_PENDING_ALERT_DESC" : "To continue taking rides on Yatri Sathi, clear your payment dues",
      "NO_OPEN_MARKET_RIDES" : "0 open market rides",
      "DOWNLOAD_NAMMA_YATRI" : "Download Yatri Sathi",
      "START_TAKING_RIDES_AND_REFER" : "Start taking rides and referring drivers to sign up on Yatri Sathi Driver App",
      "REFERRED_DRIVERS_INFO" : "Referred Drivers who have registered on Yatri Sathi",
      "REFERRED_CUSTOMERS_INFO" : "Referred Customers who have registered on Yatri Sathi",
      "SHARE_NAMMA_YATRI" : "Share Yatri Sathi",
      "YATRI_POINTS_FAQS_QUES1_ANS1" : "Yatri Points are the rewards you earn on day-to-day activities on Yatri Sathi like rides, good ratings etc.",
      "YATRI_POINTS_FAQS_QUES1_ANS2" : "The earned points can be used to avail discounts in payments to Yatri Sathi.",
      "YATRI_POINTS_FAQS_QUES1_ANS3" : "Each Yatri Point can be converted to a discount of 0.1 and can be used for discounts on payments to Yatri Sathi. More avenues to avail discounts will be added in the future",
      "YATRI_POINTS_FAQS_QUES5_ANS1" : "You must complete your first ride with Yatri Sathi to start earning Yatri Points.",
      "YATRI_POINTS_FAQS_QUES3_ANS1" : "Points are earned through day-to-day activities on the app like rides, good ratings etc. You don’t need to do anything different.",
      "YATRI_POINTS_FAQS_QUES3_ANS2" : "You can earn more points by exhibiting good behavior like low cancellations, getting good ratings from customers, etc. You lose points by exhibiting bad behaviour like high unjustified cancellations, poor (1 or 2 star ratings) etc.",
      "YATRI_POINTS_FAQS_QUES4_ANS1" : "You can use the earned points to pay your subscription dues free-of-cost within the validity period",
      "YATRI_POINTS_USAGE_POPUP" : "Yatri Points will be converted into discounts that you can avail against your ride charges.",
      "EARN_POINTS_BY_TAKING_RIDES_AND_REFERRING_THE_APP_TO_OTHERS" : "Points kamane ke liye app par active rahein",
      "NOW_EARN_POINTS_FOR_EVERY_RIDE_AND_REFERRAL_AND_USE_THEM_TO_GET_REWARDS" : "Ab app par har kriya ke liye points kamayein aur in points ko bhugtan me use karein.",
      "REFER_NAMMA_YATRI_APP_TO_CUSTOMERS_AND_EARN_POINTS" : "Refer Yatri Sathi app to customers and earn points"
    },
    "hindiStrings": {
      "MERCHANT_NAME" : "यात्री साथी",
      "NEED_IT_TO_ENABLE_LOCATION": "Yatri Sathi partner ड्राइवर के लोकेशन की निगरानी के लिए अपना स्थान साझा करने के लिए लोकेशन डेटा एकत्र करता है, तब भी जब ऐप बंद हो या उपयोग में न हो।",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "Currently,We allow only West Bengal registered number",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "आप जात्री साथी सपोर्ट टीम को कॉल करने वाले हैं। क्या आपकी आगे बढ़ने की इच्छा है?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "आपका स्थान हमारे सिस्टम को आस पास के सभी टैक्सियों को मैप करने में सहायता कर्ता है और आपको सबसे तेज सवारी प्रदान करता है",
      "EARNED_ON_APP" : "YS पर अर्जित मूल्य",
      "TRAVELLED_ON_APP" : "यात्री साथी पर तय की गई दूरी",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "अपनी समस्या बताएं। यात्री साथी 24 घंटे के अंदर इसका समाधान करने का प्रयास करेगा।",
      "MY_PLAN_TITLE" : "यात्री साथी प्लान्स",
      "OFFER_CARD_BANNER_TITLE" : "ऑटोपे सेटअप करें और 1-31 जनवरी तक केवल ₹1/सवारी का पेमेंट करें",
      "TO_CONTINUE_USING_YATRI_SATHI" : "यात्री साथी का उपयोग जारी रखने के लिए",
      "YATRI_SATHI_FEE_PAYABLE_FOR_DATE" : "यात्री साथी शुल्क लागू" ,
      "PAYMENT_FAILED_DESC" : "आप भुगतान को पुनः प्रयास कर सकते हैं, या अपने नजदीकी यात्री साथी बूथ पर भुगतान कर सकते हैं",
      "AADHAAR_LINKING_REQUIRED_DESCRIPTION" : "यात्री साथी के लिए ड्राइविंग शुरू करने के लिए, कृपया अपना आधार आईडी लिंक करें",
      "COMPLETE_PAYMENT_TO_CONTINUE" : "यात्री साथी का उपयोग जारी रखने के लिए, कृपया अपना भुगतान पूरा करें",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "यात्री साथी योजनाओं के लिए तैयार रहें!",
      "SUBSCRIPTION_PLAN_STR" : "यात्री साथी योजना",
      "CHOOSE_YOUR_PLAN" : "योजना अभी सक्रिय करें!",
      "FIND_HELP_CENTRE" : "सहायता डेस्क ढूंढें",
      "HOW_IT_WORKS" : "ऑटोपे कैसे काम करता है?",
      "GET_SPECIAL_OFFERS" : "1 जनवरी, 2025 तक निश्चित मूल्य की गारंटी",
      "PAYMENT_PENDING_ALERT_DESC" : "यात्री साथी पर यात्रा जारी रखने के लिए, अपनी शेष राशि चुकाएं",
      "NO_OPEN_MARKET_RIDES" : "0 ओपन मार्केट राइड",
      "DOWNLOAD_NAMMA_YATRI" : "यात्री साथी डाउनलोड करें",
      "START_TAKING_RIDES_AND_REFER" : "यात्री साथी ड्राइवर ऐप पर साइन अप करने के लिए सवारी लेना और ड्राइवरों को रेफर करना शुरू करें",
      "REFERRED_DRIVERS_INFO" : "यात्री साथी पर पंजीकृत हुए सुझाए गए ड्राइवर्स",
      "REFERRED_CUSTOMERS_INFO" : "यात्री साथी पर पंजीकृत हुए सुझाए गए ग्राहक",
      "SHARE_NAMMA_YATRI" : "यात्री साथी को साझा करें",
      "YATRI_POINTS_FAQS_QUES1_ANS1" : "यात्री साथी पर दैनिक गतिविधियों जैसे कि सवारियों, अच्छी रेटिंग आदि पर आपकी प्राप्तियों को यात्री अंक कहा जाता है।",
      "YATRI_POINTS_FAQS_QUES1_ANS2" : "प्राप्त किए गए अंक को यात्री साथी के भुगतानों में छूट प्राप्त करने के लिए इस्तेमाल किया जा सकता है।",
      "YATRI_POINTS_FAQS_QUES1_ANS3" : "प्रत्येक यात्री अंक 0.1 की छूट में परिवर्तित किया जा सकता है और यात्री साथी के भुगतानों में छूट प्राप्त करने के लिए उपयोग किया जा सकता है। भविष्य में छूट पाने के और भी रास्ते जोड़े जाएंगे।",
      "YATRI_POINTS_FAQS_QUES5_ANS1" : "यात्री अंक अर्जित करना शुरू करने के लिए आपको यात्री साथी के साथ अपनी पहली यात्रा पूरी करनी होगी।",
      "YATRI_POINTS_FAQS_QUES3_ANS1" : "अंक एप्लिकेशन पर दैनिक गतिविधियों जैसे कि राइड्स, अच्छी रेटिंग इत्यादि के माध्यम से कमाए जाते हैं। आपको कुछ अलग नहीं करना पड़ता।",
      "YATRI_POINTS_FAQS_QUES3_ANS2" : "आप अच्छे व्यवहार का प्रदर्शन करके अधिक अंक कमा सकते हैं, जैसे कि कम रद्दीकरण, ग्राहकों से अच्छी रेटिंग प्राप्त करना, आदि। आप बुरे व्यवहार का प्रदर्शन करके अंक खो सकते हैं, जैसे कि अधिक अनावश्यक रद्दीकरण, कम रेटिंग (1 या 2 स्टार) इत्यादि।",
      "YATRI_POINTS_FAQS_QUES4_ANS1" : "आप प्राप्त किए गए अंकों का उपयोग करके अपने सदस्यता के देयता का भुगतान मुफ्त में कर सकते हैं, जो योग्यता की अवधि के भीतर हो।",
      "YATRI_POINTS_USAGE_POPUP" : "यात्री अंक छूटों में परिवर्तित किए जाएंगे जिन्हें आप अपने राइड शुल्क के खिलाफ लाभ उठा सकते हैं।",
      "EARN_POINTS_BY_TAKING_RIDES_AND_REFERRING_THE_APP_TO_OTHERS" : "अंक कमाने के लिए एप्लिकेशन पर सक्रिय रहें।",
      "NOW_EARN_POINTS_FOR_EVERY_RIDE_AND_REFERRAL_AND_USE_THEM_TO_GET_REWARDS" : "अब आप पर हर क्रिया के लीयते अंक कमाएं और इन साइंस को भुगतान में उसे करे।",
      "REFER_NAMMA_YATRI_APP_TO_CUSTOMERS_AND_EARN_POINTS" : "ग्राहकों को यात्री साथी ऐप साझा करें और अंक अर्जित करें"
    },
    "bengaliStrings": {
      "MERCHANT_NAME" : "যাত্রী সাথী",
      "NEED_IT_TO_ENABLE_LOCATION": "জাটি সাথি ড্রাইভার ড্রাইভারের বর্তমান অবস্থান নিরীক্ষণের জন্য আপনার অবস্থানটি ভাগ করে নিতে সক্ষম করতে অবস্থানের ডেটা সংগ্রহ করে, এমনকি অ্যাপটি বন্ধ থাকলেও বা ব্যবহার না করা হয়।",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "বর্তমানে, আমরা শুধুমাত্র পশ্চিমবঙ্গ নিবন্ধিত নম্বর অনুমোদন করি",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "আপনি জাত্রি সাথি সমর্থন দলকে কল করতে চলেছেন। আপনি কি এগিয়ে যেতে চান?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "আপনার অবস্থান আমাদের সিস্টেমকে ট্যাক্সি দ্বারা আশেপাশের সমস্ত স্থান ম্যাপ করতে এবং আপনাকে দ্রুততম রাইড করতে সাহায্য করে৷",
      "EARNED_ON_APP" : "YS এ অর্জিত মূল্য",
      "TRAVELLED_ON_APP" : "যাত্রী সাথীতে দূরত্ব ভ্রমণ",
      "REPORT_ISSUE_CHAT_PLACEHOLDER" : "আপনার সমস্যাটি বর্ণনা করুন। যাত্রী সাথী 24 ঘন্টার মধ্যে এটি সমাধান করার চেষ্টা করবেন।",
      "MY_PLAN_TITLE" : "যাত্রী সাথী পরিকল্পনা",
      "OFFER_CARD_BANNER_TITLE" : "অটোপে সেটআপ করুন এবং জানুয়ারী 1-31 এর মধ্যে শুধুমাত্র ₹1/রাইড প্রদান করুন",
      "TO_CONTINUE_USING_YATRI_SATHI" : "Yatri Sathi ব্যবহার চালিয়ে যেতে, অনুগ্রহ করে আপনার অর্থপ্রদান সম্পূর্ণ করুন",
      "YATRI_SATHI_FEE_PAYABLE_FOR_DATE" : "Yatri Sathi ফি জন্য প্রদেয়" ,
      "PAYMENT_FAILED_DESC" : "আপনি আবার অর্থপ্রদানের চেষ্টা করতে পারেন, অথবা আপনার নিকটস্থ Yatri Sathi বুথে অর্থপ্রদান করতে পারেন",
      "AADHAAR_LINKING_REQUIRED_DESCRIPTION" : "যাত্রী সাথীর জন্য গাড়ি চালানো শুরু করতে, দয়া করে \n আপনার আধার আইডি লিঙ্ক করুন",
      "COMPLETE_PAYMENT_TO_CONTINUE" : "Yatri Sathi ব্যবহার চালিয়ে যেতে, অনুগ্রহ করে আপনার অর্থপ্রদান সম্পূর্ণ করুন",
      "GET_READY_FOR_YS_SUBSCRIPTION" : "যাত্রী সাথী পরিকল্পনার জন্য প্রস্তুত হন!",
      "SUBSCRIPTION_PLAN_STR" : "যাত্রী সাথী পরিকল্পনা",
      "CHOOSE_YOUR_PLAN" : "এখনই প্ল্যান সক্রিয় করুন!",
      "FIND_HELP_CENTRE" : "সহায়তা ডেস্ক খুঁজুন",
      "HOW_IT_WORKS" : "স্বতঃপে কিভাবে কাজ করে?",
      "GET_SPECIAL_OFFERS" : "জানুয়ারি 1, 2025 পর্যন্ত গ্যারান্টিযুক্ত নির্দিষ্ট মূল্য",
      "PAYMENT_PENDING_ALERT_DESC" : "যাত্রী সাথীতে যাত্রা চালিয়ে যেতে, আপনার পেমেন্ট বকেয়া পরিশোধ করুন",
      "NO_OPEN_MARKET_RIDES" : "0 ওপেন মার্কেট রাইডস",
      "DOWNLOAD_NAMMA_YATRI" : "যাত্রী সাথী ডাউনলোড করুন",
      "START_TAKING_RIDES_AND_REFER" : "যাত্রী সাথী ড্রাইভার অ্যাপে সাইন আপ করতে রাইড করা এবং ড্রাইভারদের রেফার করা শুরু করুন",
      "REFERRED_DRIVERS_INFO" : "যাত্রী সাথীতে নিবেশন করেছেন যাদেরকে সূচনা দেওয়া হয়েছে",
      "REFERRED_CUSTOMERS_INFO" : "যাত্রী সাথীতে নিবেশন করেছেন যাদেরকে সূচনা দেওয়া হয়েছে",
      "SHARE_NAMMA_YATRI" : "যাত্রী সহযোগী ভাগ করুন",
      "YATRI_POINTS_FAQS_QUES1_ANS1" : "যাত্রী সাথীতে দিনব্যাপী কার্যকলাপে রাইড, রেফারেন্স, ভালো রেটিং ইত্যাদি কিছু করে আপনি যাত্রী পয়েন্ট অর্জন করেন।",
      "YATRI_POINTS_FAQS_QUES1_ANS2" : "অর্জিত পয়েন্টগুলি যাত্রী সাথীতে অর্থ প্রদানে ছাড় পেতে ব্যবহার করা যেতে পারে।",
      "YATRI_POINTS_FAQS_QUES1_ANS3" : "প্রতিটি যাত্রী পয়েন্টকে রূপান্তরিত করা যাবে টাকা ছাড়ে। 0.10 এবং সাবস্ক্রিপশন প্ল্যানে ডিসকাউন্ট পাওয়ার জন্য ব্যবহার করা যেতে পারে। ডিসকাউন্ট পাওয়ার আরও সুযোগ ভবিষ্যতে যোগ করা হবে",
      "YATRI_POINTS_FAQS_QUES3_ANS1" : "রাইড, ভালো রেটিং ইত্যাদির মতো অ্যাপে প্রতিদিনের কার্যকলাপের মাধ্যমে পয়েন্ট উপার্জন করা যেতে পারে। আপনাকে আলাদা কিছু করতে হবে না।",
      "YATRI_POINTS_FAQS_QUES3_ANS2" : "আপনি ভাল আচরণ বজায় রেখে অতিরিক্ত পয়েন্ট উপার্জন করতে পারেন, যেমন রাইড বাতিলকরণ কমিয়ে আনা এবং গ্রাহকদের কাছ থেকে ধারাবাহিকভাবে উচ্চ রেটিং পাওয়া। আপনি খারাপ আচরণ দেখানোর জন্য পয়েন্ট হারাবেন যেমন ঘন ঘন অন্যায্য বাতিলকরণ বা খারাপ রেটিং পাওয়া (1 বা 2 স্টার রেটিং)",
      "YATRI_POINTS_FAQS_QUES4_ANS1" : "আপনি বৈধ সময়ের মধ্যে বিনামূল্যে আপনার সাবস্ক্রিপশন ফি প্রদান করতে আপনার উপার্জন করা পয়েন্ট ব্যবহার করতে পারেন।",
      "YATRI_POINTS_FAQS_QUES5_ANS1" : "আপনার প্রথম ইউসাথী রাইড সম্পন্ন করতে হবে যাত্রী পয়েন্ট উপার্জন করার জন্য।",
      "YATRI_POINTS_USAGE_POPUP" : "আপনি অ্যাপে প্রতিটি কার্যের জন্য পয়েন্ট অর্জন করতে পারেন এবং এই পয়েন্ট গুলি অর্থ পরিশোধে ব্যবহার করতে পারেন।",
      "EARN_POINTS_BY_TAKING_RIDES_AND_REFERRING_THE_APP_TO_OTHERS" : "যাত্রী পয়েন্টগুলি ডিসকাউন্টে পরিণত হবে যা আপনি আপনার যাত্রার খরচের জন্য ব্যবহার করতে পারেন।",
      "NOW_EARN_POINTS_FOR_EVERY_RIDE_AND_REFERRAL_AND_USE_THEM_TO_GET_REWARDS" : "পয়েন্ট আয় করতে অ্যাপ্লিকেশনে সক্রিয় থাকুন।",
      "REFER_NAMMA_YATRI_APP_TO_CUSTOMERS_AND_EARN_POINTS" : "গ্রাহকদেরকে যাত্রী সাথী অ্যাপ সুপারিশ করুন এবং পয়েন্ট পেতে।"
    },
    "logs": ["JUSPAY","FIREBASE","CLEVERTAP"]
    , "fontName" : "PlusJakartaSans"
    , "fontKannada" : "NotoSansKannada"
    , "allowAllMobileNumber" : false
    , "acExplanation" : false
    , "showMonthlyLeaderBoard" : true
    , "rcLimit" : 3
    , "showGenderBanner" : false
    , "defaultLanguage" : "EN_US"
    , "navigationAppConfig" : {
      "query" : "google.navigation:q=%f,%f"
      , "packageName" : "com.google.android.apps.maps"
    }
    , "subscriptionConfig" : {
      "enableBlocking" : false,
      "completePaymentPopup" : false,
      "supportNumber" : "08069724949",
      "enableSubscriptionPopups" : true,
      "faqLink" : "https://yatrisathi.in/plans/",
      "optionsMenuItems" : {
        "viewFaqs" : true,
        "viewAutopayDetails" : true,
        "paymentHistory" : true,
        "kioskLocation" : true
      },
      "offerBannerConfig" : {
        "showDUOfferBanner" : true,
        "offerBannerValidTill" : "2023-12-01T00:00:00",
        "offerBannerDeadline" : "Jan 1-31-*$*-ಜನವರಿ 1-31-*$*-1-31 जनवरी-*$*-ஜனவரி 1-31-*$*-জানুয়ারী 1-31",
        "offerBannerPlans" : ["25ade579-fd9c-4288-a015-337af085e66c"],
      },
      "highDueWarningLimit" : 250.0,
      "gradientConfig" : [{"id" : "c1a27b2c-8287-4d79-a5d9-99e1a0026203", colors : ["#29FF4D35", "#29FFE588"]},{"id" : "5eed42c1-2388-4a86-b68b-d9da2f674091", colors : ["#29FF4D35", "#29FFE588"]},{"id" : "b6d61915-65bb-4ca9-bbb7-a90be735a722", colors : ["#29FF4D35", "#29FFE588"]}],
      "enableSubscriptionSupportPopup" : true,
      "myPlanYoutubeLink" : "https://www.youtube.com/playlist?list=PLvMgI4c44A9Y2bykEuDAtHzgcubXOYqgU-*$*-https://www.youtube.com/playlist?list=PLvMgI4c44A9Y2bykEuDAtHzgcubXOYqgU-*$*-https://www.youtube.com/playlist?list=PLvMgI4c44A9Zl0IIQcZa7ZJrSjWPLfxpA-*$*-https://www.youtube.com/playlist?list=PLvMgI4c44A9Y8NLs_8TXc7biX-JkobrGB",
      "overlayYoutubeLink" : "https://youtube.com/shorts/nyJ1bIOsGfo-*$*-https://youtube.com/shorts/nyJ1bIOsGfo-*$*-https://youtu.be/RSKNT3NccPo-*$*-https://youtu.be/RSKNT3NccPo",
      "earnAmountInADay" : 5000,
      "showFeeBreakup" : true
    }
    , "OTP_MESSAGE_REGEX" : "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
    , "autoPayBanner" : false
    , "rideActionModelConfig" : {
      "showVehicleVariant" : false
    }
    , "referralType" : "QRScreen"
    , "profile" :
        { "bookingOptionMenuForTaxi" : true
        }
    , "profileVerification" : {
      "aadharVerificationRequired" : false // HV Integration is done with Aadhaar as Compulsory on onboarding
    }
    , "bottomNavConfig" : {
      "subscription" :
        { "isVisible" : true,
          "showNew" : true
        },
      "referral" :
        {
          "showNew" : true
        }
    }
    , "otpRegex" :  "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
    , "termsLink" : "https://drive.google.com/file/d/1ptFVTMK4jWfqCkI2JhSLcea4AMGd77mL/view?usp=sharing"
    , "termsVersion" : 1.0
    , "enableDriverReferral": true
    , "enableCustomerReferral": true
    , "privacyLink" : "https://docs.google.com/document/d/1-bcjLOZ_gR0Rda2BNmkKnqVds8Pm23v1e7JbSDdM70E"
    , "feature" : {
      "enableBonus" : false
      , "enableImageUpload" : true
      , "enableGender" : false
      , "enableOtpRide" : true
      , "enableYatriCoins" : true
    }
    , "appData" : {
      "link" : "https://play.google.com/store/apps/details?id=in.juspay.jatrisaathidriver"
      , "name" : "Yatri Sathi"
    }
    , "vehicle" : {
      "validationPrefix" :  "WB"
    }
    , "banners" :{
      "autoPay" : false
    }
    , "referral": {
      "type": "LeaderBoard",
      "link" : "https://nammayatri.in/link/rider/kTZ1",
      "customerAppId" : "in.juspay.jatrisaathi",
      "driverAppId" : "in.juspay.jatrisaathidriver"
    }
    , "enableMockLocation" : false
    , "flowConfig" : {
        "chooseCity" : {
          "runFlow" : true,
          "defCity" : "Kolkata"
        }
      }
    , "appUpdatePopupUrl" : "https://play.google.com/store/apps/details?id=in.juspay.jatrisaathidriver&pcampaignid=web_share"
    , "permissions" : {
      "locationPermission" : true,
      "notification" : false
    }
    , "homeScreen" : {
      "specialRideOtpView" : true,
      "showGenderBanner" : false
    }
    , "rideRequest" : {
        "negotiationUnit" : {
            "cab" : "10"
        }
    }
  , "rideCompletedCardConfig" : {
      "lottieQRAnim" : true
  }
  , "rateCardScreen" :{
    "showYoutubeVideo" : false,
    "showRateCard" : true,
    "showTollCharges" : false,
    "showDriverAdditions" : true
  }
  , "waitTimeConfig" : {
    "enableWaitTime" : true
  }
  , "cityConfig" : getAllCityConfig()
  })
}

let defRateCardConfig = {
    "showLearnMore" : false,
    "learnMoreVideoLink" : ""
  }

let staticSubscriptionConfig = [
  {"price" : 90.0, "frequency" : "PER_DAY", "variantCategory" : "CarCategory", "name" : "DAILY_UNLIMITED", "introductoryOffer" :  "FREE_RIDE_OFFER", "showSelected" : false, "planDesc" : "CAB_DAILY_UNLIMITED_OFFER"},
  {"price" : 9.0, "frequency" : "PER_RIDE", "variantCategory" : "CarCategory", "name" : "DAILY_PER_RIDE", "introductoryOffer" : "" , "showSelected" : false, "planDesc" : "Up to a maximum of ₹99 per day-*$*-ದಿನಕ್ಕೆ ಗರಿಷ್ಠ ₹99-*$*-प्रति दिन अधिकतम ₹99 तक-*$*-প্রতিদিন সর্বোচ্চ ₹99 পর্যন্ত-*$*-പ്രതിദിനം പരമാവധി ₹99 വരെ-*$*-ஒரு நாளைக்கு அதிகபட்சம் ₹99 வரை-*$*-రోజుకు గరిష్టంగా ₹99 వరకు"},
  {"price" : 25.0, "frequency" : "PER_DAY", "variantCategory" : "AutoCategory", "name" : "DAILY_UNLIMITED", "introductoryOffer" : "NO_CHARGES_TILL", "showSelected" : true, "planDesc" : ""}
]

let getAllCityConfig = () => {
  let baseCityConfig = {
    "cityName" : "",
    "mapImage" : "",
    "cityCode" : "",
    "showSubscriptions" : true,
    "enableAdvancedBooking" : true,
    "advancedRidePopUpYoutubeLink" : "" ,
    "callDriverInfoPost": false, // Dummy link need to change
    "cityLat" : 0.0,
    "cityLong" : 0.0,
    "supportNumber" : "08069724949",
    "languageKey" : "BN_IN",
    "showScheduledRides" : false,
    "showDriverReferral" : true,
    "showCustomerReferral" : true,
    "uploadRCandDL" : true,
    "enableYatriCoins" : false,
    "enableGullak": false,
    "vehicleNSImg" : "ny_ic_location_unserviceable",
  "registration" : {
        "supportWAN" : "918618963188",
        "callSupport" : true,
        "whatsappSupport" : true
    },
    "variantSubscriptionConfig" : {
      "enableVariantBasedSubscription" : false,
      "variantList" : ["CarCategory"],
      "enableCabsSubscriptionView" : false,
      "staticViewPlans" : []
    },
    "showEarningSection" : true,
    "referral" : {
        "domain" : "https://www.yatrisathi.in"
      , "customerAppId" : "in.juspay.jatrisaathi"
      , "driverAppId" : "in.juspay.jatrisaathidriver"
    },
    "waitingCharges" : 1.50,
    "waitingChargesConfig" : {
      "cab" : {
        "freeSeconds" : 300,
        "perMinCharges" : 1.0
      },
      "auto" : {
        "freeSeconds" : 180,
        "perMinCharges" : 1.50
      },
      "bike" : {
        "freeSeconds" : 180,
        "perMinCharges" : 1.0
      },
      "ambulance": {
        "freeSeconds": 480,
        "perMinCharges": 2.0
      }
    },
    "rateCardConfig" : defRateCardConfig,
    "gstPercentage" : "18",
    "rentalWaitingChargesConfig" : {
      "cab" : {
        "freeSeconds" : 180,
        "perMinCharges" : 2.0
      },
      "auto" : {
        "freeSeconds" : 180,
        "perMinCharges" : 2.0
      },
      "bike" : {
        "freeSeconds" : 180,
        "perMinCharges" : 1.0
      },
      "ambulance": {
        "freeSeconds": 480,
        "perMinCharges": 2.0
      }
    },
     "rideStartAudio" : defaultStartAudioConfig
   , "assets" :{
      "auto_image" : "ny_ic_auto_side_view",
      "onboarding_auto_image" : "ny_ic_auto_side",
      "empty_referral_auto" : "ny_ic_refer_now_auto_ny_green,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_auto_ny_yellow.png",
      "empty_referral_cab" : "ny_ic_refer_now_cab_ys,https://assets.moving.tech/beckn/common/driver/images/ny_ic_refer_now_cab_ys.png"
    },
    "enableHvSdk" : true,
    "purpleRideConfig" : {
      "purpleRideConfigForAuto" : {
        "vehicleVariant" : "Auto",
        "showVideo" : false,
        "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
        "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
      },
      "purpleRideConfigForCabs" : {
        "vehicleVariant" : "Cab",
        "showVideo" : false,
        "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
        "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
      },
      "purpleRideConfigForBikes" : {
        "vehicleVariant" : "Bike",
        "showVideo" : false,
        "disabilityToVideo" : [{"disabilityType" : "BLIND_AND_LOW_VISION", "videoUrl" : "https://www.youtube.com/watch?v=2qYXl03N6Jg"}, {"disabilityType" : "HEAR_IMPAIRMENT", "videoUrl" : "https://www.youtube.com/watch?v=udkWOt0serg"}, {"disabilityType" : "LOCOMOTOR_DISABILITY", "videoUrl" : "https://www.youtube.com/watch?v=B0C6SZTQO6k"}, {"disabilityType" : "SAFETY", "videoUrl" : ""}, {"disabilityType" : "SPECIAL_ZONE_PICKUP", "videoUrl" : ""}, {"disabilityType" : "OTHER_DISABILITY", "videoUrl" : ""}],
        "genericVideoForVariant" : "https://youtu.be/5s21p2rI58c"
      }
    }
  };

  let cityConfigs = [
    Object.assign({}, baseCityConfig, {
      cityName: "Kolkata",
      mapImage: "ys_ic_kolkata_map",
      cityCode: "std:033",
      showSubscriptions: true,
      cityLat: 22.5354064,
      cityLong: 88.2649516,
      registration : {
        supportWAN : "918088065549",
        callSupport : true,
        whatsappSupport : true
      },
      variantSubscriptionConfig: {
        enableVariantBasedSubscription: true,
        variantList: ["CarCategory","BikeCategory"],
        enableCabsSubscriptionView: true,
        staticViewPlans: []
      },
      waitingChargesConfig: {
        cab: {
          freeSeconds: 180,
          perMinCharges: 2.0
        },
        auto: {
          freeSeconds: 180,
          perMinCharges: 1.50
        },
        bike: {
          freeSeconds: 180,
          perMinCharges: 1.0
        },
        ambulance: {
          freeSeconds: 480,
          perMinCharges: 2.0
        }
      },
      rentalWaitingChargesConfig: {
        cab: {
          freeSeconds: 180,
          perMinCharges: 2.0
        },
        auto: {
          freeSeconds: 180,
          perMinCharges: 2.0
        },
        bike: {
          freeSeconds: 180,
          perMinCharges: 2.0
        },
        ambulance: {
          freeSeconds: 480,
          perMinCharges: 2.0
        }
      }
    }),
    Object.assign({}, baseCityConfig, {
      cityName: "Siliguri",
      mapImage: "ys_ic_siliguri_map",
      cityCode: "std:0353",
      showSubscriptions: true,
      cityLat: 26.708845,
      cityLong: 88.434362,
      supportNumber: "08069724949",
      languageKey: "BN_IN",
      waitingChargesConfig: {
        cab: {
          freeSeconds: 300,
          perMinCharges: 2.0
        },
        auto: {
          freeSeconds: 180,
          perMinCharges: 1.50
        },
        bike: {
          freeSeconds: 180,
          perMinCharges: 1.0
        },
        ambulance: {
          freeSeconds: 480,
          perMinCharges: 2.0
        }
      },
      rentalWaitingChargesConfig: {
        cab: {
          freeSeconds: 300,
          perMinCharges: 2.0
        },
        auto: {
          freeSeconds: 180,
          perMinCharges: 2.0
        },
        bike: {
          freeSeconds: 180,
          perMinCharges: 2.0
        },
        ambulance: {
          freeSeconds: 480,
          perMinCharges: 2.0
        }
      },
      variantSubscriptionConfig: {
        enableVariantBasedSubscription: true,
        variantList: ["CarCategory","BikeCategory","AutoCategory"],
        enableCabsSubscriptionView: true,
        staticViewPlans: []
      }
      , rideStartAudio : {
        acCab : {},
        nonAcCab : {},
        auto : {},
        bike : {}
      }
    }),
    Object.assign({}, baseCityConfig, {
      cityName: "Asansol",
      mapImage: "ys_ic_asansol_map",
      cityCode: "std:0341",
      showSubscriptions: true,
      cityLat: 23.6889,
      cityLong: 86.9661,
      supportNumber: "08069724949",
      languageKey: "BN_IN",
      variantSubscriptionConfig: {
        enableVariantBasedSubscription: true,
        variantList: ["CarCategory","BikeCategory","AutoCategory"],
        enableCabsSubscriptionView: true,
        staticViewPlans: []
      }
    }),
    Object.assign({}, baseCityConfig, {
      cityName: "Durgapur",
      mapImage: "ys_ic_durgapur_map",
      cityCode: "std:0342",
      cityLat: 23.5204,
      cityLong: 87.3119,
      showSubscriptions: true,
      variantSubscriptionConfig: {
        enableVariantBasedSubscription: true,
        variantList: ["CarCategory","BikeCategory","AutoCategory"],
        enableCabsSubscriptionView: true,
        staticViewPlans: []
      }
    }),
    Object.assign({}, baseCityConfig, {
      cityName: "Petrapole",
      mapImage: "ys_ic_petrapole_map",
      cityCode: "std:03215",
      cityLat: 22.40117,
      cityLong: 88.58293,
      showSubscriptions: false
    }),
    Object.assign({}, baseCityConfig, {
      cityName: "Digha",
      mapImage: "ys_ic_digha_map",
      cityCode: "std:03216",
      cityLat: 21.6222,
      cityLong: 87.5066,
      showSubscriptions: false
    })
  ];
  return cityConfigs;
}

let defaultStartAudioConfig = {
  "acCab" : {},
  "nonAcCab" : {},
  "auto" : {},
  "bike" : {}
}
