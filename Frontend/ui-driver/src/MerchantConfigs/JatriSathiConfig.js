
export function currentMerchantString(key){
    var selectedLanguage = JBridge.getKeysInSharedPrefs("LANGUAGE_KEY");
    switch(selectedLanguage) {
        case "HI_IN" :
            return getStringHIValue(key);
        case "BN_IN" :
            return getStringBNValue(key);
        default:
            return getStringENValue(key);
      }
}

export function isCurrentMerchantString(key){
    return ((key in bengaliStrings) && (key in hindiStrings) && (key in englishStrings));
}

export function getMerchantConfig(key){
    if (key in jatriSathiConfigs){
        return jatriSathiConfigs[key];
    }
    console.error("no value found for key "+ key);
    return "";
}

export function getStringENValue(key){
    if (key in englishStrings){
        return englishStrings[key];
    }
    console.error("no value found for key "+ key);
    return "";
}

export function getStringHIValue(key){
    if (key in hindiStrings){
       return hindiStrings[key];
    }
    console.error("no value found for key "+ key);
    return "";
}

export function getStringBNValue(key){
    if (key in bengaliStrings){
        return bengaliStrings[key];
    }
    console.error("no value found for key "+ key);
    return "";
}


const jatriSathiConfigs = {
    RC_VALIDATION_TEXT : "WB",
    DOCUMENT_LINK : "https://drive.google.com/file/d/1qYXbQUF4DVo2xNOawkHNTR_VVe46nggc",
    APP_LINK : "",
    PRIVACY_POLICY_LINK : "https://docs.google.com/document/d/1tF96MwtaEiq70y_P40E29Sy3X61moTc9",
    SPECIAL_ZONE_OTP_VIEW : "true"
}

const bengaliStrings = {
    WELCOME_TEXT : "Welcome to the Jatri Sathi Driver",
    ABOUT_TEXT : "যাত্রী সাথী অংশীদার চালকদের রাইডারদের সাথে সংযোগ করার জন্য একটি উন্মুক্ত প্ল্যাটফর্ম। অ্যাপটি চালকদের জন্য প্রস্তাবিত পছন্দসই রেট সহ রাইডার খুঁজে পেতে সুবিধাজনক করে তোলে। কোন রাইড ভিত্তিক কমিশন নেই, শুধুমাত্র মাসিক সাবস্ক্রিপশন আকারে অল্প পরিমাণ অর্থ প্রদান করুন",
    NEED_IT_TO_ENABLE_LOCATION : "জাটি সাথি ড্রাইভার ড্রাইভারের বর্তমান অবস্থান নিরীক্ষণের জন্য আপনার অবস্থানটি ভাগ করে নিতে সক্ষম করতে অবস্থানের ডেটা সংগ্রহ করে, এমনকি অ্যাপটি বন্ধ থাকলেও বা ব্যবহার না করা হয়।",
    CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER : "বর্তমানে, আমরা শুধুমাত্র পশ্চিমবঙ্গ নিবন্ধিত নম্বর অনুমোদন করি",
    YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT : "আপনি জাত্রি সাথি সমর্থন দলকে কল করতে চলেছেন। আপনি কি এগিয়ে যেতে চান?",
    YOUR_LOCATION_HELPS_OUR_SYSTEM : "আপনার অবস্থান আমাদের সিস্টেমকে ট্যাক্সি দ্বারা আশেপাশের সমস্ত স্থান ম্যাপ করতে এবং আপনাকে দ্রুততম রাইড করতে সাহায্য করে৷"
}

const hindiStrings = {
    WELCOME_TEXT : "Welcome to the Jatri Sathi Driver",
    ABOUT_TEXT : "Jatri Sathi partner चालकों को सवारियों से जोड़ने का एक खुला मंच है। ऐप ड्राइवरों के लिए प्रस्तावित वांछित दरों के साथ सवारियों को ढूंढना सुविधाजनक बनाता है। कोई सवारी आधारित कमीशन नहीं, बस मासिक सदस्यता के रूप में छोटी राशि का भुगतान करें",
    NEED_IT_TO_ENABLE_LOCATION : "Jatri Sathi partner ड्राइवर के लोकेशन की निगरानी के लिए अपना स्थान साझा करने के लिए लोकेशन डेटा एकत्र करता है, तब भी जब ऐप बंद हो या उपयोग में न हो।",
    CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER : "Currently,We allow only West Bengal registered number",
    YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT : "आप जात्री साथी सपोर्ट टीम को कॉल करने वाले हैं। क्या आपकी आगे बढ़ने की इच्छा है?",
    YOUR_LOCATION_HELPS_OUR_SYSTEM : "आपका स्थान हमारे सिस्टम को आस पास के सभी टैक्सियों को मैप करने में सहायता कर्ता है और आपको सबसे तेज सवारी प्रदान करता है"
}

const englishStrings = {
    WELCOME_TEXT : "Welcome to the Jatri Sathi Driver",
    ABOUT_TEXT : "Jatri Sathi partner is an open platform to connect drivers with riders. The app makes it convenient for drivers to find riders with proposed desired rates. No ride based commission, just pay small amount in the form of monthly subscription",
    NEED_IT_TO_ENABLE_LOCATION : "Jatri Sathi Driver collect location data to enable share your location to monitor driver current location, even when the app is closed or not in use.",
    CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER : "Currently,We allow only West Bengal registered number",
    YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT : "You are about to place a call to the Jatri Sathi Support Team. Do you want to proceed?",
    YOUR_LOCATION_HELPS_OUR_SYSTEM : "Your location helps our system to map down all the near by taxis and get you the quickest ride possible."
}