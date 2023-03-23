export function getMerchantString(key) {
  var selectedLanguage = JBridge.getKeysInSharedPrefs("LANGUAGE_KEY");
  switch (selectedLanguage) {
    case "HI_IN":
      return getStringHIValue(key);
    case "BN_IN":
      return getStringBNValue(key);
    default:
      return getStringENValue(key);
  }
}

export function isMerchantString(key) {
  return config.StringKeys.includes(key);
}

export function isMerchantConfig(key) {
  return config.includes(key);
}

export function getStringENValue(key) {
  if (key in englishStrings) {
    return englishStrings[key];
  }
  return "error in getCommonEN";
}

export function getStringHIValue(key) {
  if (key in hindiStrings) {
    return hindiStrings[key];
  }
  return "error in getCommonHI";
}

export function getStringBNValue(key) {
  if (key in bengaliStrings) {
    return bengaliStrings[key];
  }
  return "error in getCommonBN";
}


const config = {
  StringKeys: ["ABOUT_APP_DESCRIPTION", "WELCOME_TEXT", "REQUEST_AUTO_RIDE", "CURRENTLY_WE_ARE_LIVE_IN_", "DRIVER_PICKUP_CHARGES", "YOU_ARE_ABOUT_TO_CALL_JATRI_SATHI_SUPPORT", "SUCCESSFUL_ONBOARD", "ABOUT_REFERRAL_PROGRAM_DISCRIPTION", "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER"],
}

const bengaliStrings = {
  "ABOUT_APP_DESCRIPTION": "যাত্রী সাথী হল চালকদের সাথে রাইডারদের সংযোগ করার জন্য একটি উন্মুক্ত প্ল্যাটফর্ম। অ্যাপটি রাইডারদের জন্য মিটার রেট সহ একটি রাইড বুক করা সুবিধাজনক করে তোলে তাই ন্যূনতম ভাড়া।",
  "WELCOME_TEXT": "স্বাগতম সাথী",
  "REQUEST_AUTO_RIDE": "যাত্রার অনুরোধ করুন",
  "CURRENTLY_WE_ARE_LIVE_IN_": "বর্তমানে আমরা কলকাতায় থাকি, আপনি সেখানে আমাদের পরিষেবা উপভোগ করতে পারেন",
  "DRIVER_PICKUP_CHARGES": "সারভিস চারজ",
  "YOU_ARE_ABOUT_TO_CALL_JATRI_SATHI_SUPPORT": "আপনি যাত্রী সাথী সমর্থন দলকে একটি কল করতে চলেছেন৷ আপনি কি এগিয়ে যেতে চান?",
  "SUCCESSFUL_ONBOARD": "আপনি সফলভাবে যাত্রী সাথীতে \n স্বাক্ষর করেছেন",
  "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "রেফারেল প্রোগ্রাম চালকদের আরও রাইড গ্রহণ করতে, কম বাতিল করতে এবং যোগ্য ড্রাইভারদের চিনতে এবং পুরস্কৃত করার মাধ্যমে আপনাকে আরও ভাল পরিবেশন করতে উত্সাহিত করে। \n\n আপনি ড্রাইভারের রেফারেল কোড লিখে সাহায্য করতে পারেন এবং যাত্রী সাথী সম্প্রদায়ের জন্য রাইডের মান উন্নত করতে পারেন!",
  "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "\n আপনি আপনার যাত্রী সাথী ড্রাইভারকে জিজ্ঞাসা করে একটি রেফারেল কোড পেতে পারেন।"
}

const hindiStrings = {
  "ABOUT_APP_DESCRIPTION": "जात्री साथी सवारियों को चालकों से जोड़ने का एक खुला मंच है। ऐप राइडर्स के लिए मीटर रेट के साथ राइड बुक करना सुविधाजनक बनाता है इसलिए न्यूनतम किराया।",
  "WELCOME_TEXT": "स्वागत है साथी",
  "REQUEST_AUTO_RIDE": "राइड का अनुरोध करें",
  "CURRENTLY_WE_ARE_LIVE_IN_": "वर्तमान में हम कोलकाता में रहते हैं, आप वहां हमारी सेवाओं का आनंद ले सकते हैं",
  "DRIVER_PICKUP_CHARGES": "सेवा प्रभार",
  "YOU_ARE_ABOUT_TO_CALL_JATRI_SATHI_SUPPORT": "आप जात्री साथी सपोर्ट टीम को कॉल करने वाले हैं। क्या आपकी आगे बढ़ने की इच्छा है?",
  "SUCCESSFUL_ONBOARD": "आपने जात्री साथी पर सफलतापूर्वक हस्ताक्षर कर \n लिए हैं",
  "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "रेफ़रल कार्यक्रम ड्राइवरों को अधिक सवारी स्वीकार करने, कम रद्द करने और योग्य ड्राइवरों को पहचानने और पुरस्कृत करके आपको बेहतर सेवा देने के लिए प्रोत्साहित करता है। \n\n आप ड्राइवर का रेफ़रल कोड डालकर मदद कर सकते हैं और जात्री साथी समुदाय के लिए सवारी की गुणवत्ता में सुधार कर सकते हैं!",
  "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "\n आप अपने जात्री साथी ड्राइवर से पूछकर रेफ़रल कोड प्राप्त कर सकते हैं।"
}

const englishStrings = {
  "ABOUT_APP_DESCRIPTION": "Jatri Sathi is an open platform to connect riders with drivers. The app makes it convenient for riders to book a ride with meter rate hence minimal fare.",
  "WELCOME_TEXT": "Welcome to Jatri Saathi",
  "REQUEST_AUTO_RIDE": "Request Ride",
  "CURRENTLY_WE_ARE_LIVE_IN_": "Currently we're live in Kolkata, you can enjoy our services there",
  "DRIVER_PICKUP_CHARGES": "Service Charges",
  "YOU_ARE_ABOUT_TO_CALL_JATRI_SATHI_SUPPORT": "You are about to place a call to the Jatri Sathi Support Team. Do you want to proceed?",
  "SUCCESSFUL_ONBOARD": "You have successfully signed on to \n Jatri Sathi",
  "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "The referral program incentivises drivers to accept more rides, cancel less and serve you better by recognising and rewarding worthy drivers. \n\n You can help out by entering the driver’s referral code  and improve the quality of rides for the Jatri Sathi Community!",
  "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "\nYou can get a referral code by asking your Jatri Sathi Driver."
}