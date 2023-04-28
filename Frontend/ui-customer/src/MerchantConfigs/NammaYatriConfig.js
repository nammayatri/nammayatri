export function getMerchantString(key) {
  var selectedLanguage = JBridge.getKeysInSharedPrefs("LANGUAGE_KEY");
  switch (selectedLanguage) {
    case "HI_IN":
      return getStringHIValue(key);
    case "KN_IN":
      return getStringKNValue(key);
    default:
      return getStringENValue(key);
  }
}

export function isMerchantString(key) {
  return config.StringKeys.includes(key);
}

export function getMerchantConfig(key){
  if (key in config){
      return config[key];
  }
  console.error("no value found for key "+ key);
  return "";
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

export function getStringKNValue(key) {
  if (key in kannadaStrings) {
    return kannadaStrings[key];
  }
  return "error in getCommonKN";
}

export function getAppLink(os) {
  if (os == "ANDROID"){
      return "https://play.google.com/store/apps/details?id=in.juspay.nammayatri"
  }else {return "https://apps.apple.com/in/app/namma-yatri/id1637429831"}
}

const config = {
  "StringKeys": [],
  "isReferralEnabled" : "true",
  "showBookingPreference" : "true",
  "showRateCard" : "true",
  "showDashboard" : "true",
  "shareAppTitle" : "Share Namma Yatri!",
  "shareAppContent" : "Hey there!\n\nCheck India's first Zero Commission auto booking app.\n100% Open source | 100% Open Data\n\nDownload Namma Yatri now! \nhttps://nammayatri.in/link/rider/SJ8D \n\n #beOpen #chooseOpen",
  "DOCUMENT_LINK" : "https://docs.google.com/document/d/1-oRR_oI8ncZRPZvFZEJZeCVQjTmXTmHA",
  "APP_LINK" : getAppLink(window.__OS),
  "PRIVACY_POLICY_LINK" : "https://docs.google.com/document/d/128VU80K5E1iz-x6QnP1R127m_lwmDO3F",
  "CUSTOMER_TIP" : "true"
}

const kannadaStrings = {
}

const hindiStrings = {
}

const englishStrings = {
}

