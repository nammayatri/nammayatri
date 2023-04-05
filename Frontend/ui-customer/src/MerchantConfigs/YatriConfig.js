export function getMerchantString(key){
    var selectedLanguage = JBridge.getKeysInSharedPrefs("LANGUAGE_KEY");
    switch(selectedLanguage) {
        case "ML_IN" :
            return getStringMLValue(key);
        default :
            return getStringENValue(key);
      }
}

export function getMerchantConfig(key){
    if (key in config){
        return config[key];
    }
    console.error("no value found for key "+ key);
    return "";
  }

export function isMerchantString(key){
    return config.StringKeys.includes(key);
}

export function getStringENValue(key){
    if (key in englishStrings){
        return englishStrings[key];
    }
    return "error in getCommonEN";
}

export function getStringMLValue(key){
    if (key in malayalamStrings){
        return malayalamStrings[key];
    }
    return "error in getCommonKN";
}


const config = {
    "StringKeys" : ["WELCOME_TEXT", "ABOUT_TEXT"],
    "isReferralEnabled" : "false",
    "showBookingPreference" : "false",
    "showRateCard" : "false",
    "showDashboard" : "false",
    "shareAppTitle" : "Share Yatri!",
    "shareAppContent" : "Hey there!\n\nYatri is Coming soon"
}

const malayalamStrings = {
    "WELCOME_TEXT" : "യാത്രയിലേക്ക് സ്വാഗതം",
}


const englishStrings = {
    "WELCOME_TEXT" : "Welcome to the Yatri",
}