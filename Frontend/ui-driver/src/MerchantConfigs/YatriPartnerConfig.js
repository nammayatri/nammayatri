
export function currentMerchantString(key){
    var selectedLanguage = JBridge.getKeysInSharedPrefs("LANGUAGE_KEY");
    switch(selectedLanguage) {
        case "ML_IN" :
            return getStringMLValue(key);
        default:
            return getStringENValue(key);
      }
}

export function isCurrentMerchantString(key){
    return ((key in malayalamStrings) && (key in englishStrings));
}

export function getMerchantConfig(key){
    if (key in yatriPartnerConfigs){
        return yatriPartnerConfigs[key];
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


export function getStringMLValue(key){
    if (key in malayalamStrings){
        return malayalamStrings[key];
    }
    console.error("no value found for key "+ key);
    return "";
}

const yatriPartnerConfigs = {
    RC_VALIDATION_TEXT : "KL"
}

const malayalamStrings = {
    WELCOME_TEXT : "Welcome to the Yatri Partner",
    ABOUT_TEXT : "Yatri partner ಪ್ರಯಾಣಿಕರೊಂದಿಗೆ ಚಾಲಕರನ್ನು ಸಂಪರ್ಕಿಸಲು ಮುಕ್ತ ವೇದಿಕೆಯಾಗಿದೆ. ಅಪ್ಲಿಕೇಶನ್ ಚಾಲಕರು ಪ್ರಯಾಣಿಕರನ್ನು ಹುಡುಕಲು ಅನುಕೂಲವಾಗುವಂತೆ ಮಾಡುತ್ತದೆ. ಮತ್ತು ಅವುಗಳನ್ನು ಸೇವಾ ಪೂರೈಕೆದಾರರೊಂದಿಗೆ ಸಂಪರ್ಕಿಸುವ ಮೂಲಕ ಈ ಆಯ್ಕೆಗಳನ್ನು ಪಡೆದುಕೊಳ್ಳಿ",
    NEED_IT_TO_ENABLE_LOCATION : "ആപ്പ് അടച്ചിരിക്കുമ്പോഴും ഉപയോഗത്തിലില്ലെങ്കിലും ഡ്രൈവർ നിലവിലെ ലൊക്കേഷൻ നിരീക്ഷിക്കാൻ നിങ്ങളുടെ ലൊക്കേഷൻ പങ്കിടുന്നത് പ്രവർത്തനക്ഷമമാക്കാൻ യാത്രി പങ്കാളി ലൊക്കേഷൻ ഡാറ്റ ശേഖരിക്കുന്നു.",
    CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER : "നിലവിൽ കേരളത്തിൽ രജിസ്റ്റർ ചെയ്ത നമ്പർ മാത്രമേ ഞങ്ങൾ അനുവദിക്കൂ",
    YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT : "നിങ്ങൾ യാത്രി സപ്പോർട്ട് ടീമിലേക്ക് ഒരു കോൾ ചെയ്യാൻ പോകുകയാണ്. നിങ്ങൾക്ക് തുടരണോ?"
}

const englishStrings = {
    WELCOME_TEXT : "Welcome to the Yatri Partner",
    ABOUT_TEXT : "Yatri partner is an open platform to connect drivers with riders. The app makes it convenient for drivers to find riders with proposed desired rates. No ride based commission, just pay small amount in the form of monthly subscription",
    NEED_IT_TO_ENABLE_LOCATION : "Yatri Partner collect location data to enable share your location to monitor driver current location, even when the app is closed or not in use.",
    CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER : "Currently,We allow only Kerala registered number",
    YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT : "You are about to place a call to the Yatri Support Team. Do you want to proceed?"
}