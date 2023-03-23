export function getMerchantString(key){
    var selectedLanguage = JBridge.getKeysInSharedPrefs("LANGUAGE_KEY");
    switch(selectedLanguage) {
        case "ML_IN" :
            return getStringMLValue(key);
        default :
            return getStringENValue(key);
      }
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
    StringKeys : ["WELCOME_TEXT", "ABOUT_TEXT"]
}

const malayalamStrings = {
    "WELCOME_TEXT" : "യാത്രയിലേക്ക് സ്വാഗതം",
}


const englishStrings = {
    "WELCOME_TEXT" : "Welcome to the Yatri",
}