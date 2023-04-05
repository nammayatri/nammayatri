
export function currentMerchantString(key){
    var selectedLanguage = JBridge.getKeysInSharedPrefs("LANGUAGE_KEY");
    switch(selectedLanguage) {
        case "HI_IN" :
            return getStringHIValue(key);
        case "ML_IN" :
            return getStringMLValue(key);
        default:
            return getStringENValue(key);
      }
}

export function isCurrentMerchantString(key){
    return ((key in malayalamStrings) && (key in hindiStrings) && (key in englishStrings));
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

export function getStringHIValue(key){
    if (key in hindiStrings){
       return hindiStrings[key];
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
    ABOUT_TEXT : "Yatri partner ಪ್ರಯಾಣಿಕರೊಂದಿಗೆ ಚಾಲಕರನ್ನು ಸಂಪರ್ಕಿಸಲು ಮುಕ್ತ ವೇದಿಕೆಯಾಗಿದೆ. ಅಪ್ಲಿಕೇಶನ್ ಚಾಲಕರು ಪ್ರಯಾಣಿಕರನ್ನು ಹುಡುಕಲು ಅನುಕೂಲವಾಗುವಂತೆ ಮಾಡುತ್ತದೆ. ಮತ್ತು ಅವುಗಳನ್ನು ಸೇವಾ ಪೂರೈಕೆದಾರರೊಂದಿಗೆ ಸಂಪರ್ಕಿಸುವ ಮೂಲಕ ಈ ಆಯ್ಕೆಗಳನ್ನು ಪಡೆದುಕೊಳ್ಳಿ"
}

const hindiStrings = {
    WELCOME_TEXT : "Welcome to the Yatri Partner",
    ABOUT_TEXT : "Yatri partner चालकों को सवारियों से जोड़ने का एक खुला मंच है। ऐप ड्राइवरों के लिए प्रस्तावित वांछित दरों के साथ सवारियों को ढूंढना सुविधाजनक बनाता है। कोई सवारी आधारित कमीशन नहीं, बस मासिक सदस्यता के रूप में छोटी राशि का भुगतान करें"
}

const englishStrings = {
    WELCOME_TEXT : "Welcome to the Yatri Partner",
    ABOUT_TEXT : "Yatri partner is an open platform to connect drivers with riders. The app makes it convenient for drivers to find riders with proposed desired rates. No ride based commission, just pay small amount in the form of monthly subscription"
}