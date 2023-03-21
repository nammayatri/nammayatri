export function getMerchantString(key){
    var selectedLanguage = JBridge.getKeysInSharedPrefs("LANGUAGE_KEY");
    switch(selectedLanguage) {
        case "HI_IN" :
            return getStringHIValue(key);
        case "KN_IN" :
            return getStringKNValue(key);
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

export function getStringHIValue(key){
    if (key in hindiStrings){
       return hindiStrings[key];
    }
    return "error in getCommonHI";
}

export function getStringKNValue(key){
    if (key in kannadaStrings){
        return kannadaStrings[key];
    }
    return "error in getCommonKN";
}


const config = {
    StringKeys : ["WELCOME_TEXT", "ABOUT_TEXT"]
}

const kannadaStrings = {
    "WELCOME_TEXT" : "Welcome to the app kannadaStrings",
    "ABOUT_TEXT" : "Jatri Sathi ಪ್ರಯಾಣಿಕರೊಂದಿಗೆ ಚಾಲಕರನ್ನು ಸಂಪರ್ಕಿಸಲು ಮುಕ್ತ ವೇದಿಕೆಯಾಗಿದೆ. ಅಪ್ಲಿಕೇಶನ್ ಚಾಲಕರು ಪ್ರಯಾಣಿಕರನ್ನು ಹುಡುಕಲು ಅನುಕೂಲವಾಗುವಂತೆ ಮಾಡುತ್ತದೆ. ಮತ್ತು ಅವುಗಳನ್ನು ಸೇವಾ ಪೂರೈಕೆದಾರರೊಂದಿಗೆ ಸಂಪರ್ಕಿಸುವ ಮೂಲಕ ಈ ಆಯ್ಕೆಗಳನ್ನು ಪಡೆದುಕೊಳ್ಳಿ"
}

const hindiStrings = {
    "WELCOME_TEXT" : "Welcome to the  Jatri Saathi",
    "ABOUT_TEXT" : "Jatri Sathi चालकों को सवारियों से जोड़ने का एक खुला मंच है। ऐप ड्राइवरों के लिए प्रस्तावित वांछित दरों के साथ सवारियों को ढूंढना सुविधाजनक बनाता है। कोई सवारी आधारित कमीशन नहीं, बस मासिक सदस्यता के रूप में छोटी राशि का भुगतान करें"
}

const englishStrings = {
    "WELCOME_TEXT" : "Welcome to the Jatri Saathi",
    "ABOUT_TEXT" : "Jatri Saathi is an open platform to connect riders with drivers. The app makes it convenient for riders to book a ride with meter rate hence minimal fare."
}