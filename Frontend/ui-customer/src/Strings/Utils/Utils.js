const JatriConfig = require("./../../src/MerchantConfigs/JatriSaathi/Config.js");
const NammaYatriConfig = require("./../../src/MerchantConfigs/NammaYatri/Config.js");
const YatriConfig = require("./../../src/MerchantConfigs/Yatri/Config.js");


exports["getString'"] = function (key){
    switch(window.merchantID) {
        case "JATRISAATHI" :
            if (JatriConfig.isMerchantString(key)){
                return JatriConfig.getMerchantString(key);
            }
            break;
        case "NAMMAYATRI" :
            if (NammaYatriConfig.isMerchantString(key)){
                return NammaYatriConfig.getMerchantString(key);
            }
            break;
        case "YATRI" :
            if (YatriConfig.isMerchantString(key)){
                return YatriConfig.getMerchantString(key);
            }
            break;
        default:
            return getStringFromCommon(key);
      }
}

function getStringFromCommon(key){
    var selectedLanguage = JBridge.getKeysInSharedPrefs("LANGUAGE_KEY");
    switch(selectedLanguage) {
        case "HI_IN" :
            return HindiStrings(key);
        case "KN_IN" :
            return KannadaStrings(key);
        default:
            return EnglishStrings(key);
      }
}