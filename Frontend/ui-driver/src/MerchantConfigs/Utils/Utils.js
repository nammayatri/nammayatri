const hiString = require("./../../src/Strings/HI.js");
const enStrings = require("./../../src/Strings/EN.js");
const knStrings = require("./../../src/Strings/KN.js");
const taStrings = require("./../../src/Strings/TA.js");
const bnStrings = require("./../../src/Strings/BN.js");
const mlStrings = require("./../../src/Strings/ML.js");
const jatriConfig = require("./../../src/MerchantConfigs/JatriSathiConfig.js");
const nammaYatriConfig = require("./../../src/MerchantConfigs/NammaYatriPartnerConfig.js");
const yatriConfig = require("./../../src/MerchantConfigs/YatriPartnerConfig.js");


exports["getString'"] = function (constructorKey){
    let key = constructorKey.trim(); 
    switch(window.merchantID) {
        case "JATRISAATHIDRIVER" :
            if (jatriConfig.isCurrentMerchantString(key)){
                return jatriConfig.currentMerchantString(key);
            }
            break;
        case "NAMMAYATRIPARTNER" :
            if (nammaYatriConfig.isCurrentMerchantString(key)){
                return nammaYatriConfig.currentMerchantString(key);
            }
            break;
        case "YATRIPARTNER" :
            if (yatriConfig.isCurrentMerchantString(key)){
                return yatriConfig.currentMerchantString(key);
            }
            break;
        default:
            return getStringFromCommon(key);
      }
      return getStringFromCommon(key);
}

exports["getValueFromMerchant"] = function (constructorKey){
    let key = constructorKey.trim(); 
    switch(window.merchantID) {
        case "JATRISAATHIDRIVER" :
            return jatriConfig.getMerchantConfig(key);
        case "NAMMAYATRIPARTNER" :
            return nammaYatriConfig.getMerchantConfig(key);
        case "YATRIPARTNER" :
            return yatriConfig.getMerchantConfig(key);
        default:
            console.error("no value found for key "+ key);
            return "";
      }
}

function getStringFromCommon(key){
    var selectedLanguage = JBridge.getKeysInSharedPrefs("LANGUAGE_KEY");
    switch(selectedLanguage) {
        case "HI_IN" :
            return hiString.getStringValue(key);
        case "KN_IN" :
            return knStrings.getStringValue(key);
        case "EN_US" :
            return enStrings.getStringValue(key);
        case "TA_IN" :
            return taStrings.getStringValue(key);
        case "BN_IN" : 
            return bnStrings.getStringValue(key);
        case "ML_IN" :
            return mlStrings.getStringValue(key);
        default:
            return enStrings.getStringValue(key);
      }
}