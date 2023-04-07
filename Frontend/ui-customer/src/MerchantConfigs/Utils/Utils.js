const JatriConfig = require("./../../src/MerchantConfigs/JatriSaathiConfig.js");
const NammaYatriConfig = require("./../../src/MerchantConfigs/NammaYatriConfig.js");
const YatriConfig = require("./../../src/MerchantConfigs/YatriConfig.js");
const HindiStrings = require("./../../src/Strings/HI.js");
const KannadaStrings = require("./../../src/Strings/KN.js");
const EnglishStrings = require("./../../src/Strings/EN.js");
const BengaliStrings = require("./../../src/Strings/BN.js");
const MalayalamStrings = require("./../../src/Strings/ML.js");


exports["getString'"] = function (key) {
  switch (window.merchantID) {
    case "JATRISAATHI":
      if (JatriConfig.isMerchantString(key)) {
        return JatriConfig.getMerchantString(key);
      }
      break;
    case "NAMMAYATRI":
      if (NammaYatriConfig.isMerchantString(key)) {
        return NammaYatriConfig.getMerchantString(key);
      }
      break;
    case "YATRI":
      if (YatriConfig.isMerchantString(key)) {
        return YatriConfig.getMerchantString(key);
      }
      break;
    default:
      return getStringFromCommon(key);
  }
  return getStringFromCommon(key);
}

exports["getValueFromConfig"] = function (constructorKey){
  let key = constructorKey.trim(); 
  switch(window.merchantID) {
      case "JATRISAATHI" :
          return JatriConfig.getMerchantConfig(key);
      case "NAMMAYATRI" :
          return NammaYatriConfig.getMerchantConfig(key);
      case "YATRI" :
          return YatriConfig.getMerchantConfig(key);
      default:
          console.error("no value found for key "+ key);
          return "";
    }
}

function getStringFromCommon(key) {
  var selectedLanguage = JBridge.getKeysInSharedPrefs("LANGUAGE_KEY");
  switch (selectedLanguage) {
    case "HI_IN":
      return HindiStrings.getStringValue(key);
    case "KN_IN":
      return KannadaStrings.getStringValue(key);
    case "BN_IN":
      return BengaliStrings.getStringValue(key);
    case "ML_IN":
      return MalayalamStrings.getStringValue(key);
    default:
      return EnglishStrings.getStringValue(key);
  }
}