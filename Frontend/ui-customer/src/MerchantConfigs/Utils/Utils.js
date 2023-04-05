const JatriConfig = require("./../../src/MerchantConfigs/JatriSaathiConfig.js");
const NammaYatriConfig = require("./../../src/MerchantConfigs/NammaYatriConfig.js");
const YatriConfig = require("./../../src/MerchantConfigs/YatriConfig.js");
const HindiStrings = require("./../../src/Strings/HI.js");
const KannadaStrings = require("./../../src/Strings/KN.js");
const EnglishStrings = require("./../../src/Strings/EN.js");
const BengaliStrings = require("./../../src/Strings/BN.js");


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

exports["getValueFromConfig"] = function (key) {
  switch (window.merchantID) {
    case "JATRISAATHI":
      if (JatriConfig.isMerchantConfig(key)) {
        return JatriConfig.config[key];
      } else {
        console.error("No value found for key -> " + key)
      }
      break;
    case "NAMMAYATRI":
      if (NammaYatriConfig.isMerchantConfig(key)) {
        return NammaYatriConfig.config[key];
      } else {
        console.error("No value found for key -> " + key)
      }
      break;
    case "YATRI":
      if (YatriConfig.isMerchantConfig(key)) {
        return YatriConfig.config[key];
      } else {
        console.error("No value found for key -> " + key)
      }
      break;
    default:
      console.error("No merchant found");
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
    default:
      return EnglishStrings.getStringValue(key);
  }
}