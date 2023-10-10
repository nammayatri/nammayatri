const hindiStrings = require("./../../src/Strings/HI.js");
const kannadaStrings = require("./../../src/Strings/KN.js");
const englishStrings = require("./../../src/Strings/EN.js");
const bengaliStrings = require("./../../src/Strings/BN.js");
const malayalamStrings = require("./../../src/Strings/ML.js");
const tamilStrings = require("./../../src/Strings/TA.js");
const frenchStrings = require("./../../src/Strings/FR.js")


export const getStringFromConfig = function (key) {

  if (window.appConfig.StringKeys.includes(key)){
    return getMerchantString(key);
  }
  return getStringFromCommon(key);
}

export const getValueFromConfig = function (constructorKey){
  if (constructorKey in window.appConfig){
    return window.appConfig[constructorKey];
  }
  console.error("no value found for key "+ constructorKey);
  return "";
}

function getStringFromCommon(key) {
  var selectedLanguage = JBridge.getKeysInSharedPref("LANGUAGE_KEY");
  switch (selectedLanguage) {
    case "HI_IN":
      return hindiStrings.getStringValue(key);
    case "KN_IN":
      return kannadaStrings.getStringValue(key);
    case "BN_IN":
      return bengaliStrings.getStringValue(key);
    case "ML_IN":
      return malayalamStrings.getStringValue(key);
    case "TA_IN":
      return tamilStrings.getStringValue(key);
    case "FR_FR":
      return frenchStrings.getStringValue(key);
    default:
      return englishStrings.getStringValue(key);
  }
}

export const getENStrings = function (constructorKey){
  return englishStrings.getStringValue(constructorKey);
}

export const getMerchantString = function(key) {
  var selectedLanguage = JBridge.getKeysInSharedPref("LANGUAGE_KEY");
  switch (selectedLanguage) {
    case "HI_IN":
      return window.appConfig.hindiStrings[key];
    case "KN_IN":
      return window.appConfig.kannadaStrings[key];
    case "BN_IN":
      return window.appConfig.bengaliStrings[key];
    case "ML_IN":
      return window.appConfig.malayalamStrings[key];
    case "TA_IN":
      return window.appConfig.tamilStrings[key];
    case "FR_FR":
      return window.appConfig.frenchStrings[key];
    default:
      return window.appConfig.englishStrings[key];
  }
}

export const getMerchantId = function(id) {
  return window.merchantID;
}

export const getMerchantConfig = function (just) {
  return function (nothing) {
    return function () {
      if (typeof window.appConfig !== "undefined") {
        return just(window.appConfig);
      }
      return nothing;
    }
  }
}

export const getStringWithVar = function (str) {
  return function (vals) {
      try {
        let currentIndex = 0;
        
        const regex = /{}/g;
    
        const processedString = str.replace(regex, () => {
          const currentValue = vals[currentIndex];
          currentIndex++;
          return currentValue;
        });
        return processedString;
      } catch (err) {
        console.log(err);
      }
    }
}