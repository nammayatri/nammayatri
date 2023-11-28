
const JBridge = window.JBridge;

export const getValueFromConfig = function (constructorKey){
  if (constructorKey in window.appConfig){
    return window.appConfig[constructorKey];
  }
  console.error("no value found for key "+ constructorKey);
  return "";
}


export const getMerchantString = function(key) {
  const selectedLanguage = JBridge.getKeysInSharedPref("LANGUAGE_KEY");
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
    case "TE_IN": 
      return window.appConfig.teluguStrings[key];
    case "FR_FR":
      return window.appConfig.frenchStrings[key];
    default:
      return window.appConfig.englishStrings[key];
  }
}

export const getStringFromConfig = function (constructors) {
  return function (just) {
    return function (nothing) {
      const key = constructors.constructor.name;
      if (window.appConfig.StringKeys.includes(key)){
        return just(getMerchantString(key));
      }
      return nothing;
    }
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
        const currentValue = vals[currentIndex++];
        return currentValue ? currentValue : "";
      });
      return processedString;
    } catch (err) {
      console.log(err);
    }
  }
}