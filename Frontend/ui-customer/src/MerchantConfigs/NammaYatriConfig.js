export function getMerchantString(key) {
  var selectedLanguage = JBridge.getKeysInSharedPrefs("LANGUAGE_KEY");
  switch (selectedLanguage) {
    case "HI_IN":
      return getStringHIValue(key);
    case "KN_IN":
      return getStringKNValue(key);
    default:
      return getStringENValue(key);
  }
}

export function isMerchantString(key) {
  return config.StringKeys.includes(key);
}

export function isMerchantConfig(key) {
  return key in config;
}

export function getStringENValue(key) {
  if (key in englishStrings) {
    return englishStrings[key];
  }
  return "error in getCommonEN";
}

export function getStringHIValue(key) {
  if (key in hindiStrings) {
    return hindiStrings[key];
  }
  return "error in getCommonHI";
}

export function getStringKNValue(key) {
  if (key in kannadaStrings) {
    return kannadaStrings[key];
  }
  return "error in getCommonKN";
}


const config = {
  "StringKeys": [],
}

const kannadaStrings = {
}

const hindiStrings = {
}

const englishStrings = {
}