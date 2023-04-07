export function getMerchantString(key){
    var selectedLanguage = JBridge.getKeysInSharedPrefs("LANGUAGE_KEY");
    switch(selectedLanguage) {
        case "ML_IN" :
            return getStringMLValue(key);
        default :
            return getStringENValue(key);
      }
}

export function getMerchantConfig(key){
    if (key in config){
        return config[key];
    }
    console.error("no value found for key "+ key);
    return "";
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
    "StringKeys": ["ABOUT_APP_DESCRIPTION", "WELCOME_TEXT", "REQUEST_AUTO_RIDE", "CURRENTLY_WE_ARE_LIVE_IN_", "DRIVER_PICKUP_CHARGES", "SUCCESSFUL_ONBOARD", "ABOUT_REFERRAL_PROGRAM_DISCRIPTION", "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER"],
    "isReferralEnabled" : "false",
    "showBookingPreference" : "false",
    "showRateCard" : "false",
    "showDashboard" : "false",
    "shareAppTitle" : "Share Yatri!",
    "shareAppContent" : "Hey there!\n\nYatri is Coming soon"
}

const malayalamStrings = {
    "WELCOME_TEXT" : "യാത്രയിലേക്ക് സ്വാഗതം",
}

const englishStrings = {
    "ABOUT_APP_DESCRIPTION": "Yatri is an open platform to connect commuters with transport providers. The app makes it convenient for travellers to find available means of transport, and avail these options by connecting them with service providers ",
    "WELCOME_TEXT": "Welcome to Yatri",
    "REQUEST_AUTO_RIDE": "Request Ride",
    "CURRENTLY_WE_ARE_LIVE_IN_": "Currently we're live in Kochi, you can enjoy our services there",
    "DRIVER_PICKUP_CHARGES": "Service Charges",
    "SUCCESSFUL_ONBOARD": "You have successfully signed on to \n Yatri",
    "ABOUT_REFERRAL_PROGRAM_DISCRIPTION": "The referral program incentivises drivers to accept more rides, cancel less and serve you better by recognising and rewarding worthy drivers. \n\n You can help out by entering the driver’s referral code  and improve the quality of rides for the Yatri Community!",
    "YOU_CAN_GET_REFERRAL_CODE_FROM_DRIVER": "\nYou can get a referral code by asking your Yatri Driver."
  }