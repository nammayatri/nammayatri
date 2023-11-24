window.version = window.version || {};
let version = "1.0.0";
if (typeof __VERSION__ !== "undefined") {
  version = __VERSION__
}
window.version["configuration"]= version;
window.getMerchantConfig = function () {
  return JSON.stringify({
    "StringKeys": ["WELCOME_TEXT", "ABOUT_TEXT", "NEED_IT_TO_ENABLE_LOCATION", "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER", "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT", "YOUR_LOCATION_HELPS_OUR_SYSTEM"],
    "currency": "€",
    "showCorporateAddress" : false,
    "languageList": [{
      "name": "Français",
      "value": "FR_FR",
      "subtitle": "French"
    },
    { "name": "English",
      "value": "EN_US",
      "subtitle": "Anglaise"
    }],
    "englishInNative" : "Anglaise",
    "englishStrings": {
      "WELCOME_TEXT": "Welcome to the Alliance Taxis",
      "ABOUT_TEXT": "Alliance Taxis is an open platform to connect drivers with riders. The app makes it convenient for drivers to find riders with proposed desired rates. No ride based commission, just pay small amount in the form of monthly subscription",
      "NEED_IT_TO_ENABLE_LOCATION": "Alliance Taxis collect location data to enable share your location to monitor driver current location, even when the app is closed or not in use.",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "Currently,We allow only Paris registered number",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "You are about to place a call to the Yatri Support Team. Do you want to proceed?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "Your location helps our system to map down all the near by taxis and get you the quickest ride possible."
    },
    "frenchStrings": {
      "WELCOME_TEXT": "Bienvenue chez le partenaire Alliance Taxis",
      "ABOUT_TEXT": "Alliance Taxis est une plateforme ouverte pour mettre en relation chauffeurs et passagers. L'application permet aux conducteurs de trouver facilement des passagers avec les tarifs souhaités proposés. Pas de commission basée sur le trajet, payez simplement un petit montant sous forme d'abonnement mensuel",
      "NEED_IT_TO_ENABLE_LOCATION": "Alliance Taxis collecte des données de localisation pour permettre de partager votre position afin de surveiller la position actuelle du conducteur, même lorsque l'application est fermée ou non utilisée.",
      "CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER": "Actuellement, nous n'autorisons que le numéro enregistré au Paris",
      "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT": "Vous êtes sur le point d'appeler l'équipe d'assistance Yatri. Voulez-vous poursuivre?",
      "YOUR_LOCATION_HELPS_OUR_SYSTEM": "Votre emplacement aide notre système à cartographier tous les taxis à proximité et à vous offrir le trajet le plus rapide possible."
    },
    "logs": ["JUSPAY","FIREBASE","CLEVERTAP"]
    , "fontName" : "Montserrat"
    , "fontKannada" : "Montserrat"
    , "showGenderBanner" : false
    , "defaultLanguage" : "FR_FR"
    , "allowAllMobileNumber" : true
    , "navigationAppConfig" : {
      "android" : {
        "query" : "https://waze.com/ul?ll=%f,%f"
        , "packageName" : "com.waze"
      }
    }
    , "others" : {
      "otpRegex" :  "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
      , "termsLink" : "https://docs.google.com/document/d/1zmQWO_L4EjyCXC3xSlp1f3DS2wI4HfbHxg42tXelWe0"
      , "privacyLink" : "https://docs.google.com/document/d/1gI_P4oZnVwE0O71rI4Mi8rpZbL9rsIRkyewbql85Np8"
    }
    , "features" : {
      "enableBonus" : false
      , "enableImageUpload" : true
      , "enableGender" : false
      , "enableOtpRide" : false
    }
    , "appDatas" : {
      "link" : "https://play.google.com/store/apps/details?id=net.openkochi.yatripartner"
      , "name" : "Pass Culture"
    }
    , "vehicle" : {
      "validationPrefix" :  "KL"
    }
    , "banners" :{
      "autoPay" : false
    }
    , "referral": {
      "link" : "https://yatricustomer.page.link/pcJb"
    }
  })
}