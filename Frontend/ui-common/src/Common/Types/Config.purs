module Common.Types.Config where


type CommonAppConfig = (
    colors :: Colors
  , primaryButtonConfig :: PrimaryButtonConfig
  , fontConfig :: FontConfig
  , loaderConfig :: LoaderConfig
  , others :: Miscellaneous
  , currency :: String
  , internationalNumberEnabled :: Boolean
  , navigationAppConfig :: NavigationConfig
  , genericHeaderConfig :: GenericHeaderConfig
  , showCorporateAddress :: Boolean
  , appDatas :: AppDatas
  , defaultLanguage :: String)

defaultColors :: Colors
defaultColors = {
    black800 : "#454545"
  , black900 : "#2C2F3A"
  , red : "#E55454"
  }

defaultAppData :: AppDatas
defaultAppData =  {
    link : ""
  , supportMail :"nammayatri.support@juspay.in"
  , name : ""
  }

defaultPrimaryButtonConfig :: PrimaryButtonConfig
defaultPrimaryButtonConfig =  {
    isGradient : false
  , gradient : []
  , loaderUrl : "primary_button_loader.json"
  }

defaultFontConfig :: FontConfig
defaultFontConfig = {
    default : "PlusJakartaSans"
  , kannada : "NotoSansKannada"
  , "type": "Assets"
  }

defaultLoaderConfig :: LoaderConfig
defaultLoaderConfig = {
  color : "#2C2F3A"
}
defaultOthers :: Miscellaneous
defaultOthers = {
    otpRegex :  "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+"
  , termsLink : "https://docs.google.com/document/d/1-oRR_oI8ncZRPZvFZEJZeCVQjTmXTmHA"
  , privacyLink : "https://docs.google.com/document/d/128VU80K5E1iz-x6QnP1R127m_lwmDO3F"
}
defaultGenericHeader :: GenericHeaderConfig
defaultGenericHeader = {
    backArrow : "ny_ic_chevron_left_white,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_chevron_left_white.png"
  }

defaultNavigationAppConfig :: NavigationConfig
defaultNavigationAppConfig = {
      android : {  
        query : "google.navigation:q=%f,%f"
      , packageName : "com.google.android.apps.maps"
      , walkQuery : "google.navigation:q=%f,%f&mode=w"
      }
    , ios : {
        query : "http://maps.google.com///?saddr=&daddr=%@,%@&dirflg=d"
      , walkQuery : "http://maps.google.com///?saddr=&daddr=%@,%@&dirflg=w"
      , packageName : ""
      }
    }

type Colors = {
  black800 :: String
, black900 :: String
, red :: String
}

type PrimaryButtonConfig = {
  isGradient :: Boolean
, gradient :: Array String
, loaderUrl :: String
}

type FontConfig = {
    default :: String
  , kannada :: String
  , type :: String
}

type LoaderConfig = {
  color :: String
}

type Miscellaneous = {
  otpRegex :: String
, termsLink :: String
, privacyLink :: String
}

type NavigationConfig = {
  android :: NavigationAppConfig
, ios :: NavigationAppConfig
}

type NavigationAppConfig = {
  query :: String
, packageName :: String
, walkQuery :: String
}

type GenericHeaderConfig = {
  backArrow :: String
}

type AppDatas = {
    link :: String
  , supportMail :: String
  , name :: String
}