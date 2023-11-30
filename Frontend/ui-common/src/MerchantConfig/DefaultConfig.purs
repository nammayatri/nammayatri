module Common.DefaultConfig where

import Common.Types.Config

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
