module Common.DefaultConfig where

import Common.Types.Config
import Common.Types.App

defaultColors :: Colors
defaultColors = {
    black800 : "#454545"
  , black900 : "#2C2F3A"
  , green700 : "#268C6E"
  , red : "#E55454"
  }

countryCode ::  CountryCodeObj
countryCode = {
        countryName : "India" 
      , countryCode  : "+91" 
      , countryShortCode : "IN"
      }

defaultAppData :: AppDatas
defaultAppData =  {
    link : ""
  , supportMail :"support@nammayatri.in"
  , name : ""
  , website : "https://nammayatri.in"
  , logoLight : ""
  }

defaultPrimaryButtonConfig :: PrimaryButtonConfig
defaultPrimaryButtonConfig =  {
    isGradient : false
  , gradient : []
  , loaderUrl : "primary_button_loader.json"
  , forceToUseRemote : false
  }

defaultFontConfig :: FontConfig
defaultFontConfig = {
    default : "PlusJakartaSans"
  , kannada : "NotoSansKannada"
  , telugu : "NotoSansTelugu"
  , "type": "Assets"
  }

defaultLoaderConfig :: LoaderConfig
defaultLoaderConfig = {
  color : "#2C2F3A"
}
defaultGenericHeader :: GenericHeaderConfig
defaultGenericHeader = {
    backArrow : "ny_ic_chevron_left,https://assets.moving.tech/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_left.png"
  }

defaultNavigationAppConfig :: NavigationConfig
defaultNavigationAppConfig = {
      android : {  
        query : "google.navigation:q=%f,%f"
      , packageName : "com.google.android.apps.maps"
      , walkQuery : "google.navigation:q=%f,%f&mode=w"
      , directionQuery : "http://maps.google.com?saddr=&daddr=%f,%f&dirflg=d"
      , fallbackQuery : "google.navigation:q=%f,%f"
      }
    , ios : {
        query : "comgooglemaps://?saddr=&daddr=%@,%@"
      , walkQuery : "comgooglemaps://?saddr=&daddr=%@,%@&directionsmode=walking"
      , directionQuery : "comgooglemaps://?saddr=&daddr=%@,%@&directionsmode=walking"
      , packageName : "comgooglemaps://"
      , fallbackQuery : "http://maps.google.com///?saddr=&daddr=%@,%@&dirflg=d"
      }
    }

defaultBannerCarousel :: BannerCarousalConfig
defaultBannerCarousel = {
  autoScrollDelay : 5000.0,
  enableAutoScroll : true
}

defaultGeoJson :: GeoJson
defaultGeoJson = {
    type : "FeatureCollection"
  , features : []
}

defaultGeoJsonFeature :: GeoJsonFeature
defaultGeoJsonFeature = {
    type : "Feature"
  , properties : {
        name : ""
      , id : ""
      , defaultDriverExtra : 0
      , canQueueUpOnGate : false
    }
  , geometry : defaultGeoJsonGeometry
}

defaultGeoJsonGeometry :: GeoJsonGeometry
defaultGeoJsonGeometry = {
    type : "MultiPolygon"
  , coordinates : [[[[]]]]
}