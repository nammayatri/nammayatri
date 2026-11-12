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
      , twoWheelerQuery : "google.navigation:q=%f,%f&mode=l"
      }
    , ios : {
        query : "http://maps.google.com///?saddr=&daddr=%@,%@&dirflg=d"
      , walkQuery : "http://maps.google.com///?saddr=&daddr=%@,%@&dirflg=w"
      , directionQuery : "http://maps.google.com///?saddr=&daddr=%@,%@&dirflg=d"
      , twoWheelerQuery : "http://maps.google.com///?saddr=&daddr=%@,%@&dirflg=l"
      , packageName : ""
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