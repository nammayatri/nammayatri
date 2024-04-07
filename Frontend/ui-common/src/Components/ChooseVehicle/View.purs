module Components.ChooseVehicle.View where

import Common.Types.App
import ConfigProvider
import Debug

import Common.Styles.Colors as Color
import Components.ChooseVehicle.Controller (Action(..), Config, SearchType(..))
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Language.Types (STR(..))
import Language.Strings (getString)
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Mobility.Prelude (boolToVisibility)
import Data.String as DS
import Prelude (Unit, const, ($), (<>), (==), (&&), not, pure, unit, (+), show, (||) , (<), (>=))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, relativeLayout, stroke, text, textView, visibility, weight, width, id, afterRender, layoutGravity, singleLine, ellipsize)

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  let isActiveIndex = config.index == config.activeIndex
      stroke' = if config.isSingleEstimate then "0," <> Color.grey900 else if isActiveIndex then "2," <> Color.blue800 else "1," <> Color.white900
      background' = if isActiveIndex && (not config.isSingleEstimate) then Color.blue600 else Color.white900
      padding' = if config.isSingleEstimate then PaddingHorizontal 12 12 else Padding 8 16 12 16
  in 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , background background'
  , cornerRadius 6.0
  , id $ EHC.getNewIDWithTag config.id
  , stroke stroke'
  , margin $ config.layoutMargin
  , padding padding'
  , clickable config.isEnabled
  , onClick push $ const $ OnSelect config
  , afterRender push (const NoAction)
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , afterRender push (const NoAction)
      ][ linearLayout
          [ height $ V 48
          , width $ V 60
          ][imageView
            [ imageWithFallback config.vehicleImage
            , height $ V if config.vehicleVariant == "AUTO_RICKSHAW" then 45 else 48
            , width $ V 60
            ]
          ]
        , linearLayout
          [ weight 1.0
          , height WRAP_CONTENT
          , orientation VERTICAL
          , padding $ PaddingLeft 8
          ][ vehicleDetailsView push config
           , capacityView push config 
          ]
        , linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity RIGHT
          , afterRender push (const NoAction)
          ][priceDetailsView push config]
      ]
  ]

vehicleDetailsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
vehicleDetailsView push config =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation HORIZONTAL
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , singleLine true
          , ellipsize true
          , text $ getVehicleName config
          , color Color.black800
          ]
        <> FontStyle.body7 TypoGraphy
    ]
  where 
    getVehicleName :: Config -> String
    getVehicleName config = 
      case (getMerchant FunctionCall) of
        YATRISATHI -> case config.vehicleVariant of
                        "TAXI" -> "Non AC Taxi"
                        "SUV"  -> "AC SUV"
                        _      -> "AC Cab"
        _          -> case config.vehicleVariant of
                        "AUTO_RICKSHAW" -> "Auto Rickshaw"
                        "TAXI" -> "Non-AC Taxi"
                        "TAXI_PLUS" -> "AC Taxi"
                        "SEDAN" -> "Comfy" 
                        "SUV" -> "SUV"
                        "HATCHBACK" -> "Eco" 
                        _ -> "Non-AC Taxi"

priceDetailsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
priceDetailsView push config =
  let isActiveIndex = config.index == config.activeIndex
      infoIcon = if isActiveIndex && (not config.isSingleEstimate) then "ny_ic_info_blue_lg" else "ny_ic_info_grey"
  in
  linearLayout
    [ height MATCH_PARENT
    , width $  WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ PaddingLeft 8
    , gravity CENTER_VERTICAL
    , clickable isActiveIndex
    , onClick push $ const $ ShowRateCard config
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text config.price
          , color Color.black800
          ]
        <> FontStyle.body7 TypoGraphy
      , imageView
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET infoIcon
        , width $ V 15
        , height $ V 15
        , gravity CENTER_VERTICAL
        , margin $ MarginLeft 4
        , visibility $ boolToVisibility $ config.showInfo && isActiveIndex
        ]
    ]

capacityView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
capacityView push config = 
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , padding $ PaddingTop 5
    , orientation VERTICAL
    ][ vehicleInfoView "ic_user_filled" config.capacity config.vehicleVariant config.showAdditionalDesc config.vehicleShortDesc]

vehicleInfoView :: forall w. String -> String -> String -> Boolean -> String -> PrestoDOM (Effect Unit) w
vehicleInfoView imageName description vehicleVariant showAdditionalDesc shortDesc = let 
  vehicleDesc = if shortDesc == "" then getVariantDescription vehicleVariant else getVariantDescFromShortDesc shortDesc
  in
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER
    ][ imageView
        [ imageWithFallback $ fetchImage FF_ASSET imageName
        , width $ V 14
        , height $ V 14
        ]
      , textView
          $ [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text description
            , color Color.black700
            ]
          <> FontStyle.tags TypoGraphy
      , linearLayout[
          height $ WRAP_CONTENT
        , gravity CENTER
        , width MATCH_PARENT 
        , visibility $ boolToVisibility $ showAdditionalDesc 
        ][  textView
            [ height $ V 3 
            , width $ V 3 
            , cornerRadius 1.5 
            , margin $ MarginHorizontal 2 2
            , background Color.black700
            ]
          , imageView
            [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_blue_snow"
            , width $ V 12
            , visibility $ boolToVisibility vehicleDesc.airConditioned
            , height $ V 12
            ]
          , textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ vehicleDesc.text
              , color Color.black700
              ]
            <> FontStyle.tags TypoGraphy]
          
    ]

getVariantDescription :: String -> {text :: String, airConditioned :: Boolean}
getVariantDescription variant = 
  case variant of
    "AUTO_RICKSHAW" -> { text : "Commute friendly", airConditioned : false}
    "TAXI" ->{text : "Non-AC Taxi" , airConditioned : false}
    "TAXI_PLUS" ->{text : "AC Taxi" , airConditioned : false}
    "SEDAN" ->{text : "AC, Plush rides" , airConditioned : true}
    "SUV" ->{text : "AC , Spacious rides" , airConditioned : true}
    "HATCHBACK" ->{text : "Non-AC , Budget rides " , airConditioned : false}
    _ ->{text : "Non-AC Taxi" , airConditioned : false}
    
getVariantDescFromShortDesc :: String -> {text :: String, airConditioned :: Boolean}
getVariantDescFromShortDesc shortDesc = 
  let shortDesc' = DS.toLower shortDesc
  in {  text : shortDesc 
      , airConditioned : (DS.contains (DS.Pattern "ac") shortDesc') && (not $ DS.contains (DS.Pattern "non") shortDesc')}