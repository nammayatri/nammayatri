module Components.ChooseVehicle.View where

import Common.Types.App

import Components.ChooseVehicle.Controller (Action(..), Config, SearchType(..))
import Effect (Effect)
import Font.Style as FontStyle
import Prelude (Unit, const, ($), (<>), (==), (&&), not, pure, unit, (+), show, (||))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, relativeLayout, stroke, text, textView, visibility, weight, width, id, afterRender, layoutGravity, singleLine, ellipsize)
import Common.Styles.Colors as Color
import Engineering.Helpers.Commons as EHC
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Debug
import MerchantConfig.Utils (Merchant(..), getMerchant)
import Mobility.Prelude (boolToVisibility)
import ConfigProvider

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
                        "SEDAN" -> "Sedan"
                        "SUV" -> "SUV"
                        "HATCHBACK" -> "Hatchback"
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
    ][ vehicleInfoView "ic_user_filled" config.capacity]

vehicleInfoView :: forall w. String -> String -> PrestoDOM (Effect Unit) w
vehicleInfoView imageName description = do
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
    ]