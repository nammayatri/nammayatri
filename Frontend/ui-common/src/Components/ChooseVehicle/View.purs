module Components.ChooseVehicle.View where

import Common.Types.App

import Components.ChooseVehicle.Controller (Action(..), Config)
import Effect (Effect)
import Font.Style as FontStyle
import Prelude (Unit, const, ($), (<>), (==), (&&), not, pure, unit, (+), show)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, relativeLayout, stroke, text, textView, visibility, weight, width)
import Common.Styles.Colors as Color
import Engineering.Helpers.Commons (toInt)
import Merchant.Utils (getValueFromConfig)
import Data.Lens((^.))

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background if config.index == config.activeIndex && (not config.isCheckBox) then Color.blue600 else Color.white900
    , orientation HORIZONTAL
    , gravity CENTER
    , cornerRadius 6.0
    , stroke
        $ case config.isCheckBox of
            false -> if config.index == config.activeIndex then "1," <> Color.blue800 else "1," <> Color.white900
            true -> "1," <> Color.grey900
    , margin $ MarginHorizontal 16 16
    , padding $ Padding 8 16 12 16
    , clickable config.isEnabled
    , onClick push $ const $ OnSelect config
    ]
    [ imageView
        [ imageWithFallback config.vehicleImage
        , height $ V 48
        , width $ V 60
        ]
    , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][
        vehicleDetailsView push config
      , priceDetailsView push config
      ]
    , linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        ]
        []
    ]

vehicleDetailsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
vehicleDetailsView push config =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ PaddingLeft 8
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text
              $ case config.vehicleVariant of
                  "AUTO_RICKSHAW" -> "Auto Rickshaw"
                  "TAXI" -> "Non AC Taxi"
                  "TAXI_PLUS" -> "AC Taxi"
                  "SEDAN" -> "Sedan"
                  "SUV" -> "SUV"
                  "HATCHBACK" -> "Hatchback"
                  _ -> "Non AC Taxi"
          , color Color.black800
          ]
        <> FontStyle.subHeading1 TypoGraphy
    , linearLayout
        [ height $ V 4
        , width $ V 4
        , cornerRadius 2.5
        , background Color.black600
        , margin (Margin 5 12 0 0)
        ]
        []
    , linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , margin (Margin 5 5 0 0)
        ]
        [ textView
            $ [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text config.capacity
              , color Color.black700
              ]
            <> FontStyle.body3 TypoGraphy
        ]
    ]

priceDetailsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
priceDetailsView push config = do 
  let basePrice = config.base_price 
  linearLayout
    [ height MATCH_PARENT
    , width WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ PaddingLeft 8
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ if config.price == config.maxPrice then "₹" <> show ((toInt config.price) + basePrice) else "₹" <> show ((toInt config.price) + basePrice) <> " - " <> "₹" <> show((toInt config.maxPrice) + basePrice)
          , color Color.black800
          , visibility if config.isCheckBox then GONE else VISIBLE
          ]
        <> FontStyle.subHeading1 TypoGraphy
      , imageView
        [ imageWithFallback "ny_ic_info_grey,https://assets.juspay.in/nammayatri/images/user/ny_ic_information_grey.png"
        , width $ V 14
        , height $ V 14
        , margin $ Margin 4 6 0 0
        , onClick push $ const $ ShowRateCard config.vehicleVariant
        ]
    , relativeLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , margin $ MarginTop 8
        , visibility if config.isCheckBox then VISIBLE else GONE
        ]
        [ linearLayout
            [ height (V 18)
            , width (V 18)
            , stroke ("1," <> Color.black)
            , cornerRadius 2.0
            ]
            []
        , imageView
            [ width (V 18)
            , height (V 18)
            , imageWithFallback "ny_ic_check_box,https://assets.juspay.in/nammayatri/images/driver/ny_ic_check_box.png"
            , visibility if config.isSelected then VISIBLE else GONE
            ]
        ]
    ]

