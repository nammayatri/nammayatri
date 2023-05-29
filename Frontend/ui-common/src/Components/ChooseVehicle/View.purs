module Components.ChooseVehicle.View where

import Common.Types.App

import Components.ChooseVehicle.Controller (Action(..), Config)
import Effect (Effect)
import Font.Style as FontStyle
import Prelude (Unit, const, ($), (<>), (==), (&&), not)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, relativeLayout, stroke, text, textView, visibility, weight, width)
import Styles.Colors as Color
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)

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
    , vehicleDetailsView push config
    , linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        ]
        []
    , priceDetailsView push config
    ]

vehicleDetailsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
vehicleDetailsView push config =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
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
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
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
priceDetailsView push config =
  relativeLayout
    [ height MATCH_PARENT
    , width WRAP_CONTENT
    , orientation HORIZONTAL
    , gravity TOP_VERTICAL
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ "₹" <> config.price
          , color Color.black800
          , visibility if config.isCheckBox then GONE else VISIBLE
          ]
        <> FontStyle.subHeading1 TypoGraphy
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
            , imageWithFallback $ "ny_ic_check_box," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_check_box.png"
            , visibility if config.isSelected then VISIBLE else GONE
            ]
        ]
    ]
