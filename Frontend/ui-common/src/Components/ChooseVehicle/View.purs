module Components.ChooseVehicle.View where

import Common.Types.App
import Components.ChooseVehicle.Controller (Action(..), Config)
import Effect (Effect)
import Font.Style as FontStyle
import Prelude (Unit, const, ($), (<>), (==))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), PrestoDOM, Visibility(..), Padding(..), background, clickable, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, stroke, text, textView, visibility, weight, width, padding)
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background if config.index == config.activeIndex then Color.blue600 else Color.white900
    , orientation HORIZONTAL
    , gravity CENTER
    , cornerRadius 6.0
    , stroke $ if config.index == config.activeIndex then "1," <> Color.blue800 else "1," <> Color.white900
    , margin $ MarginHorizontal 16 16
    , padding $ Padding 8 5 8 5
    , clickable config.isEnabled
    , onClick push $ const $ OnSelect config
    ]
    [ imageView
        [ imageWithFallback config.vehicleImage
        , height $ V 48
        , width $ V 60
        , margin $ Margin 8 12 8 12
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
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ case config.vehicleVariant of
              "AUTO_RICKSHAW" -> "Auto Rickshaw"
              "TAXI" -> "Non AC Taxi"
              "TAXI_PLUS" -> "AC Taxi"
              "SEDAN" -> "Sedan"
              "SUV" -> "Suv"
              "HATCHBACK" -> "Hatch Back"
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
              , text config.vehicleType
              , color Color.black700
              ]
            <> FontStyle.body3 TypoGraphy
        , linearLayout
            [ height $ V 4
            , width $ V 4
            , cornerRadius 2.0
            , background Color.black600
            , margin (Margin 6 6 6 0)
            ]
            []
        , textView
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
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation HORIZONTAL
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ "₹" <> config.price
          , color Color.black800
          , visibility if config.isCheckBox then GONE else VISIBLE
          ]
        <> FontStyle.subHeading1 TypoGraphy
    -- , linearLayout
    --     [ height config.imageLayoutHeight
    --     , width  config.imageLayoutWidth
    --     , stroke $ fromMaybe ("0," <> Color.greySmoke) config.imageStroke
    --     , clickable config.secondaryImage.clickable
    --     , onClick push (const $ OnImageClick)
    --     , padding config.secondaryImage.padding
    --     , margin config.secondaryImage.margin
    --     ][  imageView
    --       [ imageWithFallback config.secondaryImage.imageUrl
    --       , height config.secondaryImage.height
    --       , width config.secondaryImage.width
    --       , visibility config.secondaryImage.visibility
    --       ]  
    --     ]
    ]
