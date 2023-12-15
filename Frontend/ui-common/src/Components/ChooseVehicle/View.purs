module Components.ChooseVehicle.View where

import Common.Types.App

import Components.ChooseVehicle.Controller (Action(..), Config, SearchType(..))
import Effect (Effect)
import Font.Style as FontStyle
import Prelude (Unit, const, ($), (<>), (==), (&&), not, pure, unit, (+), show, (||))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, relativeLayout, stroke, text, textView, visibility, weight, width, id, afterRender)
import Common.Styles.Colors as Color
import Engineering.Helpers.Commons as EHC
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import Debug
import MerchantConfig.Utils (Merchant(..), getMerchant)

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  relativeLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , background if config.index == config.activeIndex && (not config.isCheckBox) then Color.blue600 else Color.white900
  , cornerRadius 6.0
  , id $ EHC.getNewIDWithTag config.id
  , stroke $ case config.isCheckBox of
          false -> if config.index == config.activeIndex then "1," <> Color.blue800 else "1," <> Color.white900
          true -> "1," <> Color.grey900
  , margin $ MarginHorizontal 16 16
  , padding $ Padding 8 16 12 16
  , clickable config.isEnabled
  , onClick push $ const $ OnSelect config
  , afterRender push (const NoAction)
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , afterRender push (const NoAction)
      ][ imageView
          [ imageWithFallback config.vehicleImage
          , height $ V 48
          , width $ V 60
          ]
        , linearLayout
          [ width $ if config.isBookingOption then MATCH_PARENT else WRAP_CONTENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          ][ vehicleDetailsView push config
           , if config.isCheckBox || config.searchResultType == QUOTES then capacityView push config else priceDetailsView push config
          ]
        , linearLayout
          [ height WRAP_CONTENT
          , weight 1.0
          ][]
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity RIGHT
      , afterRender push (const NoAction)
      , visibility if config.isCheckBox || config.searchResultType == QUOTES then VISIBLE else GONE
      ][if config.isCheckBox then checkBox push config else priceDetailsView push config]

  ]

vehicleDetailsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
vehicleDetailsView push config =
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation $ if config.isBookingOption then VERTICAL else HORIZONTAL
    , padding $ PaddingLeft 8
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text $ case (getMerchant FunctionCall) of
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
          , color Color.black800
          ]
        <> FontStyle.subHeading1 TypoGraphy
    , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , visibility if config.isCheckBox || config.searchResultType == QUOTES then GONE else VISIBLE
      ][ linearLayout
          [ height $ V 4
          , width $ V 4
          , cornerRadius 2.5
          , background Color.black600
          , margin (Margin 5 12 0 0)
          , visibility if config.isBookingOption then GONE else VISIBLE
          ][]
        , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , margin $ if config.isBookingOption then (Margin 0 0 0 0) else (Margin 5 5 0 0)
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

    ]

priceDetailsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
priceDetailsView push config = do 
  let basePrice = config.basePrice 
  linearLayout
    [ height MATCH_PARENT
    , width $ if config.isBookingOption then MATCH_PARENT else WRAP_CONTENT
    , orientation HORIZONTAL
    , padding $ PaddingLeft 8
    , gravity $ if config.isBookingOption then RIGHT else LEFT
    ]
    [ textView
        $ [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text config.price
          , color Color.black800
          ]
        <> FontStyle.h3 TypoGraphy
      , imageView
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_info_grey"
        , width $ V 14
        , height $ V 14
        , margin $ Margin 4 6 0 0
        , visibility if config.showInfo then VISIBLE else GONE
        , onClick push $ const $ ShowRateCard config
        , visibility if config.isBookingOption then GONE else VISIBLE
        ]
    ]

checkBox :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
checkBox push config = 
  relativeLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , margin $ MarginTop 8
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
      , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_check_box"
      , visibility if config.isSelected then VISIBLE else GONE
      ]
  ]

capacityView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
capacityView push config = 
  textView $
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text config.capacity
  , margin $ Margin 8 4 0 0
  ] <> FontStyle.body3 TypoGraphy