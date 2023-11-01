module Components.ChooseVehicle.View where

import Common.Types.App

import Components.ChooseVehicle.Controller (Action(..), Config, SearchType(..))
import Effect (Effect)
import Font.Style as FontStyle
import Prelude (Unit, const, ($), (<>), (==), (&&), not, pure, unit, (+), show, (||), (<), (>), (/=), bind, when, void, (*))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, relativeLayout, stroke, text, textView, visibility, weight, width, id, afterRender, alpha)
import Common.Styles.Colors as Color
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Engineering.Helpers.Commons as EHC
import Data.Array (concat)
import Debug
import MerchantConfig.Utils (Merchant(..), getMerchant)

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config =
  linearLayout
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
  , orientation VERTICAL
  , afterRender 
    ( \action -> do
        _ <- pure $ spy "shrey00" config
        pure unit
    )
    (const NoAction)
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , afterRender push (const NoAction)
      ][ imageView
          [ imageWithFallback $ config.vehicleImage <> "," <> (getCommonAssetStoreLink FunctionCall) <> config.vehicleImage <> ".png"
          , height $ V 48
          , width $ V 60
          ]
        , linearLayout
          [ width $ if config.isBookingOption then MATCH_PARENT else WRAP_CONTENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
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
      , visibility if config.isCheckBox then VISIBLE else GONE
      ][checkBox push config]
    , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity RIGHT
          , afterRender push (const NoAction)
          , visibility if config.vehicleVariant /= "AUTO_RICKSHAW" then GONE else VISIBLE -- "BUS"
          -- , visibility if config.vehicleVariant /= "BUS" then GONE else VISIBLE -- JAYPAL: "BUS"
          ][ quantitySelectionView push config ]
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
                                      "BUS" -> "Bus"
                                      _ -> "Non-AC Taxi"
          , color Color.black800
          , visibility if config.searchResultType == QUOTES then VISIBLE else GONE
          ]
        <> FontStyle.subHeading1 TypoGraphy
    , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , visibility if config.isCheckBox || config.searchResultType == QUOTES then GONE else VISIBLE
      ][ textView
          $ [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text
                $ case config.vehicleVariant of
                    "AUTO_RICKSHAW" -> "Auto Rickshaw"
                    "TAXI" -> "Non-AC Taxi"
                    "TAXI_PLUS" -> "AC Taxi"
                    "SEDAN" -> "Sedan"
                    "SUV" -> "SUV"
                    "HATCHBACK" -> "Hatchback"
                    "BUS" -> "Bus"
                    _ -> "Non-AC Taxi"
            , color Color.black800
            ]
          <> FontStyle.subHeading1 TypoGraphy
        , linearLayout
          [ height $ V 4
          , width $ V 4
          , cornerRadius 2.5
          , background Color.black600
          , margin (Margin 5 12 0 0)
          , orientation HORIZONTAL
          , visibility if config.isBookingOption then VISIBLE else GONE
          ][]
        , linearLayout 
            [ width $ V 61
            , height $ V 19
            , cornerRadius 4.0
            , gravity CENTER
            , margin (Margin 2 6 0 0)
            , background Color.blueMagentaOpacity10
            ][ textView
                $ [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text config.id
                  , color Color.blueMagenta
                  ]
                <> FontStyle.body15 TypoGraphy
            ]
      ]
    , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      -- , orientation HORIZONTAL
      , visibility if config.isSelected then GONE else VISIBLE
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
    , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , visibility if config.isSelected then VISIBLE else GONE
      ][ imageView
          [ imageWithFallback $ "ny_ic_walk," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_walk.png"
          , height $ V 16
          , width $ V 16
          ]
        , imageView
          [ imageWithFallback $ "ny_ic_chevron_right," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_right.png"
          , height $ V 16
          , width $ V 16
          ]
        , imageView
          [ imageWithFallback $ "ny_ic_bus_vector," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_bus_vector.png"
          , height $ V 16
          , width $ V 16
          ]
        , imageView
          [ imageWithFallback $ "ny_ic_chevron_right," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_.png"
          , height $ V 16
          , width $ V 16
          ]
        , imageView
          [ imageWithFallback $ "ny_ic_walk," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_walk.png"
          , height $ V 16
          , width $ V 16
          ]
        ]

    ]

priceDetailsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
priceDetailsView push config = do 
  let totalPrice = config.basePrice * config.quantity
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
        [ imageWithFallback "ny_ic_info_grey,https://assets.juspay.in/nammayatri/images/user/ny_ic_information_grey.png"
        , width $ V 14
        , height $ V 14
        , margin $ Margin 4 6 0 0
        , visibility if config.showInfo then VISIBLE else GONE
        , onClick push $ const $ ShowRateCard config.vehicleVariant
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
      , imageWithFallback $ "ny_ic_check_box," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_check_box.png"
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

  
quantitySelectionView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
quantitySelectionView push config = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , margin $ Margin 8 0 0 0
  , gravity CENTER
  , orientation VERTICAL
  , visibility if config.isSelected then VISIBLE else GONE
  ][ linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER
    , background Color.white900
    , orientation HORIZONTAL
    , margin $ Margin 4 4 4 4
    ][  imageView
          [ imageWithFallback $ "ny_ic_stepper_down," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_stepper_down.png"
          , height $ V 36
          , width $ V 72
          , gravity CENTER
          , margin (Margin 10 6 0 6)
          , cornerRadius 4.0
          , onClick push $ const $ ChangeTicketQuantity false
          ]
        , textView $
          [ text $ "Tickets: " <> show config.quantity
          , height $ V 36
          , width $ V 168
          , gravity CENTER
          , color Color.black900
          ]<> FontStyle.h3 TypoGraphy
        , imageView
          [ imageWithFallback $ "ny_ic_stepper_up," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_stepper_up.png"
          , height $ V 36
          , width $ V 72
          , gravity CENTER
          , margin (Margin 0 6 10 6)
          , cornerRadius 4.0
          , onClick push $ const $ ChangeTicketQuantity true
          ]
      ]
    , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , gravity CENTER
      , orientation VERTICAL
      ][ textView $
          [ text "You can book a maximum of 10 tickets"
          , height $ V 16
          , color if config.quantity /= 10 then Color.transparent else Color.red900
          , gravity CENTER
          ] <> FontStyle.body3 TypoGraphy
        , textView $ 
          [ text "View Route Details"
          , width $ V 107
          , height $ V 15
          , color Color.blue900
          , margin $ Margin 0 0 0 8
          , gravity CENTER
          , clickable config.isEnabled
          , onClick push $ const ShowRouteInfo
          ] <> FontStyle.tags TypoGraphy
      ]
  ]
  