module Components.ChooseVehicle.View where

import Common.Types.App

import Components.ChooseVehicle.Controller (Action(..), Config, SearchType(..))
import Effect (Effect)
import Font.Style as FontStyle
import Prelude (Unit, const, ($), (<>), (==), (&&), not, pure, unit, (+), show, (||), (<), (>), (/=), bind, when, void, (*))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), background, clickable, color, cornerRadius, gravity, height, imageView, imageWithFallback, linearLayout, margin, onClick, orientation, padding, relativeLayout, stroke, text, textView, visibility, weight, width, id, afterRender, alpha)
import Common.Styles.Colors as Color
import Engineering.Helpers.Commons as EHC
import Helpers.Utils (fetchImage, FetchImageFrom(..), getVariantRideType)
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
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , afterRender push (const NoAction)
      ][ imageView
          [ imageWithFallback $  fetchImage FF_ASSET config.vehicleImage
          , height $ V 48
          , width $ V 60
          ]
        , linearLayout
          [ width $ if config.isBookingOption then MATCH_PARENT else WRAP_CONTENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER_VERTICAL
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
          , visibility if config.vehicleVariant /= "BUS" then GONE else VISIBLE
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
          , text $ getVariantRideType config.vehicleVariant
          , color Color.black800
          , visibility if config.searchResultType == QUOTES then VISIBLE else GONE
          ]
        <> FontStyle.subHeading1 TypoGraphy
    , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER
      , visibility if config.isCheckBox || config.searchResultType == QUOTES then GONE else VISIBLE
      ][ textView
          $ [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , text $ getVariantRideType config.vehicleVariant
            , color Color.black800
            ]
          <> FontStyle.subHeading1 TypoGraphy
        , linearLayout
          [ height $ V 4
          , width $ V 4
          , cornerRadius 2.5
          , background Color.black600
          , margin $ Margin 4 0 4 0
          , visibility if config.isBookingOption then VISIBLE else GONE
          ][]
        , linearLayout 
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , cornerRadius 4.0
            , gravity CENTER
            , background Color.purple100
            , padding $ Padding 8 2 8 2
            , visibility if config.vehicleVariant == "BUS" then VISIBLE else GONE
            ][ textView
                $ [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text config.id
                  , color Color.blueMagenta
                  , gravity CENTER
                  ]
                <> FontStyle.body15 TypoGraphy
            ]
      ]
    , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
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
    , journeyIllustationView push config
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
        [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_info_grey"
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

  
quantitySelectionView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
quantitySelectionView push config = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ Margin 0 16 0 0
  , gravity CENTER
  , orientation VERTICAL
  , visibility if config.isSelected then VISIBLE else GONE
  ][ linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity CENTER
    , background Color.white900
    , orientation HORIZONTAL
    , padding $ Padding 4 4 4 4
    ][  imageView
          [ imageWithFallback $  fetchImage FF_ASSET "ny_ic_stepper_down"
          , height $ V 36
          , width $ V 72
          , gravity LEFT
          , margin (Margin 10 6 0 6)
          , cornerRadius 4.0
          , onClick push $ const $ ChangeTicketQuantity false
          ]
        , textView $
          [ text $ "Tickets: " <> show config.quantity
          , height $ V 36
          , width WRAP_CONTENT
          , gravity CENTER
          , weight 1.0
          , color Color.black900
          ]<> FontStyle.h3 TypoGraphy
        , imageView
          [ imageWithFallback $  fetchImage FF_ASSET "ny_ic_stepper_up"
          , height $ V 36
          , width $ V 72
          , gravity RIGHT
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
  
journeyIllustationView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
journeyIllustationView push config =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , visibility if (config.isSelected && config.vehicleVariant == "BUS") then VISIBLE else GONE
  ]
  [ imageView
    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_walk"
    , height $ V 16
    , width $ V 16
    ]
  , imageView
    [ imageWithFallback $  fetchImage FF_ASSET "ny_ic_chevron_right"
    , height $ V 16
    , width $ V 16
    ]
  , imageView
    [ imageWithFallback $  fetchImage FF_ASSET "ny_ic_bus_vector"
    , height $ V 16
    , width $ V 16
    ]
  , imageView
    [ imageWithFallback $  fetchImage FF_ASSET "ny_ic_chevron_right"
    , height $ V 16
    , width $ V 16
    ]
  , imageView
    [ imageWithFallback $  fetchImage FF_ASSET "ny_ic_walk"
    , height $ V 16
    , width $ V 16
    ]
  ]