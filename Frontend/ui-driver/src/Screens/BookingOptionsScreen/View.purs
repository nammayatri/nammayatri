module Screens.BookingOptionsScreen.View where

import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, const, ($), (<>))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, afterRender, alpha, background, color, cornerRadius, fontStyle, gravity, height, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, stroke, text, textSize, textView, weight, width)
import Screens.BookingOptionsScreen.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color

screen :: ST.BookingOptionsScreenState -> Screen Action ST.BookingOptionsScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "BookingDetailsScreen"
  , globalEvents : []
  , eval
  }

view :: forall w . (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push $ const BackPressed
    , afterRender push $ const AfterRender
    ][ headerLayout push state
     , defaultVehicleView push state
    ]

defaultVehicleView :: forall w . (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
defaultVehicleView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , cornerRadius 8.0
  , padding $ Padding 10 20 10 10
  , margin $ Margin 10 10 10 10
  , stroke $ "1," <> Color.grey900
  ][  vehicleDetailsView push state
    , vehicleLogoAndType push state
  ]

vehicleDetailsView :: forall w . (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
vehicleDetailsView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER_VERTICAL
  ][ linearLayout
      [ orientation VERTICAL
      , weight 1.0
      ][  customTV (getString YOUR_VEHICLE) FontSize.a_12 FontStyle.body3 Color.black650
        , customTV state.data.vehicleName FontSize.a_20 FontStyle.h3 Color.black800
      ]
    , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , cornerRadius 6.0
      , background Color.golden
      , padding $ Padding 3 3 3 3
      ][ textView $
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , padding $ Padding 5 3 5 3
          , text state.data.vehicleNumber
          , color Color.black800
          , fontStyle $ FontStyle.bold LanguageStyle
          , textSize FontSize.a_20
          , gravity CENTER
          , cornerRadius 3.0
          , stroke $ "2," <> Color.black800
          ]
      ]
  ]

vehicleLogoAndType :: forall w . (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
vehicleLogoAndType push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ MarginTop 26
  ][ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      ][ imageView
          [ imageWithFallback "ic_suv_ac,https://assets.juspay.in/nammayatri/images/driver/ic_suv_ac.png"
          , gravity LEFT
          , height $ V 48
          , width $ V 60
          ]
        , linearLayout
          [ height MATCH_PARENT
          , weight 1.0
          , orientation VERTICAL
          , gravity CENTER_VERTICAL
          , margin $ MarginLeft 7
          ][ customTV state.data.vehicleType FontSize.a_20 FontStyle.h3 Color.black800
           , customTV state.data.vehicleCapacity FontSize.a_12 FontStyle.body3 Color.black650
          ]
      ]

  ]

headerLayout :: forall w . (Action -> Effect Unit) -> ST.BookingOptionsScreenState -> PrestoDOM (Effect Unit) w
headerLayout push state = 
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , orientation VERTICAL
 ][ linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation HORIZONTAL
    , layoutGravity "center_vertical"
    , padding $ PaddingVertical 10 10
    ][ imageView
        [ width $ V 30
        , height $ V 30
        , imageWithFallback "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
        , gravity CENTER_VERTICAL
        , onClick push $ const BackPressed
        , padding $ Padding 2 2 2 2
        , margin $ MarginLeft 5
        ]
      , textView
        [ width WRAP_CONTENT
        , height MATCH_PARENT
        , text $ getString BOOKING_OPTIONS
        , textSize FontSize.a_19
        , margin $ MarginLeft 20
        , color Color.black
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , weight 1.0
        , gravity CENTER_VERTICAL
        , alpha 0.8
        ]
    ]
  ,  linearLayout
    [ width MATCH_PARENT
    , height $ V 1
    , background Color.greyLight
    ][]
 ]


customTV :: forall w .  String -> Int -> (LazyCheck ->  forall properties. (Array (Prop properties))) -> String -> PrestoDOM (Effect Unit) w
customTV text' textSize' fontStyle' color'= 
  textView $
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text text'
  , textSize textSize'
  , color color'
  ] <> fontStyle' TypoGraphy