module Components.RideActionModal.View where

import Common.Types.App

import Components.RideActionModal.Controller (Action(..), Config)
import Effect (Effect)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (countDown)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, not, pure, show, unit, ($), (/=), (<>), (&&))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), alpha, background, clickable, color, ellipsize, fontStyle, gravity, height, imageUrl, imageView, lineHeight, linearLayout, margin, maxLines, onClick, orientation, padding, relativeLayout, scrollView, singleLine, stroke, text, textSize, textView, visibility, width, imageWithFallback)
import PrestoDOM.Properties (cornerRadii, cornerRadius)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color

view :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push config = 
  linearLayout 
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , clickable true
    ][linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER
      , margin $ MarginBottom 20
      ][ locationTrackButton push config
        , openGoogleMap push config 
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , cornerRadii $ Corners 25.0 true true false false
      , orientation VERTICAL
      , background Color.white900
      , padding (PaddingTop 8)
      , gravity CENTER
      , stroke ("1," <> Color.grey800)
      ][  rideActionDataView push config
        , linearLayout
          [ width MATCH_PARENT
          , height (V 1)
          , background Color.lightGrey
          , margin (MarginTop 24)
          ][]
        , if config.startRideActive then 
            startRide push config
          else --swipe button
            endRide push config
        , cancelRide push config
      ]
    ]

locationTrackButton :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM ( Effect Unit) w
locationTrackButton push config = 
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , margin $ MarginLeft 20
  ][  linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , gravity CENTER
      , background Color.white900
      , stroke $ "1,"<> Color.black500
      , cornerRadius 25.0
      , onClick push (const $ LocationTracking)
      ][  imageView
          [ imageWithFallback "ny_ic_location_track,https://assets.juspay.in/nammayatri/images/common/ny_ic_location_track.png"
          , height $ V 25
          , width $ V 25
          , margin (Margin 10 10 10 10)
          ]
      ]
  ]
  

openGoogleMap :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
openGoogleMap push config =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity RIGHT
  ][  linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , background Color.blue900
      , padding $ Padding 20 15 20 15
      , margin $ MarginRight 16
      , cornerRadius 30.0
      , gravity CENTER
      , orientation HORIZONTAL
      , onClick push (const OnNavigate)
      ][ textView (
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text (getString OPEN_GOOGLE_MAPS)
          , gravity CENTER
          , color Color.white900
          ] <> FontStyle.body1 TypoGraphy
          )
        , imageView
          [ width $ V 20
          , height $ V 20
          , margin (MarginLeft 6)
          , imageWithFallback "ny_ic_navigation,https://assets.juspay.in/nammayatri/images/driver/ny_ic_navigation.png"
          ]
      ]
  ] 

rideActionDataView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
rideActionDataView push config = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding (PaddingHorizontal 16 16)
    , gravity CENTER
    ]([  linearLayout
          [ width (V 34)
          , height (V 4)
          , cornerRadius 4.0
          , background Color.black500
          ][]
      , customerNameView push config
      , estimatedFareView push config
      ] <> if config.startRideActive then [sourceAndDestinationView push config] else [destinationView config push])

callCustomer :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
callCustomer push config = 
  relativeLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , visibility if config.startRideActive then VISIBLE else GONE
  , gravity RIGHT
  ][  linearLayout
      [ width $ V 56
      , height $ V if config.isDriverArrived then 56 else 40
      , background Color.darkMint
      , gravity CENTER
      , cornerRadius 28.0
      , padding $ PaddingHorizontal 5 5
      , alpha 0.1
      ][]
    , linearLayout
      [ width $ V 56
      , height $ V if config.isDriverArrived then 56 else 40
      , gravity CENTER
      , onClick push (const CallCustomer)
      ][  imageView
          [ width $ V 15
          , height $ V 15
          , imageWithFallback "ny_ic_call,https://assets.juspay.in/nammayatri/images/common/ny_ic_call.png"
          ]
        ]
    ]

totalDistance :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
totalDistance push config = 
  linearLayout 
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity RIGHT
  , visibility if (not config.startRideActive && config.totalDistance /= "0.0") then VISIBLE else GONE
  ][  linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , stroke ("1," <> Color.grey800)
      , gravity CENTER
      , cornerRadius 5.0
      ][  textView (
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text config.totalDistance
          , color Color.black800
          , margin $ Margin 12 10 12 10
          ] <> FontStyle.subHeading1 TypoGraphy
          )
      ]
  ]

sourceAndDestinationView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
sourceAndDestinationView push config = 
  relativeLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ MarginTop 24
    ][  sourceDestinationImageView config
      , sourceDestinationTextView push config
      ]

startRide :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
startRide push config = 
  linearLayout
  [ width MATCH_PARENT
  , height (V 50)
  , background Color.darkMint
  , cornerRadius 8.0
  , margin $ Margin 16 16 16 0
  , gravity CENTER
  , onClick push (const $ StartRide)
  ][  textView (
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text (getString START_RIDE)
      , color Color.white900
      ] <> FontStyle.subHeading1 TypoGraphy
      )
  ]

endRide :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
endRide push config = 
  linearLayout
  [ width MATCH_PARENT
  , height (V 50)
  , background Color.red
  , cornerRadius 8.0
  , margin $ Margin 16 16 16 16
  , gravity CENTER
  , onClick push (const $ EndRide)
  ][  textView (
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text (getString END_RIDE)
      , color Color.white900
      ] <> FontStyle.subHeading1 TypoGraphy
      )
  ]

cancelRide :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
cancelRide push config = 
  linearLayout 
  [ width WRAP_CONTENT
  , height (V 34)
  , gravity CENTER
  , visibility if config.startRideActive then VISIBLE else GONE
  , padding $ Padding 16 8 16 8
  , margin $ MarginVertical 16 16
  , onClick push (const CancelRide)
  ][  textView (
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text (getString CANCEL_RIDE)
      , color Color.red
      ] <> FontStyle.body1 TypoGraphy
      )
  ]

customerNameView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
customerNameView push config = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL  
  , gravity CENTER_VERTICAL
  , margin $ MarginVertical 16 20
  ][  linearLayout
      [ height WRAP_CONTENT
      , width  WRAP_CONTENT
      , orientation VERTICAL
      , gravity START
      ][  textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT 
          , text $ getTitle config
          , color Color.greyTextColor
          , ellipsize true
          , singleLine false
          , margin (MarginBottom 8)
          ] <> FontStyle.subHeading2 TypoGraphy
        , arrivedButtonView push config
        ]
    , callCustomer push config
    , totalDistance push config
    ]

estimatedFareView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
estimatedFareView push config = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ PaddingVertical 6 10
    , background Color.grey700
    , cornerRadius 8.0
    , gravity CENTER_HORIZONTAL
    , orientation HORIZONTAL
    ][  textView 
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text (getString ESTIMATED_RIDE_FARE)
        , color Color.black900
        , fontStyle $ FontStyle.medium TypoGraphy
        , textSize FontSize.a_18
        , lineHeight "24"
        ]
      , textView
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text $ "₹ " <> (show config.estimatedRideFare)
        , color Color.black900
        , textSize FontSize.a_22
        , lineHeight "28"
        , fontStyle $ FontStyle.semiBold TypoGraphy
        ]
      ]

sourceDestinationImageView :: forall w . Config -> PrestoDOM (Effect Unit) w
sourceDestinationImageView  config = 
  linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginTop 8
    ][ imageView
        [ height $ V 21
        , width $ V 17
        , imageWithFallback "ny_ic_source_dot,https://assets.juspay.in/nammayatri/images/common/ny_ic_source_dot.png"
        ]
      , lineImageView 36
      , if  config.totalDistance /= "0.0" then 
          linearLayout
          [ width WRAP_CONTENT
          , height MATCH_PARENT
          , stroke $ "1," <> Color.grey900
          , gravity CENTER
          , cornerRadius 4.0
          , background Color.grey700
          ][  textView $
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text config.totalDistance
              , color Color.black800
              , padding $ Padding 6 4 6 4
              ] <> FontStyle.body1 TypoGraphy
          ]
          else 
            lineImageView 26
      , lineImageView 18
      , imageView
        [ height $ V 14
        , width $ V 14
        , imageWithFallback "ny_ic_destination,https://assets.juspay.in/nammayatri/images/driver/ny_ic_destination.png"
        ]
      ]


sourceDestinationTextView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
sourceDestinationTextView push config =
  linearLayout 
    [ width WRAP_CONTENT 
    , orientation VERTICAL 
    , height WRAP_CONTENT
    , margin (MarginLeft 25)
    ][  textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text config.sourceAddress.titleText
        , color Color.black800
        , ellipsize true
        , singleLine true
        ] <> FontStyle.subHeading1 TypoGraphy
      , textView $ 
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text config.sourceAddress.detailText
        , color Color.black650
        , margin (MarginBottom 58)
        , ellipsize true
        , singleLine true
        ] <> FontStyle.body1 TypoGraphy
      , destAddressTextView config push
      ]   

arrivedButtonView :: forall w . (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
arrivedButtonView push config = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , orientation HORIZONTAL
  , cornerRadius 24.0
  , gravity CENTER_VERTICAL
  , stroke $ if config.notifiedCustomer then "0," <> Color.blackLessTrans  else "1," <> Color.blue900 
  , background if config.notifiedCustomer then Color.grey700 else Color.white900
  , padding (Padding 10 7 12 7)
  , onClick (\action -> do 
      if config.notifiedCustomer then pure unit
        else do
          _ <- countDown config.buttonTimeOut config.id push ButtonTimer
          push action) (const NotifyCustomer)
  , visibility if config.isDriverArrived then VISIBLE else GONE
  ][  imageView
      [ width $ V 20
      , height $ V 20
      , imageWithFallback if config.notifiedCustomer then "ny_ic_tick_grey,https://assets.juspay.in/nammayatri/images/driver/ny_ic_tick_grey.png" else "ny_ic_hand_wave,https://assets.juspay.in/nammayatri/images/driver/ny_ic_hand_wave.png"
      , margin $ MarginRight 4
      ]
    , textView $
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , text if config.notifiedCustomer then (getString CUSTOMER_NOTIFIED) else (getString I_ARRIVED)
      , color if config.notifiedCustomer then Color.black800 else Color.blue900
      , gravity CENTER
      ]<> FontStyle.body1 TypoGraphy
    ]

destinationView :: forall w . Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
destinationView config push = 
  linearLayout 
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , margin (MarginTop 20)
  ][  imageView
      [ height $ V 24
      , width $ V 24
      , imageWithFallback "ny_ic_loc_red,https://assets.juspay.in/nammayatri/images/common/ny_ic_loc_red.png"
      , margin $ Margin 0 3 8 0         
      ]
    , destAddressTextView config push
  ]

lineImageView :: forall w . Int -> PrestoDOM (Effect Unit) w
lineImageView val = 
  imageView
    [ height $ V val
    , width $ V 15
    , imageUrl "ic_line"
    , margin $ MarginLeft 7
    ]

destAddressTextView :: forall w . Config -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
destAddressTextView config push= 
  linearLayout 
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][  textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text config.destinationAddress.titleText
        , color Color.black800
        , ellipsize true
        , singleLine true
        ] <> FontStyle.subHeading1 TypoGraphy
      , textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text config.destinationAddress.detailText
        , color Color.black650
        , ellipsize true
        , singleLine true
        ]<> FontStyle.body1 TypoGraphy
      ]

getTitle :: Config -> String
getTitle config = case config.startRideActive of
  false -> (getString YOU_ARE_ON_A_RIDE)
  true  -> case config.isDriverArrived, config.notifiedCustomer of 
    false, false  -> (config.customerName <> " " <> (getString IS_WAITING_FOR_YOU)) 
    true, _       -> (getString YOU_ARE_AT_PICKUP)
    false,true    -> case (getValueToLocalStore LANGUAGE_KEY) of 
      "TA_IN" -> config.customerName <> (getString WAITING_FOR_CUSTOMER)
      "HI_IN" -> "आप" <> config.customerName <> "की प्रतीक्षा कर रहे हैं"
      _       -> (getString WAITING_FOR_CUSTOMER) <> config.customerName