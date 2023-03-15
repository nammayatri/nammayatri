{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.DriverInfoCard.View where

import Components.DriverInfoCard.Controller (Action(..), DriverInfoCardState)
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination as SourceToDestination
import Data.Array ((!!))
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split, length)
import Debug.Trace (spy)
import Effect (Effect)
import Engineering.Helpers.Commons (screenWidth, safeMarginBottom, os, flowRunner)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (secondsToHms)
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, (<<<), ($), (/), (<>), (==), unit, show, const, map, (>),(-),(*), bind, pure, discard, (&&))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), alignParentBottom, alignParentLeft, background, color, cornerRadius, ellipsize, fontStyle, frameLayout, gravity, height, imageUrl, imageView, letterSpacing, lineHeight, linearLayout, margin, maxLines, onClick, orientation, padding, stroke, text, textSize, textView, visibility, weight, width, singleLine, afterRender, clickable, scrollBarY, scrollView, imageWithFallback)
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (Stage(..))
import Styles.Colors as Color
import Common.Types.App
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Helpers.Utils (waitingCountdownTimer)
import Control.Monad.Except.Trans (runExceptT)
import Presto.Core.Types.Language.Flow (doAff)
import Animation (fadeIn)
import PrestoDOM.Animation as PrestoAnim

view :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit ) w
view push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background Color.transparent
  , orientation VERTICAL 
  ][  mapOptionsView push state 
    , driverInfoView push state 
    ]

mapOptionsView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w 
mapOptionsView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , background Color.transparent
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  , padding $ PaddingHorizontal 16 16
  ][  sosView push state
    , linearLayout 
      [ height WRAP_CONTENT
      , weight 1.0
      , clickable false
      ][]
    , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation VERTICAL
      ][ supportButton push state
       , locationTrackButton push state
      ]
    ]


supportButton :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
supportButton push state = 
 linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , gravity CENTER
  , background Color.white900
  , stroke $ "1,"<> Color.grey900
  , cornerRadius 20.0
  ][ imageView
      [ imageWithFallback "ny_ic_contact_support,https://assets.juspay.in/nammayatri/images/user/ny_ic_contact_support.png"
      , height $ V 18
      , width $ V 18
      , margin $ Margin 10 10 10 10
      , onClick push $ const Support
      ]
  ]


locationTrackButton :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
locationTrackButton push state = 
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , gravity CENTER
  , background Color.white900
  , stroke $ "1,"<> Color.grey900
  , visibility if state.props.currentStage == RideAccepted then GONE else VISIBLE
  , cornerRadius 20.0
  , onClick push (const $ LocationTracking)
  , margin $ MarginTop 8
  ][  imageView
      [ imageWithFallback "ny_ic_location_track,https://assets.juspay.in/nammayatri/images/common/ny_ic_location_track.png"
      , height $ V 18
      , width $ V 18
      , margin $ Margin 10 10 10 10
      ]
  ]

sosView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w 
sosView push state = 
  linearLayout
    [ height MATCH_PARENT
    , width WRAP_CONTENT
    , visibility if state.props.currentStage == RideStarted then VISIBLE else GONE
    , orientation VERTICAL
    , gravity if os == "IOS" then CENTER_VERTICAL else BOTTOM
    , onClick push $ const OpenEmergencyHelp
    ][ imageView
        [ imageWithFallback "ny_ic_sos,https://assets.juspay.in/nammayatri/images/user/ny_ic_sos.png"
        , height $ V 50
        , width $ V 50
        ]
    ]

otpAndWaitView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w 
otpAndWaitView push state = 
 linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_HORIZONTAL
  , margin $ Margin 16 0 16 16
  , visibility if state.props.currentStage == RideAccepted then VISIBLE else GONE
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      ][ linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , cornerRadius 9.0
          , background Color.grey800
          , gravity CENTER
          , padding $ Padding 10 14 10 14
          , weight 1.0
          ][ textView
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ getString OTP <> ":"
              , color Color.black700
              , textSize FontSize.a_14
              , lineHeight "18"
              , fontStyle $ FontStyle.bold LanguageStyle
              ]
            , linearLayout
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              ] (map(\item -> 
                  linearLayout
                    [ height $ V 32
                    , width $ V 32
                    , gravity CENTER
                    , cornerRadius 4.0
                    , background Color.black900
                    , margin $ MarginLeft 7
                    ][ textView
                        [ height WRAP_CONTENT
                        , width WRAP_CONTENT
                        , text item
                        , textSize FontSize.a_18
                        , color Color.white900
                        , fontStyle $ FontStyle.bold LanguageStyle
                        ]
                    ]) $ split (Pattern "")  state.data.otp)
          ]
        , waitTimeView push state
      ]
  ]


waitTimeView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w 
waitTimeView push state = 
 PrestoAnim.animationSet [ fadeIn state.data.driverArrived ] $  
 linearLayout
  [ width WRAP_CONTENT
  , height if os == "IOS" then (V 60) else MATCH_PARENT
  , orientation VERTICAL
  , cornerRadius 9.0
  , background Color.grey800
  , gravity CENTER_VERTICAL
  , margin $ MarginLeft 12
  , padding $ Padding 14 2 14 2
  , visibility if state.data.driverArrived then VISIBLE else GONE
  ][ textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ getString WAIT_TIME <> ":"
      , textSize FontSize.a_14
      , fontStyle $ FontStyle.medium LanguageStyle
      , lineHeight "18"
      , color Color.black700

      ]
    , textView
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text state.data.waitingTime
      , textSize FontSize.a_18
      , fontStyle $ FontStyle.bold LanguageStyle
      , lineHeight "24"
      , color Color.black800
      ]
  ]


---------------------------------- driverInfoView ---------------------------------------
driverInfoView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w 
driverInfoView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  ][ (if os == "IOS" then linearLayout else scrollView)
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , scrollBarY false
      ][ linearLayout
          [ orientation VERTICAL
          , height WRAP_CONTENT
          , width MATCH_PARENT
          , padding $ PaddingBottom 30
          , margin $ MarginVertical 14 0
          , background Color.white900
          , gravity CENTER
          , cornerRadii $ Corners 24.0 true true false false
          , stroke $ "1," <> Color.grey900
          ][ linearLayout
            [ gravity CENTER
            , background Color.transparentGrey
            , height $ V 4
            , width $ V 34
            , margin (MarginTop 8)
            , cornerRadius 4.0
            ][]
            , contactView push state
            , otpAndWaitView push state
            , separator (Margin 16 0 16 0) (V 1) Color.grey900 (state.props.currentStage == RideAccepted)
            , driverDetailsView push state
            , separator (Margin 16 0 16 0) (V 1) Color.grey900 true
            , paymentMethodView push state
            , separator (Margin 16 0 16 0) (V 1) Color.grey900 true
            , (if os == "IOS" then scrollView else linearLayout)
              [ width MATCH_PARENT
              , height if os == "IOS" then (V 210) else WRAP_CONTENT
              , orientation VERTICAL
              ][ sourceDistanceView push state
                , separator (Margin 0 0 0 0) (V 1) Color.grey900 (state.props.currentStage == RideAccepted)
                , cancelRideLayout push state
              ]
            ]
      ]
  ]

cancelRideLayout :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w 
cancelRideLayout push state = 
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , gravity CENTER
 , margin $ MarginTop 16
 , padding $ PaddingBottom if os == "IOS" then safeMarginBottom else 0
 , visibility if state.props.currentStage == RideAccepted then VISIBLE else GONE
 ][ linearLayout [
    height WRAP_CONTENT
   , width WRAP_CONTENT
   , padding $ Padding 5 5 5 5
   , onClick push $ const $ CancelRide state
 ] 
      [textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ getString CANCEL_RIDE
        , color Color.red
        ]]
 ]

---------------------------------- contactView ---------------------------------------
contactView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w 
contactView push state =
  linearLayout
  [ orientation HORIZONTAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , padding $ Padding 16 20 16 16
  , visibility if state.props.currentStage == RideAccepted then VISIBLE else GONE
  ][  linearLayout
      [ width (V (((screenWidth unit)/3 * 2)-27))
      , height WRAP_CONTENT
      , orientation if length state.data.driverName > 16 then VERTICAL else HORIZONTAL
      ][  textView
          [ text $ state.data.driverName <> " "
          , textSize FontSize.a_16
          , fontStyle $ FontStyle.semiBold LanguageStyle
          , color Color.black800
          , ellipsize true
          , singleLine true 
          ]
        , textView
          [ text $"is " <> secondsToHms state.data.eta
          , textSize FontSize.a_16
          , fontStyle $ FontStyle.semiBold LanguageStyle
          , color Color.black800
          , visibility if state.data.distance > 1000 then VISIBLE else GONE
          ]
        , textView
          [ text case state.data.distance > 1000 of
            true -> getString AWAY
            false -> if state.data.waitingTime == "--" then getString IS_ON_THE_WAY else getString IS_WAITING_FOR_YOU
          , textSize FontSize.a_16
          , fontStyle $ FontStyle.semiBold LanguageStyle
          , color Color.black800
          ]
      ]
    , linearLayout[
      width MATCH_PARENT
    , gravity RIGHT
    , height WRAP_CONTENT
    ][PrimaryButton.view (push <<< PrimaryButtonAC) (primaryButtonConfig)]

  ]

---------------------------------- driverDetailsView ---------------------------------------


driverDetailsView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w 
driverDetailsView push state =  
 linearLayout
  [ orientation HORIZONTAL
  , height $ V 170
  , padding $ Padding 16 16 16 16
  , width MATCH_PARENT
  , gravity BOTTOM
  ][  linearLayout
      [ orientation VERTICAL
      , height MATCH_PARENT
      , width WRAP_CONTENT
      , gravity BOTTOM
      , alignParentLeft "true,-1"
      ][  linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity LEFT
          ][imageView
              [ height $ V 50
              , width $ V 50
              , padding $ Padding 2 3 2 1
              , imageWithFallback "ny_ic_user,https://assets.juspay.in/nammayatri/images/user/ny_ic_user.png"
              ]  
          ]
        , textView
          [ text state.data.driverName
          , textSize FontSize.a_16
          , maxLines 1 
          , ellipsize true
          , fontStyle $ FontStyle.bold LanguageStyle
          , color Color.black800
          , width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity LEFT
          ]
        , textView
          [ text state.data.vehicleDetails
          , textSize FontSize.a_12
          , color Color.black700 
          , fontStyle $ FontStyle.regular LanguageStyle
          , width MATCH_PARENT
          , height WRAP_CONTENT
          , margin $ Margin 0 4 0 13
          , gravity LEFT
          ]
        , ratingView push state 
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity RIGHT
      ][  frameLayout
          [ height MATCH_PARENT
          , width $ V 172
          , gravity BOTTOM
          ][  imageView
              [ imageWithFallback "ny_ic_driver_auto,https://assets.juspay.in/nammayatri/images/user/ny_ic_driver_auto.png"
              , height $ V 120
              , gravity RIGHT
              , width MATCH_PARENT
              , margin $ MarginBottom 15
              ]
            , linearLayout
              [ height $ V 138
              , width MATCH_PARENT
              , gravity BOTTOM
              ][  linearLayout
                  [ height $ V 38
                  , width MATCH_PARENT
                  , background Color.golden
                  , cornerRadius 4.0
                  , orientation HORIZONTAL
                  , gravity BOTTOM
                  , padding $ Padding 2 2 2 2
                  , alignParentBottom "true,-1"
                  ][ 
                    linearLayout
                    [ height $ V 34
                    , width MATCH_PARENT
                    , stroke $ "2," <> Color.black
                    , cornerRadius 4.0
                    , orientation HORIZONTAL  
                    ][  imageView
                        [ imageWithFallback "ny_ic_number_plate,https://assets.juspay.in/nammayatri/images/user/ny_ic_number_plate.png"
                        , gravity LEFT
                        , background "#1C4188"
                        , height MATCH_PARENT
                        , width $ V 22
                        ]
                        , textView
                        [ margin $ Margin 2 2 2 2
                        , width MATCH_PARENT
                        , height MATCH_PARENT
                        , text state.data.registrationNumber
                        , color Color.black
                        , fontStyle $ FontStyle.bold LanguageStyle
                        , textSize FontSize.a_16
                        , gravity CENTER
                        , cornerRadius 4.0
                        ]
                      ]
                    ]
                ]

            ]
        ]
    ]

---------------------------------- ratingView ---------------------------------------

ratingView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w 
ratingView push state = 
  linearLayout
  [ orientation HORIZONTAL
  , height $ V 34
  , width $ V 90
  , padding $ Padding 16 9 16 9
  , background Color.grey800
  , gravity CENTER_VERTICAL
  , cornerRadius 6.0
  ][  imageView
      [ imageWithFallback "ny_ic_star_active,https://assets.juspay.in/nammayatri/images/common/ny_ic_star_active.png"
      , height $ V 13
      , width $ V 13
      ]
    , textView
      [ text $ if state.data.rating == 0.0 then "New" else show state.data.rating
      , textSize FontSize.a_12 
      , color Color.black800
      , gravity CENTER
      , fontStyle $ FontStyle.medium LanguageStyle
      , margin (Margin 8 0 2 0)
      , width $ V 26
      , height $ V 30
      , lineHeight "15"
      ]
    ]

---------------------------------- paymentMethodView ---------------------------------------

paymentMethodView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w 
paymentMethodView push state =
  linearLayout
  [ orientation HORIZONTAL
  , width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER_VERTICAL
  ][  linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , padding $ Padding 16 16 16 16
      , width WRAP_CONTENT
      , gravity LEFT
      ][  textView $
          [ text $ getString RIDE_FARE
          , color Color.black700
          ] <> FontStyle.body3 TypoGraphy
        , textView $
          [ text $ "₹" <> show state.data.price
          , color Color.black800
          ] <> FontStyle.h2 TypoGraphy
      ]
      , linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        ][]
      , linearLayout
          [ orientation HORIZONTAL
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER
          ][  imageView
              [ imageWithFallback "ny_ic_wallet,https://assets.juspay.in/nammayatri/images/user/ny_ic_wallet.png"
              , height $ V 20
              , width $ V 20
              ]
            , textView $
              [ text $ getString PAYMENT_METHOD_STRING
              , color Color.black800
              , padding $ Padding 8 0 20 0
              ] <> FontStyle.body1 TypoGraphy
            ]
    ]

---------------------------------- tripDetailsView ---------------------------------------

sourceDistanceView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w 
sourceDistanceView push state =
  linearLayout 
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding $ Padding 0 10 0 if (os == "IOS" && state.props.currentStage == RideStarted) then safeMarginBottom else 16
  ][  textView
      [ text $ getString PICKUP_AND_DROP
      , fontStyle $ FontStyle.regular LanguageStyle
      , margin $ Margin 16 0 0 10
      , textSize $ FontSize.a_12
      ]
    , SourceToDestination.view (push <<< SourceToDestinationAC) (sourceToDestinationConfig state)
  ]

---------------------------------- separator ---------------------------------------

separator :: forall w. Margin -> Length -> String -> Boolean -> PrestoDOM (Effect Unit) w 
separator margin' height' color' isVisible =
  linearLayout
  [ height $ height'
  , margin $ margin'
  , width MATCH_PARENT
  , visibility if isVisible then VISIBLE else GONE
  , background color'
  ][]

---------------------------------- primaryButtonConfig ---------------------------------------

primaryButtonConfig :: PrimaryButton.Config
primaryButtonConfig = let 
    config' = PrimaryButton.config
    primaryButtonConfig' = config' 
      { width = WRAP_CONTENT
      , height = WRAP_CONTENT
      , background = Color.mint
      , cornerRadius = 17.0
      , isPrefixImage = true 
      , prefixImageConfig {
          height = V 18
        , width = V 18
        , imageUrl = "ny_ic_call,https://assets.juspay.in/nammayatri/images/common/ny_ic_call.png"
        , margin = Margin 20 10 20 10
        }
      }
  in primaryButtonConfig'


---------------------------------- sourceToDestinationConfig ---------------------------------------

sourceToDestinationConfig :: DriverInfoCardState -> SourceToDestination.Config
sourceToDestinationConfig state = let 
  config = SourceToDestination.config
  sourceToDestinationConfig' = config
    {
      margin = Margin 2 0 40 0
    , lineMargin = Margin 19 7 0 0
    , destinationMargin = MarginTop 16
    , sourceImageConfig {
        imageUrl = "ny_ic_pickup,https://assets.juspay.in/nammayatri/images/user/ny_ic_pickup.png"
      , height = V 14
      , width = V 14
      , margin = Margin 13 4 0 0
      }
    , sourceTextConfig {
        text = state.data.source
      , textSize = FontSize.a_14
      , padding = Padding 2 0 2 2
      , margin = Margin 12 0 15 0
      , fontStyle = FontStyle.medium LanguageStyle
      , ellipsize = true
      , maxLines = 1
      }
    , destinationImageConfig {
        imageUrl = "ny_ic_drop,https://assets.juspay.in/nammayatri/images/user/ny_ic_drop.png"
      , height = V 17
      , width = V 14
      , margin = Margin 13 2 0 0
      }
    , destinationTextConfig {
        text = state.data.destination
      , textSize = FontSize.a_14
      , padding = Padding 2 0 2 2
      , margin = Margin 12 0 15 0
      , maxLines = 1
      , fontStyle = FontStyle.medium LanguageStyle
      , ellipsize = true
      }
    , distanceConfig {
        distanceVisibility = VISIBLE
      , distanceValue = state.data.estimatedDistance <> " km"
      , background = Color.grey700
      , margin = Margin 12 28 0 0
  }
    }
  in sourceToDestinationConfig'

configurations = 
  case os of 
    "IOS" -> {paddingOTPText : PaddingVertical 4 4
              , letterSpacing : 6.0 
              , paddingOTP : Padding 20 5 18 7}
    _     -> {paddingOTPText : PaddingVertical 2 2
              , letterSpacing : 3.0 
              , paddingOTP : Padding 11 0 11 7
              }