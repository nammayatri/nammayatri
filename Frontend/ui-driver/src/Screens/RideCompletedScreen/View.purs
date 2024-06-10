{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideCompletedScreen.View where

import Common.Types.App
import Prelude
import Screens.RideCompletedScreen.ComponentConfig

import Animation as Anim
import Common.Styles.Colors as Colors
import Components.PrimaryButton as PrimaryButton
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Font.Style as FontStyle
import Font.Style as FontStyle
import Helpers.Utils (FetchImageFrom(..), fetchImage)
import Language.Strings (getString)
import Language.Types (STR(..))
import PrestoDOM (weight, Gradient(..), text, color, gradient, onClick, cornerRadius, scrollBarY, imageWithFallback, Gravity(..), Orientation(..), orientation, gravity, textView, imageView, margin, padding, scrollView, alignParentBottom, relativeLayout, stroke, width, height, background, Screen, linearLayout, PrestoDOM, Gravity(..), Margin(..), Length(..), Padding(..), Visibility(..), Orientation(..))
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.RideCompletedScreen.Controller (RatingType(..), Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Debug (spy)
import PrestoDOM.Animation as PrestoAnim
import Components.PopUpModal as PopUpModal
import Animation (fadeIn, fadeOut, translateYAnimFromTop, scaleAnim, translateYAnimFromTopWithAlpha, translateInXAnim, translateOutXAnim, translateInXForwardAnim, translateOutXBackwardAnimY, translateInXSidebarAnim, screenAnimation, fadeInWithDuration, fadeOutWithDuration, scaleYAnimWithDelay, shimmerAnimation)
import Animation.Config as AnimConfig
import Engineering.Helpers.Utils (priceToBeDisplayed)

screen :: ST.RideCompletedScreenState -> Screen Action ST.RideCompletedScreenState ScreenOutput 
screen initialState =
  { initialState
  , view
  , name : "RideCompletedScreen"
  , globalEvents : []
  , eval : \state  action -> do
      let _ = spy  "RideCompletedScreen action " action
      let _ = spy  "RideCompletedScreen state " state
      eval state action
  }

view :: forall w . (Action -> Effect Unit) -> ST.RideCompletedScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , background $ Color.white900
  ]( [ linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , background Color.white900
     , orientation VERTICAL
     ][ rideCompletedPaymentInformationView push state
     ,  rideRateCardView push state
     ]
  ,  bottomButtonView push state
  ] <> if state.props.isFareBreakDownVisible then [rideFareBreakdownCardView push state]
       else if state.props.showCallSupportPopup then [callSupportView push state]
       else [] )

bottomButtonView :: forall w . (Action -> Effect Unit) -> ST.RideCompletedScreenState -> PrestoDOM (Effect Unit) w
bottomButtonView push state =
  linearLayout
  [ width MATCH_PARENT
  , height $ WRAP_CONTENT
  , background $ Color.white900
  , alignParentBottom "true,-1"
  , stroke $ "1," <>  Color.grey900
  , cornerRadii $ Corners 16.0 true true false false
  ][ PrimaryButton.view (push <<< RideCompletedButtonClick) (rideCompletedButtonConfig state)
  ]

rideRateCardView :: forall w . (Action -> Effect Unit) -> ST.RideCompletedScreenState -> PrestoDOM (Effect Unit) w
rideRateCardView push state =
  linearLayout
  [ width $ MATCH_PARENT
  , height $ WRAP_CONTENT
  , margin $ Margin 16 24 16 0
  , padding $ Padding 16 16 16 16
  , cornerRadius 16.0
  , stroke $ "1," <> Color.grey900
  , gravity CENTER
  , orientation VERTICAL
  ][  imageView
      [ imageWithFallback $ fetchImage FF_ASSET "ic_new_avatar_2"
      , width $ V 64
      , height $ V 64
      ]
    , textView  $
      [ text $ getString $ HOW_WAS_YOUR_RIDE_WITH_NAME state.props.endRideData.riderName
      , color Color.black800
      , width MATCH_PARENT
      , gravity CENTER_HORIZONTAL
      , margin $ MarginTop 20
      ] <> FontStyle.subHeading1 TypoGraphy
    , linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , orientation HORIZONTAL
      , margin $ MarginTop 24
      ][ ratingView push state POSITIVE
      ,  space push state 0 16
      ,  ratingView push state NEGATIVE
      ]
  ]

rideCompletedPaymentInformationView :: forall w . (Action -> Effect Unit) -> ST.RideCompletedScreenState -> PrestoDOM (Effect Unit) w
rideCompletedPaymentInformationView push state =
  linearLayout
  [ width $ MATCH_PARENT
  , height $ WRAP_CONTENT
  , stroke $ "1," <> Color.grey900
  , cornerRadii $ Corners 16.0 false false true true
  , padding $ Padding 20 20 20 0
  , orientation VERTICAL
  , gradient (Linear 180.0 state.data.appConfig.rideCompletedCardConfig.topCardGradient)
  ][ ridePaymentDetailsCardView push state
   , separatorView push state
   , rideDetailsPillView push state
  ]

ratingView :: forall w . (Action -> Effect Unit) -> ST.RideCompletedScreenState -> RatingType -> PrestoDOM (Effect Unit) w
ratingView push state ratingType =
  let ratingDetails = getRatingDetails 
  in
  linearLayout
  [ width $ V 50
  , height $ V 50
  , cornerRadius 50.0
  , background ratingDetails.background
  , stroke $ "1," <> Color.grey900
  , gravity CENTER
  , orientation VERTICAL
  , onClick push $ const (SelectRating ratingType)
  ][  imageView
      [ imageWithFallback $ fetchImage FF_ASSET ratingDetails.icon
      , width $ V 24
      , height $ V 24
      ]
  ]
  where
    getRatingDetails = case ratingType of
                          POSITIVE -> if state.props.selectedRating == ST.SEL_P then {icon : "ny_ic_thumps_up_filled", action : NoAction, background : Color.black800}
                                      else {icon : "ny_ic_thumps_up_unfilled", action : NoAction, background : Color.grey800}
                          NEGATIVE -> if state.props.selectedRating == ST.SEL_N then {icon : "ny_ic_thumps_down_filled", action : NoAction, background : Color.black800}
                                      else {icon : "ny_ic_thumps_down_unfilled", action : NoAction, background : Color.grey800}

separatorView :: forall w . (Action -> Effect Unit) -> ST.RideCompletedScreenState -> PrestoDOM (Effect Unit) w
separatorView push state =
  linearLayout
  [ width $ MATCH_PARENT
  , height $ V 1
  , background Color.grey900
  ][]

ridePaymentDetailsCardView :: forall w . (Action -> Effect Unit) -> ST.RideCompletedScreenState -> PrestoDOM (Effect Unit) w
ridePaymentDetailsCardView push state =
  linearLayout
  [ width $ MATCH_PARENT
  , height $ WRAP_CONTENT
  , orientation VERTICAL
  , gravity CENTER
  , padding $ PaddingVertical 48 28
  ][ supportView push state
  ,  textView $ 
     [ text $ getString $ HAS_PAID_YOU state.props.endRideData.riderName
     , color Color.black800
     ] <> FontStyle.h2 TypoGraphy
  ,  textView $ 
     [ text $ priceToBeDisplayed state.props.endRideData.finalAmountWithCurrency true
     , margin $ MarginTop 12
     , color Color.black800
     ] <> FontStyle.title3 TypoGraphy

  -- todo :: once it is finalized, will add these
  -----------------------------------------------
  -- ,  textView $ 
  --    [ text $ getString $ TOTAL_AMOUNT_INCLUDING_CHARGES ""
  --    , color Color.black400
  --    ] <> FontStyle.subHeading2 TypoGraphy   
  -- ,  linearLayout
  --    [ width $ MATCH_PARENT
  --    , height $ WRAP_CONTENT
  --    , gravity CENTER
  --    , onClick push $ const ViewFareBreakDown
  --    ][ textView $ 
  --       [ text $ getString VIEW_FARE_BREAKDOWN
  --       , color Color.purple700
  --       , margin $ MarginTop 12
  --       ] <> FontStyle.body1 TypoGraphy
  --     ]
  ]

rideDetailsPillView :: forall w . (Action -> Effect Unit) -> ST.RideCompletedScreenState -> PrestoDOM (Effect Unit) w
rideDetailsPillView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , padding $ PaddingVertical 16 16
  , orientation HORIZONTAL
  , onClick push $ const ShowRideDetails
  ][ textView $
     [ text $ getString RIDE_DETAILS
     , color $ Color.black800
     , gravity LEFT
     , height WRAP_CONTENT
     , width $ MATCH_PARENT -- todo :: check this.. currently it is working in ios -- check in android
     ] <> FontStyle.body2 TypoGraphy
  ,  imageView
     [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right_black"
     , width $ V 20
     , height $ V 20
     ]
  ]

supportView :: forall w . (Action -> Effect Unit) -> ST.RideCompletedScreenState -> PrestoDOM (Effect Unit) w
supportView push state =
  linearLayout
  [ width $ MATCH_PARENT
  , height $ WRAP_CONTENT
  , gravity $ RIGHT
  ][linearLayout
    [ width $ V 40
    , height $ V 40
    , gravity CENTER
    , background $ Color.white50Alpha
    , onClick push $ const HelpAndSupportAC
    , cornerRadius $ 350.0 -- cornerRadius not working
    ][  imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_headphones"
        , width $ V 20
        , height $ V 20
        ]
    ]
  ]

rideFareBreakdownCardView :: forall w . (Action -> Effect Unit) -> ST.RideCompletedScreenState -> PrestoDOM (Effect Unit) w
rideFareBreakdownCardView push state =
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.black9000
  , onClick push $ const ViewFareBreakDown
  ][  PrestoAnim.animationSet 
     [ Anim.translateYAnimFromTopWithAlpha $ AnimConfig.translateYAnimConfig {fromY = -500, toY = 0}
     ] $
     linearLayout
     [ width $ MATCH_PARENT
     , orientation VERTICAL
     , height WRAP_CONTENT
     , alignParentBottom "true,-1"
     ][ fareBreakupCard push state ]
  ]

-----------------------------------------------------------

fareBreakupCard :: forall w . (Action -> Effect Unit) -> ST.RideCompletedScreenState -> PrestoDOM (Effect Unit) w
fareBreakupCard push state =

  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , background Color.white900
  , cornerRadii $ Corners 24.0 true true false false
  ][ headerView push state
  ,  fareDetailsView push state
  ]

headerView :: forall w . (Action -> Effect Unit) -> ST.RideCompletedScreenState -> PrestoDOM (Effect Unit) w
headerView push state =
  linearLayout
  [ width $ MATCH_PARENT
  , height $ WRAP_CONTENT
  , orientation HORIZONTAL
  , background Color.purple500
  , cornerRadii $ Corners 24.0 true true false false
  , padding $ Padding 16 20 16 20
  ][ textView $ 
     [ text $ getString FARE_BREAKDOWN
     , color Color.black900
     ] <> FontStyle.body13 TypoGraphy
  ,  linearLayout [ weight 1.0 ] []
  ,  linearLayout
     [ width $ V 24
     , height $ V 24
     , orientation VERTICAL
     , gravity CENTER
     , cornerRadius 10.0
     , background Color.white900
     ][ imageView
        [ height $ V 15
        , width $ V 15
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_close"
        ]
    ]
  ]

fareDetailsView :: forall w . (Action -> Effect Unit) -> ST.RideCompletedScreenState -> PrestoDOM (Effect Unit) w
fareDetailsView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , padding $ Padding 16 16 16 16
  , margin $ MarginBottom 32
  , orientation VERTICAL
  ][ fareDetailsPill push state "Base Fare" "$20"
   , fareDetailsPill push state "Insurance Charges" "$2"
   , fareDetailsPill push state "Taxes" "$2"
   , separatorView push state
   , space push state 24 0
   , fareDetailsPill push state "Total amount paid" "$24"
   , fareDetailsPill push state "Amount you receive" "$20"
  ]

space :: forall w . (Action -> Effect Unit) -> ST.RideCompletedScreenState -> Int -> Int -> PrestoDOM (Effect Unit) w
space push state heightVal widthVal =
  linearLayout
  [ height $ V heightVal
  , width $ V widthVal
  ]
  []

fareDetailsPill :: forall w . (Action -> Effect Unit) -> ST.RideCompletedScreenState -> String -> String -> PrestoDOM (Effect Unit) w
fareDetailsPill push state key value =
  linearLayout
  [ width $ MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , margin $ MarginBottom 20
  ][ textView $ 
     [ text key
     , color Color.black800
     ] <> FontStyle.paragraphText TypoGraphy
   , linearLayout [ weight 1.0 ][]
   , textView $ 
     [ text value
     , color Color.black800
     ] <> FontStyle.body4 TypoGraphy
  ]

callSupportView :: forall w. (Action -> Effect Unit) -> ST.RideCompletedScreenState -> PrestoDOM (Effect Unit) w
callSupportView push state = 
  PrestoAnim.animationSet [ Anim.fadeIn true ]
    $ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      ][PopUpModal.view (push <<< GoToCallSupportAction) (callSupportPopupConfig state)]
