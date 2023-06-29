{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideDetailScreen.View
  ( cashCollected
  , earningsDetailView
  , rideDetailView
  , screen
  , view
  )
  where

import Prelude (Unit, bind, discard, const, pure, unit, ($), (+), (-), (/), (<>), (==), show,(<<<),(*))
import PrestoDOM
import PrestoDOM.Properties (lineHeight, cornerRadii)
import PrestoDOM.Types.DomAttributes (Gravity(..), Corners(..))
import Effect (Effect)
import Language.Strings (getString)
import Language.Types (STR(..))
import Screens.RideDetailScreen.Controller (Action(..), eval, ScreenOutput)
import Screens.Types as ST
import Styles.Colors as Color
import Font.Size as FontSize
import Font.Style as FontStyle
import Engineering.Helpers.Commons as EHC
import JBridge as JB
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Presto.Core.Types.Language.Flow (doAff)
import Services.APITypes(Route(..))
import Services.Backend as Remote
import Services.Backend (walkCoordinate)
import Common.Types.App
import Data.Array (any, length, mapWithIndex, null, (!!),(:))
import Components.RatingCard.View as RatingCardView
import Components.RatingCard as RatingCard
import Screens.Types (HomeScreenStage(..))
import Engineering.Helpers.Commons(getNewIDWithTag)
import Data.Maybe (Maybe(..),isJust)
import JBridge (startLottieProcess)
import Screens.RideDetailScreen.ComponentConfig (ratingCardViewState,callSupportConfig)
import Components.PopUpModal as PopUpModal
import Merchant.Utils(getMerchant,Merchant(..))
import Data.Int(round,toNumber)

screen :: ST.RideDetailScreenState -> Screen Action ST.RideDetailScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "RideDetailScreen"
  , globalEvents : []
  , eval
  }

view :: forall w . (Action -> Effect Unit) -> ST.RideDetailScreenState -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    ][ linearLayout
      [ width MATCH_PARENT
      , height$ WRAP_CONTENT 
      , background "#2C2F3A"
      , orientation VERTICAL
      ][ callSupport state push
      , linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER_HORIZONTAL
      ][ textView 
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , gravity CENTER
        , margin $ MarginVertical 24 16
        , textSize FontSize.a_22
        , text $ getString RIDE_COMPLETED
        , color Color.white900
        ]
        , totalAmount state
        , paymentMode state
        ]
        , linearLayout
          [ height $ V 1
          , width MATCH_PARENT
          , background Color.black800
          , margin $ MarginHorizontal 16 16
          ][]
        , rideDetailView state push
      ]
      ,  earningsDetailView state push
      , cashCollected state push
    ]
      , if state.props.supportPopUpView then callSupportView push state else linearLayout [][]
      , if state.props.rateCardView then rideRatingCardView state push else linearLayout[][]
  ]
  


rideRatingCardView :: forall w. ST.RideDetailScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
rideRatingCardView state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , gravity BOTTOM
    , background Color.black9000
    ]
    [ RatingCard.view (push <<< RateCardAction) $ ratingCardViewState state
    ]

callSupport :: forall w. ST.RideDetailScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
callSupport state push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    ][ linearLayout 
      [ height WRAP_CONTENT
      , weight 1.0
      ][]
      , linearLayout
      [ width (V 40)
      , height (V 40)
      , cornerRadius 360.0
      , background "#336D7280"
      , gravity CENTER
      , margin (Margin 0 16 16 0)
      , onClick push $ const OnCallSupport
      ][
       imageView
        [ imageWithFallback "ic_headphones,https://assets.juspay.in/nammayatri/images/driver/ic_headphones.png"
        , width (V 20)
        , height (V 20)
        ]
      ]
    ]

earningsDetailView :: forall w . ST.RideDetailScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
earningsDetailView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , padding $ Padding 10 10 10 10
    , margin $ Margin 16 20 16 20
    , stroke ("1," <> Color.grey800)
    , background "#FEF1E5"
    , gravity CENTER_VERTICAL
    , cornerRadius 8.0
    ][ linearLayout
        [ height WRAP_CONTENT
        , width (V ((EHC.screenWidth unit)/2 + 10))
        , orientation VERTICAL
        , margin $ MarginRight 30
        , gravity CENTER_VERTICAL
        ]
        ( mapWithIndex
            ( \index item ->
                linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , orientation VERTICAL
                  , gravity CENTER_VERTICAL
                  ]
                  [
                    linearLayout
                      [ height $ V 1
                      , width MATCH_PARENT
                      , background "#F0DCCA"
                      , margin $ MarginVertical 8 8
                      , visibility if(index == 1) then VISIBLE else GONE
                      ]
                      []
                    , linearLayout
                      [
                        width MATCH_PARENT
                        , height MATCH_PARENT
                        , orientation HORIZONTAL
                        , gravity CENTER_VERTICAL
                      ]
                      [
                      textView
                        [ width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , text $ "₹" <> show item
                        , color "#40AA5C"
                        , textSize FontSize.a_24
                        , padding $ PaddingBottom 8
                        , fontStyle $ FontStyle.bold LanguageStyle
                        ],
                      textView 
                        [ width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , text if (index == 1) then "Tip earned from customer" else (getString SAVED_DUE_TO_NO_COMMISSION)
                        , color Color.black800
                        , textSize FontSize.a_17
                        , margin $ MarginLeft 8 
                        ,  fontStyle $ if (isJust state.data.extraFare) then FontStyle.regular LanguageStyle else FontStyle.bold LanguageStyle 
                        ]
                      ]
                  ]
            ) (arrayAmount state)
        )
    , lottieLoaderView state push
    ]

lottieLoaderView :: forall w. ST.RideDetailScreenState -> (Action -> Effect Unit) -> PrestoDOM ( Effect Unit) w 
lottieLoaderView state push = 
      lottieAnimationView 
      [ id (getNewIDWithTag "coin_lottie")
      , afterRender (\action-> do
                    _ <- pure $ startLottieProcess "coin_lottie" (getNewIDWithTag "coin_lottie") true 0.6 ""
                    pure unit)(const CountDown)
      , height $ V 100
      , width  $ V 100
      ]
rideDetailView :: forall w . ST.RideDetailScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
rideDetailView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , padding $ Padding 16 16 16 16 
    , onClick push $ const OnRideDetailsAC
    ][ textView 
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , text $ getString RIDE_DETAILS
    , color Color.white900
    , textSize FontSize.a_17
    , weight 1.0
    ]
    , imageView
        [ imageWithFallback "ic_white_chevron_right, https://assets.juspay.in/beckn/merchantcommon/images/ic_white_chevron_right.png"
        , height $ V 15
        , width $ V 10
        , gravity CENTER_VERTICAL
        ]
    ]

totalAmount :: forall w . ST.RideDetailScreenState -> PrestoDOM (Effect Unit) w
totalAmount state =
 linearLayout
 [ width WRAP_CONTENT
 , height WRAP_CONTENT
 , orientation HORIZONTAL
 ][ textView 
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text $  "₹" <> (show state.data.totalAmount)
  , color Color.white900
  , textSize FontSize.a_72
  ] 
 ]


paymentMode :: forall w . ST.RideDetailScreenState -> PrestoDOM (Effect Unit) w
paymentMode state = 
  linearLayout
    [
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , background if getMerchant unit == JATRISAATHIDRIVER then "#0FFCC32C" else "#1480B2FF"
    , margin $ MarginVertical 16 48
    , cornerRadii $ Corners 20.0 true true true true
    , gravity CENTER_VERTICAL 
    , stroke $ if getMerchant unit == JATRISAATHIDRIVER then "1,#80FCC32C" else "0,#80FCC32C"
    ][ imageView
         [ width (V 34)
         , height (V 22)
         , imageWithFallback "ic_cash,https://assets.juspay.in/jatrisaathi/driver/images/common/ic_cash.png"
         , visibility if getMerchant unit == JATRISAATHIDRIVER then VISIBLE else GONE
         , margin (MarginLeft 19)
         , background Color.white900
         ]
    , textView 
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , gravity CENTER
      , textSize FontSize.a_14
      , padding (Padding 9 12 16 12)
      , text $ "Collect via Cash/UPI"
      , color Color.white900
      ] 
    ]


cashCollected :: forall w . ST.RideDetailScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
cashCollected state push =
  linearLayout
  [
    width MATCH_PARENT
    , height WRAP_CONTENT
    , gravity BOTTOM
    , weight 1.0
  ][
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background Color.black900
    , alignParentBottom "true,-1"
    , gravity CENTER
    , margin $ Margin 16 0 16 38
    , padding $ Padding 0 14 0 14
    , cornerRadii $ Corners 8.0 true true true true
    , onClick push $ const GoToRateCardView
    ][  textView (
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ getString FARE_COLLECTED
        , fontStyle $ FontStyle.bold LanguageStyle
        , color Color.yellowText
        , textSize FontSize.a_16
        ] <> FontStyle.subHeading1 TypoGraphy
    )
    ]
  ]

callSupportView :: forall w . (Action -> Effect Unit) -> ST.RideDetailScreenState -> PrestoDOM (Effect Unit) w
callSupportView push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][PopUpModal.view (push <<< PopUpModalAction) (callSupportConfig state)]

arrayAmount ::  ST.RideDetailScreenState  -> Array Int
arrayAmount state = 
  case state.data.extraFare of 
    Just extraFare -> [round (0.19 * (toNumber (state.data.totalAmount))),extraFare]
    Nothing -> [round (0.19 * (toNumber (state.data.totalAmount)))]