{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.TicketBookingFlow.MetroTicketStatus.View where

import Common.Types.App
import Screens.TicketBookingFlow.MetroTicketStatus.ComponentConfig

import Animation as Anim
import Animation.Config (translateYAnimConfig, translateYAnimMapConfig, removeYAnimFromTopConfig)
import Common.Types.App as Common
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Data.Array as DA
import Data.Foldable (or)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.String as DS
import Data.String.Common (joinWith)
import Effect (Effect)
import Engineering.Helpers.Commons (getCurrentUTC, screenWidth, flowRunner)
import Data.Foldable (foldl, foldMap)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils 
import JBridge as JB
import Prelude 
import PrestoDOM 
import PrestoDOM.Animation as PrestoAnim
import Screens.TicketBookingFlow.MetroTicketStatus.Controller (Action(..), ScreenOutput, eval)
import Screens.Types as ST
import Styles.Colors as Color
import Screens.TicketBookingFlow.MetroTicketStatus.ComponentConfig 
import Resources.Constants -- TODO:: Replace these constants with API response
import Engineering.Helpers.Commons (screenWidth, convertUTCtoISC, getNewIDWithTag, convertUTCTimeToISTTimeinHHMMSS)
import Services.API 
import Animation (fadeInWithDelay, translateInXBackwardAnim, translateInXBackwardFadeAnimWithDelay, translateInXForwardAnim, translateInXForwardFadeAnimWithDelay)
import Halogen.VDom.DOM.Prop (Prop)
import Data.Array 
import Data.Maybe 
import Debug
import Effect.Aff (launchAff)
import Control.Monad.Except.Trans (runExceptT , lift)
import Control.Transformers.Back.Trans (runBackT)
import Services.Backend as Remote
import Data.Either (Either(..))
import Presto.Core.Types.Language.Flow (doAff, Flow, delay)
import Effect.Class (liftEffect)
import Types.App (GlobalState, defaultGlobalState)
import Data.Time.Duration (Milliseconds(..))
import Services.API as API
import Storage (KeyStore(..), setValueToLocalStore, getValueToLocalStore)
import Effect.Uncurried  (runEffectFn1)
import PaymentPage (consumeBP)
import Engineering.Helpers.Commons as EHC
import Data.Ord (comparing)
import Data.Function.Uncurried (runFn3)
import Mobility.Prelude (groupAdjacent)
import Language.Strings (getString)
import Language.Types (STR(..))
import Mobility.Prelude  
import Effect.Uncurried
import Data.Function.Uncurried
import Timers (startTimer)
import Engineering.Helpers.Commons (screenHeight)

screen :: ST.MetroTicketStatusScreenState -> Screen Action ST.MetroTicketStatusScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "MetroTicketStatusScreen"
  , globalEvents : [getMetroStatusEvent]
  , eval :
    \action state -> do
        let _ = spy "MetroTicketStatusScreen action " action
        let _ = spy "MetroTicketStatusScreen state " state
        eval action state
  }
  where
  getMetroStatusEvent push = do
    void $ pure $ spy "getMetroStatusEvent" ""
    void $ launchAff $ flowRunner defaultGlobalState $ metroPaymentStatusPooling initialState.data.bookingId initialState.data.validUntil 3000.0 initialState push MetroPaymentStatusAction
    if initialState.props.paymentStatus == Common.Success then do
      void $ pure $ spy "getMetroStatusEvent" ""
      startTimer 4 "success" "1" push CountDown
    else pure unit
    pure $ pure unit
--------------------------------------------------------------------------------------------

metroPaymentStatusPooling :: forall action. String -> String -> Number -> ST.MetroTicketStatusScreenState -> (action -> Effect Unit) -> (MetroTicketBookingStatus -> action) -> Flow GlobalState Unit
metroPaymentStatusPooling bookingId validUntil delayDuration state push action = do
  let dummy = spy (getCurrentUTC "") validUntil
  let diffSec = spy "DIFF" $ runFn2 JB.differenceBetweenTwoUTC  validUntil (getCurrentUTC "")
  if (getValueToLocalStore METRO_PAYMENT_STATUS_POOLING) == "true" && bookingId /= "" then do
    ticketStatus <-  Remote.getMetroBookingStatus bookingId 
    void $ pure $ spy "ticketStatus" ticketStatus
    case ticketStatus of
      Right (API.GetMetroBookingStatusResp resp) -> do
        let (MetroTicketBookingStatus statusResp) = resp
        if ((DA.any (_ == statusResp.status) ["CONFIRMED", "FAILED", "EXPIRED", "PAYMENT_PENDING"]) || diffSec < 0) then do
            _ <- pure $ setValueToLocalStore METRO_PAYMENT_STATUS_POOLING "false"
            doAff do liftEffect $ push $ action resp
        else do
            void $ delay $ Milliseconds delayDuration
            metroPaymentStatusPooling bookingId validUntil delayDuration state push action
      Left _ -> pure unit
  else pure unit

view :: forall w . (Action -> Effect Unit) -> ST.MetroTicketStatusScreenState -> PrestoDOM (Effect Unit) w
view push state =
  PrestoAnim.animationSet [Anim.fadeIn true]  $ relativeLayout[
    height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.darkGradientBlue
  , onBackPressed push $ const BackPressed
  ][ 
    shimmerView state
  , linearLayout[
      width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , visibility $ boolToVisibility $ not $ state.props.showShimmer
    , afterRender push $ const AfterRender
    ][
      scrollView
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , fillViewport true
        ][ 
          bookingStatusView state push state.props.paymentStatus
        ]
    , linearLayout [
        width WRAP_CONTENT
      , weight 1.0
      ][]
    , bookingConfirmationActions state push state.props.paymentStatus
    ]
  ]

shimmerView :: forall w . ST.MetroTicketStatusScreenState -> PrestoDOM (Effect Unit) w
shimmerView state =
  shimmerFrameLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , visibility $ boolToVisibility $  state.props.showShimmer
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height (V 235)
        , margin (Margin 16 15 16 0)
        , background Color.greyDark
        , cornerRadius 16.0
        ] []
    , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , margin (MarginTop 258)
        ] (DA.mapWithIndex 
            (\index item ->
                linearLayout
                  [ width MATCH_PARENT
                  , height (V 60)
                  , margin (Margin 16 16 16 0)
                  , cornerRadius 12.0
                  , background Color.greyDark
                  ][]
            ) (1 .. 7)
          )
    ]

bookingStatusView :: forall w. ST.MetroTicketStatusScreenState -> (Action -> Effect Unit) -> Common.PaymentStatus -> PrestoDOM (Effect Unit) w
bookingStatusView state push paymentStatus = 
  let refundInfoView = if state.props.paymentStatus == Common.Pending then
                         refundInfoTextView 
                       else
                         linearLayout [visibility GONE] []
  in 
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , padding $ PaddingTop 20
    , background Color.darkGradientBlue
    , gravity CENTER
    , orientation VERTICAL
    ][ 
      paymentStatusHeader state push paymentStatus
    , bookingStatusBody state push paymentStatus
    , refundInfoView
    ]

copyTransactionIdView :: forall w. ST.MetroTicketStatusScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
copyTransactionIdView state push  = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , gravity CENTER
  , onClick push $ const $ Copy state.data.shortOrderId
  ][ 
    textView $
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text "TransactionID"
      , color Color.black700
      , gravity CENTER
      ] <> (FontStyle.body3 TypoGraphy)
    , textView $ 
      [ text state.data.shortOrderId
      , margin $ MarginLeft 3
      , color Color.black700
      , padding $ PaddingBottom 1
      ] <> FontStyle.h3 TypoGraphy
  , imageView
     [ width $ V 16
     , height $ V 16
     , margin $ MarginLeft 3
     , imageWithFallback $ fetchImage FF_ASSET "ny_ic_copy"
     ] 
  ]

bookingStatusBody :: forall w. ST.MetroTicketStatusScreenState -> (Action -> Effect Unit) -> Common.PaymentStatus ->  PrestoDOM (Effect Unit) w
bookingStatusBody state push paymentStatus = 
  let 
    headerImgConfig = {
                          src : fetchImage FF_COMMON_ASSET "ny_ic_chennai_metro"
                        , width : V 41
                        , height : V 41
                        }
                      
  in 
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , weight 1.0
    , orientation VERTICAL
    , margin $ Margin 16 16 16 0
    , visibility if paymentStatus == Common.Failed then GONE else VISIBLE
    ][ scrollView
        [ width MATCH_PARENT
        , height MATCH_PARENT
        ][ linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , gravity CENTER
            , orientation VERTICAL
            , padding $ Padding 10 10 10 10
            , cornerRadius 8.0
            , background Color.white900
            , visibility if paymentStatus == Common.Success then GONE else VISIBLE
            ][ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , gravity CENTER_VERTICAL
                ][ imageView
                    [ width headerImgConfig.width
                    , height headerImgConfig.height
                    , imageWithFallback headerImgConfig.src
                    , margin $ MarginRight 4
                    ]
                  , commonTV push state.data.ticketName Color.black900 (FontStyle.subHeading1 TypoGraphy) 0 LEFT NoAction
                ]
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ] $ DA.mapWithIndex ( \index item ->  keyValueView push state item.key item.val index) state.data.keyValArray
          ]
        ]
      ]

bookingConfirmationActions :: forall w. ST.MetroTicketStatusScreenState -> (Action -> Effect Unit) -> Common.PaymentStatus -> PrestoDOM (Effect Unit) w
bookingConfirmationActions state push paymentStatus = 
  let 
    topBtnText = case paymentStatus of 
      Common.Success -> "View Ticket"
      Common.Failed  -> "Try Again"
      _ -> ""
    topBtnVisibility = case paymentStatus of 
      Common.Success -> VISIBLE
      Common.Failed  -> VISIBLE
      _ -> GONE 
  in
    linearLayout
    [ width MATCH_PARENT
    , gravity CENTER
    , orientation VERTICAL
    , padding $ PaddingBottom 20
    , alignParentBottom "true,-1"
    , background Color.white900
    ][ linearLayout
        [ width MATCH_PARENT
        , height $ V 1
        , background Color.grey900
        ][]
    , PrimaryButton.view (push <<< ViewTicketBtnOnClick ) (tryAgainBtnConfig topBtnText topBtnVisibility)
    , linearLayout
      [ width $ MATCH_PARENT
      , height WRAP_CONTENT
      , onClick push $ const BackPressed
      , gravity CENTER
      ][
        textView $ [ 
          width WRAP_CONTENT
        , height WRAP_CONTENT
        , text "Go Back"
        , color Color.black900
        , gravity CENTER
        , margin $ MarginTop 5
        ] <> (FontStyle.subHeading1 TypoGraphy)
      ]
    ]

paymentStatusHeader :: forall w. ST.MetroTicketStatusScreenState -> (Action -> Effect Unit) -> Common.PaymentStatus -> PrestoDOM (Effect Unit) w
paymentStatusHeader state push paymentStatus = 
  let transcationConfig = getTransactionConfig paymentStatus
      paymentSt = spy "paymentStatus" paymentStatus
      refreshStatusBtn = 
        if(paymentStatus == Common.Pending) then
          (PrimaryButton.view (push <<< RefreshStatusAC) (refreshStatusButtonConfig state))
        else 
          linearLayout [visibility GONE] []
      copyTransactionView = 
        if paymentStatus == Common.Failed then
          copyTransactionIdView state push 
        else
          linearLayout [visibility GONE] []
  in
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , gravity CENTER
    ][ relativeLayout
      [ width $ MATCH_PARENT
      , height $ WRAP_CONTENT
      , gravity CENTER
      ][imageView
        [ width $ MATCH_PARENT
        , height $ V 100
        , visibility if paymentStatus == Common.Success then VISIBLE else GONE
        , imageWithFallback $ fetchImage FF_ASSET "ny_ic_confetti"
        ] 
      , linearLayout
        [ width $ MATCH_PARENT
        , height $ WRAP_CONTENT
        , gravity CENTER
        , margin $ MarginTop 50
        ][ imageView
          [ width $ V 65
          , height $ V 65
          , imageWithFallback transcationConfig.image
          ]
        ]
      ]
      , commonTV push transcationConfig.title Color.black900 (FontStyle.h2 TypoGraphy) 14 CENTER NoAction
      , commonTV push transcationConfig.statusTimeDesc Color.black700 (FontStyle.body3 TypoGraphy) 5 CENTER NoAction
      , copyTransactionView
      , refreshStatusBtn
      , paymentLottieLoader push state
    ]
paymentLottieLoader push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_HORIZONTAL
  ][  lottieAnimationView
      [ height $ if EHC.os == "IOS" then V 170 else V 130
      , width $ V 130
      , padding $ PaddingBottom 80
      , margin (MarginTop ((screenHeight unit)/ 7 - (if EHC.os == "IOS" then 140 else 90)))
      , gravity CENTER
      , id (getNewIDWithTag "paymentLoader")
      , visibility if state.props.paymentStatus == Common.Success  then VISIBLE else GONE
      , afterRender (\action -> do
        void $ pure $ JB.startLottieProcess JB.lottieAnimationConfig {rawJson = (getAssetsBaseUrl FunctionCall) <> "lottie/payment_lottie_loader.json", lottieId = (getNewIDWithTag "paymentLoader"), scaleType="CENTER_CROP", repeat = true, speed = 0.8 }
        push action
        ) (const NoAction)
      ]
    ]

commonTV :: forall w. (Action -> Effect Unit) -> String -> String -> (forall properties. (Array (Prop properties))) -> Int -> Gravity -> Action -> PrestoDOM (Effect Unit) w
commonTV push text' color' fontStyle marginTop gravity' action =
  textView $
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text text'
  , color color'
  , gravity gravity'
  , onClick push $ const action
  , margin $ MarginTop marginTop
  ] <> fontStyle

keyValueView :: (Action -> Effect Unit) -> ST.MetroTicketStatusScreenState -> String -> String -> Int -> forall w . PrestoDOM (Effect Unit) w
keyValueView push state key value index = 
  linearLayout 
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , orientation VERTICAL
  ][ linearLayout
      [ width MATCH_PARENT
      , margin $ Margin 5 12 5 12
      , height $ V 1
      , background Color.grey700
      ][]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginHorizontal 5 5
      ][ textView $ 
        [ text key
        , margin $ MarginRight 16
        , color Color.black700
        ] <> FontStyle.body3 TypoGraphy
      , linearLayout
        [ width MATCH_PARENT
        , gravity RIGHT
        ][ textView $ 
              [ text value
              , color Color.black800
              , onClick push $ const $ 
                  if key == "Booking ID" || key == "Transaction ID" then
                    Copy value 
                  else
                    NoAction -- needs refactoring
              ] <> FontStyle.body6 TypoGraphy
          ]
      ]
  ]


getTransactionConfig :: Common.PaymentStatus -> {image :: String, title :: String, statusTimeDesc :: String}
getTransactionConfig status = 
  case status of
    Common.Success -> {image : fetchImage FF_COMMON_ASSET "ny_ic_green_tick", statusTimeDesc : "Please wait while we generate your ticket", title : "Payment Received!"}
    Common.Pending -> {image : fetchImage FF_COMMON_ASSET "ny_ic_transaction_pending", statusTimeDesc : "Please check back in a few minutes.", title : "Your booking is Pending!"}
    Common.Failed  -> {image : fetchImage FF_COMMON_ASSET "ny_ic_payment_failed", statusTimeDesc : "Please retry booking.", title : "Booking Failed!"}
    Common.Scheduled  -> {image : fetchImage FF_COMMON_ASSET "ny_ic_pending", statusTimeDesc : "", title : ""}


refundInfoTextView :: forall w. PrestoDOM (Effect Unit) w
refundInfoTextView = 
  linearLayout[
    width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ Margin 16 12 16 0
  ][
    linearLayout[
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER_VERTICAL
    ][
      imageView [
        width $ V 16
      , height $ V 16
      , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info"
      ]
    ]
  , textView $ [
      width WRAP_CONTENT
    , height WRAP_CONTENT
    , margin $ MarginLeft 8
    , text "Incase of failure, any money debited will be refunded within 5 - 7 working days."
    , color Color.black700
    ] <> FontStyle.body3 TypoGraphy
  ]