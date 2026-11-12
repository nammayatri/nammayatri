{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.CustomerReferralTrackerScreen.View where

import Common.Types.App
import Debug (spy)
import Screens.CustomerReferralTrackerScreen.ComponentConfig
import Engineering.Helpers.BackTrack (liftFlowBT)
import Animation (fadeIn, translateInYAnim)
import Animation as Anim
import Animation.Config (Direction(..), animConfig)
import Data.Function.Uncurried (runFn1, runFn2, runFn5)
import Components.Calendar.View as Calendar
import Components.ErrorModal as ErrorModal
import Components.PrimaryButton.View as PrimaryButton
import Components.ReferralStepsView as ReferralStepsView
import Components.RequestInfoCard as RequestInfoCard
import Components.PopUpModal as PopUpModal
import Control.Monad.Except (runExceptT, runExcept)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (length, (..), foldl, filter, (!!), null, last, mapWithIndex, any, head)
import Data.Either (Either(..), either)
import Data.Int (ceil, floor, fromNumber, toNumber, fromString)
import Data.Maybe
import Data.String as DS
import Effect (Effect)
import Effect.Aff (launchAff)
import Engineering.Helpers.Commons (getCurrentUTC, flowRunner, getFormattedDate, getNewIDWithTag, screenHeight, getVideoID, getDayName, safeMarginBottom, screenWidth, convertUTCtoISC, liftFlow, formatCurrencyWithCommas)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils 
import JBridge 
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude
import Presto.Core.Types.Language.Flow (doAff, Flow)
import PrestoDOM (textFromHtml, scrollView, frameLayout, shimmerFrameLayout, layoutGravity, Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, LoggableScreen, Visibility(..), alignParentBottom, background, calendar, clickable, color, cornerRadius, fontSize, fontStyle, gravity, height, id, imageView, imageWithFallback, linearLayout, margin, onAnimationEnd, onBackPressed, onClick, onRefresh, onScroll, onScrollStateChange, orientation, padding, relativeLayout, scrollBarX, scrollBarY, stroke, swipeRefreshLayout, text, textSize, textView, visibility, weight, width, onAnimationEnd, alpha, singleLine, rippleColor, lineHeight, maxLines, ellipsize, minHeight)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Services.API (GetRidesSummaryListResp(..))
import Screens as ScreenNames
import Screens.CustomerReferralTrackerScreen.Controller (Action(..), ScreenOutput, eval, getTransactionItems, getDatesList)
import Screens.CustomerReferralTrackerScreen.Types (CustomerReferralTrackerScreenState, DailyEarning(..), Stage(..), PayoutStatus(..), OrderStatus(..))
import Screens.CustomerReferralTrackerScreen.ScreenData (dummyDateItem, dummyEarningsItem)
import Screens.Types as ST
import Services.API as API
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color
import Types.App (defaultGlobalState, GlobalState(..))
import Mobility.Prelude
import Locale.Utils

screen :: CustomerReferralTrackerScreenState -> LoggableScreen Action CustomerReferralTrackerScreenState ScreenOutput
screen initialState =
  { initialState: initialState
  , view: view
  , name: "CustomerReferralTrackerScreen"
  , globalEvents:
      [ ( \push -> do
            _ <-
              launchAff $ flowRunner defaultGlobalState $ runExceptT $ runBackT
                $ do
                    if initialState.props.callEarningsAPI then do
                      let
                        currentDate = getcurrentdate ""
                        datesList = getDatesList currentDate initialState
                        firstDate = maybe "" (\date -> date) $ head datesList
                        lastDate = maybe "" (\date -> date) $ last datesList
                        today = (convertUTCtoISC (getcurrentdate "") "YYYY-MM-DD") 
                        fromDate = if firstDate == "" then today else firstDate 
                        toDate = if lastDate == "" then today else lastDate
                      (API.ReferralEarningsResp referralEarningsResp) <- Remote.getReferralEarningsBT fromDate toDate
                      liftFlowBT $ push $ RideSummaryAPIResponseAction (API.ReferralEarningsResp referralEarningsResp) currentDate datesList
                    else
                      pure unit
                    pure unit
            pure $ pure unit
        )
      ]
  , eval:
      ( \action state -> do
          let
            _ = spy "CustomerReferralTrackerScreenState action" action
          let
            _ = spy "CustomerReferralTrackerScreenState state" state
          eval action state
      )
  , parent : Nothing
  , logWhitelist: initialState.data.config.logWhitelistConfig.customerReferralTrackerScreenLogWhitelist
  }

view :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push $ const BackPressed
    ] $ [ frameLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.blue600 
        ]
        [ Anim.screenAnimationFadeInOut
            $ linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , visibility $ boolToVisibility (not state.props.showShimmer)
                ]
                [ genericHeader push state $ getString CUSTOMER_REFERRAL_TRACKER
                , separatorView Color.grey900 true
                , earningsView push state
                ]
        , if state.props.showShimmer then shimmerView push state else dummyView state
        ]
    , if state.props.calendarState.calendarPopup then Calendar.view (push <<< CalendarAC) (calendarConfig state) else dummyView state
    , if state.data.currentStage == ReferralSteps then referralStepsView push state else dummyView state
    , if state.data.currentStage == UPIDetails && (not state.props.showShimmer) then upiDetailsScreen push state else dummyView state
    , if state.data.currentStage == TransactionHistory then transactionHistoryView push state else dummyView state
    , if state.props.showMenu && state.data.currentStage == Tracker then menuView push state else dummyView state
    , if (any (_ == state.data.orderStatus) [Just CHARGED, Just FAILED]) && state.props.showInfoPopUp then infoCardView push state else dummyView state
    ]

referralStepsView :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
referralStepsView push state = 
  relativeLayout
  [ height MATCH_PARENT 
  , width MATCH_PARENT
  ][ReferralStepsView.view (push <<< ReferralStepsViewAction) (referralStepsViewConfig state)]

earningsView :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
earningsView push state =
  (if isNothing state.data.upiID || state.data.referralCount > 0 then scrollView else linearLayout) 
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , scrollBarY false
  ][ linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onBackPressed push $ const $ BackPressed
    , padding $ Padding 16 16 16 70
    ][ addUPIView push state
     , paymentPendingView push state
     , emptyReferralView push state
     , totalEarningsView push state
     , transactionViewForEarnings push state
     ]
   ]

addUPIView :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
addUPIView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , stroke $ "1," <> Color.pumpkin30
  , background Color.seaShell
  , cornerRadius 16.0
  , margin $ MarginBottom 16
  , gravity CENTER_VERTICAL
  , onClick push $ const $ AddUPIAction
  , visibility $ boolToVisibility $ isNothing state.data.upiID && state.data.orderStatus /= Just PENDING
  ][ linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , gravity CENTER_VERTICAL
      ][ linearLayout
         [ height WRAP_CONTENT
         , width $ V ((screenWidth unit) - 158)
         , orientation VERTICAL
         , margin $ Margin 16 16 16 16
         ][ textView $ 
            [ text $ (getString ADD_UPI_TO_RECEIVE_REWARD) <> " ⚠️"
            , color Color.darkOrange
            ] <> FontStyle.body4 TypoGraphy
          , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , margin $ MarginTop 6
            , cornerRadius 14.0 
            , padding $ Padding 12 2 12 2
            , background Color.orange900
            ][ textView $ 
               [ height $ MATCH_PARENT
               , width $ WRAP_CONTENT
               , text $ (getString $ PAY_TO_ADD_UPI (state.data.config.currency <> (show state.data.registrationAmount))) <> " →"
               , color Color.white900
               ] <> FontStyle.body27 TypoGraphy
            ]
         ]
       , imageView
         [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_add_upi_small"
         , height $ V 84
         , width $ V 84
         , margin $ MarginRight 10
         ]
      ]
  ]

paymentPendingView :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
paymentPendingView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , stroke $ "1," <> Color.dodgerBlue30
  , background $ Color.dodgerBlue8
  , cornerRadius 16.0
  , margin $ MarginBottom 16
  , visibility $ boolToVisibility $ state.data.orderStatus == Just PENDING
  ][linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    ][ linearLayout
       [ height WRAP_CONTENT
       , width $ V ((screenWidth unit) - 164)
       , orientation VERTICAL 
       , margin $ Margin 16 16 16 16
       ][ textView $ 
          [ text $ getString PAYMENT_IN_PROGRESS
          , color Color.blue800
          ] <> FontStyle.body4 TypoGraphy
        , linearLayout
          [ height MATCH_PARENT
          , width WRAP_CONTENT
          , margin $ MarginTop 12
          ][linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , cornerRadius 14.0 
            , padding $ Padding 12 2 12 2
            , background Color.blue800
            , onClick push $ const $ CheckOrderStatus
            ][ textView $ 
              [ text $ getString $ REFRESH_PAYMENT
              , color Color.white900
              ] <> FontStyle.body27 TypoGraphy
            ]
          , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , cornerRadius 14.0 
            , padding $ Padding 12 2 12 2
            , margin $ MarginLeft 8
            , onClick push $ const $ RetryPayment
            , background Color.black800
            , gravity CENTER_VERTICAL
            ][ textView $ 
              [ text $ getString RETRY_STR
              , color Color.white900
              ] <> FontStyle.body27 TypoGraphy
            ]
          ]
        ]
      , imageView
        [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_payment_pending_blue"
        , height $ V 96
        , width $ V 84
        , margin $ MarginRight 16
        ]
      ]
   ]

upiDetailsScreen :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
upiDetailsScreen push state = 
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][ linearLayout  
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    ][ genericHeader push state $ getString UPI_DETAILS
    , separatorView Color.grey900 true
    , if isJust state.data.upiID then upiDetailsView push state else dummyView state
    , if isNothing state.data.upiID then emptyUpiDetailsView push state else dummyView state
    ]
  , upiMenuView push state
  ]

emptyReferralView :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
emptyReferralView push state = 
  let hasVPA = isJust state.data.upiID
  in 
  linearLayout
  [ height $ if hasVPA then MATCH_PARENT else WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginTop $ if hasVPA then 0 else 32
  , gravity $ CENTER
  , visibility $ boolToVisibility $ state.data.referralCount < 1
  ][ imageView 
     [ height $ V 248
     , width MATCH_PARENT
     , imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_refer_now"
     ]
   , textView $ 
     [ text $ getString $ EARN_FOR_EACH_REFERRAL $ state.data.config.currency <> (show state.data.referralRewardAmountPerRide)
     , margin $ MarginTop 28
     , gravity CENTER
     ] <> FontStyle.h3 TypoGraphy
   , textView $ 
     [ height WRAP_CONTENT
     , text $ getString START_REFERRING_NOW
     , width MATCH_PARENT
     , gravity CENTER
     , margin $ MarginTop 8
     ] <> FontStyle.paragraphText TypoGraphy
  ]

upiDetailsView :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
upiDetailsView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ Margin 16 24 16 16 
  , padding $ Padding 16 16 16 16
  , cornerRadius 12.0
  , background Color.blue600
  , orientation VERTICAL
  , clickable true
  ][ textView $ 
    [ text $ "UPI ID"
    , color Color.black700
    ] <> FontStyle.body3 TypoGraphy
  , textView $ 
    [ text $ fromMaybe "" state.data.upiID
    , color Color.black800
    ] <> FontStyle.body30 TypoGraphy
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 12 8 12 8
    , margin $ MarginTop 16
    , cornerRadius 6.0
    , background Color.white900
    ][ textView $ 
        [ text $ getString WILL_GET_REFERRAL_TO_UPI_ID
        , color Color.black700
        ] <> FontStyle.body3 TypoGraphy
    ]
  ]

emptyUpiDetailsView :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
emptyUpiDetailsView push state = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.blue600
  , gravity CENTER_VERTICAL
  , orientation VERTICAL
  , clickable true
  ][ linearLayout[weight 1.0, width WRAP_CONTENT][] 
   , imageView
     [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_add_upi_details"
     , height $ V 264
     , width MATCH_PARENT
     , margin $ Margin 24 0 24 24
     ]
   , textView $ 
     [ text $ getString ADD_UPI_TO_RECEIVE_REWARD
     , color $ Color.black800
     , width MATCH_PARENT
     , gravity CENTER
     , margin $ MarginHorizontal 24 24
     ] <> FontStyle.h3 TypoGraphy
   , linearLayout[weight 1.0, width WRAP_CONTENT][]
   , addUPIButton push state
  ]

addUPIButton :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
addUPIButton push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding $ Padding 16 16 16 16
  , stroke $ "1," <> Color.grey900
  , background Color.white900
  , alignParentBottom "true,-1"
  ][ PrimaryButton.view (push <<< EmptyUPIPrimaryAction) (addUPIDetailsButtonConfig state)]

upiMenuView :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
upiMenuView push state = 
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.black9000
  , gravity BOTTOM
  , clickable true
  , onClick push $ const $ BackPressed
  , visibility $ boolToVisibility $ state.props.showUPIOptions
  ][ upiOptionView push state
   , deleteUPIView push state
  ]

upiOptionView :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
upiOptionView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadii $ Corners 24.0 true true false false
  , background Color.white900
  , padding $ Padding 16 24 16 32
  , visibility $ boolToVisibility state.props.showUPIOptions
  , clickable true
  ][ linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ PaddingVertical 4 4
    , onClick push $ const $ ShowDeleteUPI
    ][ imageView
      [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_bin_black"
      , height $ V 24
      , width $ V 24
      , margin $ MarginRight 12
      ]
    , textView $ 
      [ text $ getString DELETE
      , color Color.black800
      ] <> FontStyle.subHeading2 TypoGraphy
    , linearLayout[height WRAP_CONTENT, weight 1.0][]
    , imageView
      [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_chevron_right_black_700"
      , height $ V 24
      , width $ V 24
      ]
    ]
  ]

deleteUPIView :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
deleteUPIView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadii $ Corners 24.0 true true false false
  , background Color.white900
  , padding $ Padding 16 24 16 24
  , stroke $ "1," <> Color.grey900
  , visibility $ boolToVisibility $ state.props.showDeleteUPIView
  , clickable true
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      , orientation VERTICAL
      ][ textView $ 
        [ text $ getString DELETE_UPI_ID
        , color Color.black800
        , margin $ MarginBottom 12
        ] <> FontStyle.h2 TypoGraphy
      , textView $
        [ text $ getString $ CONFIRM_DELETE_UPI_ID $ fromMaybe "" state.data.upiID
        , color Color.black600
        , gravity CENTER
        , margin $ Margin 20 0 20 24
        ] <> FontStyle.body20 TypoGraphy
      , PrimaryButton.view (push <<< DeleteUPIAction) (deleteUPIButtonConfig state)
      , linearLayout
        [ height $ V 48
        , width MATCH_PARENT
        , gravity CENTER
        , onClick push $ const $ BackPressed
        ][ textView $ 
            [ text $ getString CANCEL
            , color Color.black650
            ] <> FontStyle.subHeading2 TypoGraphy
        ]
      ]
  ]

infoCardView :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
infoCardView push state = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.black9000
  , gravity CENTER
  , clickable false
  ][ PopUpModal.view (push <<< PopUpModalAction) (if state.data.orderStatus == Just CHARGED then paymentSuccessConfig state else paymentFailedConfig state) ]

shimmerView :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
shimmerView push state =
  shimmerFrameLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.blue600
    ]
    [ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        [ linearLayout
            [ height $ V 350
            , width MATCH_PARENT
            , background Color.greyDark
            , cornerRadius 12.0
            , orientation VERTICAL
            ]
            []
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , margin $ MarginTop 24
            ]
            ( map
                ( \_ ->
                    linearLayout
                      [ width MATCH_PARENT
                      , height $ V 60
                      , orientation VERTICAL
                      , margin $ MarginVertical 10 10
                      , background Color.greyDark
                      , cornerRadius 12.0
                      ][]
                )
                (1 .. 3)
            )
        ]
    ]

dummyView :: forall w. CustomerReferralTrackerScreenState -> PrestoDOM ( Effect Unit) w
dummyView state = 
  linearLayout
  [ height $ V 0
  , width $ V 0
  ][]

transactionViewForEarnings :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
transactionViewForEarnings push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , margin $ MarginTop 24
  , visibility $ boolToVisibility $ state.data.referralCount > 0
  ][linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , margin $ Margin 8 0 8 8
    ][ textView
        $ [ text $ getString PAYOUT_HISTORY
          , weight 1.0
          , color Color.black800
          ]
        <> FontStyle.h2 TypoGraphy
    , calendarView push state
    ]
   , historyViewForEarnings push state
 ]

historyViewForEarnings :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
historyViewForEarnings push state =
  let
    historyItems = state.data.currPayoutHistory
  in
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  ][emptyPayoutsView state
   , linearLayout
     [ width MATCH_PARENT
     , height WRAP_CONTENT
     , orientation VERTICAL
     , cornerRadius 12.0
     , background Color.white900
     , stroke $ "1," <> Color.grey900
     , visibility $ boolToVisibility $ not $ null state.data.currPayoutHistory
     ](mapWithIndex (\index item -> historyViewItemForEarnings push item state index) historyItems)
  ]

getDailyEarnings :: Array DailyEarning -> Int
getDailyEarnings list =
  foldl
    ( \acc record -> acc + record.earnings) 0 list

menuView :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
menuView push state = 
  linearLayout  
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][ genericHeader push state $ getString CUSTOMER_REFERRAL_TRACKER 
   , linearLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , background Color.black9000
     , clickable true
     , onClick push $ const $ BackPressed 
     ][ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity RIGHT
        ][ linearLayout
           [ height WRAP_CONTENT
           , width WRAP_CONTENT
           , orientation VERTICAL
           , background Color.white900
           , cornerRadius 8.0
           , margin $ Margin 0 8 16 0
           , padding $ Padding 16 8 16 8
           ][ linearLayout
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , padding $ PaddingVertical 14 14
              , onClick push $ const $ ShowReferralSteps
              ][ imageView 
                 [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_help_circle_outline"
                 , height $ V 20
                 , width $ V 20
                 , margin $ MarginRight 8
                 ]
               , textView $ 
                 [ text $ getString HOW_TO_EARN
                 , color Color.black800
                 ] <> FontStyle.body1 TypoGraphy
               ]
            ,  linearLayout
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , padding $ PaddingVertical 14 14
              , onClick push $ const $ ShowUPIDetails
              ][ imageView 
                 [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_bhim_black"
                 , height $ V 20
                 , width $ V 20
                 , margin $ MarginRight 8
                 ]
               , textView $ 
                 [ text $ getString UPI_DETAILS
                 , color Color.black800
                 ] <> FontStyle.body1 TypoGraphy
              ]
           ]
        ]
     ]
  ]

calendarView :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
calendarView push state =
  let
    selectedDate = state.props.date

    dateToShow =
      if DS.null selectedDate || convertUTCtoISC selectedDate "YYYY-MM-DD" == getcurrentdate "" then
        getString TODAY
      else
        convertUTCtoISC selectedDate "Do MMM"
  in
    linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , cornerRadius 100.0
      , background Color.white900
      , gravity CENTER_VERTICAL
      , stroke $ "1," <> Color.grey900
      , padding $ Padding 12 4 12 4
      , onClick push $ const $ ShowCalendarPopup
      , visibility GONE
      ]
      [ imageView
          [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_calendar_unfilled_blue,"
          , height $ V 16
          , width $ V 16
          , margin $ MarginRight 4
          ]
      , textView
          $ [ text dateToShow
            , color Color.black700
            ]
          <> FontStyle.tags TypoGraphy
      , imageView
          [ imageWithFallback
              $ fetchImage FF_ASSET
                  if state.props.calendarState.calendarPopup then
                    "ny_ic_chevron_up"
                  else
                    "ny_ic_chevron_down"
          , height $ V 12
          , width $ V 12
          , margin $ MarginLeft 6
          ]
      ]

transactionHistoryView :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
transactionHistoryView push state = 
  let transactionItems = getTransactionItems state 
      selectedItem = fromMaybe dummyEarningsItem state.data.selectedItem
  in
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , orientation VERTICAL
  , clickable true
  , gravity CENTER_HORIZONTAL
  ][ genericHeader push state $ getString TRANSACTION_DETAILS
   , separatorView Color.grey900 true 
   , imageView 
     [ imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_check_circle_green"
     , height $ V 114
     , width $ V 114
     , margin $ MarginVertical 60 24
     ]
   , textView $
     [ text $ getString $ RECEIVED $ state.data.config.currency <> (show selectedItem.earnings)
     , color Color.black800
     ] <> FontStyle.h2 TypoGraphy
   , linearLayout
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , margin $ Margin 16 40 16 0
     , background Color.blue600
     , cornerRadius 10.0
     , padding $ Padding 24 20 24 20
     , visibility $ boolToVisibility $ not $ null transactionItems
     ][ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ](mapWithIndex (\index item ->
                          linearLayout
                          [ height WRAP_CONTENT
                          , width MATCH_PARENT
                          , orientation VERTICAL
                          ][ transactionItem push state item.key item.value
                           , linearLayout
                              [ height $ V 1
                              , width MATCH_PARENT
                              , background Color.white900
                              , margin $ MarginVertical 16 16
                              , visibility $ boolToVisibility $ (index /= (1))
                              ][]
                          ]
                       ) transactionItems
           )
     ]
  ]

transactionItem :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> String -> String -> PrestoDOM (Effect Unit) w
transactionItem push state key value = 
  let value' = if key == "Order ID" then ((DS.take 8 value) <> "...") else value
  in
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][ textView $ 
     [ text $ key
     , color Color.black700
     ] <> FontStyle.body3 TypoGraphy
   , textView $ 
     [ height WRAP_CONTENT
     , weight 1.0
     , gravity RIGHT
     , singleLine true
     , text $ value'
     , color Color.black900
     , ellipsize true
     , lineHeight "18"
     ] <> FontStyle.body6 TypoGraphy
   , linearLayout
     [ height MATCH_PARENT
     , width WRAP_CONTENT
     , gravity CENTER_VERTICAL
     , padding $ PaddingLeft 4
     , visibility $ boolToVisibility $ key == "Order ID"
     , onClick push $ const $ Copy value
     ][ imageView
        [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_copy_blue"
        , height $ V 16
        , width $ V 16
        ]
     ]
  ]

totalEarningsView :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
totalEarningsView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background Color.white900
  , padding $ Padding 12 16 12 12
  , cornerRadius 12.0
  , stroke $ "1," <> Color.grey900
  , orientation VERTICAL
  , visibility $ boolToVisibility $ state.data.referralCount > 0
  ][linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background Color.white900
    , gravity CENTER
    ][ linearLayout
       [ height MATCH_PARENT
       , width WRAP_CONTENT
       , gravity START
       ][ imageView
          [ width $ V 32
          , height $ V 32
          , margin $ MarginTop 8
          , gravity LEFT
          , imageWithFallback $ fetchImage COMMON_ASSET $ if state.props.weekIndex == 0 then "ny_ic_chevron_left_light_grey" else "ny_ic_chevron_left_light"
          , onClick push $ const $ LeftChevronClicked state.props.weekIndex
          , clickable $ state.props.weekIndex > 0
          ]
       ]
    , linearLayout[height WRAP_CONTENT, weight 1.0][]
    , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation VERTICAL
      , gravity CENTER
      ][textView $ 
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , color Color.black700
        , text $ convertUTCtoISC state.data.totalEarningsData.fromDate "DD MMM"
                  <> ( if DS.null state.data.totalEarningsData.toDate then
                    ""
                    else
                    " - " <> convertUTCtoISC state.data.totalEarningsData.toDate "DD MMM"
                    )
        ] <> FontStyle.body3 TypoGraphy
      , textView $ 
        [ text $ getString REFERRAL_BONUS_EARNED
        , color Color.black700
        ] <> FontStyle.body3 TypoGraphy
      , textView $ 
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text $ (state.data.config.currency) <> (formatCurrencyWithCommas (show state.data.totalEarningsData.totalEarnings))
        , color Color.black900
        ] <> FontStyle.title2 TypoGraphy
      ]
    , linearLayout[height WRAP_CONTENT, weight 1.0][]
    , linearLayout
       [ height MATCH_PARENT
       , width WRAP_CONTENT
       , gravity START
       ][ imageView
          [ width $ V 32
          , height $ V 32
          , imageWithFallback $ fetchImage COMMON_ASSET $ if state.props.weekIndex == 3 then "ny_ic_chevron_right_light_grey" else "ny_ic_chevron_right"
          , margin $ MarginTop 8
          , gravity RIGHT
          , onClick push $ const $ RightChevronClicked state.props.weekIndex
          , clickable $ state.props.weekIndex < 3
          ] 
       ] 
    ]
  , barGraphView push state
  , linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , margin $ MarginTop 12
    , background Color.grey800
    ][]
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER
    , padding $ PaddingVertical 15 15
    ][ linearLayout
       [ height WRAP_CONTENT
       , gravity CENTER
       , width $ V (((screenWidth unit) - 75) / 2)
       ][ textView $ 
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ getString ACTIVATED
          , color Color.black700
          , margin $ MarginRight 8
          ] <> FontStyle.body3 TypoGraphy
        , textView $ 
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , color Color.black800
          , text $ show $ state.data.totalEarningsData.totalActivations
          ] <> FontStyle.body20 TypoGraphy
       ]
    , linearLayout
      [ height MATCH_PARENT
      , width $ V 1
      , background Color.grey800
      ][]
    , linearLayout
      [ height WRAP_CONTENT
      , gravity CENTER
      , width $ V (((screenWidth unit) - 75) / 2)
      ][ textView $ 
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ getString REFERRALS
          , color Color.black700
          , margin $ MarginRight 8
          ] <> FontStyle.body3 TypoGraphy
      , textView $ 
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , color Color.black800
        , text $ show (state.data.totalEarningsData.totalReferrals)
        ] <> FontStyle.body20 TypoGraphy
      ]
    ]
  , linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER
    , margin $ MarginTop 6
    ] (map (\index -> dotView push index state) [ 0, 1, 2, 3 ])
  ]


genericHeader :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> String -> PrestoDOM (Effect Unit) w
genericHeader push state title = 
  let suffixImageVisibility = ((any (_ == state.data.currentStage) [Tracker, ReferralSteps]) || (state.data.currentStage == UPIDetails && isJust state.data.upiID))
  in
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , background Color.white900
  ][ headerIconView push BackPressed (fetchImage COMMON_ASSET "ny_ic_chevron_left_black") true
   , textView $ 
     [ text title
     , width $ V ((screenWidth unit) - 112)
     , height WRAP_CONTENT
     , minHeight 24
     , margin $ MarginVertical 14 14
     , gravity CENTER_VERTICAL
     , singleLine false
     , padding $ PaddingBottom 3
     , color Color.darkCharcoal
     ] <> FontStyle.h3 TypoGraphy
   , headerIconView push HeaderSuffixImgOnClick (fetchImage COMMON_ASSET "ny_ic_more_vertical") suffixImageVisibility
  ]

headerIconView :: forall w. (Action -> Effect Unit) -> Action -> String -> Boolean ->PrestoDOM (Effect Unit) w 
headerIconView push action image visible = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , onClick push $ const $ action
  , margin $ Margin 8 8 8 8
  , cornerRadius 30.0
  , rippleColor Color.rippleShade
  , visibility $ boolToVisibility visible
  ][imageView
    [ width $ V 24
    , height $ V 24
    , margin $ Margin 8 8 8 8
    , imageWithFallback image
    ]
  ]

barGraphView :: forall w. (Action -> Effect Unit) -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
barGraphView push state =
  let
    currWeekMaxEarning = if state.props.currentWeekMaxEarning > 0 then state.props.currentWeekMaxEarning else 1500
  in
  relativeLayout
  [ height $ V 150
  , width MATCH_PARENT
  , margin $ MarginTop 20
  ][ dottedLineView push 4 currWeekMaxEarning state.data.config.currency
   , dottedLineView push 37 ((currWeekMaxEarning * 2) / 3) state.data.config.currency
   , dottedLineView push 70 (currWeekMaxEarning / 3) state.data.config.currency
   , linearLayout
     [ height $ V 2
     , width MATCH_PARENT
     , background Color.grey900
     , margin $ MarginTop 106
     ][]
   , linearLayout
     [ height $ V 150
     , width MATCH_PARENT
     , background Color.transparent
     , orientation HORIZONTAL
     , margin $ MarginBottom 8
     , gravity BOTTOM
     ](mapWithIndex (\index item -> (barView push index item state)) state.data.currWeekData)
  ]

dottedLineView :: forall w. (Action -> Effect Unit) -> Int -> Int -> String -> PrestoDOM (Effect Unit) w
dottedLineView push margintop earnings currency =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ MarginTop margintop
  , gravity CENTER
  ][ imageView
      [ height $ V 2
      , width MATCH_PARENT
      , margin $ MarginRight 8
      , imageWithFallback $ fetchImage COMMON_ASSET "ny_ic_dotted_line"
      , weight 1.0
      ]
  , textView $ 
    [ height $ WRAP_CONTENT
    , width $ WRAP_CONTENT
    , gravity RIGHT
    , color Color.black500
    , text $ currency <> formatCurrencyWithCommas (show earnings)
    ] <> FontStyle.body31 TypoGraphy
  ]
  
dotView :: forall w. (Action -> Effect Unit) -> Int -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
dotView push index state =
  linearLayout
  [ height $ V 6
  , width $ V 6
  , cornerRadius 12.0
  , background $ if index == state.props.weekIndex then Color.black800 else Color.grey900
  , margin $ MarginHorizontal 2 2
  ][]

emptyPayoutsView :: forall w. CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
emptyPayoutsView state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , padding $ Padding 16 32 16 32
  , cornerRadius 12.0
  , background Color.white900
  , stroke $ "1," <> Color.grey900
  , visibility $ boolToVisibility $ null $ state.data.currPayoutHistory
  ][textView $ 
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER
    , text $ getString NO_ACTIVATED_REFERRAL
    , color Color.black900
    , margin $ MarginBottom 8
    ] <> FontStyle.body25 TypoGraphy
  , textView $ 
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , gravity CENTER
    , text $ getString NO_ACTIVE_REFERRAL_ON_DATE
    , color Color.black700
    ] <> FontStyle.paragraphText TypoGraphy
  ]

historyViewItemForEarnings :: forall w. (Action -> Effect Unit) -> DailyEarning -> CustomerReferralTrackerScreenState -> Int -> PrestoDOM (Effect Unit) w
historyViewItemForEarnings push item state index =
  let priceColor = if item.status == Success then  Color.green900 else Color.black800
  in
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding $ Padding 16 16 16 0
  , onClick push $ const $ SelectEarning item
  ][linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , margin $ MarginBottom 12
    ][linearLayout
      [ height MATCH_PARENT
      , width WRAP_CONTENT
      , orientation VERTICAL
      ][ textView $ 
         [ text $ convertUTCtoISC item.earningDate "DD/MM/YYYY"
         , color $ Color.black900
         ] <> FontStyle.body3 TypoGraphy
       , linearLayout[weight 1.0, height WRAP_CONTENT][]
       , payoutStatusItem push item
       ]
     , linearLayout[weight 1.0, height MATCH_PARENT][]
     , linearLayout
       [ height MATCH_PARENT
       , width WRAP_CONTENT
       , orientation VERTICAL
       , gravity RIGHT
       ][ textView $ 
          [ text $ state.data.config.currency <> formatCurrencyWithCommas (show $ item.earnings)
          , color priceColor
          , gravity RIGHT
          ] <> FontStyle.body6 TypoGraphy
        , textView $ 
          [ text $ (getString ACTIVATED) <> ": " <> (show $ item.activatedItems)
          , color Color.black700
          , gravity RIGHT
          ] <> FontStyle.body21 TypoGraphy
       ]
     ]
    , separatorView Color.grey700 $ index /= ((length state.data.earningHistoryItems) - 1) 
  ]

payoutStatusItem :: forall w. (Action -> Effect Unit) -> DailyEarning -> PrestoDOM (Effect Unit) w
payoutStatusItem push item = 
  let textColor = if item.status == Verifying then Color.orange900 else if item.status == Processing then Color.blue800 else if item.status == Failed then Color.red900 else Color.black700
      color' = if item.status == Verifying then Color.pumpkin10 else if item.status == Processing then Color.blue600 else if item.status == Failed then Color.indianRed8 else Color.white900
  in
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , margin $ MarginTop 4
  ][linearLayout
    [ height WRAP_CONTENT
    , width WRAP_CONTENT
    , gravity CENTER_VERTICAL
    , padding $ Padding 8 2 8 2
    , cornerRadius 12.0
    , background color'
    , visibility $ boolToVisibility $ item.status /= Success
    ][ textView $ 
      [ text $ show item.status
      , color textColor
      ] <> FontStyle.body17 TypoGraphy 
    ]
   , textView $ 
     [ text $ getString PAID
     , color Color.black700
     , visibility $ boolToVisibility $ item.status == Success
     ] <> FontStyle.captions TypoGraphy
   , textView $ 
     [ text $ getString CONTACT_SUPPORT
     , height MATCH_PARENT
     , gravity CENTER_VERTICAL
     , color Color.blue900
     , margin $ MarginLeft 8
     , visibility $ boolToVisibility $ item.status == Failed
     ] <> FontStyle.captions TypoGraphy
  ]

barView :: forall w. (Action -> Effect Unit) -> Int -> DailyEarning -> CustomerReferralTrackerScreenState -> PrestoDOM (Effect Unit) w
barView push index item state =
  let
    selectedIndex = state.props.selectedBarIndex

    setMargin = case index of
      0 -> MarginHorizontal 3 6
      6 -> MarginHorizontal 3 (screenWidth unit / 8)
      _ -> MarginHorizontal 6 6
  in
    linearLayout
    [ height $ WRAP_CONTENT
    , width $ WRAP_CONTENT
    , margin setMargin
    , weight 1.0
    , orientation VERTICAL
    ][linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , onClick push $ const $ BarViewSelected index
      , gravity CENTER
      ][ PrestoAnim.animationSet [ Anim.translateInYAnim $ animConfig { duration = 1000 + (ceil item.percentLength), fromY = 200 + (ceil item.percentLength) } ] $ 
         linearLayout
         [ height WRAP_CONTENT
         , width WRAP_CONTENT
         ][ linearLayout
            [ height $ V if item.percentLength > 0.0 then (ceil item.percentLength) else 1
            , width $ V (screenWidth unit / 16)
            , background if selectedIndex < 0 || selectedIndex == index then Color.blue800 else Color.blue100
            , cornerRadius 4.0
            ][]
          ]
      ]
      , textView $ 
        [ height $ WRAP_CONTENT
        , width $ MATCH_PARENT
        , text $ DS.drop 8 item.earningDate
        , gravity CENTER
        , color Color.black700
        ] <> FontStyle.body3 TypoGraphy
      , textView $ 
        [ height $ WRAP_CONTENT
        , width $ MATCH_PARENT
        , text (fromMaybe "" (state.props.weekDay !! index))
        , gravity CENTER
        , singleLine true
        , color Color.black700
        , margin $ if (getLanguageLocale languageKey == "KN_IN") then MarginBottom 0 else MarginBottom 2
        ] <> FontStyle.body3 TypoGraphy
    ]

separatorView :: forall w. String -> Boolean -> PrestoDOM (Effect Unit) w
separatorView color' visible =
  linearLayout
  [ height $ V 1
  , width MATCH_PARENT
  , background $ color'
  , visibility $ boolToVisibility visible
  ][]

tagview :: String -> forall w. PrestoDOM (Effect Unit) w
tagview name =
  imageView
    [ width (V 16)
    , height (V 16)
    , imageWithFallback name
    , margin $ Margin 4 4 4 3
    ]
