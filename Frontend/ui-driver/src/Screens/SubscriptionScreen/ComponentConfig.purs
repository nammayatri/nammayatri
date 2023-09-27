{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.SubscriptionScreen.ComponentConfig where

import Language.Strings
import PrestoDOM

import Animation as Anim
import Animation.Config as AnimConfig
import Common.Types.App (LazyCheck(..))
import Common.Types.App (PaymentStatus(..))
import Components.Banner as Banner
import Components.DueDetailsList (DueDetailsListState)
import Components.OptionsMenu as OptionsMenuConfig
import Components.PopUpModal as PopUpModalConfig
import Components.PrimaryButton as PrimaryButton
import Data.Array as DA
import Data.Function.Uncurried (runFn1)
import Data.Int as DI
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Maybe as Mb
import Data.Semigroup ((<>))
import Data.String (Pattern(..), Replacement(..), replace, split)
import Debug (spy)
import Engineering.Helpers.Commons (convertUTCtoISC, screenWidth)
import Engineering.Helpers.Commons as EHC
import Font.Style (Style(..))
import Helpers.Utils as HU
import JBridge as JB
import Language.Types (STR(..))
import Prelude (map, not, show, unit, ($), (&&), (*), (+), (/), (/=), (==), (>))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.PaymentHistoryScreen.Transformer (getAutoPayStageData)
import Screens.SubscriptionScreen.Transformer (decodeOfferPlan, getPromoConfig)
import Screens.Types (AutoPayStatus(..), OptionsMenuState(..), PlanCardConfig(..), SubscribePopupType(..), OfferBanner)
import Screens.Types as ST
import Services.API (FeeType(..), OfferEntity(..))
import Storage (KeyStore(..), getValueToLocalStore)
import Styles.Colors as Color

clearDueButtonConfig :: ST.SubscriptionScreenState -> PrimaryButton.Config
clearDueButtonConfig state = let
    config = PrimaryButton.config
    buttonText = 
      case state.data.myPlanData.manualDueAmount > 0.0, state.data.myPlanData.autoPayStatus, isJust state.data.orderId of
        true, ACTIVE_AUTOPAY, true  -> (getString RETRY_STR) <>  if state.props.myPlanProps.overDue then (getString CLEAR_DUES) else (getString CLEAR_MANUAL_DUES) <> "(₹" <> HU.getFixedTwoDecimals state.data.myPlanData.manualDueAmount <> ")"
        true, ACTIVE_AUTOPAY, false  -> if state.props.myPlanProps.overDue then (getString CLEAR_DUES) else (getString CLEAR_MANUAL_DUES) <> "(₹" <> HU.getFixedTwoDecimals state.data.myPlanData.manualDueAmount <> ")"
        true, _, true  -> (getString RETRY_AUTOPAY) <> " & " <>  (getString CLEAR_DUES) <> " (₹" <> HU.getFixedTwoDecimals state.data.myPlanData.manualDueAmount <> ")" 
        true, _, false  -> (getString SETUP_AUTOPAY_STR) <> " & " <>  (getString CLEAR_DUES) <> " (₹" <> HU.getFixedTwoDecimals state.data.myPlanData.manualDueAmount <> ")" 
        false,_, _ -> getString SETUP_AUTOPAY_STR
    primaryButtonConfig' = config 
      { textConfig { text = buttonText }
      , isClickable = true
      , alpha = if true then 1.0 else 0.6
      , height = (V 48)
      , cornerRadius = 8.0
      , id = "SetupAutoPayPrimaryButton"
      , enableLoader = JB.getBtnLoader "SetupAutoPayPrimaryButton"
      , margin = (Margin 16 12 16 12)
      }
  in primaryButtonConfig'

retryPaymentButtonConfig :: ST.SubscriptionScreenState -> PrimaryButton.Config
retryPaymentButtonConfig state =
  let
    layouts = runFn1 JB.getLayoutBounds $ EHC.getNewIDWithTag $ "RetryPaymentPrimaryButton" <> "_buttonLayout"
  in
    PrimaryButton.config
      { textConfig
        { text = getString RETRY_PAYMENT_STR
        , width = WRAP_CONTENT
        , gravity = LEFT
        , height = WRAP_CONTENT
        , textStyle = case getValueToLocalStore LANGUAGE_KEY of
                      "KN_IN" -> Body3
                      _ -> Body4
        , weight = Mb.Just 1.0
        }
      , height = WRAP_CONTENT
      , gravity = CENTER
      , cornerRadius = 8.0
      , padding = case getValueToLocalStore LANGUAGE_KEY of
                      "KN_IN" -> Padding 10 10 10 10
                      _ -> Padding 10 8 10 10
      , margin = MarginLeft 0
      , isSuffixImage = true
      , suffixImageConfig
        { imageUrl = "ny_ic_arrow_right_yellow," <> (HU.getAssetStoreLink FunctionCall) <> "ny_ic_arrow_right_yellow.png"
        , height = V 16
        , width = V 16
        , margin = (MarginLeft 5)
        }
      , id = "RetryPaymentPrimaryButton"
      , enableButtonLayoutId = true
      , enableLoader = JB.getBtnLoader "RetryPaymentPrimaryButton"
      , lottieConfig
        { width = V $ DI.ceil $ (DI.toNumber layouts.width) * 0.8
        , height = V $ DI.ceil $ (DI.toNumber layouts.height) * 0.8
        , lottieURL = (HU.getAssetsBaseUrl FunctionCall) <> "lottie/primary_button_loader.json"
        }
      }

checkStatusButtonConfig :: ST.SubscriptionScreenState -> PrimaryButton.Config
checkStatusButtonConfig state = 
    PrimaryButton.config
      { textConfig
        { text = getString REFRESH_STR
        , gravity = LEFT
        , height = WRAP_CONTENT
        , textStyle = Body4
        , weight = Mb.Just 1.0
        , color = Color.black900
        }
      , height = WRAP_CONTENT
      , gravity = CENTER
      , cornerRadius = 8.0
      , padding = Padding 10 7 10 9
      , margin = MarginLeft 0
      , isPrefixImage = true
      , stroke = "1," <> Color.black900
      , background = Color.yellow800
      , prefixImageConfig
        { imageUrl = "ny_ic_refresh_unfilled," <> (HU.getAssetStoreLink FunctionCall) <> "ny_ic_refresh_unfilled.png"
        , height = V 16
        , width = V 16
        , margin = case getValueToLocalStore LANGUAGE_KEY of
                      "KN_IN" -> (Margin 0 0 5 0)
                      _ -> (Margin 0 1 5 0)
        , animation = [Anim.rotateAnim (AnimConfig.rotateAnimConfig state.props.refreshPaymentStatus)]
        }
      , id = "CheckStatusButtonLayout"
      }

switchPlanButtonConfig :: ST.SubscriptionScreenState -> PrimaryButton.Config
switchPlanButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getVarString SWITCH_TO [getSelectedAlternatePlan state]) }
      , height = (V 48)
      , cornerRadius = 8.0
      , margin = (Margin 0 16 0 16)
      , id = "SwitchPlanPrimaryButton"
      , visibility = if state.data.myPlanData.planEntity.id == state.props.managePlanProps.selectedPlanItem.id then GONE else VISIBLE
      }
  in primaryButtonConfig'

resumeAutopayButtonConfig :: ST.SubscriptionScreenState -> PrimaryButton.Config
resumeAutopayButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = (getString SETUP_AUTOPAY_STR) }
      , isClickable = true
      , alpha = if true then 1.0 else 0.6
      , height = (V 48)
      , cornerRadius = 8.0
      , margin = (Margin 16 16 16 16)
      , id = "ResumeAutopayPrimaryButton"
      }
  in primaryButtonConfig'

joinPlanButtonConfig :: ST.SubscriptionScreenState -> PrimaryButton.Config
joinPlanButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = case (getSelectedJoiningPlan state) of 
                                Mb.Just _ -> (getString PAY_TO_JOIN_THIS_PLAN)
                                Mb.Nothing -> (getString TAP_A_PLAN_TO_VIEW_DETAILS) }
      , isClickable = if isNothing state.props.joinPlanProps.selectedPlanItem then false else true
      , alpha = if isNothing state.props.joinPlanProps.selectedPlanItem then 0.6 else 1.0
      , height = (V 48)
      , cornerRadius = 8.0
      , id = "JoinPlanPrimaryButton"
      , enableLoader = (JB.getBtnLoader "JoinPlanPrimaryButton")
      , margin = (MarginBottom 16)
      }
  in primaryButtonConfig'

popupModalConfig :: ST.SubscriptionScreenState -> PopUpModalConfig.Config
popupModalConfig state = let
    config = PopUpModalConfig.config
    popUpConf' = config {
      cornerRadius = Corners 15.0 true true true true
      , margin = MarginHorizontal 16 16
      , padding = Padding 16 16 16 16
      , gravity = CENTER
      , backgroundColor =  Color.black9000
      , backgroundClickable = DA.any (_ == state.props.popUpState) [Mb.Just SupportPopup, Mb.Just FailedPopup]
      , buttonLayoutMargin = MarginBottom 0
      , optionButtonOrientation = if state.props.popUpState == Mb.Just SupportPopup then "VERTICAL" else "HORIZONTAL"
    ,primaryText {
        text = case state.props.popUpState of
                  Mb.Just SuccessPopup -> (getString PLAN_ACTIVATED_SUCCESSFULLY)
                  Mb.Just FailedPopup -> (getString PAYMENT_FAILED)
                  Mb.Just DuesClearedPopup -> (getString DUES_CLEARED_SUCCESSFULLY)
                  Mb.Just CancelAutoPay -> (getString NOT_PLANNING_TO_TAKE_RIDES)
                  Mb.Just SwitchedPlan -> (getString PLAN_SWITCHED_TO) <> (if state.data.managePlanData.currentPlan.title == getString DAILY_UNLIMITED then getString DAILY_UNLIMITED else getString DAILY_PER_RIDE)
                  Mb.Just SupportPopup -> ""
                  Mb.Just PaymentSuccessPopup -> ""
                  Mb.Nothing -> ""
      , margin = Margin 16 16 16 0
      , visibility = if state.props.popUpState == Mb.Just SupportPopup then GONE else VISIBLE
      , color = Color.black800
      , textStyle = Heading2
     },
      option1 {
        text = case state.props.popUpState of
                  Mb.Just SuccessPopup -> getString GOT_IT
                  Mb.Just FailedPopup -> getString RETRY_PAYMENT_STR
                  Mb.Just DuesClearedPopup -> getString GOT_IT
                  Mb.Just SwitchedPlan -> getString GOT_IT
                  Mb.Just CancelAutoPay -> getString PAUSE_AUTOPAY_STR
                  Mb.Just SupportPopup -> getString CALL_SUPPORT
                  Mb.Just PaymentSuccessPopup -> getString PAYMENT_SUCCESSFUL
                  Mb.Nothing -> ""
      , color = Color.yellow900
      , background = Color.black900
      , visibility =true
      , margin = MarginTop 16
      , width = case state.props.popUpState of
                  Mb.Just SupportPopup -> MATCH_PARENT
                  _                    -> (V 156)
      , image {
          imageUrl = "ny_ic_phone_filled_yellow,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_phone_filled_yellow.png"
          , height = (V 16)
          , width = (V 16)
          , visibility = if Mb.Just SupportPopup == state.props.popUpState then VISIBLE else GONE
          , margin = MarginRight 8
        }
      },
      coverImageConfig {
        imageUrl =  case state.props.popUpState of
          Mb.Just SuccessPopup -> "ny_ic_green_tick,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_driver_near.png"
          Mb.Just SwitchedPlan -> "ny_ic_green_tick,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_driver_near.png"
          Mb.Just FailedPopup -> "ny_failed,"
          Mb.Just DuesClearedPopup -> "ny_ic_green_tick,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_driver_near.png"
          Mb.Just CancelAutoPay -> "ny_ic_pause_autopay,"
          Mb.Just PaymentSuccessPopup -> "ny_ic_green_tick,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_driver_near.png"
          Mb.Just SupportPopup -> ""
          Mb.Nothing -> ""
      , visibility = case state.props.popUpState of
          Mb.Just SupportPopup -> GONE
          Mb.Nothing -> GONE
          _                    -> VISIBLE
      , width = V 114
      , height = V 114
      },
    secondaryText {
      text = if state.props.popUpState == Mb.Just FailedPopup 
                then getString YOUR_PAYMENT_WAS_UNSUCCESSFUL 
             else if state.props.popUpState == Mb.Just SupportPopup
                then getString NEED_HELP_JOINING_THE_PLAN
             else if state.data.managePlanData.currentPlan.title == getString DAILY_PER_RIDE 
                then getString DAILY_UNLIMITED_OFFER_NOT_AVAILABLE 
             else ""
      , color = Color.black700
      , margin = Margin 16 4 16 0
      , visibility = if DA.any (_ == state.props.popUpState) [Mb.Just FailedPopup, Mb.Just SwitchedPlan, Mb.Just SupportPopup] then VISIBLE else GONE
      , textStyle = if Mb.Just SupportPopup == state.props.popUpState then SubHeading1 else Body1
      },
    option2 { 
      visibility = state.props.popUpState == Mb.Just SupportPopup
      , text = getString CANCEL
      , color = Color.black650
      , background = Color.white900
      , strokeColor = Color.white900
      , width = MATCH_PARENT
      , margin = (Margin 0 0 0 0)
    },
    optionWithHtml {
      textOpt1 {
        color = Color.black650
        , text = getString NEED_HELP
        , textStyle = SubHeading2
        , visibility = VISIBLE
      }
      , textOpt2 {
        color = Color.blue800
        , textStyle = SubHeading2
        , text = getString CALL_SUPPORT
        , visibility = VISIBLE
      } 
      , image {
          imageUrl = "ny_ic_phone_filled_blue,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_phone_filled_blue.png"
          , height = (V 16)
          , width = (V 16)
          , visibility = VISIBLE
          , margin = Margin 3 1 3 0
        }
      , strokeColor = Color.white900
      , margin = Margin 16 4 16 0
      , background = Color.white900
      , visibility = state.props.popUpState == Mb.Just FailedPopup
      , isClickable = true
      },
    dismissPopup = DA.any (_ == state.props.popUpState) [Mb.Just SupportPopup, Mb.Just FailedPopup]
    }
  in popUpConf'

confirmCancelPopupConfig :: ST.SubscriptionScreenState -> PopUpModalConfig.Config
confirmCancelPopupConfig state = let
    config = PopUpModalConfig.config
    popUpConfig' = config {
      gravity = CENTER
    , cornerRadius = Corners 15.0 true true true true
    , margin = MarginHorizontal 16 16
    , backgroundColor =  Color.black9000
    , backgroundClickable = false
    , primaryText {
        text = getString DO_YOU_WANT_TO_CANCEL
      , margin = (Margin 16 24 16 0)
      },
      secondaryText {
        text = getString DO_YOU_WANT_TO_CANCEL_DESC
      , color = Color.black700
      , margin = (Margin 16 12 16 40)
        },
      option1 {
        text = getString NO
      , color = Color.black900
      , strokeColor = Color.black700
      },
      option2 {text = getString YES_CANCEL
      , background = Color.red
      , color = Color.white900
      , strokeColor = Color.red
      , margin = MarginLeft 12
      },
      coverImageConfig {
        imageUrl = "ny_ic_pause_autopay,"
      , visibility = VISIBLE
      , width = V 265
      , height = V 265
      }
    }
  in popUpConfig'

getSelectedAlternatePlan :: ST.SubscriptionScreenState -> String
getSelectedAlternatePlan state = do
  let plan = (DA.filter(\item -> item.id == state.props.managePlanProps.selectedPlanItem.id) state.data.managePlanData.alternatePlans)
  case plan DA.!! 0 of 
    Mb.Just value -> value.title
    Mb.Nothing -> state.data.myPlanData.planEntity.title

getSelectedJoiningPlan :: ST.SubscriptionScreenState -> Mb.Maybe String
getSelectedJoiningPlan state = do
  case state.props.joinPlanProps.selectedPlanItem of
    Mb.Just planEntity -> do 
                    let plan = (DA.filter(\item -> item.id == planEntity.id) state.data.joinPlanData.allPlans)
                    case plan DA.!! 0 of 
                      Mb.Just value -> Mb.Just value.title
                      Mb.Nothing -> Mb.Nothing
    Mb.Nothing -> Mb.Nothing
  
tryAgainButtonConfig :: ST.SubscriptionScreenState -> PrimaryButton.Config
tryAgainButtonConfig state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = getString TRY_AGAIN }
      , isClickable = true
      , alpha = if true then 1.0 else 0.6
      , height = V 48
      , cornerRadius = 8.0
      , margin = Margin 16 16 16 16
      , id = "TryAgainPrimaryButton"
      }
  in primaryButtonConfig'

optionsMenuConfig :: ST.SubscriptionScreenState -> OptionsMenuConfig.Config
optionsMenuConfig state = 
  OptionsMenuConfig.config {
  menuItems = [
    {image : "ny_ic_settings_unfilled,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_settings_unfilled.png", textdata : getString MANAGE_PLAN, action : "manage_plan", isVisible : true},
    {image : "ny_ic_calendar_black,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_calendar_black.png", textdata : getString PAYMENT_HISTORY, action : "payment_history", isVisible : true},
    {image : "ny_ic_phone_unfilled,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_phone_unfilled.png", textdata : getString CALL_SUPPORT, action : "call_support", isVisible :  false},
    -- {image : "ny_ic_message_unfilled,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_message_unfilled.png", textdata : getString CHAT_FOR_HELP, action : "chat_for_help", isVisible : true}, -- TODO:: Removed for some time
    {image : "ny_ic_loc_grey,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_loc_grey.png", textdata : getString FIND_HELP_CENTRE, action : "find_help_centre", isVisible : false},
    {image : "ny_ic_help_circle_transparent,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_help_circle_transparent.png", textdata : getString VIEW_FAQs, action : "view_faq", isVisible : true}],
  backgroundColor = Color.blackLessTrans,
  menuBackgroundColor = Color.white900,
  gravity = RIGHT,
  menuExpanded = state.props.optionsMenuState /= ALL_COLLAPSED,
  width = 170,
  marginRight = 16,
  itemHeight = 50,
  itemPadding = 16,
  cornerRadius = 4.0

}

clearManualDuesBtn :: ST.SubscriptionScreenState -> PrimaryButton.Config
clearManualDuesBtn state = let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig{ text = getString CLEAR_MANUAL_DUES}
      , height = (V 48)
      , cornerRadius = 8.0
      , id = "ClearManualDuesBtn"
      , enableLoader = (JB.getBtnLoader "ClearManualDuesBtn")
      , margin = (MarginHorizontal 16 16)
      }
  in primaryButtonConfig'

getHeaderConfig :: ST.SubscriptionSubview -> Boolean -> Boolean -> HeaderData
getHeaderConfig subView isManualPayDue isMultiDueType = 
  case subView of
    ST.JoinPlan    -> {title : (getString NAMMA_YATRI_PLANS), actionText : getString SUPPORT, backbutton : false}
    ST.ManagePlan  -> {title : (getString MANAGE_PLAN), actionText : "", backbutton : true}
    ST.MyPlan      -> {title : (getString PLAN), actionText : "", backbutton : false}
    ST.PlanDetails -> {title : (getString AUTOPAY_DETAILS), actionText : "", backbutton : true}
    ST.FindHelpCentre -> {title : (getString FIND_HELP_CENTRE), actionText : "", backbutton : true}
    ST.DuesView -> {title : (getString DUE_OVERVIEW), actionText : "", backbutton : true}
    ST.DueDetails -> {title : getString case isMultiDueType, isManualPayDue of 
                                          true, false -> AUTOPAY_DUE_DETAILS
                                          true, true -> MANUAL_DUE_DETAILS
                                          _, _ -> DUE_DETAILS , actionText : "", backbutton : true}
    _           -> {title : (getString NAMMA_YATRI_PLANS), actionText : "", backbutton : false}

type HeaderData = {title :: String, actionText :: String, backbutton :: Boolean}


dueDetailsListState :: ST.SubscriptionScreenState -> DueDetailsListState
dueDetailsListState state = 
  {
  dues : map (\ item -> do
    let planOfferData = decodeOfferPlan item.plan
        autoPayStageData = getAutoPayStageData item.autoPayStage
    {
      date : convertUTCtoISC item.tripDate "Do MMM, YYYY",
      planType : planOfferData.plan,
      offerApplied : (getPromoConfig [OfferEntity{title : Mb.Just planOfferData.offer, description : Mb.Nothing, tnc : Mb.Nothing}]) DA.!! 0,
      noOfRides : item.noOfRides,
      totalEarningsOfDay : item.earnings,
      dueAmount : item.amount,
      fareBreakup : item.feeBreakup,
      expanded : item.randomId == state.data.myPlanData.selectedDue,
      isAutoPayFailed : Mb.isJust item.autoPayStage && item.mode == MANUAL_PAYMENT,
      isSplitPayment : item.isSplit,
      id : item.randomId,
      paymentMode : item.mode,
      scheduledAt : if item.mode == AUTOPAY_REGISTRATION then Just (convertUTCtoISC item.scheduledAt "Do MMM YYYY, h:mm A") else Nothing,
      paymentStatus : if item.mode == AUTOPAY_REGISTRATION then Just (autoPayStageData.stage) else Nothing
    }) (DA.filter (\item -> if state.props.myPlanProps.dueType == AUTOPAY_PAYMENT then item.mode == AUTOPAY_PAYMENT else item.mode /= AUTOPAY_PAYMENT ) state.data.myPlanData.dueItems)
}

offerCardBannerConfig :: Boolean -> OfferBanner ->  Banner.Config
offerCardBannerConfig isPlanCard bannerProps= 
  let 
    strArray = split (Pattern "-*$*-") bannerProps.offerBannerDeadline
    getLanguage len = do
        case getValueToLocalStore LANGUAGE_KEY of
            "KN_IN" | len > 1 -> 1
            "HI_IN" | len > 2 -> 2
            "TA_IN" | len > 3 -> 3
            _ -> 0
    date = Mb.fromMaybe "" (strArray DA.!! (getLanguage (DA.length strArray)))
    title' = getVarString OFFER_CARD_BANNER_TITLE [date]
    config = Banner.config
    config' = config  
      {
        backgroundColor = Color.yellow800,
        title = title',
        titleColor = Color.black800,
        actionText = getString OFFER_CARD_BANNER_DESC,
        actionTextColor = Color.black800,
        imageUrl = "ny_ic_autopay_setup_banner,"<>(HU.getAssetStoreLink FunctionCall)<>"ny_ic_autopay_setup_banner.png",
        isBanner = true,
        alertText = getString OFFER_CARD_BANNER_ALERT,
        alertTextColor = Color.red,
        alertTextVisibility = true,
        showActionArrow = false,
        bannerClickable = false,
        titleStyle = if isPlanCard then Body6 else Body7,
        imageHeight = if isPlanCard then (V 80) else (V 95),
        imageWidth = if isPlanCard then (V 98) else (V 108),
        margin = MarginTop 8,
        padding = PaddingTop 0,
        actionTextVisibility = false
      }
  in config'