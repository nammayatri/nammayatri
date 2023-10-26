{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}



module Screens.SubscriptionScreen.View where

import Screens.SubscriptionScreen.ComponentConfig

import Animation as Anim
import Animation.Config as AnimConfig
import Common.Types.App (APIPaymentStatus(..), LazyCheck(..), PaymentStatus(..))
import Components.Banner as Banner
import Components.BottomNavBar (navData)
import Components.BottomNavBar as BottomNavBar
import Components.DueDetailsList (DueDetailsListState)
import Components.DueDetailsList as DueDetailsList
import Components.OptionsMenu as OptionsMenu
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (any, elem, length, filter, (!!))
import Data.Array as DA
import Data.Bifunctor.Join (Join)
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn1)
import Data.Int (toNumber, pow, ceil)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Maybe as Mb
import Data.Number (fromString) as Number
import Data.String as DS
import Data.Time.Duration (Seconds(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (convertUTCtoISC, flowRunner, getNewIDWithTag, screenHeight, screenWidth, getImageUrl)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink, getValueBtwRange, getAssetsBaseUrl)
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (getValueFromConfig)
import Prelude (Unit, bind, const, discard, map, not, pure, show, unit, void, when, ($), (&&), (*), (+), (-), (/), (/=), (<), (<<<), (<>), (==), (>), (||))
import Presto.Core.Types.API (ErrorResponse)
import Presto.Core.Types.Language.Flow (Flow, doAff, getState, delay)
import PrestoDOM (Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), afterRender, alignParentBottom, alpha, background, clickable, color, cornerRadius, ellipsize, fontStyle, frameLayout, gradient, gravity, height, horizontalScrollView, id, imageView, imageWithFallback, lineHeight, linearLayout, lottieAnimationView, margin, maxLines, onBackPressed, onClick, orientation, padding, relativeLayout, scrollBarX, scrollBarY, scrollView, shimmerFrameLayout, singleLine, stroke, text, textFromHtml, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.List as PrestoList
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens (getScreen)
import Screens as ScreenNames
import Screens.SubscriptionScreen.Controller (Action(..), ScreenOutput, eval, getAllFareFromArray, getPlanPrice)
import Screens.Types (AutoPayStatus(..), DueItem, GlobalProps, KioskLocation(..), MyPlanData, OfferBanner, OptionsMenuState(..), PlanCardConfig, PromoConfig, SubscriptionScreenState, SubscriptionSubview(..))
import Services.API (FeeType(..), GetCurrentPlanResp(..), GetDriverInfoResp(..), KioskLocationRes(..), KioskLocationResp(..), OrderStatusRes(..), PaymentBreakUp(..), UiPlansResp(..), GetDriverInfoReq(..))
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalNativeStore, getValueToLocalStore, setValueToLocalStore, isOnFreeTrial)
import Styles.Colors as Color
import Types.App (GlobalState(..), defaultGlobalState)
import Screens.DriverProfileScreen.ScreenData (dummyDriverInfo)

screen :: SubscriptionScreenState -> GlobalState -> Screen Action SubscriptionScreenState ScreenOutput
screen initialState globalState =
  { initialState
  , view
  , name: "SubscriptionScreen"
  , globalEvents: [(\push -> do
      void $ launchAff $ flowRunner defaultGlobalState $ loadData push LoadPlans LoadAlternatePlans LoadMyPlans LoadHelpCentre ShowError initialState globalState
      case initialState.data.orderId of 
        Just id -> void $ launchAff $ flowRunner defaultGlobalState $ paymentStatusPooling id 7 2 1 initialState push PaymentStatusAction
        Mb.Nothing -> pure unit
      pure (pure unit)
    )]
  , eval:
      ( \state action -> do
          let _ = spy "SubscriptionScreen ----- state" state
          let _ = spy "SubscriptionScreen --------action" action
          eval state action
      )
  }

loadData :: forall action. (action -> Effect Unit) ->  (UiPlansResp -> action) -> (UiPlansResp -> action) -> (GetCurrentPlanResp -> action) -> (Number -> Number -> Array KioskLocationRes -> action) -> (ErrorResponse -> action) -> SubscriptionScreenState -> GlobalState -> Flow GlobalState Unit
loadData push loadPlans loadAlternatePlans loadMyPlans loadHelpCentre errorAction state (GlobalState globalState) = do
  if any ( _ == state.props.subView )[JoinPlan, MyPlan, NoSubView] then do
    let globalProp = globalState.globalProps
    (GetDriverInfoResp driverInfo) <- getDriverInfoDataFromCache globalProp.driverInformation
    if isJust driverInfo.autoPayStatus then do 
      currentPlan <- Remote.getCurrentPlan ""
      _ <- pure $ setValueToLocalStore TIMES_OPENED_NEW_SUBSCRIPTION "5"
      case currentPlan of
        Right resp -> doAff do liftEffect $ push $ loadMyPlans resp
        Left err -> doAff do liftEffect $ push $ errorAction err
    else do
      currentPlan <- Remote.getCurrentPlan ""
      case currentPlan of
        Right resp' -> do
          let (GetCurrentPlanResp resp) = resp'
          case resp.currentPlanDetails of
            Nothing -> do
              uiPlans <- Remote.getUiPlans ""
              case uiPlans of
                Right plansResp -> doAff do liftEffect $ push $ loadPlans plansResp
                Left err -> doAff do liftEffect $ push $ errorAction err
            Just _ -> do
                _ <- pure $ setValueToLocalStore TIMES_OPENED_NEW_SUBSCRIPTION "5"
                doAff do liftEffect $ push $ loadMyPlans resp'
        Left err -> doAff do liftEffect $ push $ errorAction err
  else if (state.props.subView == FindHelpCentre) then do
    locations <- Remote.getKioskLocations ""
    case locations of
      Right (KioskLocationResp locationsResp) -> doAff do liftEffect $ push $ loadHelpCentre state.props.currentLat state.props.currentLon locationsResp
      Left err -> if err.code /= 404 then doAff do liftEffect $ push $ errorAction err
                  else doAff do liftEffect $ push $ loadHelpCentre state.props.currentLat state.props.currentLon []
  else pure unit


getDriverInfoDataFromCache :: Maybe GetDriverInfoResp -> Flow GlobalState GetDriverInfoResp
getDriverInfoDataFromCache resp = do
  case resp of 
    Just cacheResp -> pure cacheResp
    Nothing -> do
      driverInfoResp <- Remote.getDriverInfoApi (GetDriverInfoReq {})
      case driverInfoResp of
        Right latestResp -> pure latestResp
        Left _ -> pure dummyDriverInfo

paymentStatusPooling :: forall action. String -> Int -> Int -> Int -> SubscriptionScreenState -> (action -> Effect Unit) -> (APIPaymentStatus -> action) -> Flow GlobalState Unit
paymentStatusPooling orderId count base power state push action = do
  if (getValueToLocalStore PAYMENT_STATUS_POOLING) == "true" && count > 0 && orderId /= "" then do
    orderStatus <- Remote.paymentOrderStatus orderId
    _ <- pure $ spy "polling inside paymentStatusPooling function" orderStatus
    case orderStatus of
      Right (OrderStatusRes resp) -> do
        if (DA.any (_ == resp.status) [CHARGED, AUTHORIZATION_FAILED, AUTHENTICATION_FAILED, JUSPAY_DECLINED]) then do
            _ <- pure $ setValueToLocalStore PAYMENT_STATUS_POOLING "false"
            doAff do liftEffect $ push $ action resp.status
        else do
            void $ delay $ Seconds $ toNumber $ pow base power
            paymentStatusPooling orderId (count - 1) base (power+1) state push action
      Left err -> pure unit
    else pure unit

view :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
view push state =
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , onBackPressed push $ const BackPressed
  , afterRender push $ const AfterRender
  , background Color.white900
  ][ relativeLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER
      ][ Anim.screenAnimationFadeInOut $
          linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , weight 1.0
          , orientation VERTICAL
          ][ errorView push state
            , shimmerView state
            , headerView push state
            , frameLayout [
              height MATCH_PARENT
              , width MATCH_PARENT
              , visibility if state.props.showShimmer then  GONE else VISIBLE
              ][ joinPlanView push state (state.props.subView == JoinPlan)
                , managePlanView push state (state.props.subView == ManagePlan)
                , myPlanView push state (state.props.subView == MyPlan)
                , autoPayDetailsView push state (state.props.subView == PlanDetails)
                , if (state.props.subView == FindHelpCentre) then findHelpCentreView push state (state.props.subView == FindHelpCentre) else dummyView
                , if (state.props.subView == DuesView) then duesOverView push state (state.props.subView == DuesView) else dummyView
                , if (state.props.subView == DueDetails) then dueDetails push state (state.props.subView == DueDetails) else dummyView
              ]
          ]
        , linearLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , background if state.props.myPlanProps.isDueViewExpanded && state.props.subView == MyPlan then Color.blackLessTrans else Color.transparent
          , clickable $ state.props.myPlanProps.isDueViewExpanded && state.props.subView == MyPlan
          , onClick push $ const $ if state.props.myPlanProps.isDueViewExpanded then ToggleDueDetailsView else NoAction
          , gravity BOTTOM
          ][ duesView push state
           , if any (_ == state.props.subView) [MyPlan, JoinPlan, NoSubView] && not state.props.isEndRideModal then BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.SUBSCRIPTION_SCREEN state.data.bottomNavConfig) else dummyView
          ]
          , if state.props.optionsMenuState /= ALL_COLLAPSED then
              linearLayout
              [ height MATCH_PARENT
              , width MATCH_PARENT
              , margin $ MarginTop 55
              ][ OptionsMenu.view (push <<< OptionsMenuAction) (optionsMenuConfig state) ]
            else dummyView
      ]
    , PrestoAnim.animationSet [ Anim.fadeIn (not Mb.isNothing state.props.popUpState) ] $
      linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , visibility if (not Mb.isNothing state.props.popUpState) then VISIBLE else GONE
      ][PopUpModal.view (push <<< PopUpModalAC) (popupModalConfig state)]
    , PrestoAnim.animationSet [ Anim.fadeIn state.props.confirmCancel] $
      linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , visibility if (state.props.confirmCancel) then VISIBLE else GONE
      ][PopUpModal.view (push <<< ConfirmCancelPopup) (confirmCancelPopupConfig state)]
  ]

joinPlanView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> Boolean -> PrestoDOM (Effect Unit) w
joinPlanView push state visibility' = 
  PrestoAnim.animationSet [ Anim.fadeIn visibility' ] $
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , visibility if visibility' then VISIBLE else GONE
  , margin $ MarginBottom 48
  ][ 
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    ][
      lottieView state "lottieSubscriptionScreen" (Margin 0 0 0 0) (Padding 16 16 16 0)
    , relativeLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , background Color.blue600
      ][  imageView
          [ width $ V 116
          , height $ V 368
          , margin $ MarginTop 20
          , imageWithFallback $ "ny_ic_ny_driver," <> (HU.getAssetStoreLink FunctionCall) <> "ny_ic_ny_driver.png"
          ]
        , enjoyBenefitsView push state
        , plansBottomView push state
      ]
    ]
  ]

enjoyBenefitsView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
enjoyBenefitsView push state = 
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , gravity RIGHT
    , orientation VERTICAL
    , margin $ Margin 116 10 10 0
    ][  linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ][  commonTV push (getString GET_READY_FOR_YS_SUBSCRIPTION) Color.black800 (FontStyle.h1 TypoGraphy) 0 LEFT state.data.config.enableIntroductoryView
          , commonTV push (getString ENJOY_THESE_BENEFITS) Color.black800 (FontStyle.body4 TypoGraphy) 0 LEFT true
          , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            ](map
                (\(item) ->
                    linearLayout
                      [ width MATCH_PARENT
                      , height WRAP_CONTENT
                      , gravity CENTER_VERTICAL
                      , margin $ MarginTop 5
                      ][ imageView
                          [ imageWithFallback $ "ny_ic_check_green," <> (HU.getCommonAssetStoreLink FunctionCall) <> "ny_ic_check_green.png"
                          , width $ V 11
                          , height $ V 8
                          ]
                        , textView $
                          [ margin $ MarginLeft 11
                          , text item
                          , color Color.black700
                          , height WRAP_CONTENT
                          , width WRAP_CONTENT
                          ] <> FontStyle.body1 TypoGraphy
                      ]
                )
              ([(getString ZERO_COMMISION), (getString EARN_TODAY_PAY_TOMORROW)] 
                  <> if state.data.config.enableIntroductoryView || not state.data.config.enableSubscriptionPopups
                      then [(getString SIGNUP_EARLY_FOR_SPECIAL_OFFERS), getString GUARANTEED_FIXED_PRICE]
                      else [(getString PAY_ONLY_IF_YOU_TAKE_RIDES), getString GET_SPECIAL_OFFERS])
            ) 
            , textView $ [
                text $ getString VALID_ONLY_IF_PAYMENT
                , color Color.black700
                , margin $ Margin 22 3 0 0
                , visibility if state.data.config.enableIntroductoryView  || not state.data.config.enableSubscriptionPopups then GONE else VISIBLE
              ] <> FontStyle.body16 TypoGraphy
        ]
        
    ]

paymentPendingView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
paymentPendingView push state = let isAutoPayPending = state.props.lastPaymentType == Just "AUTOPAY_REGISTRATION"
  in
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.yellow800
  , cornerRadii $ Corners 24.0 false false true true
  , padding $ Padding 16 12 16 12
  , visibility if ((state.data.config.enableSubscriptionPopups && state.data.orderId /= Nothing) || state.props.lastPaymentType == Just "AUTOPAY_REGISTRATION") then VISIBLE else GONE -- Condition will be updated when dues are introduced to YS flow.
  ][  commonTV push (getString if isAutoPayPending then AUTOPAY_SETUP_PENDING_STR else PAYMENT_PENDING) Color.black800 (FontStyle.h2 TypoGraphy) 0 LEFT true
    , commonTV push (getString AUTOPAY_PENDING_DESC_STR) Color.black800 (FontStyle.tags TypoGraphy) 0 LEFT true
    , textView $
      [ text $ getString OFFERS_NOT_APPLICABLE
      , color Color.red
      , visibility if isAutoPayPending && not HU.isDateGreaterThan state.props.offerBannerProps.offerBannerValidTill && (any (_ == state.data.myPlanData.planEntity.id) state.data.config.offerBannerConfig.offerBannerPlans) then VISIBLE else GONE
      ] <> if state.props.isSelectedLangTamil then FontStyle.body16 TypoGraphy else FontStyle.tags TypoGraphy
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin $ MarginTop 10
      ]
      [
        linearLayout
            [ margin $ MarginRight 6
            , weight 1.0
            ]
            [ PrimaryButton.view (push <<< CheckPaymentStatusButton) (checkStatusButtonConfig state) ]
        , linearLayout
            [ margin $ MarginLeft 6
            , weight 1.0
            ]
            [ PrimaryButton.view (push <<< RetryPaymentAC) (retryPaymentButtonConfig state) ]
        ]
      , linearLayout [
          width MATCH_PARENT
          , height $ V 1
          , margin $ MarginVertical 16 12
          , background $ "#E2D7BB"
      ][]
      , linearLayout [
          width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity CENTER
          , onClick push $ const $ CallSupport
      ][
        textView $ [
          textFromHtml $ getString NEED_HELP
        ] <> FontStyle.tags TypoGraphy
        , imageView [
          imageWithFallback "ny_ic_phone_filled_blue,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_phone_filled_blue.png"
          , height $ V 12
          , width $ V 12
          , margin $ Margin 2 1 2 0
        ]
        , textView $ [
          textFromHtml $ getString CALL_SUPPORT
          , color Color.blue800
        ] <> FontStyle.tags TypoGraphy
      ]
  ]

plansBottomView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
plansBottomView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , alignParentBottom "true,-1"
  , cornerRadii $ Corners 20.0 true true false false
  , background Color.white900
  , padding $ Padding 20 20 20 if length state.data.joinPlanData.allPlans == 1 then 16 else 0
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ][ linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity CENTER_VERTICAL
          ][ textView $
              [ weight 1.0
              , height WRAP_CONTENT
              , text if state.data.config.enableIntroductoryView then getString COMING_SOON else (getString CHOOSE_YOUR_PLAN)
              , color Color.black800
              ] <> FontStyle.body8 TypoGraphy
          , linearLayout
            [ weight 1.0
            , height WRAP_CONTENT
            , gravity RIGHT
            ][ imageView
                [ width $ V 85
                , height $ V 20
                , imageWithFallback $ "ny_ic_upi_autopay," <> (HU.getAssetStoreLink FunctionCall) <> "ny_ic_upi_autopay.png"
                ]
            ]
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity CENTER_VERTICAL
          , onClick (\action -> do
                        let url = if state.data.config.myPlanYoutubeLink == "" then state.data.config.faqLink else state.data.config.myPlanYoutubeLink
                        _ <- push action
                        _ <- pure $ JB.cleverTapCustomEvent "ny_driver_nyplans_watchvideo_clicked"
                        _ <- pure $ JB.metaLogEvent "ny_driver_nyplans_watchvideo_clicked"
                        _ <- pure $ JB.firebaseLogEvent "ny_driver_nyplans_watchvideo_clicked"
                        _ <- JB.openUrlInApp url
                        pure unit
                        ) (const NoAction)
          ][ textView $
              [ height WRAP_CONTENT
              , width $ V $ JB.getWidthFromPercent 70
              , gravity LEFT
              , text ( (languageSpecificTranslation (getString GET_FREE_TRAIL_UNTIL) state.data.joinPlanData.subscriptionStartDate) <> " ✨")
              , color Color.black800
              , visibility GONE
              ] <> FontStyle.body1 TypoGraphy 
            , imageView [
                imageWithFallback "ny_ic_youtube,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_youtube.png"
                , height $ V 16
                , width $ V 16
                , margin $ Margin 0 3 6 0
                , visibility if state.data.config.myPlanYoutubeLink == "" then GONE else VISIBLE
            ]
            , textView $
              [ weight 1.0
              , height WRAP_CONTENT
              , gravity LEFT
              , textFromHtml $ "<u>" <> (getString HOW_IT_WORKS) <> "</u>"
              , color Color.blue900
              ] <> FontStyle.body1 TypoGraphy
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height $ V 1
          , background Color.grey700
          , margin $ MarginVertical 10 10
          ][]
        , scrollView
          [ width MATCH_PARENT
          , weight 1.0
          ][ linearLayout
              [ weight 1.0
              , width MATCH_PARENT
              , orientation VERTICAL
              ](map 
                  (\item ->
                    let selectedPlan = state.props.joinPlanProps.selectedPlanItem
                    in case selectedPlan of
                        Just plan -> planCardView push item (item.id == plan.id) true ChoosePlan state.props.isSelectedLangTamil false false false Nothing state.data.config.enableIntroductoryView []
                        Nothing -> planCardView push item false true ChoosePlan state.props.isSelectedLangTamil false false false Nothing state.data.config.enableIntroductoryView []
                  ) state.data.joinPlanData.allPlans)
          ]
        , PrimaryButton.view (push <<< JoinPlanAC) (joinPlanButtonConfig state)
      ]
  ]

commonTV :: forall w. (Action -> Effect Unit) -> String -> String -> (forall properties. (Array (Prop properties))) -> Int -> Gravity -> Boolean -> PrestoDOM (Effect Unit) w
commonTV push text' color' fontStyle marginTop gravity' visibility' =
  textView $
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text text'
  , color color'
  , gravity gravity'
  , margin $ MarginTop marginTop
  , visibility if visibility' then VISIBLE else GONE
  ] <> fontStyle


managePlanView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> Boolean -> PrestoDOM (Effect Unit) w
managePlanView push state visibility' =
  PrestoAnim.animationSet [ Anim.fadeIn visibility' ] $
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , visibility if visibility' then VISIBLE else GONE
  ][ managePlanBodyView push state
   , linearLayout
     [ height $ V 45
     , width MATCH_PARENT
     , gravity CENTER
     , alignParentBottom "true,-1"
     , background Color.grey700
     , stroke $ "1," <> Color.grey900
     , visibility if state.data.myPlanData.autoPayStatus `elem` [ACTIVE_AUTOPAY, PAUSED_PSP] then VISIBLE else GONE
     ][ textView $
        [ textFromHtml $ "<u>" <> (getString VIEW_AUTOPAY_DETAILS) <> "</u>"
        , padding $ Padding 5 5 5 5
        , color Color.black800
        , onClick push $ const ViewAutopayDetails
        ] <> if state.props.isSelectedLangTamil then FontStyle.captions TypoGraphy else FontStyle.body3 TypoGraphy
      ]
   ]


myPlanView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> Boolean -> PrestoDOM (Effect Unit) w
myPlanView push state visibility' =
  PrestoAnim.animationSet [ Anim.fadeIn visibility' ] $
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , visibility if visibility' then VISIBLE else GONE
  , gradient (Linear 180.0 [Color.darkGradientBlue, Color.lightGradientBlue])
  ][ paymentPendingView push state
   , myPlanBodyview push state
  ]

headerView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w 
headerView push state =
  let config = getHeaderConfig state.props.subView (state.props.myPlanProps.dueType /= AUTOPAY_PAYMENT) state.props.myPlanProps.multiTypeDues
  in 
    linearLayout
    [ height $ V 55
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , padding $ PaddingLeft 16
    , background Color.white900
    , stroke $ "1," <> Color.grey900
    ][ imageView
      [ width $ V 24
      , height $ V 24
      , margin $ MarginRight 16
      , visibility if config.backbutton || state.props.isEndRideModal then VISIBLE else GONE
      , onClick push $ const $ BackPressed
      , imageWithFallback if config.backbutton then "ny_ic_chevron_left,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_left.png"
                          else if state.props.isEndRideModal then "ny_ic_close_bold,"<> (HU.getAssetStoreLink FunctionCall) <>"ny_ic_close_bold.png"
                          else ""
      ]
    , textView $
      [ text config.title
      , color Color.darkCharcoal
      , padding $ PaddingBottom 4
      , weight 1.0
      ] <> if state.props.isSelectedLangTamil then FontStyle.body7 TypoGraphy else FontStyle.h2 TypoGraphy
    , linearLayout [
        height WRAP_CONTENT
        , padding $ Padding 10 10 10 10
        , gravity CENTER_VERTICAL
        , visibility if any (_ == state.props.subView) [MyPlan, JoinPlan] then VISIBLE else GONE
      ][
        imageView [
          imageWithFallback "ny_ic_phone_filled_blue,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_phone_filled_blue.png"
          , height $ V 56
          , width $ V 56
          , padding $ Padding 16 16 16 15
          , margin $ MarginRight 3
          , onClick push $ const $ CallSupport
          ]
        , textView
          $ [ textFromHtml config.actionText
          , visibility GONE -- Not being used now.
          , padding $ PaddingBottom 3
          , color Color.blue800
          ] <> FontStyle.body1 TypoGraphy
        , linearLayout [
            height $ V 20
            , width $ V 1
            , background Color.grey900
            , visibility if state.props.subView == MyPlan then VISIBLE else GONE
          ][]
        , imageView [
            imageWithFallback "ny_ic_settings_filled_blue,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_settings_filled_blue.png"
            , height $ V 56
            , width $ V 56
            , padding $ Padding 16 16 16 15
            , margin $ MarginLeft 3
            , onClick push $ const $ HeaderRightClick PLAN_MENU
            , visibility if state.props.subView == MyPlan then VISIBLE else GONE
          ]
      ]
    ]

myPlanBodyview :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w 
myPlanBodyview push state =
  let isFreezed = ((state.data.config.enableSubscriptionPopups && state.data.orderId /= Nothing) || state.props.lastPaymentType == Just "AUTOPAY_REGISTRATION")
      paddingBottom = case state.data.myPlanData.autoPayStatus /= ACTIVE_AUTOPAY , state.data.myPlanData.manualDueAmount > 0.0 of
                        true, true -> 270
                        _, _ -> 250
  in 
  scrollView
  [ height MATCH_PARENT
  , width MATCH_PARENT
  ][ linearLayout
    [ height $ V 300 
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding $ PaddingVertical 24 paddingBottom
    ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      , margin $ Margin 16 0 16 16 
      ][ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , weight 1.0
        ][ textView $
          [ text (getString MY_PLAN)
          , color Color.black800
          , padding $ PaddingBottom 5
          ] <> if state.props.isSelectedLangTamil then FontStyle.h2 TypoGraphy else FontStyle.body8 TypoGraphy
        , imageView
          [ width $ V 38
          , height $ V 38
          , margin (MarginLeft 4)
          , padding $ Padding 8 8 8 8
          , visibility if state.data.config.myPlanYoutubeLink == "" then GONE else VISIBLE
          , imageWithFallback "ny_ic_youtube,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_youtube.png"
          , onClick (\action -> do
                      _<- push action
                      _ <- pure $ JB.cleverTapCustomEvent "ny_driver_myplan_watchvideo_clicked"
                      _ <- pure $ JB.metaLogEvent "ny_driver_myplan_watchvideo_clicked"
                      _ <- pure $ JB.firebaseLogEvent "ny_driver_myplan_watchvideo_clicked"
                      _ <- JB.openUrlInApp state.data.config.myPlanYoutubeLink
                      pure unit
                      ) (const NoAction)
          ]
          ]
        , paymentMethodView push state.data.myPlanData
      ]
    , lottieView state "lottieSubscriptionScreen2" (Margin 16 0 16 16) (Padding 0 0 0 0)
    , planCardView push state.data.myPlanData.planEntity (state.data.myPlanData.planEntity.isSelected || not state.data.config.enableSubscriptionPopups) (not isFreezed) TogglePlanDescription state.props.isSelectedLangTamil false true true Nothing false []
    , offerCardBannerView push true (state.data.myPlanData.autoPayStatus /= ACTIVE_AUTOPAY && (any (_ == state.data.myPlanData.planEntity.id) state.data.config.offerBannerConfig.offerBannerPlans)) false state.props.offerBannerProps isFreezed
    , alertView push (getImageURL "ny_ic_about") Color.black800 (getString PAYMENT_MODE_CHANGED_TO_MANUAL) (getString PAYMENT_MODE_CHANGED_TO_MANUAL_DESC) "" NoAction (state.data.myPlanData.autoPayStatus == PAUSED_PSP) state.props.isSelectedLangTamil true isFreezed
    , alertView push (getImageURL "ny_ic_about") Color.black800 (getString PAYMENT_MODE_CHANGED_TO_MANUAL) (getString PAYMENT_CANCELLED) "" NoAction (any (_ == state.data.myPlanData.autoPayStatus) [CANCELLED_PSP, SUSPENDED]) state.props.isSelectedLangTamil false isFreezed
    , alertView push (getImageURL "ny_ic_warning_red") Color.red (getString LOW_ACCOUNT_BALANCE) (DS.replace (DS.Pattern "<X>") (DS.Replacement $ HU.getFixedTwoDecimals $ fromMaybe 0.0 state.data.myPlanData.lowAccountBalance) (getString LOW_ACCOUNT_BALANCE_DESC)) "" NoAction (Mb.isJust state.data.myPlanData.lowAccountBalance) state.props.isSelectedLangTamil false isFreezed
    , alertView push (getImageURL "ny_ic_warning_blue") Color.blue800 (getString SWITCH_AND_SAVE) (getString SWITCH_AND_SAVE_DESC) (getString SWITCH_NOW) NoAction state.data.myPlanData.switchAndSave state.props.isSelectedLangTamil false isFreezed
  ]
]

duesView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w 
duesView push state = 
  linearLayout 
     [ height WRAP_CONTENT
      , width MATCH_PARENT
      , background Color.white900
      , cornerRadii $ Corners 16.0 true true false false
      , stroke $ "1," <> Color.grey900
      , orientation VERTICAL
      , padding $ PaddingBottom 15
      , visibility if state.props.subView == MyPlan then VISIBLE else GONE
     ][ linearLayout[
          gravity CENTER
        , padding $ Padding 16 16 16 8
        , width MATCH_PARENT
        , onClick push $ const $ ToggleDueDetailsView
      ][ imageView
          [ width $ V 16
          , height $ V 16
          , margin (MarginRight 4)
          , visibility if state.props.myPlanProps.overDue then VISIBLE else GONE
          , imageWithFallback $ "ny_ic_warning_unfilled_red," <> (HU.getAssetStoreLink FunctionCall) <> "ny_ic_warning_unfilled_red.png"
          ]
          , textView $
            [ text (getString YOUR_DUES)
            , weight 1.0
            , gravity CENTER_VERTICAL
            , color if state.props.myPlanProps.overDue then Color.red else Color.black800
            ]  <> if state.props.isSelectedLangTamil then FontStyle.body9 TypoGraphy else FontStyle.body6 TypoGraphy
          , textView $
            [ text $  "₹" <> HU.getFixedTwoDecimals state.data.myPlanData.totalDueAmount
            , color if state.props.myPlanProps.overDue then Color.red else Color.blue800
            , padding $ PaddingBottom 2
            , visibility if state.props.myPlanProps.isDueViewExpanded then GONE else VISIBLE
            ] <> if state.props.isSelectedLangTamil then FontStyle.body7 TypoGraphy else FontStyle.h2 TypoGraphy
          , imageView [
            imageWithFallback if state.props.myPlanProps.isDueViewExpanded 
                                    then "ny_ic_chevron_up,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_up.png"
                                    else "ny_ic_chevron_down,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_down.png"
            , height $ V 12
            , width $ V 12
            , margin $ MarginLeft 6
          ]
      ]
    , PrestoAnim.animationSet [ Anim.translateYAnim AnimConfig.translateYAnimConfig ] $ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , padding $ PaddingHorizontal 16 16
      , visibility if state.props.myPlanProps.isDueViewExpanded then VISIBLE else GONE
      ][
        textView $
        [ textFromHtml $ getString if state.data.myPlanData.mandateStatus == "active" then YOUR_DUES_DESCRIPTION else YOUR_DUES_DESCRIPTION_MANUAL
        , color Color.black600
        , margin $ MarginBottom 16
        ] <> if state.props.isSelectedLangTamil then FontStyle.body16 TypoGraphy else FontStyle.tags TypoGraphy
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ Padding 16 12 16 12
        , margin $ MarginBottom 12
        , orientation VERTICAL
        , background Color.blue600
        , cornerRadius 8.0
        ][ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER_VERTICAL
            ][ textView $
              [ text (getString CURRENT_DUES)
              , color Color.black600
              , weight 1.0
              ] <> if state.props.isSelectedLangTamil then FontStyle.body16 TypoGraphy else FontStyle.tags TypoGraphy
            , textView $
              [ text (getString YOUR_LIMIT)
              , color Color.black600
              ] <> if state.props.isSelectedLangTamil then FontStyle.body16 TypoGraphy else FontStyle.tags TypoGraphy
            ]
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER_VERTICAL
            ][ textView $
              [ text $  "₹" <> HU.getFixedTwoDecimals state.data.myPlanData.totalDueAmount
              , color if state.props.myPlanProps.overDue then Color.red 
                      else if state.props.myPlanProps.multiTypeDues then Color.black900 
                      else Color.blue800
              , weight 1.0
              ] <> if state.props.isSelectedLangTamil then FontStyle.body7 TypoGraphy else FontStyle.h2 TypoGraphy
            , textView $
              [ text $ "₹" <>  HU.getFixedTwoDecimals state.data.myPlanData.maxDueAmount
              , color Color.black700
              ] <> if state.props.isSelectedLangTamil then FontStyle.body7 TypoGraphy else FontStyle.h2 TypoGraphy
            ]
          , relativeLayout
            [ height $ V 4
            , width MATCH_PARENT
            , margin $ MarginTop 8
            , gravity CENTER
            ][ linearLayout
              [ height $ V 4
              , width $ V $ (screenWidth unit) - 60
              , background Color.black700
              , cornerRadius 4.0
              ][]
            , linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              ][ linearLayout
                  [ height $ V 4
                  , width $ V $ ceil $ HU.getValueBtwRange state.data.myPlanData.manualDueAmount 0.0 state.data.myPlanData.maxDueAmount 0.0 (toNumber $ (screenWidth unit) - 60)
                  , background case state.props.myPlanProps.overDue, state.props.myPlanProps.multiTypeDues of
                                true, _ ->  Color.red 
                                false, true ->  Color.orange900
                                _, _ -> Color.blue800
                  , cornerRadius 4.0
                  ][]
                  , linearLayout
                    [ height $ V 4
                    , width $ V $ ceil $ HU.getValueBtwRange state.data.myPlanData.autoPayDueAmount 0.0 state.data.myPlanData.maxDueAmount 0.0 (toNumber $ (screenWidth unit) - 60)
                    , background if state.props.myPlanProps.overDue then Color.red else Color.blue800
                    , cornerRadius 4.0
                    ][]
              ]
            ]
            , linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , gravity CENTER_VERTICAL
              , margin $ MarginTop 12
              , visibility if state.props.myPlanProps.multiTypeDues then VISIBLE else GONE
              ][ 
                linearLayout 
                [
                  height $ V 8
                  , width $ V 8
                  , background Color.orange900
                  , margin $ Margin 0 1 4 0
                  , cornerRadius 8.0
                ][]
                , textView $
                  [ text $  getString MANUAL_DUES
                  , color Color.black600
                  , margin $ MarginRight 16
                  ] <> FontStyle.captions TypoGraphy
                , linearLayout 
                  [
                    height $ V 8
                    , width $ V 8
                    , margin $ Margin 0 1 4 0
                    , background Color.blue800
                    , cornerRadius 8.0
                  ][]
                , textView $
                  [ text $  getString AUTOPAY_IN_PROGRESS
                  , color Color.black600
                  ] <> FontStyle.captions TypoGraphy
              ]
        ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , background Color.blue600
      , cornerRadius 8.0
      , orientation VERTICAL
      , gravity CENTER_VERTICAL
      ][ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , padding $ Padding 16 12 16 12
          , gravity CENTER_VERTICAL
          , onClick push $ const $ if state.props.myPlanProps.multiTypeDues then ViewDuesOverView else ToggleDueDetails
          , visibility if (DA.null state.data.myPlanData.dueItems) then GONE else VISIBLE
          ] [ textView $
                [ text (getString DUE_DETAILS)
                , color Color.black800
                , weight 1.0
                ] <> FontStyle.tags TypoGraphy
            , commonImageView (  if state.props.myPlanProps.multiTypeDues then "ny_ic_chevron_right"
                                  else if state.props.myPlanProps.isDuesExpanded then "ny_ic_chevron_up,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_up.png"
                                  else "ny_ic_chevron_down,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_down.png") 12 12 (MarginRight 4) (Padding 0 0 0 0)
            ]
          , tripList push state.data.myPlanData.dueItems (state.data.myPlanData.manualDueAmount /= 0.0) state.props.myPlanProps.isDuesExpanded true true
        ] 
    ]
    , if (state.data.myPlanData.autoPayStatus /= ACTIVE_AUTOPAY || state.data.myPlanData.manualDueAmount > 0.0) then PrimaryButton.view (push <<< ResumeAutoPay) (clearDueButtonConfig state) else dummyView 
    , if (state.data.myPlanData.autoPayStatus /= ACTIVE_AUTOPAY && state.data.myPlanData.manualDueAmount > 0.0) then PrimaryButton.view (push <<< OneTimeSettlement) (settlementButtonConfig state) else dummyView
    ]
  

promoCodeView :: forall w. (Action -> Effect Unit) -> PromoConfig -> PrestoDOM (Effect Unit) w 
promoCodeView push state =
  linearLayout 
  ([ height WRAP_CONTENT
  , width MATCH_PARENT
  , cornerRadius 100.0
  , padding $ Padding 10 4 10 4
  , stroke $ "1," <> Color.grey900
  , background Color.white900
  , margin $ MarginRight 4
  , gravity CENTER_VERTICAL
  , visibility if state.title == Nothing then GONE else VISIBLE
  ]<> if state.isGradient then [gradient (Linear 90.0 state.gradient)] else [])
   [ imageView
     [ width $ V 12
     , height $ V 12
     , margin (MarginRight 4)
     , visibility if state.hasImage then VISIBLE else GONE
     , imageWithFallback state.imageURL
     ] 
   , textView $
     [ color Color.blue900
     , singleLine true
     , padding $ PaddingBottom 3
     ]  <> FontStyle.body16 TypoGraphy
        <> case state.title of
            Mb.Nothing -> [visibility GONE]
            Mb.Just txt -> [text txt]
  ]

alertView :: forall w. (Action -> Effect Unit) -> String -> String -> String -> String -> String -> Action -> Boolean -> Boolean -> Boolean -> Boolean -> PrestoDOM (Effect Unit) w
alertView push image primaryColor title description buttonText action visible isSelectedLangTamil showRefresh isFreezed = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , visibility if visible then VISIBLE else GONE
  , background Color.white900
  , stroke $ "1," <> Color.grey900
  , padding $ Padding 16 16 16 16
  , margin $ Margin 16 16 16 0
  , cornerRadius 8.0
  , alpha if isFreezed then 0.5 else 1.0
  , orientation VERTICAL
  ][ linearLayout
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , margin $ MarginBottom 4
     , gravity CENTER_VERTICAL
     ][ imageView
        [ width $ V 16
        , height $ V 16
        , margin $ MarginRight 4
        , imageWithFallback image
        ]
      , textView $
        [ text title
        , color primaryColor
        , padding $ PaddingBottom 3
        ] <> if isSelectedLangTamil then FontStyle.body9 TypoGraphy else FontStyle.body6 TypoGraphy
      ] 
   , textView $
     [ textFromHtml description
     , color Color.black600
     , margin $ if buttonText /= "" then MarginBottom 12 else MarginBottom 0
     ] <> if isSelectedLangTamil then FontStyle.body16 TypoGraphy else FontStyle.tags TypoGraphy
   , if buttonText /= "" then arrowButtonView push buttonText true action isSelectedLangTamil else dummyView
   , linearLayout [
      width MATCH_PARENT
      , gravity CENTER
      , margin $ MarginTop 14
      , onClick push $ const RefreshPage
      , visibility if showRefresh then VISIBLE else GONE
   ][ imageView [
        imageWithFallback $ getImageURL "ny_ic_refresh"
        , height $ V 16
        , width $ V 16
      ]
      , textView $ [
          text $ getString REFRESH_STRING
          , color Color.blue800
          , margin $ MarginLeft 4
      ] <> if isSelectedLangTamil then FontStyle.body9 TypoGraphy else FontStyle.body6 TypoGraphy
   ]
  ]

arrowButtonView :: forall w. (Action -> Effect Unit) -> String -> Boolean -> Action -> Boolean -> PrestoDOM (Effect Unit) w
arrowButtonView push title arrowVisibility action isSelectedLangTamil = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER
  ][ linearLayout
     [ height WRAP_CONTENT
     , width WRAP_CONTENT
     , gravity CENTER_VERTICAL
     , onClick push $ const $ action
     ][ textView $
        [ text title
        , color Color.blue800
        , margin (MarginRight 4)
        , padding $ PaddingBottom 3
        ] <> if isSelectedLangTamil then FontStyle.body9 TypoGraphy else FontStyle.body6 TypoGraphy
      , imageView
        [ width $ V 18
        , height $ V 18
        , visibility if arrowVisibility then VISIBLE else GONE
        , imageWithFallback "ny_ic_arrow_right_blue,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_arrow_right_blue.png"
        ]
     ]
  ]

paymentMethodView :: forall w. (Action -> Effect Unit) -> MyPlanData -> PrestoDOM (Effect Unit) w
paymentMethodView push state =
  let statusPillData = getAutoPayStatusPillData state.autoPayStatus
  in linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , gravity CENTER_VERTICAL
  , cornerRadius 100.0
  , background Color.grey700
  , padding $ Padding 8 5 8 5
  ][ imageView
    [ width $ V 12
    , height $ V 12
    , margin (MarginRight 4)
    , imageWithFallback (getImageURL "ny_ic_upi_logo")
    ]
  , textView $
    [ text (getString UPI_AUTOPAY_S)
    , color Color.black900
    , padding $ PaddingBottom 3
    ] <> FontStyle.body16 TypoGraphy
  , linearLayout
    [ height $ V 4
    , width $ V 4
    , background statusPillData.color
    , cornerRadius 12.0
    , margin $ MarginHorizontal 4 4
    ][]
  , textView $
    [ text $ statusPillData.status
    , color statusPillData.color
    , padding $ PaddingBottom 3
    ] <> FontStyle.body16 TypoGraphy
  ]

managePlanBodyView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
managePlanBodyView push state =
  scrollView
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , scrollBarY false
  ][ linearLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , padding $ PaddingTop 24
     , margin $ MarginHorizontal 16 16
     , orientation VERTICAL
     ][ textView $
        [ text (getString CURRENT_PLAN)
        , color Color.black700
        , margin $ MarginBottom 12
        ] <> if state.props.isSelectedLangTamil then FontStyle.body17 TypoGraphy else FontStyle.body9 TypoGraphy
      , planCardView push state.data.managePlanData.currentPlan (state.data.managePlanData.currentPlan.id == state.props.managePlanProps.selectedPlanItem.id) true SelectPlan state.props.isSelectedLangTamil (state.data.myPlanData.autoPayStatus /= ACTIVE_AUTOPAY) false true (Just state.props.offerBannerProps) false state.data.config.offerBannerConfig.offerBannerPlans
      , textView $
        [ text (getString ALTERNATE_PLAN)
        , color Color.black700
        , margin $ MarginVertical 32 12 
        ] <> if state.props.isSelectedLangTamil then FontStyle.body17 TypoGraphy else FontStyle.body9 TypoGraphy
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ](map(
             (\item -> planCardView push item (item.id == state.props.managePlanProps.selectedPlanItem.id) true SelectPlan state.props.isSelectedLangTamil (state.data.myPlanData.autoPayStatus /= ACTIVE_AUTOPAY) false false (Just state.props.offerBannerProps) false state.data.config.offerBannerConfig.offerBannerPlans)
             ) state.data.managePlanData.alternatePlans)
      , textView $ [
        text (getString OFFERS_APPLICABLE_ON_DAILY_UNLIMITED)
        , color Color.black600
        , margin $ MarginBottom 16
        , visibility if showOfferApplicable state then VISIBLE else GONE
      ] <> if state.props.isSelectedLangTamil then FontStyle.body17 TypoGraphy else FontStyle.body9 TypoGraphy
      , PrimaryButton.view (push <<< SwitchPlan) (switchPlanButtonConfig state)
     ]
   ]

planCardView :: forall w. (Action -> Effect Unit) -> PlanCardConfig -> Boolean -> Boolean -> (PlanCardConfig -> Action) -> Boolean -> Boolean -> Boolean -> Boolean -> Maybe OfferBanner -> Boolean -> Array String -> PrestoDOM (Effect Unit) w
planCardView push state isSelected clickable' action isSelectedLangTamil showBanner isMyPlan isActivePlan offerBannerProps isIntroductory offerBannerPlans =
  -- PrestoAnim.animationSet                TODO :: Animations
  -- [ translateInXForwardAnim true] $
  let dummyOfferConfig = { showOfferBanner : false, offerBannerValidTill : "", offerBannerDeadline : ""}
      gradient' = Linear 180.0 if isSelected && not isMyPlan then ["#53BB6F", "#2194FF"] else ["#53BB6F", "#E5E7EB"]
  in
  relativeLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , clickable if isMyPlan then true else clickable'
  , alpha if not clickable' && isMyPlan then 0.5 else 1.0
  ][ linearLayout
    ([ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 1 1 1 1
    , margin if isMyPlan then Margin 16 13 16 0 else MarginVertical 13 16    
    , cornerRadius 8.0 
   ] <> if isActivePlan then [gradient gradient'] else [])
   [ linearLayout
      [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , cornerRadius 8.0
        ][
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , background if isSelected && not isMyPlan && not isIntroductory then Color.blue600 else Color.white900
        , stroke $ "1," <> (if isSelected && isActivePlan then Color.transparent
                            else if isSelected && not isMyPlan || isIntroductory then Color.blue800 
                            else Color.grey900)
        , padding $ Padding 16 12 16 (if isMyPlan then 16 else 12)
        , cornerRadius 8.0
        , orientation VERTICAL
        , onClick push $ const $ action state
        ][ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER_VERTICAL
          , margin $ MarginBottom 5
          ][ textView
              [ text state.title
              , textSize if isSelectedLangTamil then FontSize.a_12 else FontSize.a_14
              , weight 1.0
              , fontStyle $ (if isSelected && not isMyPlan then FontStyle.bold else FontStyle.semiBold) LanguageStyle
              , color if isSelected && not isMyPlan || isIntroductory then Color.blue900 else Color.black700
              ]
            , planPriceView state.priceBreakup state.frequency isSelectedLangTamil isIntroductory
            ]
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , gravity CENTER_VERTICAL
            ][ textView $
              [ text state.description
              , color Color.black600
              , weight 1.0
              , visibility if isIntroductory then GONE else VISIBLE
              ] <> if isSelectedLangTamil then FontStyle.body16 TypoGraphy else FontStyle.tags TypoGraphy
            , if state.showOffer && DA.length state.offers > 1 then offerCountView (DA.length state.offers) isSelected else linearLayout[visibility GONE][]
            ]
          , horizontalScrollView 
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , scrollBarX false
            , margin $ MarginTop 8
            , visibility if DA.length state.offers == 1 || (isSelected && DA.length state.offers > 0) then VISIBLE else GONE
            ][ linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              ](map  (\item -> promoCodeView push item) state.offers)
            ]
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , visibility if isSelected && (DA.length state.offers > 0) || isIntroductory then VISIBLE else GONE
            ](map (\item ->
                linearLayout
                  ([ height WRAP_CONTENT
                  , width MATCH_PARENT
                  , orientation VERTICAL
                  , padding $ Padding 8 8 8 8
                  , margin $ MarginTop 8
                  , background if isMyPlan || isIntroductory then Color.grey700 else Color.white900
                  , cornerRadius 4.0
                  ] <> case item.offerDescription of 
                        Mb.Just desc -> [text desc, visibility if isSelected || isIntroductory then VISIBLE else GONE]
                        Mb.Nothing -> [visibility GONE])
                  [ textView $
                    [ textFromHtml $ Mb.fromMaybe "" item.offerDescription
                    , color Color.black600
                    ] <> if isSelectedLangTamil then FontStyle.captions TypoGraphy else FontStyle.body3 TypoGraphy
                  ]
              )state.offers)
          , offerCardBannerView push false (isJust offerBannerProps && (any (_ == state.id) offerBannerPlans) && showBanner) true (fromMaybe dummyOfferConfig offerBannerProps) false
          ]
      ]
    ]
   , linearLayout 
     [  height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      , visibility if isActivePlan then VISIBLE else GONE
     ][ textView $ [
        text $ getString ACTIVE_PLAN
      , background Color.green900
      , color Color.white900
      , padding $ Padding 8 5 8 5
      , cornerRadius 100.0
      ] <> FontStyle.tags TypoGraphy
    ]
   
]
  
  

offerCountView :: forall w. Int -> Boolean -> PrestoDOM (Effect Unit) w
offerCountView count isSelected = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , visibility if (count > 0 && not isSelected) then VISIBLE else GONE
  ][ linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , cornerRadius 100.0
      , stroke $ "1," <> Color.blue700
      , background Color.blue600
      , padding $ Padding 10 2 10 2
      , gravity CENTER_VERTICAL
      ][ imageView
        [ imageWithFallback $ getImageURL "ny_ic_discount"
        , width $ V 12
        , height $ V 12
        , margin $ MarginRight 4
        ]
      , textView $
        [ text $ show count <> " " <> if count == 1 then getString OFFER else getString OFFERS
        , color Color.blue900
        , padding $ PaddingBottom 3
        ] <> FontStyle.body17 TypoGraphy
      ]
  ]

dummyView :: forall w. PrestoDOM (Effect Unit) w
dummyView = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  ][]

getImageURL :: String -> String
getImageURL imageName = imageName <> "," <> (HU.getAssetStoreLink FunctionCall) <> imageName <> ".png"

autoPayDetailsView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> Boolean -> PrestoDOM (Effect Unit) w
autoPayDetailsView push state visibility' = 
  PrestoAnim.animationSet [ Anim.fadeIn visibility' ] $
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , visibility if visibility' then VISIBLE else GONE
  ][ linearLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , orientation VERTICAL
     ][ autoPayPGView push state
      , scrollView
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , scrollBarY false
        , margin $ Margin 20 16 20 16
        , cornerRadius 8.0
        , background Color.blue600
        ][ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , padding $ Padding 16 8 16 8
            ] (DA.mapWithIndex (\index item -> 
              linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              ][ linearLayout
                  [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , padding $ PaddingVertical 8 8
                  ][ commonTV push item.key Color.black700 (FontStyle.body3 TypoGraphy) 0 LEFT true
                  , linearLayout
                      [ weight 1.0
                      , height WRAP_CONTENT
                      , gravity RIGHT
                      ][ commonTV push item.val Color.black900 (FontStyle.body6 TypoGraphy) 0 RIGHT true]
                  ]
                , linearLayout
                  [ width MATCH_PARENT
                  , height $ V 1
                  , background Color.white900
                  , visibility if index == (DA.length state.data.autoPayDetails.detailsList -1) then GONE else VISIBLE
                  ][]
              ]
              ) state.data.autoPayDetails.detailsList)
    ]
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height $ V 45
      , alignParentBottom "true,-1"
      , background Color.grey900
      , padding $ Padding 5 5 5 5
      , gravity CENTER
      , visibility if state.data.myPlanData.mandateStatus == "active" then VISIBLE else GONE 
      ][ textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , textFromHtml $ "<u>" <> (getString CANCEL_AUTOPAY_STR) <> "</u>"
          , color Color.black800
          , padding $ Padding 5 5 5 5
          , onClick push $ const CancelAutoPayAC
          ] <> FontStyle.body3 TypoGraphy
      ]
   ]

autoPayPGView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
autoPayPGView push state = 
  relativeLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , margin $ Margin 20 22 20 22
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      ][ imageView
          [ imageWithFallback state.data.autoPayDetails.pspLogo
          , height $ V 45
          , width $ V 45
          ]
        , linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , orientation VERTICAL
          , margin $ MarginLeft 10
          ]([ linearLayout
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , gravity CENTER_VERTICAL
              ][ commonTV push "UPI Autopay" Color.black800 (FontStyle.body1 TypoGraphy) 0 LEFT true
               , imageView
                  [ imageWithFallback $ "ny_ic_upi_logo," <> (HU.getAssetStoreLink FunctionCall) <> "ny_ic_upi_logo.png"
                  , height $ V 14
                  , width $ V 14
                  ]
              ]
          ] <> if (isJust state.data.autoPayDetails.payerUpiId) then [commonTV push (fromMaybe "" state.data.autoPayDetails.payerUpiId) Color.black800 (FontStyle.paragraphText TypoGraphy) 0 LEFT true] else [])
          
        , linearLayout
          [ height WRAP_CONTENT
          , weight 1.0
          ][]
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , gravity RIGHT
      ][ textView $
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity RIGHT
          , cornerRadius 20.0
          , padding $ Padding 10 7 10 7
          , background if state.data.myPlanData.mandateStatus == "active" then "#1653BB6F" else Color.grey700
          , color if state.data.myPlanData.mandateStatus == "active" then Color.green900 else Color.orange900
          , text state.data.myPlanData.mandateStatus
          ] <> FontStyle.tags TypoGraphy
      ]
  ]

errorView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
errorView push state = 
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , visibility if state.props.showError then VISIBLE else GONE
  ]([ linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , orientation VERTICAL
      , background Color.white900
      , margin $ MarginHorizontal 30 30
      , clickable true
      , gravity CENTER
      ][ imageView
          [ imageWithFallback $ "ny_ic_api_failed," <> (HU.getAssetStoreLink FunctionCall) <> "ny_ic_api_failed.png"
          , height $ V 180
          , width $ V 280
          ]
        , commonTV push (getString WE_MIGHT_BE_LOST) Color.black900 (FontStyle.h2 TypoGraphy) 0 CENTER true
        , textView $ 
          [ textFromHtml $ (getString EXEPERIENCING_ERROR) <> " " <> state.data.errorMessage <> " \n" <> (getString PLEASE_TRY_AGAIN)
          , color Color.black700
          ] <> FontStyle.paragraphText TypoGraphy
      ]
    , linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , alignParentBottom "true,-1"
      ][PrimaryButton.view (push <<< TryAgainButtonAC) (tryAgainButtonConfig state)]
  ] <> if state.props.subView == FindHelpCentre then [headerView push state] else [] )
  
shimmerView :: forall w. SubscriptionScreenState -> PrestoDOM (Effect Unit) w
shimmerView state = linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  , visibility if state.props.showShimmer then VISIBLE else GONE
  ][ linearLayout
      [ height $ V 55
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      , padding $ PaddingHorizontal 10 10
      , stroke $ "2," <> Color.grey900
      ][  customTextView 26 80 false
        , linearLayout [weight 1.0][]
        ,  customTextView 26 60 false
      ] 
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL 
        ][
          linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , padding $ PaddingHorizontal 16 16
            , margin $ MarginTop 30][
              customTextView 40 100 true
              , linearLayout [weight 1.0][]
              ,  customTextView 40 100 true
            ]
          , sfl 180 
          , sfl 100
          , sfl 100
          , sfl 100
      ]
  ]


customTextView :: forall w. Int -> Int -> Boolean -> PrestoDOM (Effect Unit) w
customTextView height' width' showBorder =
  shimmerFrameLayout
    [ 
    cornerRadius 8.0
    , stroke if showBorder then  "1," <> Color.grey900 else "0," <> Color.grey900
    ][
      linearLayout [
        width $ V width'
        , height $ V height'
        , margin $ Margin 8 8 8 8 
        , background Color.grey900
        , cornerRadius 8.0
      ][]
    ]

sfl :: forall w. Int -> PrestoDOM (Effect Unit) w
sfl height' = 
  shimmerFrameLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ Margin 16 16 16 16
    , cornerRadius 8.0
    , padding $ Padding 15 15 15 15
    , stroke $ "2," <> Color.grey900
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height $ V height'
        , background Color.grey900
        , cornerRadius 8.0
        ][]
    ]

planPriceView :: forall w. Array PaymentBreakUp -> String -> Boolean -> Boolean -> PrestoDOM (Effect Unit) w
planPriceView fares frequency isSelectedLangTamil isIntroductory =
  let finalFee = "₹" <> (getPlanPrice fares "FINAL_FEE") <> "/" <> case frequency of
                                                                    "PER_RIDE" -> getString RIDE
                                                                    "DAILY" -> getString DAY
                                                                    _ -> getString DAY
  in
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , gravity CENTER_VERTICAL
  ][ textView $ 
     [ textFromHtml $ "<strike> ₹" <> getPlanPrice fares "INITIAL_BASE_FEE" <> "</stike>"
     , visibility if (getAllFareFromArray fares ["INITIAL_BASE_FEE", "FINAL_FEE"]) > 0.0 && not isIntroductory then VISIBLE else GONE
     , color Color.black600
     ] <> FontStyle.body7 TypoGraphy
   , textView $
      [ textFromHtml if isIntroductory then "<strike>" <> finalFee <> "</stike>" else finalFee
      , margin $ MarginLeft 3
      , color if isIntroductory then Color.black600 else Color.black800
      ] <> if isSelectedLangTamil then FontStyle.body4 TypoGraphy else FontStyle.body7 TypoGraphy
   , imageView 
     [ imageWithFallback $ "ny_ic_discount," <> (HU.getAssetStoreLink FunctionCall) <> "ny_ic_discount.png" 
     , height $ V 16  
     , width $ V 16
     , margin $ MarginLeft 4
     , visibility if isIntroductory then VISIBLE else GONE
     ]
   ]

findHelpCentreView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> Boolean -> PrestoDOM (Effect Unit) w
findHelpCentreView push state visibility' = 
  PrestoAnim.animationSet [ Anim.fadeIn true ] $
  relativeLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , orientation VERTICAL
  , visibility if visibility' then VISIBLE else GONE
  , gravity CENTER
  ][ linearLayout
     [ height $ WRAP_CONTENT
     , width MATCH_PARENT
     ][ findHelpCentreBodyView push state 
      , findHelpCentreNoDataView push state]
  ]

findHelpCentreBodyView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
findHelpCentreBodyView push state = 
  scrollView
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , scrollBarY false
  , visibility $ if state.props.noKioskLocation then GONE else VISIBLE
  ][ linearLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , padding $ PaddingTop 24
     , margin $ MarginHorizontal 16 16
     , orientation VERTICAL
     , background Color.white900
     ](map  (\item -> helpCentreCardView push item) state.props.kioskLocation)
  ]

findHelpCentreNoDataView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
findHelpCentreNoDataView push state =
  PrestoAnim.animationSet [ Anim.fadeIn true ] $
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , gravity CENTER
  , margin $ MarginHorizontal 24 24
  , visibility $ if state.props.noKioskLocation then VISIBLE else GONE
  ][  commonImageView "ny_ic_location_unserviceable" 150 141 (MarginBottom 24) (Padding 0 0 0 0)
    , textView $
      [ text $ getString NO_HELP_CENTER_IS_ACTIVE_NOW
      , color Color.black900
      , maxLines 3
      ] <> FontStyle.h2 TypoGraphy
    , textView $
      [ text $ getString HELP_CENTERS_LOCATION_WILL_APPEAR_HERE_ONCE_THEY_ARE_ACTIVE
      , color Color.black700
      , maxLines 3
      , margin $ MarginTop 10
      , gravity CENTER
      ] <> FontStyle.paragraphText TypoGraphy
  ]


helpCentreCardView :: forall w. (Action -> Effect Unit) -> KioskLocation -> PrestoDOM (Effect Unit) w
helpCentreCardView push state =
  PrestoAnim.animationSet [ Anim.fadeIn true ] $
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , stroke $ "1," <> Color.grey900
  , padding $ Padding 14 16 14 0
  , margin $ MarginBottom 16
  , cornerRadius 8.0
  , orientation VERTICAL
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , onClick push $ const $ OpenGoogleMap state.latitude state.longitude
      ][  textView $
          [ text state.landmark
          , color Color.black800
          , weight 1.0
          ] <> FontStyle.body4 TypoGraphy
        , textView $
          [ text state.address
          , color Color.black700
          , weight 1.0
          , maxLines 2
          , margin $ MarginVertical 3 3
          , ellipsize true
          ] <> FontStyle.paragraphText TypoGraphy
      ]
    , linearLayout
      [ height $ WRAP_CONTENT
      , width $ MATCH_PARENT
      , orientation $ HORIZONTAL
      , padding $ PaddingVertical 15 15
      , gravity CENTER_VERTICAL
      , onClick push $ const $ OpenGoogleMap state.latitude state.longitude
      ][  commonImageView "ny_ic_loc_grey" 24 24 (MarginLeft 4) (Padding 2 2 2 2)
        , textView
          [ text $ getString GO_TO_LOCATION
          , fontStyle $ FontStyle.regular LanguageStyle
          , margin $ MarginLeft 8
          , weight 1.0
          , color Color.black800
          ]
        , commonImageView "ny_ic_chevron_right" 24 24 (MarginLeft 4) (Padding 2 2 2 2)
      ]
    , linearLayout
      [ height $ V 1
      , width $ MATCH_PARENT
      , padding $ PaddingHorizontal 5 5
      , background Color.grey700
      ][]
    , linearLayout
      [ height $ WRAP_CONTENT
      , width $ MATCH_PARENT
      , orientation $ HORIZONTAL
      , padding $ PaddingVertical 15 15
      , gravity CENTER_VERTICAL
      , onClick push $ const $ CallHelpCenter (fromMaybe "" state.contact)
      , visibility $ case state.contact of 
                      Just _ -> VISIBLE
                      Nothing -> GONE
      ][  commonImageView "ny_ic_phone_unfilled" 24 24 (MarginLeft 4) (Padding 2 2 2 2)
        , textView
          [ text $ getString CONTACT
          , fontStyle $ FontStyle.regular LanguageStyle
          , color Color.black800
          , margin $ MarginLeft 8
          , weight 1.0
          ]
        , commonImageView "ny_ic_chevron_right" 24 24 (MarginLeft 4) (Padding 2 2 2 2)
      ]
  ]

duesOverView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> Boolean -> PrestoDOM (Effect Unit) w
duesOverView push state visibility' = 
  scrollView
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , gradient (Linear 180.0 ["#E2EAFF", "#F5F8FF"])
  ][ linearLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , orientation VERTICAL
     , visibility if visibility' then VISIBLE else GONE
     , padding $ PaddingBottom 16
     ][ 
       dueOverViewCard push state true
       , dueOverViewCard push state false
     ]
  ]
  

dueOverViewCard :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> Boolean -> PrestoDOM (Effect Unit) w
dueOverViewCard push state isManual =
  let items = filter (\item -> if isManual then item.mode /= AUTOPAY_PAYMENT else item.mode == AUTOPAY_PAYMENT) state.data.myPlanData.dueItems
  in
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  , cornerRadius 8.0
  , stroke $ "1," <> Color.grey900
  , margin $ Margin 16 24 16 0
  , padding $ PaddingTop 16
  ][ 
    linearLayout[
        height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        , padding $ PaddingHorizontal 16 16
    ][ textView $
        [ text (getString if isManual then MANUAL_DUE_OVERVIEW else AUTOPAY_DUE_OVERVIEW)
        , color Color.black800
        ] <> FontStyle.tags TypoGraphy
      , imageView
        [ width $ V 32
        , height $ V 32
        , margin (MarginLeft 4)
        , padding $ Padding 8 8 8 8
        , visibility if isManual then VISIBLE else GONE
        , imageWithFallback "ny_ic_youtube,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_youtube.png"
        , onClick (\action -> do
            _<- push action
            _ <- JB.openUrlInApp state.data.config.myPlanYoutubeLink
            pure unit
          ) (const NoAction)
        ]
      , textView $
        [ text $  "₹" <> HU.getFixedTwoDecimals if isManual then state.data.myPlanData.manualDueAmount else state.data.myPlanData.autoPayDueAmount
        , weight 1.0
        , gravity RIGHT
        , color if isManual then Color.orange900 else Color.blue800
        ] <> FontStyle.body7 TypoGraphy
    ]
    , textView $ [
          text $ getString MANUAL_DUE_AS_AUTOPAY_EXECUTION_FAILED
          , color Color.black600        
          , margin $ Margin 16 4 0 12
          , visibility if isManual then VISIBLE else GONE
        ] <> FontStyle.tags TypoGraphy
    , tripList push items isManual true false false
    , linearLayout
        [ height $ V 2
        , width MATCH_PARENT
        , background Color.grey700
        , visibility if isManual then GONE else VISIBLE
        , margin $ MarginHorizontal 16 16
        ][]
      , if isManual then PrimaryButton.view (push <<< ClearManualDues) (clearManualDuesBtn state) else dummyView
      , textView $
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , textFromHtml $ "<u>"<>(getString VIEW_DETAILS)<>"</u>"
        , color Color.black650
        , onClick push $ const $ ViewDueDetails if isManual then MANUAL_PAYMENT else AUTOPAY_PAYMENT
        , padding $ PaddingVertical 16 16
        ] <> FontStyle.body1 TypoGraphy
  ]
tripList :: forall w. (Action -> Effect Unit) -> Array DueItem -> Boolean -> Boolean -> Boolean -> Boolean -> PrestoDOM (Effect Unit) w
tripList push trips isManual isExpanded viewDatailsText useFixedHeight = 
  let adjustedHeight = if length trips == 1 then 30 else 70
  in
  linearLayout [
      height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin $ MarginTop 4
      , padding $ PaddingHorizontal 16 16
      , visibility if isExpanded then VISIBLE else GONE
    ][ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ MarginVertical 4 8
        ][ textView $
          [ text (getString TRIP_DATE)
          , color Color.black600
          , weight 1.0
          ] <> FontStyle.tags TypoGraphy
        , textView $
          [ text (getString AMOUNT)
          , color Color.black600
          ] <> FontStyle.tags TypoGraphy
        ]
      , scrollView
        [ height if useFixedHeight then V adjustedHeight else WRAP_CONTENT
        , width MATCH_PARENT
        ][  linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , margin $ MarginBottom 12
            ] (map
                (\item -> 
                linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , margin $ MarginBottom 8
                ][ textView $
                  [ text $ convertUTCtoISC item.tripDate "Do MMM YYYY"
                  , color Color.black700
                  , weight 1.0
                  ] <> FontStyle.body15 TypoGraphy
                , textView $
                  [ text $ "₹" <>  HU.getFixedTwoDecimals item.amount
                  , color Color.black700
                  ] <> FontStyle.body15 TypoGraphy
                ]
                ) trips) 
        ] 
      , textView $
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity CENTER
        , textFromHtml $ "<u>"<>(getString VIEW_DETAILS)<>"</u>"
        , color Color.black650
        , onClick push $ const $ ViewDueDetails if isManual then MANUAL_PAYMENT else AUTOPAY_PAYMENT
        , padding $ PaddingVertical 4 20
        , visibility if viewDatailsText then VISIBLE else GONE
        ] <> FontStyle.body1 TypoGraphy
  ]
  
dueDetails :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> Boolean -> PrestoDOM (Effect Unit) w
dueDetails push state visibility'= 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , visibility if visibility' then VISIBLE else GONE
  ][
    DueDetailsList.view (push <<< DueDetailsListAction) (dueDetailsListState state)
  ]

textView' :: forall w. (Action -> Effect Unit) -> Maybe Action -> String -> String -> FontStyle.Style -> Maybe Padding -> Maybe Margin -> PrestoDOM (Effect Unit) w
textView' push action txt txtColor style padding' margin' =  
  textView $
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , color txtColor
  , text txt
  , gravity CENTER
  ] <> (FontStyle.getFontStyle style LanguageStyle)
    <> case padding' of  
         Just value -> [padding value]
         Nothing -> []
    <> case margin' of  
        Just value -> [margin value]
        Nothing -> []
    <> case action of  
        Just value -> [onClick push $ const $ value]
        Nothing -> []

languageSpecificTranslation :: String -> String -> String
languageSpecificTranslation str variable = 
  case getValueToLocalStore LANGUAGE_KEY of
    "EN_US" -> str <> " " <> variable
    _ -> variable <> " " <> str 

getAutoPayStatusPillData :: AutoPayStatus -> {color :: String, status :: String}
getAutoPayStatusPillData autoPayStatus =
  case autoPayStatus of 
    ACTIVE_AUTOPAY -> {color: Color.green900, status : getString ACTIVE_STR }
    SUSPENDED -> {color: Color.red, status : getString CANCELLED_ }
    PAUSED_PSP -> {color: Color.orange900, status : getString PAUSED_STR }
    CANCELLED_PSP ->  {color: Color.red, status : getString CANCELLED_  }
    PENDING -> {color: Color.orange900, status : getString PENDING_STR }
    MANDATE_FAILED -> {color: Color.orange900, status : getString PENDING_STR }
    _ -> {color: Color.orange900, status : getString PENDING_STR }

showOfferApplicable :: SubscriptionScreenState -> Boolean
showOfferApplicable state = 
  let currentPlanOffers = length $ filter (\item -> not item.addedFromUI) state.data.managePlanData.currentPlan.offers
      selectedPlanOffers = length $ filter (\item -> not item.addedFromUI) state.props.managePlanProps.selectedPlanItem.offers
  in state.props.managePlanProps.selectedPlanItem.id /= state.data.managePlanData.currentPlan.id && selectedPlanOffers < currentPlanOffers

commonImageView :: String -> Int -> Int -> Margin -> Padding -> forall w . PrestoDOM (Effect Unit) w
commonImageView imageName imageHeight imageWidth imageViewMargin imageViewPadding =
  imageView $
      [ imageWithFallback $ imageName <> "," <> (HU.getAssetStoreLink FunctionCall) <> imageName <> ".png"
      , height $ V imageHeight
      , width $ V imageWidth
      , margin imageViewMargin
      , padding imageViewPadding
      ]

lottieView :: SubscriptionScreenState -> String -> Margin -> Padding -> forall w . PrestoDOM (Effect Unit) w
lottieView state viewId margin' padding'= 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin margin'
  , padding padding'
  , cornerRadius 4.0
  , background Color.blue600
  , alpha if ((state.data.config.enableSubscriptionPopups && state.data.orderId /= Nothing) || state.props.lastPaymentType == Just "AUTOPAY_REGISTRATION") then 0.4 else 1.0
  ][
    lottieAnimationView
    [ id (getNewIDWithTag viewId)
    , afterRender (\action-> do
                  void $ pure $ JB.startLottieProcess JB.lottieAnimationConfig {rawJson = lottieJsonAccordingToLang (isOnFreeTrial FunctionCall), lottieId = (getNewIDWithTag viewId), scaleType = "CENTER_CROP", forceToUseRemote = true}
                  )(const NoAction)
    , height $ V 35
    , width MATCH_PARENT
    ]
  ]

offerCardBannerView :: forall w. (Action -> Effect Unit) -> Boolean -> Boolean -> Boolean -> OfferBanner -> Boolean -> PrestoDOM (Effect Unit) w
offerCardBannerView push useMargin visibility' isPlanCard offerBannerProps isFreezed =
  let horizontalMargin = if useMargin then 16 else 0
  in
  linearLayout
    [ height MATCH_PARENT
    , width  MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin horizontalMargin 0 horizontalMargin 0
    , visibility if visibility' && offerBannerProps.showOfferBanner && not HU.isDateGreaterThan offerBannerProps.offerBannerValidTill then VISIBLE else GONE
    , weight 1.0
    , clickable false
    , alpha if isFreezed then 0.6 else 1.0
    ][
        Banner.view (push <<< OfferCardBanner) (offerCardBannerConfig isPlanCard offerBannerProps)
    ]

lottieJsonAccordingToLang :: Boolean -> String
lottieJsonAccordingToLang isOnFreeTrial = 
  (HU.getAssetsBaseUrl FunctionCall) <> case getValueToLocalStore LANGUAGE_KEY of 
    "HI_IN" -> if isOnFreeTrial then "lottie/ny_ic_subscription_info_hindi_01.json" else "lottie/ny_ic_subscription_info_hindi_02.json"
    "KN_IN" -> if isOnFreeTrial then "lottie/ny_ic_subscription_info_kannada_01.json" else "lottie/ny_ic_subscription_info_kannada_02.json"
    "TA_IN" -> if isOnFreeTrial then "lottie/ny_ic_subscription_info_tamil_01.json" else "lottie/ny_ic_subscription_info_tamil_02.json"
    "BN_IN" -> if isOnFreeTrial then "lottie/ny_ic_subscription_info_bengali_01.json" else "lottie/ny_ic_subscription_info_bengali_02.json"
    _ -> if isOnFreeTrial then "lottie/ny_ic_subscription_info_01.json" else "lottie/ny_ic_subscription_info_02.json"
