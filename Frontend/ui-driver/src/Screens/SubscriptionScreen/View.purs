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
import Data.Either (Either(..))
import Data.Int (toNumber, pow, ceil)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Maybe as Mb
import Data.Number (fromString) as Number
import Data.String as DS
import Data.Time.Duration (Seconds(..))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (convertUTCtoISC, flowRunner, screenHeight, screenWidth)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink, getImageUrl, getValueBtwRange)
import Helpers.Utils as HU
import JBridge (getWidthFromPercent)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, discard, map, not, pure, show, unit, void, ($), (&&), (*), (+), (-), (/), (/=), (<), (<<<), (<>), (==), (>), (||))
import Presto.Core.Types.API (ErrorResponse)
import Presto.Core.Types.Language.Flow (Flow, doAff, getState, delay)
import PrestoDOM (Gradient(..), Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), afterRender, alignParentBottom, alpha, background, clickable, color, cornerRadius, ellipsize, fontStyle, frameLayout, gradient, gravity, height, horizontalScrollView, imageView, imageWithFallback, lineHeight, linearLayout, margin, maxLines, onBackPressed, onClick, orientation, padding, relativeLayout, scrollBarX, scrollBarY, scrollView, shimmerFrameLayout, singleLine, stroke, text, textFromHtml, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.List as PrestoList
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens as ScreenNames
import Screens.SubscriptionScreen.Controller (Action(..), ScreenOutput, eval, getAllFareFromArray, getPlanPrice)
import Screens.Types (AutoPayStatus(..), DueItem, GlobalProps, KioskLocation(..), MyPlanData, OptionsMenuState(..), PlanCardConfig, PromoConfig, SubscriptionScreenState, SubscriptionSubview(..))
import Services.API (FeeType(..), GetCurrentPlanResp(..), GetDriverInfoResp(..), KioskLocationRes(..), KioskLocationResp(..), OrderStatusRes(..), PaymentBreakUp(..), UiPlansResp(..))
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalNativeStore, getValueToLocalStore, setValueToLocalStore)
import Styles.Colors as Color
import Types.App (GlobalState(..), defaultGlobalState)

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
    let (GetDriverInfoResp driverInfo) = globalProp.driverInformation
    if isJust driverInfo.autoPayStatus then do 
      currentPlan <- Remote.getCurrentPlan ""
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
            Just _ -> doAff do liftEffect $ push $ loadMyPlans resp'
        Left err -> doAff do liftEffect $ push $ errorAction err
  else if (state.props.subView == FindHelpCentre) then do
    locations <- Remote.getKioskLocations ""
    case locations of
      Right (KioskLocationResp locationsResp) -> doAff do liftEffect $ push $ loadHelpCentre state.props.currentLat state.props.currentLon locationsResp
      Left err -> if err.code /= 404 then doAff do liftEffect $ push $ errorAction err
                  else doAff do liftEffect $ push $ loadHelpCentre state.props.currentLat state.props.currentLon []
  else pure unit

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
  ][ linearLayout
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
              ][
                joinPlanView push state (state.props.subView == JoinPlan)
                , managePlanView push state (state.props.subView == ManagePlan)
                , myPlanView push state (state.props.subView == MyPlan)
                , autoPayDetailsView push state (state.props.subView == PlanDetails)
                , if (state.props.subView == FindHelpCentre) then findHelpCentreView push state (state.props.subView == FindHelpCentre) else dummyView
                , if (state.props.subView == DuesView) then duesOverView push state (state.props.subView == DuesView) else dummyView
                , if (state.props.subView == DueDetails) then dueDetails push state (state.props.subView == DueDetails) else dummyView
                , if state.props.optionsMenuState /= ALL_COLLAPSED then
                      OptionsMenu.view (push <<< OptionsMenuAction) (optionsMenuConfig state) 
                  else linearLayout[][]
              ]
          ]
        , BottomNavBar.view (push <<< BottomNavBarAction) (navData ScreenNames.SUBSCRIPTION_SCREEN)
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
  ][ relativeLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , background Color.blue600
      ][  imageView
          [ width $ V 116
          , height $ V 368
          , margin $ MarginTop 20
          , imageWithFallback "ny_ic_ny_driver,"
          ]
        , enjoyBenefitsView push state
        , plansBottomView push state
      ]

  ]

enjoyBenefitsView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
enjoyBenefitsView push state = 
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , gravity RIGHT
    , orientation VERTICAL
    , margin $ Margin 116 (screenHeight unit / 30) 10 0
    ][  linearLayout
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        ][ commonTV push (getString ENJOY_THESE_BENEFITS) Color.black800 (FontStyle.subHeading2 TypoGraphy) 0 LEFT
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
              [(getString ZERO_COMMISION), (getString EARN_TODAY_PAY_TOMORROW), (getString PAY_ONLY_IF_YOU_TAKE_RIDES), getString GET_SPECIAL_OFFERS]
            ) 
            , textView [
                text $ getString VALID_ONLY_IF_PAYMENT
                , textSize if state.props.isSelectedLangTamil then FontSize.a_8 else FontSize.a_10
                , fontStyle $ FontStyle.medium LanguageStyle
                , color Color.black700
                , margin $ Margin 22 3 0 0
          ]
        ]
        
    ]

paymentPendingView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
paymentPendingView push state = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.yellow800
  , cornerRadii $ Corners 24.0 false false true true
  , padding $ Padding 16 12 16 12
  , visibility if (state.data.myPlanData.autoPayStatus == PENDING && state.data.orderId /= Nothing) then VISIBLE else GONE
  ][  textView
      [ text $ getString AUTOPAY_SETUP_PENDING_STR
      , textSize if state.props.isSelectedLangTamil then FontSize.a_12 else FontSize.a_14
      , fontStyle $ FontStyle.semiBold LanguageStyle
      , color Color.black800
      ]
    , textView
      [ text $ getString PAYMENT_PENDING_DESC_STR
      , textSize if state.props.isSelectedLangTamil then FontSize.a_10 else FontSize.a_12
      , fontStyle $ FontStyle.medium LanguageStyle
      , color Color.black800
      ]
    , textView
      [ text $ getString OFFERS_NOT_APPLICABLE
      , textSize if state.props.isSelectedLangTamil then FontSize.a_10 else FontSize.a_12
      , fontStyle $ FontStyle.medium LanguageStyle
      , color Color.red
      ]
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
  , padding $ Padding 20 20 20 0
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
              , width $ V $ getWidthFromPercent 70
              , text (getString CHOOSE_YOUR_PLAN)
              , color Color.black800
              ] <> FontStyle.body8 TypoGraphy
          , linearLayout
            [ weight 1.0
            , height WRAP_CONTENT
            , gravity RIGHT
            ][ imageView
                [ width $ V 85
                , height $ V 20
                , imageWithFallback "ny_ic_upi_autopay,"
                ]
            ]
          ]
        , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity CENTER_VERTICAL
          , onClick (\action -> do
                        _ <- push action
                        _ <- pure $ JB.cleverTapCustomEvent "ny_driver_nyplans_watchvideo_clicked"
                        _ <- pure $ JB.metaLogEvent "ny_driver_nyplans_watchvideo_clicked"
                        _ <- pure $ JB.firebaseLogEvent "ny_driver_nyplans_watchvideo_clicked"
                        _ <- JB.openUrlInApp $ case getValueToLocalNativeStore LANGUAGE_KEY of
                                          "EN_US" -> "https://www.youtube.com/playlist?list=PL4AEiRR3V7kHcg2-fgzvDXDqWihZD9mTK"
                                          "KN_IN" -> "https://www.youtube.com/playlist?list=PL4AEiRR3V7kHcg2-fgzvDXDqWihZD9mTK"
                                          _ -> "https://www.youtube.com/playlist?list=PL4AEiRR3V7kHcg2-fgzvDXDqWihZD9mTK"
                        pure unit
                        ) (const NoAction)
          ][ textView $
              [ height WRAP_CONTENT
              , width $ V $ getWidthFromPercent 70
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
                        Just plan -> planCardView push item (item.id == plan.id) true ChoosePlan state.props.isSelectedLangTamil
                        Nothing -> planCardView push item false true ChoosePlan state.props.isSelectedLangTamil
                  ) state.data.joinPlanData.allPlans)
          ]
        , PrimaryButton.view (push <<< JoinPlanAC) (joinPlanButtonConfig state)
      ]
  ]

commonTV :: forall w. (Action -> Effect Unit) -> String -> String -> (forall properties. (Array (Prop properties))) -> Int -> Gravity -> PrestoDOM (Effect Unit) w
commonTV push text' color' fontStyle marginTop gravity' =
  textView $
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , text text'
  , color color'
  , gravity gravity'
  , margin $ MarginTop marginTop
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
     ][ textView
        [ textFromHtml $ "<u>" <> (getString VIEW_AUTOPAY_DETAILS) <> "</u>"
        , textSize if state.props.isSelectedLangTamil then FontSize.a_10 else FontSize.a_12
        , padding $ Padding 5 5 5 5
        , fontStyle $ FontStyle.regular LanguageStyle
        , color Color.black800
        , onClick push $ const ViewAutopayDetails
        ]
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
  , gradient (Linear 180.0 ["#E2EAFF", "#F5F8FF"])
  ][ paymentPendingView push state
   , myPlanBodyview push state
  ]

headerView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w 
headerView push state =
  let config = getHeaderConfig state.props.subView (state.props.myPlanProps.dueType == MANUAL_PAYMENT)
  in 
    linearLayout
    [ height $ V 55
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , gravity CENTER_VERTICAL
    , padding $ PaddingHorizontal 16 0
    , background Color.white900
    , stroke $ "1," <> Color.grey900
    ][ imageView
      [ width $ V 24
      , height $ V 24
      , margin $ MarginRight 16
      , visibility if config.backbutton then VISIBLE else GONE
      , onClick push $ const $ BackPressed
      , imageWithFallback "ny_ic_chevron_left,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_left.png"
      ]
    , textView
      [ text config.title
      , textSize if state.props.isSelectedLangTamil then FontSize.a_16 else FontSize.a_18
      , fontStyle $ FontStyle.semiBold LanguageStyle
      , color Color.darkCharcoal
      , padding $ PaddingBottom 4
      , weight 1.0
      ]
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
  let isFreezed = (state.data.myPlanData.autoPayStatus == PENDING && state.data.orderId /= Nothing)
  in
  scrollView  
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , scrollBarY false
  ][ linearLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , orientation VERTICAL
     , padding $ PaddingVertical 24 12
     ][ linearLayout
       [ height WRAP_CONTENT
       , width MATCH_PARENT
       , orientation HORIZONTAL
       , gravity CENTER_VERTICAL
       , margin $ Margin 16 0 16 16 
       ][ linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , gravity CENTER_VERTICAL
          , weight 1.0
          ][ textView 
            [ text (getString MY_PLAN)
            , textSize if state.props.isSelectedLangTamil then FontSize.a_18 else FontSize.a_20
            , fontStyle $ FontStyle.bold LanguageStyle
            , color Color.black800
            , padding $ PaddingBottom 5
            ]
          , imageView
            [ width $ V 38
            , height $ V 38
            , margin (MarginLeft 4)
            , padding $ Padding 8 8 8 8
            , imageWithFallback "ny_ic_youtube,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_youtube.png"
            , onClick (\action -> do
                        _<- push action
                        _ <- pure $ JB.cleverTapCustomEvent "ny_driver_myplan_watchvideo_clicked"
                        _ <- pure $ JB.metaLogEvent "ny_driver_myplan_watchvideo_clicked"
                        _ <- pure $ JB.firebaseLogEvent "ny_driver_myplan_watchvideo_clicked"
                        _ <- JB.openUrlInApp $ case getValueToLocalNativeStore LANGUAGE_KEY of
                                          "EN_US" -> "https://www.youtube.com/playlist?list=PL4AEiRR3V7kHcg2-fgzvDXDqWihZD9mTK"
                                          "KN_IN" -> "https://www.youtube.com/playlist?list=PL4AEiRR3V7kHcg2-fgzvDXDqWihZD9mTK"
                                          _ -> "https://www.youtube.com/playlist?list=PL4AEiRR3V7kHcg2-fgzvDXDqWihZD9mTK"
                        pure unit
                        ) (const NoAction)
            ]
           ]
         , paymentMethodView push state.data.myPlanData
       ]
      , textView [
          textFromHtml $ getString NO_RIDES_NO_CHARGE
          , textSize if state.props.isSelectedLangTamil then FontSize.a_12 else FontSize.a_14
          , fontStyle $ FontStyle.semiBold LanguageStyle
          , color Color.white900
          , background if state.data.myPlanData.autoPayStatus == PENDING && state.data.orderId /= Nothing then Color.greenDisabled else Color.greenDull
          , cornerRadius 4.0
          , width MATCH_PARENT
          , height WRAP_CONTENT
          , margin $ Margin 16 0 16 16
          , padding $ PaddingVertical 8 8
          , gravity CENTER
         ]
     , planDescriptionView push state.data.myPlanData.planEntity  (state.data.myPlanData.autoPayStatus == PENDING) state.props.isSelectedLangTamil
     , alertView push (getImageURL "ny_ic_about,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_about.png") Color.black800 (getString PAYMENT_MODE_CHANGED_TO_MANUAL) (getString PAYMENT_MODE_CHANGED_TO_MANUAL_DESC) "" NoAction (state.data.myPlanData.autoPayStatus == PAUSED_PSP) state.props.isSelectedLangTamil true isFreezed
     , alertView push (getImageURL "ny_ic_about") Color.black800 (getString PAYMENT_MODE_CHANGED_TO_MANUAL) (getString PAYMENT_CANCELLED) "" NoAction (any (_ == state.data.myPlanData.autoPayStatus) [CANCELLED_PSP, SUSPENDED]) state.props.isSelectedLangTamil false isFreezed
     , alertView push (getImageURL "ny_ic_warning_red") Color.red (getString LOW_ACCOUNT_BALANCE) (DS.replace (DS.Pattern "<X>") (DS.Replacement $ show $ fromMaybe 0.0 state.data.myPlanData.lowAccountBalance) (getString LOW_ACCOUNT_BALANCE_DESC)) "" NoAction (Mb.isJust state.data.myPlanData.lowAccountBalance) state.props.isSelectedLangTamil false isFreezed
     , alertView push (getImageURL "ny_ic_warning_blue") Color.blue800 (getString SWITCH_AND_SAVE) (getString SWITCH_AND_SAVE_DESC) (getString SWITCH_NOW) NoAction state.data.myPlanData.switchAndSave state.props.isSelectedLangTamil false isFreezed
     , duesView push state
    ]
  ]


planDescriptionView :: forall w. (Action -> Effect Unit) -> PlanCardConfig -> Boolean -> Boolean -> PrestoDOM (Effect Unit) w 
planDescriptionView push state isFreezed isSelectedLangTamil = 
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background Color.white900
  , stroke $ "1," <> Color.grey900
  , padding $ Padding 16 12 16 12
  , margin $ MarginHorizontal 16 16
  , cornerRadius 8.0
  , orientation VERTICAL
  , alpha if isFreezed then 0.5 else 1.0
  ][ linearLayout
     [ height WRAP_CONTENT
     , width MATCH_PARENT
     , gravity CENTER_VERTICAL
     , margin $ MarginBottom 5
     ][ textView
        [ text state.title
        , textSize if isSelectedLangTamil then FontSize.a_12 else FontSize.a_14
        , weight 1.0
        , fontStyle $ FontStyle.bold LanguageStyle
        , color Color.black700
        ]
      , planPriceView state.priceBreakup state.frequency isSelectedLangTamil
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      ][ textView
         [ text state.description
         , textSize if isSelectedLangTamil then FontSize.a_10 else FontSize.a_12
         , fontStyle $ FontStyle.medium LanguageStyle
         , color Color.black600
         , weight 1.0
         ]
       ]
    , horizontalScrollView 
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , scrollBarX false
      ][ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        , margin $ MarginTop 8
        , visibility if (DA.length state.offers > 0) then VISIBLE else GONE
        ](map  (\item -> promoCodeView push item) state.offers)
       ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ](map (\item ->
          linearLayout
            ([ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , padding $ Padding 8 8 8 8
            , margin $ MarginVertical 8 8
            , background Color.grey700  
            , cornerRadius 4.0
            ] <> case item.offerDescription of 
                  Mb.Just desc -> [text desc, visibility VISIBLE]
                  Mb.Nothing -> [visibility GONE])
            [ textView
              [ textSize if isSelectedLangTamil then FontSize.a_10 else FontSize.a_12
              , textFromHtml $ Mb.fromMaybe "" item.offerDescription
              , fontStyle $ FontStyle.regular LanguageStyle
              , color Color.black600
              , lineHeight "20"
              ]
            ]
         )state.offers)
    --  , arrowButtonView push (getString MANAGE_PLAN) true (if isFreezed then NoAction else ManagePlanAC) isSelectedLangTamil -- TODO: Removing this for now.
  ]

duesView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w 
duesView push state = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , padding $ PaddingBottom 16
  , orientation VERTICAL
  , background Color.white900
  , cornerRadius 8.0
  , stroke $ "1," <> Color.grey900
  , margin $ Margin 16 16 16 0
  -- , visibility if (state.data.myPlanData.autoPayStatus == PENDING) then GONE else VISIBLE
  , visibility VISIBLE
  ][ 
    linearLayout[
        gravity CENTER
      , padding $ Padding 16 16 16 8
      , width MATCH_PARENT
      , onClick push $ const $ ToggleDueDetailsView
    ][ imageView
        [ width $ V 16
        , height $ V 16
        , margin (MarginRight 4)
        , visibility if state.props.myPlanProps.overDue then VISIBLE else GONE
        , imageWithFallback "ny_ic_warning_unfilled_red,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_up.png"
        ]
        , textView
          [ text (getString YOUR_DUES)
          , textSize if state.props.isSelectedLangTamil then FontSize.a_12 else FontSize.a_14
          , weight 1.0
          , fontStyle $ FontStyle.semiBold LanguageStyle
          , gravity CENTER_VERTICAL
          , color if state.props.myPlanProps.overDue then Color.red else Color.black800
          ]
        , textView
          [ text $  "₹" <> show state.data.myPlanData.currentDueAmount
          , textSize if state.props.isSelectedLangTamil then FontSize.a_16 else FontSize.a_18
          , fontStyle $ FontStyle.bold LanguageStyle
          , color if state.props.myPlanProps.overDue then Color.red else Color.blue800
          , padding $ PaddingBottom 2
          ]
        , imageView [
          imageWithFallback if state.props.myPlanProps.isDueViewExpanded 
                                  then "ny_ic_chevron_up,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_up.png"
                                  else "ny_ic_chevron_down,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_down.png"
          , height $ V 12
          , width $ V 12
          , margin $ MarginLeft 6
        ]
    ]
   , linearLayout[
      orientation VERTICAL
      , padding $ PaddingHorizontal 16 16
      , visibility if state.props.myPlanProps.isDueViewExpanded then VISIBLE else GONE
   ][
      textView
      [ textFromHtml $ getString if state.data.myPlanData.mandateStatus == "active" then YOUR_DUES_DESCRIPTION else YOUR_DUES_DESCRIPTION_MANUAL
      , textSize if state.props.isSelectedLangTamil then FontSize.a_10 else FontSize.a_12
      , fontStyle $ FontStyle.medium LanguageStyle
      , color Color.black600
      , margin $ MarginBottom 16
      ]
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
          ][ textView
            [ text (getString CURRENT_DUES)
            , textSize if state.props.isSelectedLangTamil then FontSize.a_10 else FontSize.a_12
            , fontStyle $ FontStyle.medium LanguageStyle
            , color Color.black600
            , weight 1.0
            ] 
          , textView
            [ text (getString YOUR_LIMIT)
            , textSize if state.props.isSelectedLangTamil then FontSize.a_10 else FontSize.a_12
            , fontStyle $ FontStyle.medium LanguageStyle
            , color Color.black600
            ]              
          ]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity CENTER_VERTICAL
          ][ textView
            [ text $  "₹" <> show state.data.myPlanData.currentDueAmount
            , textSize if state.props.isSelectedLangTamil then FontSize.a_16 else FontSize.a_18
            , fontStyle $ FontStyle.bold LanguageStyle
            , color if state.props.myPlanProps.overDue then Color.red else Color.blue800
            , weight 1.0
            ] 
          , textView
            [ text $ "₹" <>  show state.data.myPlanData.maxDueAmount
            , textSize if state.props.isSelectedLangTamil then FontSize.a_16 else FontSize.a_18
            , fontStyle $ FontStyle.bold LanguageStyle
            , color Color.black700
            ]             
          ]
        , relativeLayout
          [ height $ V 4
          , width MATCH_PARENT
          , orientation HORIZONTAL
          , margin $ MarginTop 8
          ][ linearLayout
            [ height $ V 4
            , width $ V $ (screenWidth unit) - 100
            , background Color.black700
            , cornerRadius 4.0
            ][]
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            ][ linearLayout
                [ height $ V 4
                , width $ V $ ceil $ getValueBtwRange state.data.myPlanData.currentDueAmount 0.0 state.data.myPlanData.maxDueAmount 0.0 (toNumber $ (screenWidth unit) - 100)
                , background if state.props.myPlanProps.overDue then Color.red else Color.orange900
                , cornerRadius 4.0
                ][]
                , linearLayout
                  [ height $ V 4
                  , width $ V $ ceil $ getValueBtwRange state.data.myPlanData.currentDueAmount 0.0 state.data.myPlanData.maxDueAmount 0.0 (toNumber $ (screenWidth unit) - 100)
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
     , visibility VISIBLE --TODO
     ][ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , padding $ Padding 16 12 16 12
        , gravity CENTER_VERTICAL
        , onClick push $ const $ if state.props.myPlanProps.multiTypeDues then ViewDuesOverView else ToggleDueDetails
        ] [ textView $
              [ text (getString DUE_DETAILS)
              , color Color.black800
              , weight 1.0
              ] <> FontStyle.tags TypoGraphy
           , commonImageView (  if state.props.myPlanProps.multiTypeDues then "ny_ic_chevron_right"
                                else if state.props.myPlanProps.isDuesExpanded then "ny_ic_chevron_up,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_up.png"
                                else "ny_ic_chevron_down,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_down.png") 12 12 (MarginRight 4) (Padding 0 0 0 0)
          ]
        , tripList state.data.myPlanData.dueItems (state.props.myPlanProps.dueType == MANUAL_PAYMENT) state.props.myPlanProps.isDuesExpanded
      ] 
   ]
   , if state.data.myPlanData.autoPayStatus `elem` [SUSPENDED, CANCELLED_PSP, PAUSED_PSP, PENDING, NO_AUTOPAY] then  PrimaryButton.view (push <<< ResumeAutoPay) (clearDueButtonConfig state) else dummyView
   , if true then arrowButtonView push (getString SETUP_AUTOPAY) false NoAction state.props.isSelectedLangTamil else dummyView
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
     [ textSize FontSize.a_10
     , fontStyle $ FontStyle.medium LanguageStyle
     , color Color.blue900
     , singleLine true
     , padding $ PaddingBottom 3
     ] <> case state.title of
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
      , textView
        [ text title
        , textSize if isSelectedLangTamil then FontSize.a_12 else FontSize.a_14
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , color primaryColor
        , padding $ PaddingBottom 3
        ]
      ] 
   , textView
     [ textFromHtml description
     , textSize if isSelectedLangTamil then FontSize.a_10 else FontSize.a_12
     , fontStyle $ FontStyle.medium LanguageStyle
     , color Color.black600
     , margin $ if buttonText /= "" then MarginBottom 12 else MarginBottom 0
     ]
   , if buttonText /= "" then arrowButtonView push buttonText true action isSelectedLangTamil else dummyView
   , linearLayout [
      width MATCH_PARENT
      , gravity CENTER
      , margin $ MarginTop 14
      , onClick push $ const RefreshPage
      , visibility if showRefresh then VISIBLE else GONE
   ][ imageView [
        imageWithFallback "ny_ic_refresh"
        , height $ V 16
        , width $ V 16
      ]
      , textView [
          text $ getString REFRESH_STRING
          , color Color.blue800
          , textSize if isSelectedLangTamil then FontSize.a_12 else FontSize.a_14
          , fontStyle $ FontStyle.semiBold LanguageStyle
          , margin $ MarginLeft 4
      ]
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
     ][ textView
        [ text title
        , textSize if isSelectedLangTamil then FontSize.a_12 else FontSize.a_14
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , color Color.blue800
        , margin (MarginRight 4)
        , padding $ PaddingBottom 3
        ]
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
  , orientation HORIZONTAL
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
  , textView 
    [ text (getString UPI_AUTOPAY_S)
    , textSize FontSize.a_10
    , fontStyle $ FontStyle.medium LanguageStyle
    , color Color.black900
    , padding $ PaddingBottom 3
    ]
  , linearLayout
    [ height $ V 4
    , width $ V 4
    , background statusPillData.color
    , cornerRadius 12.0
    , margin $ MarginHorizontal 4 4
    ][]
  , textView
    [ text $ statusPillData.status
    , textSize FontSize.a_10
    , fontStyle $ FontStyle.medium LanguageStyle
    , color statusPillData.color
    , padding $ PaddingBottom 3
    ]
  ]

managePlanBodyView :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> PrestoDOM (Effect Unit) w
managePlanBodyView push state =
  scrollView
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , scrollBarY false
  -- , margin $ MarginVertical 55 45
  ][ linearLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , padding $ PaddingTop 24
     , margin $ MarginHorizontal 16 16
     , orientation VERTICAL
     ][ textView
        [ text (getString CURRENT_PLAN)
        , textSize if state.props.isSelectedLangTamil then FontSize.a_10 else FontSize.a_12
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , color Color.black700
        , margin $ MarginBottom 12
        ]
      , planCardView push state.data.managePlanData.currentPlan (state.data.managePlanData.currentPlan.id == state.props.managePlanProps.selectedPlanItem.id) true SelectPlan state.props.isSelectedLangTamil
      , textView
        [ text (getString ALTERNATE_PLAN)
        , textSize if state.props.isSelectedLangTamil then FontSize.a_10 else FontSize.a_12
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , color Color.black700
        , margin $ MarginVertical 32 12 
        ]
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ](map(
             (\item -> planCardView push item (item.id == state.props.managePlanProps.selectedPlanItem.id) true SelectPlan state.props.isSelectedLangTamil)
             ) state.data.managePlanData.alternatePlans)
      , textView [
        text (getString OFFERS_APPLICABLE_ON_DAILY_UNLIMITED)
        , textSize if state.props.isSelectedLangTamil then FontSize.a_10 else FontSize.a_12
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , color Color.black600
        , margin $ MarginBottom 16
        , visibility if showOfferApplicable state then VISIBLE else GONE
      ]
      , PrimaryButton.view (push <<< SwitchPlan) (switchPlanButtonConfig state)
     ]
   ]

planCardView :: forall w. (Action -> Effect Unit) -> PlanCardConfig -> Boolean -> Boolean -> (PlanCardConfig -> Action) -> Boolean -> PrestoDOM (Effect Unit) w
planCardView push state isSelected clickable' action isSelectedLangTamil =
  -- PrestoAnim.animationSet                TODO :: Animations
  -- [ translateInXForwardAnim true] $
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background if isSelected then Color.blue600 else Color.white900
  , stroke $ "1," <> (if isSelected then Color.blue800 else Color.grey900)
  , padding $ Padding 16 12 16 12
  , cornerRadius 8.0
  , orientation VERTICAL
  , margin $ MarginBottom 16
  , clickable clickable'
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
        , fontStyle $ (if isSelected then FontStyle.bold else FontStyle.semiBold) LanguageStyle
        , color if isSelected then Color.blue900 else Color.black700
        ]
      , planPriceView state.priceBreakup state.frequency isSelectedLangTamil
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      ][ textView
         [ text state.description
         , textSize if isSelectedLangTamil then FontSize.a_10 else FontSize.a_12
         , fontStyle $ FontStyle.medium LanguageStyle
         , color Color.black600
         , weight 1.0
         ]
       , if state.showOffer then offerCountView (DA.length state.offers) isSelected else linearLayout[visibility GONE][]
       ]
    , horizontalScrollView 
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , scrollBarX false
      , margin $ MarginVertical 8 8
      , visibility if isSelected && (DA.length state.offers > 0) then VISIBLE else GONE
      ][ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        ](map  (\item -> promoCodeView push item) state.offers)
       ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , visibility if isSelected && (DA.length state.offers > 0) then VISIBLE else GONE
      ](map (\item ->
          linearLayout
            ([ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , padding $ Padding 8 8 8 8
            , margin $ MarginTop if isSelected then 0 else 8
            , background Color.white900
            , cornerRadius 4.0
            ] <> case item.offerDescription of 
                  Mb.Just desc -> [text desc, visibility if isSelected then VISIBLE else GONE]
                  Mb.Nothing -> [visibility GONE])
            [ textView
              [ textSize if isSelectedLangTamil then FontSize.a_10 else FontSize.a_12
              , textFromHtml $ Mb.fromMaybe "" item.offerDescription
              , fontStyle $ FontStyle.regular LanguageStyle
              , color Color.black600
              , lineHeight "20"
              ]
            ]
         )state.offers)
    ]

offerCountView :: forall w. Int -> Boolean -> PrestoDOM (Effect Unit) w
offerCountView count isSelected = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , cornerRadius 100.0
  , stroke $ "1," <> Color.blue700
  , background Color.blue600
  , padding $ Padding 10 2 10 2
  , visibility if (count > 0 && not isSelected) then VISIBLE else GONE
  , gravity CENTER_VERTICAL
  ][ imageView
     [ imageWithFallback $ getImageURL "ny_ic_discount"
     , width $ V 12
     , height $ V 12
     , margin $ MarginRight 4
     ]
   , textView
     [ text $ show count <> " " <> "Offer" <> if count == 1 then "" else "s"
     , textSize FontSize.a_10
     , fontStyle $ FontStyle.semiBold LanguageStyle
     , color Color.blue900
     , padding $ PaddingBottom 3
     ]
  ]

dummyView :: forall w. PrestoDOM (Effect Unit) w
dummyView = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  ][]

getImageURL :: String -> String
getImageURL imageName = imageName <> "," <> (getAssetStoreLink FunctionCall) <> imageName <> ".png"

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
            , height MATCH_PARENT
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
                  ][ commonTV push item.key Color.black700 (FontStyle.body3 TypoGraphy) 0 LEFT
                  , linearLayout
                      [ weight 1.0
                      , height WRAP_CONTENT
                      , gravity RIGHT
                      ][ commonTV push item.val Color.black900 (FontStyle.body6 TypoGraphy) 0 RIGHT ]
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
      , height WRAP_CONTENT
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
              ][ commonTV push "UPI Autopay" Color.black800 (FontStyle.body1 TypoGraphy) 0 LEFT
               , imageView
                  [ imageWithFallback "ny_ic_upi_logo,"
                  , height $ V 14
                  , width $ V 14
                  ]
              ]
          ] <> if (isJust state.data.autoPayDetails.payerUpiId) then [commonTV push (fromMaybe "" state.data.autoPayDetails.payerUpiId) Color.black800 (FontStyle.paragraphText TypoGraphy) 0 LEFT] else [])
          
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
          [ imageWithFallback "ny_ic_api_failed,"
          , height $ V 180
          , width $ V 280
          ]
        , commonTV push (getString WE_MIGHT_BE_LOST) Color.black900 (FontStyle.h2 TypoGraphy) 0 CENTER
        , textView $ 
          [ textFromHtml $ (getString EXEPERIENCING_ERROR) <> " " <> state.data.errorMessage <> " \n" <> (getString PLEASE_TRY_AGAIN)
          , textSize if state.props.isSelectedLangTamil then FontSize.a_12 else FontSize.a_14
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
      , orientation HORIZONTAL
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
            , margin $ MarginTop 30
            , orientation HORIZONTAL][
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

planPriceView :: forall w. Array PaymentBreakUp -> String -> Boolean -> PrestoDOM (Effect Unit) w
planPriceView fares frequency isSelectedLangTamil =
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  ][ textView $ 
     [ textFromHtml $ "<strike> ₹" <> getPlanPrice fares "INITIAL_BASE_FEE" <> "</stike>"
     , visibility if (getAllFareFromArray fares ["INITIAL_BASE_FEE", "FINAL_FEE"]) > 0.0 then VISIBLE else GONE
     , color Color.black600
     ] <> FontStyle.body7 TypoGraphy
   , textView
      [ text $ "₹" <> (getPlanPrice fares "FINAL_FEE") <> "/" <> case frequency of
                                                                    "PER_RIDE" -> getString RIDE
                                                                    "DAILY" -> getString DAY
                                                                    _ -> getString DAY
      , textSize if isSelectedLangTamil then FontSize.a_14 else FontSize.a_16
      , fontStyle $ FontStyle.bold LanguageStyle
      , margin $ MarginLeft 3
      , color Color.black800
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
    , textView
      [ text $ getString NO_HELP_CENTER_IS_ACTIVE_NOW
      , textSize FontSize.a_18
      , color Color.black900
      , fontStyle $ FontStyle.bold LanguageStyle
      , maxLines 3
      ]
    , textView
      [ text $ getString HELP_CENTERS_LOCATION_WILL_APPEAR_HERE_ONCE_THEY_ARE_ACTIVE
      , textSize FontSize.a_14
      , color Color.black700
      , fontStyle $ FontStyle.regular LanguageStyle
      , maxLines 3
      , margin $ MarginTop 10
      , gravity CENTER
      ]
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
      ][  textView
          [ text state.landmark
          , color Color.black800
          , textSize FontSize.a_14
          , fontStyle $ FontStyle.bold LanguageStyle
          , weight 1.0
          ]
        , textView
          [ text state.address
          , color Color.black700
          , textSize FontSize.a_14
          , fontStyle $ FontStyle.regular LanguageStyle
          , weight 1.0
          , maxLines 2
          , margin $ MarginVertical 3 3
          , ellipsize true
          ]
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
  ][ linearLayout
     [ height MATCH_PARENT
     , width MATCH_PARENT
     , orientation VERTICAL
     , visibility if visibility' then VISIBLE else GONE
     , padding $ PaddingBottom 16
     , gradient (Linear 180.0 ["#E2EAFF", "#F5F8FF"])
     ][ 
       dueOverViewCard push state true
       , dueOverViewCard push state false
     ]
  ]
  

dueOverViewCard :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> Boolean -> PrestoDOM (Effect Unit) w
dueOverViewCard push state isManual =
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
            _ <- JB.openUrlInApp $ case getValueToLocalNativeStore LANGUAGE_KEY of
                              "EN_US" -> "https://www.youtube.com/playlist?list=PL4AEiRR3V7kHcg2-fgzvDXDqWihZD9mTK"
                              "KN_IN" -> "https://www.youtube.com/playlist?list=PL4AEiRR3V7kHcg2-fgzvDXDqWihZD9mTK"
                              _ -> "https://www.youtube.com/playlist?list=PL4AEiRR3V7kHcg2-fgzvDXDqWihZD9mTK"
            pure unit
          ) (const NoAction)
        ]
      , textView $
        [ text $  "₹" <> show state.data.myPlanData.currentDueAmount
        , weight 1.0
        , gravity RIGHT
        , color if isManual then Color.orange900 else Color.blue800
        ] <> FontStyle.body7 TypoGraphy
    ]
    , textView $ [
          text $ getString MANUAL_DUE_AS_AUTOPAY_EXECUTION_FAILED
          , color Color.black600        
          , margin $ Margin 16 4 0 0
          , visibility if isManual then VISIBLE else GONE
        ] <> FontStyle.tags TypoGraphy
    , tripList state.data.myPlanData.dueItems isManual true
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
        , onClick push $ const ViewDueDetails
        , padding $ PaddingVertical 16 16
        ] <> FontStyle.body1 TypoGraphy
  ]
tripList :: forall w. Array DueItem -> Boolean -> Boolean -> PrestoDOM (Effect Unit) w
tripList trips isManual isExpanded = 
  linearLayout [
      height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin $ MarginTop 16
      , padding $ PaddingHorizontal 16 16
      , visibility if isExpanded then VISIBLE else GONE
    ][ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
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
      , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , margin $ MarginBottom 16
        ] (map
            (\item -> 
            linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            , margin $ MarginBottom 8
            ][ textView $
              [ text $ convertUTCtoISC item.tripDate "Do MMM, YYYY"
              , color Color.black700
              , weight 1.0
              ] <> FontStyle.body15 TypoGraphy
            , textView $
              [ text $ show item.amount
              , color Color.black700
              ] <> FontStyle.body15 TypoGraphy
            ]
            ) trips)
      

  ]
  
dueDetails :: forall w. (Action -> Effect Unit) -> SubscriptionScreenState -> Boolean -> PrestoDOM (Effect Unit) w
dueDetails push state visibility'= 
  linearLayout
  [ width MATCH_PARENT
  , height MATCH_PARENT
  , visibility if visibility' then VISIBLE else GONE
  ][
    DueDetailsList.view (push <<< DueDetailsListAction) dummyData
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
    _ -> {color: Color.orange900, status : getString PENDING_STR }

showOfferApplicable :: SubscriptionScreenState -> Boolean
showOfferApplicable state = 
  let currentPlanOffers = length $ filter (\item -> not item.addedFromUI) state.data.managePlanData.currentPlan.offers
      selectedPlanOffers = length $ filter (\item -> not item.addedFromUI) state.props.managePlanProps.selectedPlanItem.offers
  in state.props.managePlanProps.selectedPlanItem.id /= state.data.managePlanData.currentPlan.id && selectedPlanOffers < currentPlanOffers

commonImageView :: String -> Int -> Int -> Margin -> Padding -> forall w . PrestoDOM (Effect Unit) w
commonImageView imageName imageHeight imageWidth imageViewMargin imageViewPadding =
  imageView $
      [ imageWithFallback $ imageName <> "," <> (getAssetStoreLink FunctionCall) <> imageName <> ".png"
      , height $ V imageHeight
      , width $ V imageWidth
      , margin imageViewMargin
      , padding imageViewPadding
      ]

dummyData :: DueDetailsListState
dummyData = {
  dues : [{
    date : "05 Oct 2023",
    planType : "DAILY UNLIMITED PLAN",
    offerApplied : 
                {
                title : Just "Freedom Offer: 76% off APPLIED",
                offerDescription : Nothing,
                isGradient : true,
                gradient : ["#FFE7C2", "#FFFFFF", "#DDFFEB"],
                hasImage : true,
                imageURL : "ny_ic_discount,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_discount.png",
                addedFromUI : true
                },
    noOfRides : 4,
    totalEarningsOfDay : 210.0,dueAmount : 25.0,
    fareBreakup : "-",
    expanded : true,
    isAutoPayFailed : true,
    isSplitPayment : false
  },
  {
    date : "05 Oct 2023",
    planType : "DAILY UNLIMITED PLAN",
    offerApplied : {
                title : Just "First Ride FREE",
                offerDescription : Nothing,
                isGradient : false,
                gradient : [],
                hasImage : false,
                imageURL : "",
                addedFromUI : true
                },
    noOfRides : 4,
    totalEarningsOfDay : 210.0,dueAmount : 25.0,
    fareBreakup : "-",
    expanded : true,
    isAutoPayFailed : false,
    isSplitPayment : true
  },
  {
    date : "05 Oct 2023",
    planType : "DAILY UNLIMITED PLAN",
    offerApplied : {
                title : Just "First Ride FREE",
                offerDescription : Nothing,
                isGradient : false,
                gradient : [],
                hasImage : false,
                imageURL : "",
                addedFromUI : true
                },
    noOfRides : 4,
    totalEarningsOfDay : 210.0,
    dueAmount : 25.0,
    fareBreakup : "-",
    expanded : true,
    isAutoPayFailed : true,
    isSplitPayment : true
  }]
}