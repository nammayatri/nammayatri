{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RegistrationScreen.View where

import Common.Types.App
import Debug
import Mobility.Prelude
import Screens.RegistrationScreen.ComponentConfig

import Animation as Anim
import Common.Animation.Config as AnimConfig
import Components.AppOnboardingNavBar as AppOnboardingNavBar
import Components.InAppKeyboardModal as InAppKeyboardModal
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Control.Monad.ST (for)
import Data.Array (all, any, elem, filter, fold, length, mapWithIndex)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.String as DS
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge (lottieAnimationConfig, startLottieProcess)
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import PaymentPage (consumeBP)
import Prelude (Unit, bind, const, map, not, pure, show, unit, void, ($), (&&), (+), (-), (<<<), (<>), (==), (>=), (||), (/=), (*))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Prop, Screen, Visibility(..), afterRender, alignParentBottom, background, clickable, color, cornerRadius, editText, fontStyle, gravity, height, hint, id, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, lottieAnimationView, margin, onAnimationEnd, onBackPressed, onChange, onClick, orientation, padding, pattern, relativeLayout, stroke, text, textSize, textView, visibility, weight, width, scrollView)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.RegistrationScreen.Controller (Action(..), eval, ScreenOutput)
import Screens.Types (RegisterationStep(..), StageStatus(..), ValidationStatus(..))
import Screens.Types as ST
import Storage (KeyStore(..), getValueToLocalNativeStore)
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color

screen :: ST.RegistrationScreenState -> Screen Action ST.RegistrationScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "RegistrationScreen"
  , globalEvents : []
  , eval :
      ( \state action -> do
          let _ = spy "RegistrationScreen ----- state" state
          let _ = spy "RegistrationScreen --------action" action
          eval state action
      )
  }

view ::
  forall w.
  (Action -> Effect Unit) ->
  ST.RegistrationScreenState ->
  PrestoDOM (Effect Unit) w
view push state =
  let showSubscriptionsOption = (getValueToLocalNativeStore SHOW_SUBSCRIPTIONS == "true") && state.data.config.bottomNavConfig.subscription.isVisible
      completedStatusCount = length $ filter (\val -> val == COMPLETED) [ state.data.drivingLicenseStatus, state.data.vehicleDetailsStatus, state.data.permissionsStatus ]
      progressPercent = case showSubscriptionsOption of
        true -> completedStatusCount * 25 + if state.data.subscriptionStatus == IN_PROGRESS then 25 else 0
        false -> if completedStatusCount * 33 == 99 then 100 else completedStatusCount * 33
  in
    Anim.screenAnimation
      $ relativeLayout
          [ height MATCH_PARENT
          , width MATCH_PARENT
          , orientation VERTICAL
          , background Color.white900
          , clickable true
          , onBackPressed push (const BackPressed)
          , afterRender
              ( \action -> do
                  _ <- push action
                  pure unit
              )
              $ const (AfterRender)
          ]
      $ [ linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , gravity CENTER
            , orientation VERTICAL
            ]
            [ PrestoAnim.animationSet [ Anim.fadeIn true]
            $ headerView state push                    
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , padding $ Padding 16 16 16 0
                , weight 1.0
                ]
                [ imageView
                    [ width (V 20)
                    , height (V 20)
                    , imageWithFallback $ fetchImage FF_ASSET "ny_ic_back"
                    , visibility GONE
                    ]
                , linearLayout
                    [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , orientation VERTICAL
                    ]
                    [ linearLayout
                        [ width MATCH_PARENT
                        , height WRAP_CONTENT
                        , margin $ MarginBottom 10
                        ]
                        [ textView
                            $ [ width WRAP_CONTENT
                              , height WRAP_CONTENT
                              , text $ getVarString START_EARNING_IN_FOUR_STEPS [ show $ length state.data.registerationSteps - if showSubscriptionsOption then 0 else 1 ]
                              , weight 1.0
                              ]
                            <> FontStyle.body2 TypoGraphy
                        , textView
                            $ [ width WRAP_CONTENT
                              , height WRAP_CONTENT
                              , text $ show progressPercent <> "% " <> getString COMPLETE
                              ]
                            <> FontStyle.body2 TypoGraphy
                        ]
                    , linearLayout
                        [ width MATCH_PARENT
                        , height WRAP_CONTENT
                        , margin $ MarginBottom 20
                        , weight 1.0
                        ]
                        ( mapWithIndex
                            ( \index item ->
                                linearLayout
                                  [ height $ V 5
                                  , weight 1.0
                                  , cornerRadius 2.0
                                  , visibility case item.stage of
                                      ST.SUBSCRIPTION_PLAN -> boolToVisibility showSubscriptionsOption
                                      _ -> VISIBLE
                                  , background case getStatus item.stage state of
                                      ST.COMPLETED -> Color.green900
                                      ST.IN_PROGRESS -> Color.yellow900
                                      ST.FAILED -> Color.red
                                      ST.NOT_STARTED -> Color.grey900
                                  , margin $ MarginLeft if index == 0 then 0 else 15
                                  ]
                                  []
                            )
                            (state.data.registerationSteps)
                        )
                    , cardItemView push state
                    ]
                ]
            , textView
                $ [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , margin $ Margin 16 0 16 16
                  , padding $ Padding 12 12 12 12
                  , text $ getString COMPLETE_AUTOPAY_LATER
                  , color Color.black700
                  , background Color.yellowOpacity10
                  , cornerRadius 8.0
                  , visibility if state.data.subscriptionStatus == IN_PROGRESS && not state.props.logoutModalView && showSubscriptionsOption then VISIBLE else GONE
                  ]
                <> FontStyle.body1 TypoGraphy
            , textView
                $ [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , margin $ Margin 16 0 16 16
                  , padding $ Padding 12 12 12 12
                  , text if state.data.vehicleDetailsStatus == FAILED then state.data.rcVerficationMessage else state.data.dlVerficationMessage
                  , color Color.black700
                  , background Color.redOpacity10
                  , cornerRadius 8.0
                  , visibility $ boolToVisibility $ state.data.vehicleDetailsStatus == FAILED || state.data.drivingLicenseStatus == FAILED
                  ]
                <> FontStyle.body1 TypoGraphy
            , refreshView push state
            , linearLayout
                [ height $ V 1
                , width MATCH_PARENT
                , background Color.grey900
                , margin $ MarginBottom 16
                , visibility if all (_ == COMPLETED) [ state.data.vehicleDetailsStatus, state.data.drivingLicenseStatus, state.data.permissionsStatus ] then VISIBLE else GONE
                ]
                []
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
                , margin $ Margin 16 0 16 16
                , visibility if all (_ == COMPLETED) [ state.data.vehicleDetailsStatus, state.data.drivingLicenseStatus, state.data.permissionsStatus ] then VISIBLE else GONE
                ]
                [ PrimaryButton.view (push <<< PrimaryButtonAction) (primaryButtonConfig state) ]
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , margin $ Margin 16 0 16 16
                , clickable false
                , visibility $ boolToVisibility $ state.data.cityConfig.showDriverReferral || state.data.config.enableDriverReferral
                ][enterReferralCode push state]
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , margin $ Margin 16 0 16 16
                , clickable false
                , visibility $ boolToVisibility callSupportVisibility
                ][contactSupportView push state]
            ]
            , if state.props.enterReferralCodeModal then enterReferralCodeModal push state else linearLayout[][]
        ]
      <> if state.props.logoutModalView then [ logoutPopupModal push state ] else []
      <> if state.props.contactSupportModal /= ST.HIDE then [contactSupportModal push state] else []
      where callSupportVisibility = (state.data.drivingLicenseStatus == ST.FAILED && state.data.enteredDL /= "__failed") || (state.data.vehicleDetailsStatus == ST.FAILED && state.data.enteredRC /= "__failed")

headerView :: forall w. ST.RegistrationScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerView state push = AppOnboardingNavBar.view (push <<< AppOnboardingNavBarAC) (appOnboardingNavBarConfig state)

contactSupportView :: forall w. (Action -> Effect Unit) -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
contactSupportView push state = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , stroke $ "1," <> Color.grey900
    , cornerRadius 4.0
    , padding $ Padding 10 8 10 8
    , gravity CENTER_VERTICAL
    , onClick push $ const $ SupportClick true
    , visibility $ boolToVisibility $ viewVisibility
    ][  textView $
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , color Color.black800
        , text $ getString NEED_HELP
        , weight 1.0
        ] <> FontStyle.body3 TypoGraphy
      , textView $
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text $ getString CONTACT_SUPPORT
        , margin $ MarginRight 7
        , color Color.blue900
        ] <> FontStyle.tags TypoGraphy
      ]
      where viewVisibility = state.props.contactSupportView && (state.data.cityConfig.registration.callSupport || state.data.cityConfig.registration.whatsappSupport)

contactSupportModal :: forall w. (Action -> Effect Unit) -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
contactSupportModal push state = 
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.black9000
  , clickable true
  , gravity BOTTOM
  , onClick push $ const BackPressed
  , visibility $ boolToVisibility $ state.props.contactSupportModal /= ST.HIDE
  ][  PrestoAnim.animationSet
      [ Anim.translateYAnim AnimConfig.animConfig {fromY = 300, toY = 0, ifAnim = state.props.contactSupportModal == ST.SHOW}
      , Anim.translateYAnim AnimConfig.animConfig {fromY = 0, toY = 300, ifAnim = state.props.contactSupportModal == ST.ANIMATING}
      ] $
        linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity BOTTOM
          , orientation VERTICAL
          , background Color.white900
          , cornerRadii $ Corners 24.0 true true false false
          , padding $ Padding 16 16 16 24
          , onAnimationEnd push $ const $ SupportClick false
          ][  commonTV push (getString CONTACT_SUPPORT_VIA) Color.black700 FontStyle.subHeading2 LEFT 8 NoAction true (Padding 16 0 0 20)
            , supportComponent push state {prefixImg : "ny_ic_whatsapp_black", title : "Whatsapp", desc : getString YOU_CAN_SHARE_SCREENSHOT , postFixImg : "ny_ic_chevron_right", action : WhatsAppClick, visibility : state.data.cityConfig.registration.whatsappSupport}
            , linearLayout[width MATCH_PARENT, height $ V 1, background Color.grey900, margin $ MarginVertical 16 16, visibility border][]
            , supportComponent push state {prefixImg : "ny_ic_direct_call", title : getString CALL, desc : getString PLACE_A_CALL, postFixImg : "ny_ic_chevron_right", action : CallButtonClick, visibility : state.data.cityConfig.registration.callSupport}
          ]
  ]
  where border = boolToVisibility $ state.data.cityConfig.registration.callSupport && state.data.cityConfig.registration.whatsappSupport

type SupportComponent = {
  prefixImg :: String,
  title :: String,
  desc :: String,
  postFixImg :: String,
  action :: Action,
  visibility :: Boolean
}

supportComponent :: forall w. (Action -> Effect Unit) -> ST.RegistrationScreenState -> SupportComponent -> PrestoDOM (Effect Unit) w
supportComponent push state supportComponent = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER_VERTICAL
  , onClick push $ const supportComponent.action
  , visibility $ boolToVisibility supportComponent.visibility
  ][  imageView
      [ width $ V 26
      , height $ V 26
      , imageWithFallback $ fetchImage FF_ASSET supportComponent.prefixImg
      ]
    , linearLayout
      [ weight 1.0
      , height WRAP_CONTENT
      , orientation VERTICAL
      , margin $ MarginHorizontal 10 10
      ][  commonTV push supportComponent.title Color.black800 FontStyle.subHeading2 LEFT 0 NoAction true (PaddingTop 0)
        , commonTV push supportComponent.desc Color.black600 FontStyle.tags LEFT 4 NoAction true (PaddingTop 0)
      ]
    , imageView
      [ width $ V 26
      , height $ V 26
      , imageWithFallback $ fetchImage FF_ASSET supportComponent.postFixImg
      ]
  ]

commonTV :: forall w .  (Action -> Effect Unit) -> String -> String -> (LazyCheck -> forall properties. (Array (Prop properties))) -> Gravity -> Int -> Action -> Boolean -> Padding -> PrestoDOM (Effect Unit) w
commonTV push text' color' theme gravity' marginTop action visibility' padding' = 
  textView $
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , color color'
  , gravity gravity'
  , margin $ MarginTop marginTop
  , padding padding'
  , text text'
  , visibility $ boolToVisibility visibility'
  ] <> theme TypoGraphy
    <>  if action == NoAction then []
        else [onClick push $ const action]

cardItemView :: forall w. (Action -> Effect Unit) -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
cardItemView push state = 
  scrollView
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , weight 1.0
  ][  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      , weight 1.0
      ](map
          (\item -> 
              linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation HORIZONTAL
              , padding $ Padding 12 12 12 12
              , cornerRadius 8.0
              , visibility $ boolToVisibility $ cardVisibility item
              , stroke $ "1,"<> case getStatus item.stage state of
                                        ST.COMPLETED -> Color.green900
                                        ST.IN_PROGRESS -> Color.yellow900
                                        ST.NOT_STARTED -> Color.black500
                                        ST.FAILED -> Color.red
              , background case getStatus item.stage state of
                                        ST.COMPLETED -> Color.greenOpacity10
                                        ST.IN_PROGRESS -> Color.yellowOpacity10
                                        ST.NOT_STARTED -> Color.white900
                                        ST.FAILED -> Color.redOpacity10
              , clickable $ not case item.stage of
                            ST.DRIVING_LICENSE_OPTION -> state.props.limitReachedFor == Just "DL" || any (_ == state.data.drivingLicenseStatus) [COMPLETED, IN_PROGRESS]
                            ST.VEHICLE_DETAILS_OPTION -> state.props.limitReachedFor == Just "RC" || any (_ == state.data.vehicleDetailsStatus) [COMPLETED, IN_PROGRESS]
                            ST.GRANT_PERMISSION -> state.data.permissionsStatus == COMPLETED
                            ST.SUBSCRIPTION_PLAN -> state.data.subscriptionStatus == COMPLETED
              , onClick push (const (RegistrationAction item.stage))
              , margin (MarginBottom 20)
              , gravity CENTER_VERTICAL
              ][  linearLayout
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , background case getStatus item.stage state of
                                        ST.NOT_STARTED -> Color.blue600
                                        _ -> Color.white900
                  , cornerRadius 24.0
                  , padding $ Padding 8 8 8 8
                  , margin (MarginRight 14)
                  ][ imageView
                    [ imageWithFallback $ fetchImage FF_ASSET $ case item.stage of
                        ST.DRIVING_LICENSE_OPTION -> "ny_ic_dl_blue"
                        ST.VEHICLE_DETAILS_OPTION -> "ny_ic_vehicle_onboard"
                        ST.GRANT_PERMISSION -> "ny_ic_dl_blue"
                        ST.SUBSCRIPTION_PLAN -> "ny_ic_plus_circle_blue"
                    , width $ V 24
                    , height $ V 24
                    ]
                  ]
              ,  linearLayout
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , weight 1.0
                ][ textView $
                    [ text case item.stage of
                              ST.DRIVING_LICENSE_OPTION -> (getString DRIVING_LICENSE)
                              ST.VEHICLE_DETAILS_OPTION -> getString VEHICLE_REGISTERATON_CERTIFICATE
                              ST.GRANT_PERMISSION -> getString GRANT_PERMISSIONS
                              ST.SUBSCRIPTION_PLAN -> getString $ SUBSCRIPTION_PLAN_STR "SUBSCRIPTION_PLAN_STR"
                    , color Color.black800
                    ] <> FontStyle.body1 TypoGraphy
                  , textView $
                    [ text $ getString RETRY_UPLOAD
                    , color Color.blue900
                    , visibility if getStatus item.stage state == ST.FAILED && not checkLimitReached item.stage state.props.limitReachedFor then VISIBLE else GONE
                    , margin $ MarginTop 2
                    ] <> FontStyle.tags TypoGraphy
                ]
                  , imageView
                    [ imageWithFallback case getStatus item.stage state of
                                          ST.COMPLETED -> "ny_ic_green_tick,https://assets.juspay.in/nammayatri/images/driver/ny_ic_green_tick"
                                          ST.IN_PROGRESS -> "ny_ic_pending,https://assets.juspay.in/nammayatri/images/driver/ny_ic_pending"
                                          ST.NOT_STARTED -> "ny_ic_chevron_right,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_right"
                                          ST.FAILED -> "ny_ic_warning_filled_red,https://assets.juspay.in/nammayatri/images/driver/ny_ic_warning_filled_red"
                    , width (V 20)
                    , height (V 20)
                    ]
              ]
          ) state.data.registerationSteps
      )
  ]
  where cardVisibility item = 
          case item.stage of
            SUBSCRIPTION_PLAN -> (getValueToLocalNativeStore SHOW_SUBSCRIPTIONS == "true") && state.data.config.bottomNavConfig.subscription.isVisible
            _ -> true

getStatus :: ST.RegisterationStep -> ST.RegistrationScreenState -> ST.StageStatus
getStatus step state = 
  case step of
    ST.DRIVING_LICENSE_OPTION -> state.data.drivingLicenseStatus
    ST.VEHICLE_DETAILS_OPTION -> state.data.vehicleDetailsStatus
    ST.GRANT_PERMISSION -> state.data.permissionsStatus
    ST.SUBSCRIPTION_PLAN -> state.data.subscriptionStatus

logoutPopupModal :: forall w . (Action -> Effect Unit) -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
logoutPopupModal push state =
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.blackLessTrans
    ][ PopUpModal.view (push <<< PopUpModalLogoutAction) (logoutPopUp Language) ] 

refreshView :: forall w . (Action -> Effect Unit) -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
refreshView push state =
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , stroke $ "1,"<> Color.grey900
      , padding $ Padding 16 12 16 12
      , cornerRadius 8.0
      , alignParentBottom "true,-1"
      , onClick push $ const Refresh
      , margin $ Margin 16 0 16 16
      , visibility if any (_ == IN_PROGRESS)[state.data.vehicleDetailsStatus, state.data.drivingLicenseStatus] && isNothing state.props.limitReachedFor  then VISIBLE else GONE
      ][ textView $
          [ text $ getString LAST_UPDATED
          , gravity CENTER
          ] <> FontStyle.body3 TypoGraphy
        , textView $
          [ text state.data.lastUpdateTime
          , gravity CENTER
          , margin $ MarginLeft 6
          ] <> FontStyle.body15 TypoGraphy
        , linearLayout
          [ width WRAP_CONTENT
          , weight 1.0
          ][] 
        , imageView
          [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_refresh"
          , height $ V 16
          , width $ V 16
          ]
        , textView $ 
          [ text $ getString REFRESH_STRING
          , color Color.blue800
          , margin $ MarginLeft 4
          ] <> FontStyle.body9 TypoGraphy
      ]

enterReferralCode :: forall w . (Action -> Effect Unit) -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
enterReferralCode push state =
  let allStepsCompleted = all (_ == COMPLETED) [ state.data.vehicleDetailsStatus, state.data.drivingLicenseStatus, state.data.permissionsStatus ]
    in linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , stroke $ "1," <> Color.grey900
            , cornerRadius 4.0
            , padding $ Padding 10 8 10 8
            ][  textView $
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , color if allStepsCompleted then Color.black900 else Color.greyTextColor
                , text $ getString if state.props.referralCodeSubmitted then REFERRAL_APPLIED else HAVE_A_REFERRAL_CODE
                , weight 1.0
                ] <> FontStyle.body3 TypoGraphy
              , textView $
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text $ getString ENTER_CODE
                , margin $ MarginRight 7
                , color if allStepsCompleted then Color.darkBlue else Color.primaryBG
                , onClick push $ const $ EnterReferralCode allStepsCompleted
                , visibility $ boolToVisibility $ not state.props.referralCodeSubmitted
                ] <> FontStyle.body3 TypoGraphy
              , imageView
                [ width $ V 20
                , height $ V 20 
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_green_tick"
                , visibility $ boolToVisibility state.props.referralCodeSubmitted
                , margin $ MarginRight 7
                ]
              ]

enterReferralCodeModal :: forall w . (Action -> Effect Unit) -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
enterReferralCodeModal push state =
  InAppKeyboardModal.view (push <<< InAppKeyboardModalAction) (enterReferralStateConfig state)

checkLimitReached :: ST.RegisterationStep -> Maybe String -> Boolean
checkLimitReached step limitReachedFor = 
  case limitReachedFor of
    Just "RC" -> step == ST.VEHICLE_DETAILS_OPTION
    Just "DL" -> step == ST.DRIVING_LICENSE_OPTION
    _ -> false