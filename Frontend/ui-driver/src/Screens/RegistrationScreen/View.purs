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
import Common.Types.App
import Debug
import Screens.RegistrationScreen.ComponentConfig
import Screens.RegistrationScreen.ComponentConfig

import Animation as Anim
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.StepsHeaderModal as StepsHeaderModel
import Control.Monad.ST (for)
import Data.Array (all, any, elem, filter, fold, length, mapWithIndex)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Font.Size as FontSize
import Font.Style as FontStyle
import PaymentPage (consumeBP)
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import JBridge (lottieAnimationConfig, startLottieProcess)
import Language.Strings (getString, getVarString)
import Language.Types (STR(..))
import Prelude (Unit, bind, const, map, not, pure, show, unit, void, ($), (&&), (+), (-), (<<<), (<>), (==), (>=), (||), (/=), (*))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alignParentBottom, background, clickable, color, cornerRadius, fontStyle, gravity, height, id, imageUrl, imageView, imageWithFallback, linearLayout, lottieAnimationView, margin, onBackPressed, onClick, orientation, padding, relativeLayout, stroke, text, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import Screens.RegistrationScreen.Controller (Action(..), eval, ScreenOutput)
import Screens.Types (RegisterationStep(..), StageStatus(..), ValidationStatus(..))
import Screens.Types as ST
import Storage (KeyStore(..), getValueToLocalNativeStore)
import Styles.Colors as Color
import Storage(getValueToLocalStore , KeyStore(..))

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
  let isShowSubscriptions = getValueToLocalNativeStore SHOW_SUBSCRIPTIONS == "true"
      completedStatusCount = length $ filter (\val -> val == COMPLETED) [ state.data.drivingLicenseStatus, state.data.vehicleDetailsStatus, state.data.permissionsStatus ]
      progressPercent = case isShowSubscriptions of
        true -> completedStatusCount * 25 + if state.data.subscriptionStatus == IN_PROGRESS then 25 else 0
        false -> if completedStatusCount * 33 == 99 then 100 else completedStatusCount * 33
  in
    -- lottieLoaderView state push
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
            [ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                -- , visibility if state.props.logoutModalView then GONE else VISIBLE
                ]
                [ PrestoAnim.animationSet
                    [ Anim.fadeIn true
                    ]
                    $ StepsHeaderModel.view (push <<< StepsHeaderModelAC) (stepsHeaderModelConfig state)
                ]
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , padding (Padding 16 16 16 0)
                , visibility VISIBLE
                , onClick push (const BackPressed)
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
                              , text $ getVarString START_EARNING_IN_FOUR_STEPS [ show $ length state.data.registerationSteps - if (getValueToLocalNativeStore SHOW_SUBSCRIPTIONS /= "true") then 1 else 0 ]
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
                                  , visibility if item.stage == ST.SUBSCRIPTION_PLAN && (getValueToLocalNativeStore SHOW_SUBSCRIPTIONS /= "true") then GONE else VISIBLE
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
                    -- , registrationHeader state
                    -- , tutorialView state
                    , cardItemView push state
                    ]
                ]
            , textView
                $ [ width MATCH_PARENT
                  , height WRAP_CONTENT
                  , margin $ Margin 16 20 16 16
                  , padding $ Padding 12 12 12 12
                  , text $ getString COMPLETE_AUTOPAY_LATER
                  , color Color.black700
                  , background Color.yellowOpacity10
                  , cornerRadius 8.0
                  , visibility if state.data.subscriptionStatus == IN_PROGRESS && not state.props.logoutModalView && (getValueToLocalStore SHOW_SUBSCRIPTIONS == "true") then VISIBLE else GONE
                  ]
                <> FontStyle.body1 TypoGraphy
            , messageView push state
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
            ]
        ]
      <> if state.props.logoutModalView then [ logoutPopupModal push state ] else []

registrationHeader :: ST.RegistrationScreenState -> forall w . PrestoDOM (Effect Unit) w
registrationHeader state = 
  linearLayout
  [ orientation VERTICAL
  , width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][textView 
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , fontStyle $ FontStyle.bold LanguageStyle
    , color Color.black800
    , textSize FontSize.a_26
    , margin (MarginVertical 15 10)
    , text (getString REGISTRATION)
    ]
  , textView
    [ text (getString FOLLOW_STEPS)
    , textSize FontSize.a_16
    , margin (MarginBottom 20)
    ]
  ]

-- lottieLoaderView :: forall w. ST.RegistrationScreenState -> (Action -> Effect Unit)-> PrestoDOM (Effect Unit) w
-- lottieLoaderView state push=
--   linearLayout
--   [ width MATCH_PARENT
--   , height MATCH_PARENT
--   ][ lottieAnimationView
--     [ height MATCH_PARENT
--     , width MATCH_PARENT
--     , id (getNewIDWithTag "activeIndex")
--     , background Color.black900
--     , afterRender
--         ( \action ->
--             void $ pure $ startLottieProcess lottieAnimationConfig {rawJson = "splash_lottie.json", lottieId = (getNewIDWithTag "activeIndex"), speed = 1.0 }
--         )(const NoAction)
--     ]
--   ]


tutorialView :: ST.RegistrationScreenState -> forall w . PrestoDOM (Effect Unit) w
tutorialView state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation HORIZONTAL
  , cornerRadius 7.0
  , background Color.lightBlue
  , gravity LEFT
  , padding (Padding 10 10 10 10)
  ][ textView
      ([ width WRAP_CONTENT
      , height MATCH_PARENT
      , text (getString WATCH_A_TUTORIAL_FOR_EASY_REGISTRATION)
      , weight 1.0
      , gravity CENTER_VERTICAL
      , color Color.black800
      ] <> FontStyle.body1 TypoGraphy)
    , imageView
      [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_media"
      , width (V 40)
      , height (V 40)
      ]
  ]


cardItemView :: forall w. (Action -> Effect Unit) -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
cardItemView push state = 
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ](map
      (\item -> 
          linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , padding $ Padding 12 12 12 12
          , cornerRadius 8.0
          , visibility if item.stage == ST.SUBSCRIPTION_PLAN && (getValueToLocalNativeStore SHOW_SUBSCRIPTIONS /= "true") then GONE else VISIBLE
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
                          ST.SUBSCRIPTION_PLAN -> getString SUBSCRIPTION_PLAN_STR
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

messageView :: forall w . (Action -> Effect Unit) -> ST.RegistrationScreenState -> PrestoDOM (Effect Unit) w
messageView push state =
    let isHidden = all (\x -> x `elem ` [NOT_STARTED, COMPLETED]) [state.data.vehicleDetailsStatus, state.data.drivingLicenseStatus] && isNothing state.props.limitReachedFor
    in
    linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , stroke $ "1,"<> Color.grey900
      , padding $ Padding 16 12 16 12
      , margin $ Margin 16 0 16 30
      , cornerRadius 8.0
      , alignParentBottom "true,-1"
      , visibility if state.props.logoutModalView || isHidden then GONE else VISIBLE
      ][ textView $
          [ text if isJust state.props.limitReachedFor then 
                    case state.props.limitReachedFor of
                      Just "RC" -> getString RC_UPLOAD_LIMIT_REACHED
                      Just "DL" -> getString DL_UPLOAD_LIMIT_REACHED
                      _ -> ""
                 else if state.data.vehicleDetailsStatus == FAILED && state.data.drivingLicenseStatus == FAILED then getString RC_AND_DL_UPLOAD_FAILED
                 else if state.data.vehicleDetailsStatus == FAILED then getString RC_UPLOAD_FAILED
                 else if state.data.drivingLicenseStatus == FAILED then getString DL_UPLOAD_FAILED
                 else getString LAST_UPDATED
          , gravity CENTER
          , visibility if any (\x -> x `elem` [IN_PROGRESS, FAILED])[state.data.vehicleDetailsStatus, state.data.drivingLicenseStatus]  then VISIBLE else GONE
          ] <> FontStyle.body3 TypoGraphy
        , textView $
          [ text  if state.data.vehicleDetailsStatus == FAILED || state.data.drivingLicenseStatus == FAILED then getString PLEASE_RETRY_THE_UPLOAD_AGAIN
                 else state.data.lastUpdateTime
          , gravity CENTER
          , margin $ MarginLeft 6
          , visibility if any (\x -> x `elem` [IN_PROGRESS, FAILED])[state.data.vehicleDetailsStatus, state.data.drivingLicenseStatus] && isNothing state.props.limitReachedFor  then VISIBLE else GONE
          ] <> FontStyle.body15 TypoGraphy
        , textView $
          [ text $ getString CONTACT_SUPPORT
          , gravity RIGHT
          , color Color.blue900
          , weight 1.0
          , visibility if isJust state.props.limitReachedFor then VISIBLE else GONE
          , onClick push (const (ContactSupport))
          ] <> FontStyle.tags TypoGraphy
        , linearLayout [
            width MATCH_PARENT
          , gravity CENTER
          , onClick push $ const Refresh
          , visibility if any (_ == IN_PROGRESS)[state.data.vehicleDetailsStatus, state.data.drivingLicenseStatus] && isNothing state.props.limitReachedFor  then VISIBLE else GONE
          ][ linearLayout [
                width WRAP_CONTENT
              , weight 1.0
              ][] 
            , imageView [
                imageWithFallback $ fetchImage FF_ASSET "ny_ic_refresh"
                , height $ V 16
                , width $ V 16
              ]
            , textView $ [
                text $ getString REFRESH_STRING
                , color Color.blue800
                , margin $ MarginLeft 4
            ] <> FontStyle.body9 TypoGraphy
          ]
      ]

checkLimitReached :: ST.RegisterationStep -> Maybe String -> Boolean
checkLimitReached step limitReachedFor = 
  case limitReachedFor of
    Just "RC" -> step == ST.VEHICLE_DETAILS_OPTION
    Just "DL" -> step == ST.DRIVING_LICENSE_OPTION
    _ -> false