{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.DriverProfileScreen.View where

import Data.List
import Screens.DriverProfileScreen.ComponentConfig
import Screens.SubscriptionScreen.Transformer
import Animation as Anim
import Animation.Config as AnimConfig
import Common.Types.App (LazyCheck(..))
import Components.BottomNavBar.Controller (navData)
import Components.BottomNavBar.View as BottomNavBar
import Components.CheckListView.View as CheckListView
import Components.GenericHeader.View as GenericHeader
import Components.InAppKeyboardModal.Controller as InAppKeyboardModalController
import Components.InAppKeyboardModal.View as InAppKeyboardModal
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Components.PrimaryEditText.View as PrimaryEditText
import Control.Applicative (unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (length, mapWithIndex, null, any, (!!), take, range)
import Data.Either (Either(..))
import Data.Enum (enumFromThenTo)
import Data.Function.Uncurried (runFn2)
import Data.Int (toNumber, round)
import Data.List (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as DS
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (getNewIDWithTag, isPreviousVersion, liftFlow, screenWidth)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (fetchImage, FetchImageFrom(..), getVehicleType, parseFloat, getCityConfig)
import Helpers.Utils as HU
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils as MU
import MerchantConfig.Types
import Mobility.Prelude as MP
import Prelude (Unit, ($), const, map, (+), (==), (<), (||), (/), (/=), unit, bind, (-), (<>), (<=), (>=), (<<<), (>), pure, discard, show, (&&), void, negate, not, (*), otherwise, show)
import Presto.Core.Types.Language.Flow (Flow, doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), horizontalScrollView, afterRender, alpha, background, color, cornerRadius, fontStyle, frameLayout, gravity, height, id, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, scrollView, text, textSize, textView, visibility, weight, width, webView, url, clickable, relativeLayout, stroke, alignParentBottom, disableClickFeedback, onAnimationEnd, rippleColor, fillViewport, rotation)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii, scrollBarY)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Resource.Constants (verifiedVehicleOnly, pendingVehicleOnly,decodeVehicleType)
import Resource.Constants as Const
import Screens as ScreenNames
import Screens.DriverProfileScreen.Controller (Action(..), ScreenOutput, eval, getTitle, checkGenderSelect, getGenderName, optionList)
import Screens.DriverProfileScreen.Transformer (fetchVehicles)
import Screens.Types (MenuOptions(..), AutoPayStatus(..))
import Screens.Types as ST
import Services.API (DriverInfoReq(..), GetDriverInfoResp(..), DriverRegistrationStatusReq(..), DriverProfileDataReq(..))
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore)
import Storage (isLocalStageOn)
import Styles.Colors as Color
import Types.App (defaultGlobalState)
import Mobility.Prelude (boolToVisibility)
import PrestoDOM (FontWeight(..), fontStyle, lineHeight, textSize, fontWeight)
import Font.Style (bold, semiBold)
import RemoteConfig.Utils
import RemoteConfig.Types
import Data.Array (filter)
import Engineering.Helpers.BackTrack (getState, liftFlowBT)
import Types.App (GlobalState(..), FlowBT)
import Services.API (DriverProfileDataRes(..))

screen :: ST.DriverProfileScreenState -> Screen Action ST.DriverProfileScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name: "DriverProfileScreen"
  , globalEvents:
      [ ( \push -> do
            if initialState.props.openSettings then
              pure unit
            else do
              void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT
                $ do
                    driverRegistrationStatusResp <- Remote.driverRegistrationStatusBT $ DriverRegistrationStatusReq true
                    lift $ lift $ doAff do liftEffect $ push $ RegStatusResponse driverRegistrationStatusResp
              void $ launchAff $ EHC.flowRunner defaultGlobalState $ do
                driverProfileResp <- Remote.fetchDriverProfile false
                case driverProfileResp of
                    Right resp -> do
                      liftFlow $ push $ ProfileDataAPIResponseAction resp
                    Left _ -> void $ pure $ JB.toast $ getString ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER
                pure unit
              void $ launchAff $ EHC.flowRunner defaultGlobalState
                $ do
                    void $ EHU.loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
                    EHU.toggleLoader true
                    summaryResponse <- Remote.driverProfileSummary ""
                    let cityConfig = getCityConfig initialState.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION)
                    profileResponse <- Remote.getDriverInfoApi ""
                    case summaryResponse, profileResponse of
                      Right summaryResp, Right profileResp -> do
                        liftFlow $ push $ DriverSummary summaryResp
                        liftFlow $ push $ GetDriverInfoResponse profileResp
                      _, _ -> liftFlow $ push $ BackPressed
                    EHU.toggleLoader false
                    pure unit
            pure $ pure unit
        )
      ]
  , eval:
      \action state -> do
        let
          _ = spy "DriverProfileScreen action " action
        let
          _ = spy "DriverProfileScreen state " state
        eval action state
  }

view :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
view push state =
  frameLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    ]
    [ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push $ const BackPressed
        , background Color.white900
        , visibility if state.props.updateLanguages || state.props.updateDetails then GONE else VISIBLE
        ]
        [ settingsView state push
        , profileView push state
        , manageVehiclesView state push
        ]
    , linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , background Color.lightBlack900
        , visibility if state.props.logoutModalView then VISIBLE else GONE
        ]
        [ PopUpModal.view (push <<< PopUpModalAction) (logoutPopUp state) ]
    , if state.props.upiQrView then renderQRView state push else dummyTextView
    , if state.props.showLiveDashboard then showLiveStatsDashboard push state else dummyTextView
    , if state.props.showGenderView || state.props.alternateNumberView then driverNumberGenderView state push else dummyTextView
    , if state.props.removeAlternateNumber then PopUpModal.view (push <<< RemoveAlternateNumberAC) (removeAlternateNumberConfig state) else dummyTextView
    , if state.props.enterOtpModal then enterOtpModal push state else dummyTextView
    , if state.props.updateLanguages then updateLanguageView state push else dummyTextView
    , if any (_ == state.props.detailsUpdationType) [ Just ST.VEHICLE_AGE, Just ST.VEHICLE_NAME ] then updateDetailsView state push else dummyTextView
    , if state.props.activateRcView then rcEditPopUpView push state else dummyTextView
    , if state.props.alreadyActive then rcActiveOnAnotherDriverProfilePopUpView push state else dummyTextView
    , if state.props.activateOrDeactivateRcView then activateAndDeactivateRcConfirmationPopUpView push state else dummyTextView
    , if state.props.paymentInfoView then paymentInfoPopUpView push state else dummyTextView
    , if state.props.deleteRcView then deleteRcPopUpView push state else dummyTextView
    , if state.props.showDriverBlockedPopup then driverBlockedPopupView push state else dummyTextView
    ]

updateDetailsView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
updateDetailsView state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    , afterRender
        ( \action -> do
            _ <- push action
            _ <- JB.requestKeyboardShow (EHC.getNewIDWithTag "UpdateDetailsEditText")
            pure unit
        )
        (const NoAction)
    ]
    [ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
    , horizontalLineView 1 1.0 0 0 0
    , PrimaryEditText.view (push <<< PrimaryEditTextAC) (primaryEditTextConfig state)
    , linearLayout
        [ height MATCH_PARENT
        , weight 1.0
        , gravity BOTTOM
        , margin $ MarginBottom 24
        ]
        [ PrimaryButton.view (push <<< UpdateValueAC) (updateButtonConfig state) ]
    ]

renderQRView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
renderQRView state push =
  let
    pspIcon = (Const.getPspIcon state.data.payerVpa)
  in
    linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity CENTER
      , background Color.black9000
      , onClick push $ const DismissQrPopup
      ]
      [ linearLayout
          [ height WRAP_CONTENT
          , width WRAP_CONTENT
          , background Color.white900
          , orientation VERTICAL
          , gravity CENTER
          , cornerRadius 16.0
          , clickable false
          ]
          [ linearLayout
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , id $ getNewIDWithTag "QRpaymentview"
              , gravity CENTER
              , orientation VERTICAL
              , padding $ Padding 16 32 16 16
              , background Color.white900
              , cornerRadius 16.0
              , clickable false
              ]
              [ textView
                  $ [ text $ getString GET_DIRECTLY_TO_YOUR_BANK_ACCOUNT
                    ]
                  <> FontStyle.subHeading1 TypoGraphy
              , linearLayout
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , background Color.white900
                  , orientation HORIZONTAL
                  , gravity CENTER
                  , margin $ MarginTop 15
                  ]
                  [ imageView
                      [ width $ V 24
                      , height $ V 24
                      , margin $ MarginRight 6
                      , imageWithFallback $ fetchImage FF_ASSET pspIcon
                      ]
                  , textView
                      $ [ text state.data.payerVpa
                        ]
                      <> FontStyle.body2 TypoGraphy
                  ]
              , PrestoAnim.animationSet [ Anim.fadeInWithDelay 250 true ]
                  $ imageView
                      [ height $ V 280
                      , width $ V 280
                      , margin $ MarginVertical 15 24
                      , id $ getNewIDWithTag "renderQRView"
                      , onAnimationEnd push (const (UpiQrRendered $ getNewIDWithTag "renderQRView"))
                      ]
              ]
          -- To enable after an apk update
          -- , PrimaryButton.view (push <<< DownloadQR) (downloadQRConfig state)
          -- , PrimaryButton.view (push <<< ShareQR) (shareOptionButtonConfig state)
          ]
      ]

------------------------------------------ manageVehiclesView -----------------------------------------------------------
manageVehiclesView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
manageVehiclesView state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility $ MP.boolToVisibility state.props.manageVehicleVisibility
    ]
    [ GenericHeader.view (push <<< ManageVehicleHeaderAC) (genericHeaderConfigManageVehicle state)
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.grey900
        ]
        []
    , scrollView
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , weight 1.0
        , padding $ Padding 16 22 16 18
        , scrollBarY false
        ]
        [ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ]
            (map (\vehicleDetail -> manageVehicleItem state vehicleDetail push) (state.data.vehicleDetails))
        -- [  manageVehicleItem state push
        --   ]
        ]
    ]

------------------------------------------ manageVehicleItem -----------------------------------------------------------
manageVehicleItem :: forall w. ST.DriverProfileScreenState -> ST.DriverVehicleDetails -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
manageVehicleItem state vehicle push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.blue600
    , cornerRadius 12.0
    , margin $ MarginBottom 18
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        ]
        [ imageView
            [ width $ V 36
            , height $ V 36
            , margin $ Margin 16 0 16 0
            , imageWithFallback $ fetchImage FF_COMMON_ASSET $ getVehicleImage (fromMaybe vehicle.userSelectedVehicleCategory vehicle.verifiedVehicleCategory) state
            ]
        , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , weight 1.0
            ]
            [ textView
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text vehicle.registrationNo
                , margin $ MarginTop 16
                , color Color.black900
                , textSize FontSize.a_16
                , fontStyle $ FontStyle.semiBold LanguageStyle
                ]
            , textView
                ( [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text $ fromMaybe "" vehicle.vehicleModel
                  , textSize FontSize.a_14
                  , margin $ MarginBottom 16
                  , color Color.black700
                  ]
                    <> FontStyle.body3 TypoGraphy
                )
            ]
        ]
    , linearLayout
        [ height $ V 2
        , width MATCH_PARENT
        , background Color.white900
        , cornerRadius 15.0
        , margin $ MarginHorizontal 16 16
        ]
        []
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , padding $ PaddingVertical 12 12
        , visibility $ MP.boolToVisibility vehicle.isActive
        , gravity CENTER_HORIZONTAL
        ]
        [ textView
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , onClick push $ const $ DeactivateRc ST.DELETING_RC vehicle.registrationNo
            , weight 1.0
            , color Color.red900
            , text $ getString DELETE
            , gravity CENTER
            , textSize FontSize.a_16
            , fontStyle $ FontStyle.semiBold LanguageStyle
            ]
        , linearLayout
            [ height MATCH_PARENT
            , width $ V 2
            , cornerRadius 15.0
            , background Color.white900
            ]
            []
        , textView
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , onClick push $ const $ DeactivateRc ST.DEACTIVATING_RC vehicle.registrationNo
            , weight 1.0
            , color Color.blue900
            , text $ getString DEACTIVATE
            , gravity CENTER
            , textSize FontSize.a_16
            , fontStyle $ FontStyle.semiBold LanguageStyle
            ]
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , padding $ PaddingVertical 12 12
        , gravity CENTER_HORIZONTAL
        , visibility $ MP.boolToVisibility $ not vehicle.isActive
        , onClick push $ const $ DeactivateRc ST.DELETING_RC vehicle.registrationNo
        ]
        [ textView
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , weight 1.0
            , color Color.red900
            , text $ getString DELETE
            , gravity CENTER
            , textSize FontSize.a_16
            , fontStyle $ FontStyle.semiBold LanguageStyle
            ]
        ]
    ]

---------------------------------------- PROFILE VIEW -----------------------------------------------------------
profileView :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
profileView push state =
  let driverBlockedHeaderVisibility = state.data.driverBlocked && (not $ DS.null state.data.blockedExpiryTime) && (runFn2 JB.differenceBetweenTwoUTC (state.data.blockedExpiryTime) (EHC.getCurrentUTC "") > 0)
  in
  PrestoAnim.animationSet [ Anim.fadeIn (not state.props.openSettings) ]
    $ linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , background Color.blue600
        , orientation VERTICAL
        , visibility $ if state.props.openSettings || state.props.manageVehicleVisibility then GONE else VISIBLE
        ]
        [ headerView state push
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , background Color.red900
            , padding $ Padding 16 16 16 16
            , gravity LEFT
            , visibility $ boolToVisibility driverBlockedHeaderVisibility
            ]
            [ textView $ 
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , color Color.white900
                , text $ getString $ BLOCKED_TILL (EHC.convertUTCtoISC state.data.blockedExpiryTime "hh:mm A") (EHC.convertUTCtoISC state.data.blockedExpiryTime "DD-MM-YYYY")
                , weight 1.0
                ] <> FontStyle.subHeading3 TypoGraphy
            , imageView 
                [ height $ V 24
                , width $ V 24
                , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_question_mark_with_circle"
                , onClick push $ const ShowDrvierBlockedPopup
                ]
            ]
        , linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , background Color.grey900
            ]
            []
        , scrollView
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , background if state.props.screenType == ST.DRIVER_DETAILS then Color.white900 else Color.blue600
            , orientation VERTICAL
            , weight 1.0
            , fillViewport true
            , scrollBarY false
            ]
            [ linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ]
                [ linearLayout
                    [ height MATCH_PARENT
                    , width MATCH_PARENT
                    , orientation VERTICAL
                    , background Color.blue600
                    , padding $ if state.props.screenType == ST.DRIVER_DETAILS then (PaddingVertical 16 24) else (PaddingTop 16)
                    ]
                    [ tabView state push
                    , relativeLayout[
                        height WRAP_CONTENT
                      , width MATCH_PARENT
                      ][
                        tabImageView state push
                      , completedProfile state push 
                      ]
                    , infoView state push
                    , verifiedVehiclesView state push
                    , pendingVehiclesVerificationList state push
                    ]
                , if state.props.screenType == ST.DRIVER_DETAILS then driverDetailsView push state else vehicleDetailsView push state -- TODO: Once APIs are deployed this code can be uncommented
                , if state.props.screenType == ST.DRIVER_DETAILS && state.data.config.showPaymentDetails then payment push state else dummyTextView
                , if state.props.screenType == ST.DRIVER_DETAILS then additionalDetails push state else dummyTextView
                -- , if (not null state.data.inactiveRCArray) && state.props.screenType == ST.VEHICLE_DETAILS then vehicleRcDetails push state else dummyTextView
                ]
            ]
        , if (length state.data.inactiveRCArray < 2) && state.props.screenType == ST.VEHICLE_DETAILS then addRcView state push else dummyTextView
        ]

completedProfile :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
completedProfile state push =
  linearLayout[
    height WRAP_CONTENT,
    width MATCH_PARENT,
    visibility $ boolToVisibility $ state.props.screenType == ST.DRIVER_DETAILS,
    gravity CENTER,
    orientation VERTICAL,
    margin $ MarginTop 110
  ][
    linearLayout[
      height WRAP_CONTENT,
      width WRAP_CONTENT,
      cornerRadius 50.0,
      background Color.blue900,
      padding $ Padding 12 2 12 2,
      visibility $ boolToVisibility $ ((state.data.profileCompletedModules*100)/4) /= 100
    ][
      textView
      $ [ text $ show((state.data.profileCompletedModules*100)/4)<> "%"
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          , color Color.white900
          , lineHeight "16"
          , textSize FontSize.a_15
          , fontStyle $ semiBold LanguageStyle
          , fontWeight $ FontWeight 600
          ]
    ]
  , linearLayout[
      height WRAP_CONTENT,
      width MATCH_PARENT,
      margin $ if ((state.data.profileCompletedModules*100)/4) /= 100 then Margin 0 7 0 12 else Margin 0 20 0 12,
      onClick push $ const CompleteProfile,
      gravity CENTER
    ][
      imageView
        [ width $ V 11
        , height $ V 11
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_blue_pen"
        , visibility $ boolToVisibility $ ((state.data.profileCompletedModules*100)/4) /= 100
        ]
    , textView
        $ [ text $ getString $ if ((state.data.profileCompletedModules*100)/4) == 100 then EDIT_PROFILE else COMPLETE_PROFILE
            , width WRAP_CONTENT
            , height WRAP_CONTENT
            , color Color.blue900
            , margin $ MarginLeft 5
            , lineHeight "15"
            , textSize FontSize.a_14
            , fontStyle $ semiBold LanguageStyle
            , fontWeight $ FontWeight 600
            ]
    ]
  ]

getVerifiedVehicleCount :: Array ST.DriverVehicleDetails -> Int
getVerifiedVehicleCount vehicles = length $ filter (\item -> item.isVerified == true) vehicles
  
verifiedVehiclesView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
verifiedVehiclesView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , visibility if state.props.screenType == ST.DRIVER_DETAILS then GONE else VISIBLE
    , orientation VERTICAL
    , margin $ Margin 0 0 0 0
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        ( map (\vehicleItem -> vehicleListItem state push vehicleItem) $ fetchVehicles verifiedVehicleOnly state.data.vehicleDetails
        )
    ]

pendingVehiclesVerificationList :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
pendingVehiclesVerificationList state push =
  let
    pendingVehicleList = fetchVehicles pendingVehicleOnly state.data.vehicleDetails
  in
    linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , weight 1.0
      , visibility $ MP.boolToVisibility $ not $ state.props.screenType == ST.DRIVER_DETAILS || null pendingVehicleList
      , background Color.white900
      , orientation VERTICAL
      , margin $ MarginTop 16
      , padding $ PaddingVertical 12 18
      ]
      [ textView
          [ text $ getString VEHICLES_PENDING
          , textSize FontSize.a_16
          , color Color.black900
          , margin $ MarginLeft 16
          , fontStyle $ FontStyle.semiBold LanguageStyle
          ]
      , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          ]
          ( map (\vehicleItem -> vehicleListItem state push vehicleItem) pendingVehicleList
          )
      ]

------------------------------------------- HEADER VIEW -------------------------------------------------------------
headerView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , background Color.white900
    , gravity BOTTOM
    , padding $ Padding 5 16 5 16
    ]
    [ imageView
        [ width $ V 40
        , height $ V 40
        , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_left"
        , onClick push $ const BackPressed
        , rippleColor Color.rippleShade
        , cornerRadius 20.0
        , padding $ Padding 7 7 7 7
        , margin $ MarginLeft 5
        ]
    , textView
        ( [ weight 1.0
          , height MATCH_PARENT
          , text (getString MY_PROFILE)
          , margin $ Margin 10 2 0 0
          , color Color.black900
          ]
            <> FontStyle.h3 TypoGraphy
        )
    , linearLayout
        [ height MATCH_PARENT
        , width WRAP_CONTENT
        , gravity CENTER
        , onClick push $ const OpenSettings
        , rippleColor Color.rippleShade
        , cornerRadius 20.0
        , padding $ PaddingHorizontal 3 6
        ]
        [ imageView
            [ height $ V 20
            , width $ V 20
            , margin $ MarginRight 4
            , imageWithFallback $ fetchImage FF_ASSET "ic_settings"
            ]
        , textView
            ( [ text (getString SETTINGS)
              , color Color.blue900
              , padding $ PaddingBottom 2
              ]
                <> FontStyle.body1 TypoGraphy
            )
        ]
    ]

-- vehicleRcDetails :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
-- vehicleRcDetails push state =
--   linearLayout
--     [ height WRAP_CONTENT
--     , width MATCH_PARENT
--     , margin $ Margin 0 0 12 0
--     , orientation VERTICAL
--     ]
--     [ linearLayout
--         [ height WRAP_CONTENT
--         , width MATCH_PARENT
--         , orientation VERTICAL
--         ]
--         ( map (\rcData -> additionalRcsView state push rcData) $ getRcNumDetails state.data.inactiveRCArray
--         )
--     ]
getRcNumDetails :: Array ST.RcData -> Array { key :: String, idx :: Int, value :: String, action :: Action, model :: Maybe String, color :: Maybe String }
getRcNumDetails config = do
  mapWithIndex (\index item -> { key: "RC", idx: index + 1, value: item.rcDetails.certificateNumber, action: NoAction, model: item.rcDetails.vehicleModel, color: item.rcDetails.vehicleColor }) config

--------------------------------------------------- TAB VIEW -----------------------------------------------------
tabView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
tabView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadius 24.0
    , margin $ MarginHorizontal 16 16
    , background Color.white900
    , padding $ Padding 6 6 6 6
    , gravity CENTER
    ]
    [ textView
        [ height WRAP_CONTENT
        , weight 1.0
        , background if state.props.screenType == ST.DRIVER_DETAILS then Color.black900 else Color.white900
        , text (getString DRIVER_DETAILS)
        , cornerRadius 24.0
        , padding $ PaddingVertical 6 6
        , onClick push $ const $ ChangeScreen ST.DRIVER_DETAILS
        , fontStyle $ FontStyle.medium LanguageStyle
        , gravity CENTER
        , color if state.props.screenType == ST.DRIVER_DETAILS then Color.white900 else Color.black900
        ]
    , textView
        [ height WRAP_CONTENT
        , weight 1.0
        , gravity CENTER
        , cornerRadius 24.0
        , onClick push $ const $ ChangeScreen ST.VEHICLE_DETAILS
        , padding $ PaddingVertical 6 6
        , text (getString VEHICLE_DETAILS)
        , fontStyle $ FontStyle.medium LanguageStyle
        , background if state.props.screenType == ST.VEHICLE_DETAILS then Color.black900 else Color.white900
        , color if state.props.screenType == ST.VEHICLE_DETAILS then Color.white900 else Color.black900
        ]
    ]

------------------------------------------ TAB IMAGE VIEW ---------------------------------------------------------

getVehicleCategory :: ST.DriverProfileScreenState -> ST.VehicleCategory
getVehicleCategory state =
  let
    vehicles = state.data.vehicleDetails
    mbVehicle = find (\item -> item.isActive == item.isVerified) vehicles
  in
    case mbVehicle of
      Just vehicle -> fromMaybe state.data.cachedVehicleCategory vehicle.verifiedVehicleCategory
      Nothing -> state.data.cachedVehicleCategory

tabImageView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
tabImageView state push =
  let
    vc = getVehicleCategory state
    driverImage = case (fromMaybe "UNKNOWN" state.data.driverGender) of
      "MALE" | vc == ST.AutoCategory -> "ny_ic_new_avatar_profile"
      "MALE" | vc == ST.CarCategory -> "ny_ic_white_avatar_profile"
      "MALE" | vc == ST.BikeCategory -> "ny_ic_new_avatar_profile"
      "MALE" | vc == ST.AmbulanceCategory -> "ny_ic_new_avatar_profile"
      "MALE" | vc == ST.TruckCategory -> "ny_ic_new_avatar_profile"
      "MALE" | vc == ST.BusCategory -> "ny_ic_new_avatar_profile"
      "FEMALE" -> "ny_ic_profile_female"
      _ -> "ny_ic_generic_mascot"
    per = (state.data.profileCompletedModules*100)/4
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER_HORIZONTAL
      , padding $ PaddingVertical 32 $ if state.props.screenType == ST.DRIVER_DETAILS then 12 else 32
      , background Color.blue600
      , orientation HORIZONTAL
      ]
      [ PrestoAnim.animationSet
          [ Anim.motionMagnifyAnim $ (scaleUpConfig (state.props.screenType == ST.DRIVER_DETAILS)) { fromX = -44, toX = 44 }
          , Anim.motionMagnifyAnim $ (scaleDownConfig (state.props.screenType == ST.VEHICLE_DETAILS)) { fromX = 44, toX = -44 }
          ]
          $ relativeLayout[
              height $ V 98
            , width $ V 100
            ][
              linearLayout[
                height $ V 98,
                width $ V 100,
                orientation VERTICAL, 
                visibility $ boolToVisibility $ state.props.screenType == ST.DRIVER_DETAILS && per < 100
            ][
                linearLayout[
                  height $ V 49,
                  width $ V 100
                ][
                  linearLayout[
                    height $ V 49,
                    width $ V 50,
                    background if per == 100 then Color.white900 else if per >= 50 then Color.blue900 else Color.white900,
                    cornerRadii $ Corners 100.0 true false false false
                  ][]
                , linearLayout[
                    height $ V 49,
                    width $ V 50,
                    background if per == 100 then Color.white900 else if per >= 75 then Color.blue900 else Color.white900,
                    cornerRadii $ Corners 100.0 false true false false
                  ][]
                ]
                , linearLayout[
                  height $ V 49,
                  width $ V 100
                ][
                  linearLayout[
                    height $ V 49,
                    width $ V 50,
                    background if per == 100 then Color.white900 else if per >= 25 then Color.blue900 else Color.white900,
                    cornerRadii $ Corners 80.0 false false false true
                  ][]
                , linearLayout[
                    height $ V 49,
                    width $ V 50,
                    background if per == 100 then Color.white900 else if per >= 100 then Color.blue900 else Color.white900,
                    cornerRadii $ Corners 80.0 false false true false
                  ][]
                ]
            ]
            , linearLayout
              [ height $ V 88
              , width $ V 88
              , cornerRadius 44.0
              , margin $ Margin 6 6 0 0
              , onClick push $ const $ ChangeScreen ST.DRIVER_DETAILS
              , alpha if (state.props.screenType == ST.DRIVER_DETAILS) then 1.0 else 0.4
              ]
              [ ( if state.data.profileImg == Nothing then
                    imageView
                      [ height $ V 88
                      , width $ V 88
                      , imageWithFallback $ fetchImage FF_ASSET driverImage
                      ]
                  else
                    linearLayout
                      [ height $ V 88
                      , width $ V 88
                      , afterRender (\action -> do JB.renderBase64Image (fromMaybe "" state.data.profileImg) (getNewIDWithTag "driver_prof_img") false "CENTER_CROP") (const NoAction)
                      , id (getNewIDWithTag "driver_prof_img")
                      ]
                      []
                )
              ]
            ]
      , PrestoAnim.animationSet
          [ Anim.motionMagnifyAnim $ (scaleUpConfig (state.props.screenType == ST.VEHICLE_DETAILS)) { fromX = 44, toX = -44 }
          , Anim.motionMagnifyAnim $ (scaleDownConfig (state.props.screenType == ST.DRIVER_DETAILS)) { fromX = -44, toX = 44 }
          ]
          $ linearLayout
              [ height $ V 88
              , width $ V 88
              , cornerRadius 44.0
              , background Color.white900
              , onClick push $ const $ ChangeScreen ST.VEHICLE_DETAILS
              , gravity CENTER
              , alpha if (state.props.screenType == ST.VEHICLE_DETAILS) then 1.0 else 0.4
              ]
              [ imageView
                  [ imageWithFallback $ fetchImage FF_COMMON_ASSET $ getVehicleImage (getVehicleCategory state) state
                  , height $ V 68
                  , width $ V 68
                  ]
              ]
      ]

---------------------------------------------- DRIVER DETAILS VIEW ------------------------------------------------------------
driverDetailsView :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
driverDetailsView push state =
  let city = getValueToLocalStore DRIVER_LOCATION
      configs = cancellationThresholds "cancellation_rate_thresholds" city
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin $ MarginHorizontal 16 16
      ] $ if state.data.cancellationRate > configs.warning1 then cancellationRateOnTop configs else cancellationRateOnBottom configs
  where
    cancellationRateOnTop configs = 
      [ cancellationRateView state push configs
      , driverAnalyticsView state push
      , badgeLayoutView state
      ]
    cancellationRateOnBottom configs =
      [ driverAnalyticsView state push
      , badgeLayoutView state
      , cancellationRateView state push configs
      ]

------------------------------------------- MISSED OPPORTUNITY VIEW -----------------------------------------
missedOpportunityView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
missedOpportunityView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin 0 40 0 0
    ]
    [ textView
        [ text (getString MISSED_OPPORTUNITY)
        , margin $ Margin 0 0 16 4
        , textSize FontSize.a_16
        , color Color.black900
        , fontStyle $ FontStyle.semiBold LanguageStyle
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        (map (\item -> infoCard state push item) (missedOppArray state.data.analyticsData))
    ]

missedOppArray :: ST.AnalyticsData -> Array MissedOpportunity
missedOppArray analyticsData =
  [ { key: (getString CANCELLATION_RATE), value: (show analyticsData.cancellationRate <> "%"), value1: "", infoImageUrl: fetchImage FF_COMMON_ASSET "ny_ic_info_blue", postfixImage: fetchImage FF_ASSET "ny_ic_api_failure_popup", showPostfixImage: false, showInfoImage: false, valueColor: Color.charcoalGrey, action: NoAction }
  , { key: (getString RIDES_CANCELLED), value: show analyticsData.ridesCancelled, value1: show analyticsData.totalRidesAssigned, infoImageUrl: fetchImage FF_COMMON_ASSET "ny_ic_info_blue", postfixImage: fetchImage FF_ASSET "ny_ic_api_failure_popup", showPostfixImage: false, showInfoImage: false, valueColor: Color.charcoalGrey, action: NoAction }
  , { key: (getString EARNINGS_MISSED), value: "₹" <> EHC.formatCurrencyWithCommas (show analyticsData.missedEarnings), value1: "", infoImageUrl: fetchImage FF_COMMON_ASSET "ny_ic_info_blue", postfixImage: fetchImage FF_ASSET "ny_ic_api_failure_popup", showPostfixImage: false, showInfoImage: false, valueColor: Color.charcoalGrey, action: NoAction }
  ]

------------------------------------------- DRIVER ANALYTICS VIEW  ----------------------------------------------------------
driverAnalyticsView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
driverAnalyticsView state push =
  let
    analyticsData = state.data.analyticsData
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , margin $ Margin 0 40 0 0
      ]
      [ textView
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text (getString SUMMARY)
          , textSize FontSize.a_16
          , color Color.black900
          , fontStyle $ FontStyle.semiBold LanguageStyle
          ]
      , let
          bonusActivated = state.data.config.feature.enableBonus
        in
          linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , margin if bonusActivated then (MarginVertical 12 12) else (MarginVertical 4 12)
            , background if bonusActivated then Color.blue600 else Color.transparent
            , cornerRadius 10.0
            ]
            [ if bonusActivated then
                linearLayout
                  [ height WRAP_CONTENT
                  , width MATCH_PARENT
                  ]
                  [ infoTileView state { primaryText: "₹ " <> (EHC.formatCurrencyWithCommas analyticsData.totalEarnings), subText: (getString $ EARNED_ON_APP "EARNED_ON_APP"), postImgVisibility: false, seperatorView: false, margin: Margin 0 0 0 0 }
                  , linearLayout
                      [ height MATCH_PARENT
                      , width (V 1)
                      , margin (Margin 0 16 0 16)
                      , background Color.lightGreyShade
                      ]
                      []
                  , infoTileView state { primaryText: "₹ " <> EHC.formatCurrencyWithCommas analyticsData.bonusEarned, subText: (getString $ NAMMA_BONUS "NAMMA_BONUS"), postImgVisibility: false, seperatorView: false, margin: Margin 0 0 0 0 }
                  ]
              else
                infoCard state push { key: (getString $ EARNED_ON_APP "EARNED_ON_APP"), value: "₹" <> (EHC.formatCurrencyWithCommas analyticsData.totalEarnings), value1: "", infoImageUrl: "", postfixImage: "", showPostfixImage: false, showInfoImage: false, valueColor: Color.charcoalGrey, action: NoAction }
            ]
      , linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , margin $ Margin 0 12 0 12
          ]
          [ infoTileView state { primaryText: (parseFloat (fromMaybe 0.0 analyticsData.rating) 1), subText: (getString RATED_BY_USERS1) <> " " <> show analyticsData.totalUsersRated <> " " <> (getString RATED_BY_USERS2), postImgVisibility: true, seperatorView: true, margin: MarginRight 6 }
          , infoTileView state { primaryText: show analyticsData.totalCompletedTrips, subText: (getString TRIPS_COMPLETED), postImgVisibility: false, seperatorView: true, margin: MarginLeft 6 }
          ]
      , horizontalScrollView
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          ]
          [ linearLayout
              [ width MATCH_PARENT
              , height MATCH_PARENT
              , orientation HORIZONTAL
              ]
              $ [
                linearLayout
                  [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , cornerRadius 20.0
                  , background Color.blue600
                  , padding $ Padding 12 10 12 10
                  , margin $ MarginHorizontal 5 5
                  , gravity CENTER_VERTICAL
                  ]
                  [ imageView 
                    [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_blue_heart"
                    , width $ V 15
                    , height $ V 15
                    , margin $ Margin 0 2 4 0
                    ]
                  , textView
                      [ text $ show (fromMaybe 0 state.data.favCount)
                      , width WRAP_CONTENT
                      , height WRAP_CONTENT
                      , textSize FontSize.a_14
                      , fontStyle $ FontStyle.bold LanguageStyle
                      , color Color.black900
                      , margin $ MarginRight 4
                      ]
                  , textView
                      $ [ text $ getString FAVOURITES
                        , width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , textSize FontSize.a_12
                        , color Color.black700
                        ]
                      <> FontStyle.body3 TypoGraphy
                  ]
              ]
              <> map (\item -> chipRailView item) (getChipRailArray state.data.analyticsData.lateNightTrips state.data.analyticsData.lastRegistered state.data.languagesSpoken state.data.analyticsData.totalDistanceTravelled)
          ]
      ]

addRcView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
addRcView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , background Color.white900
    , orientation VERTICAL
    , alignParentBottom "true,-1"
    ]
    [ linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.grey900
        ]
        []
    , if (getVerifiedVehicleCount state.data.vehicleDetails < state.data.config.rcLimit) then PrimaryButton.view (push <<< AddRcButtonAC) (addRCButtonConfig state) else dummyTextView
    , PrimaryButton.view (push <<< ManageVehicleButtonAC) (addRCButtonConfigs state)
    ]

------------------------------ CANCELLATION RATE VIEW ---------------------------------------------

cancellationRateView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> CancellationThresholdConfig -> PrestoDOM (Effect Unit) w
cancellationRateView state push configs =
  let cancellationRate = if (isJust state.data.cancellationWindow) then state.data.cancellationRate else state.data.analyticsData.cancellationRate
  in
    linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , background Color.aliceBlueLight
      , cornerRadius 24.0
      , margin $ MarginTop 20
      , padding $ Padding 16 16 16 16
      ]
      [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , margin if cancellationRate > configs.warning2 then MarginBottom 16 else MarginBottom 0
        ] 
        [ 
          relativeLayout
          [ height $ V 103
          , width $ V $ ((screenWidth unit)/ 2) - 32
          , gravity CENTER
          ][ 
            imageView 
            [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_gauge_image"
            , width $ V $ ((screenWidth unit)/ 2) - 32
            , height $ V 100
            , gravity LEFT
            ]
          , linearLayout
            [ gravity CENTER
            , width MATCH_PARENT
            ][
              imageView
              [ imageWithFallback $ fetchImage FF_ASSET "ny_ic_gauge_tip"
              , width $ V $ round (((toNumber (screenWidth unit - 32)/ 2.0) ) * (11.0 / 75.0))
              , height $ V 120
              , rotation (((toNumber cancellationRate) * 1.8) - 90.0)
              , margin $ MarginTop 35
              ]
            ]
            ]
        , linearLayout
          [ height MATCH_PARENT
          , weight 1.0
          , orientation VERTICAL
          , gravity CENTER
          ] [ 
              textView $
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , text $ show cancellationRate <> "%"
              , color Color.black900
              , padding $ PaddingBottom 4
              , textSize FontSize.a_24
              , gravity CENTER
              ] <> FontStyle.body8 LanguageStyle
            , textView $
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , text (getString CANCELLATION_RATE)
              , color Color.black700
              , textSize FontSize.a_14
              , padding $ Padding 11 0 11 8
              , gravity CENTER
              ] <> FontStyle.body3 LanguageStyle
            , textView $
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , text (getString VIEW_MORE)
              , color Color.blue800
              , textSize FontSize.a_14
              , gravity CENTER
              , onClick push $ const $ OpenCancellationRateScreen
              ] <> FontStyle.body3 LanguageStyle
          ]
        ]
      , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , cornerRadius 8.0
          , background Color.surfaceRed
          , padding $ PaddingVertical 8 8
          , gravity CENTER
          , visibility $ boolToVisibility $ cancellationRate > configs.warning2
          ] 
          [ textView $
            [ text (getString HIGH_CANCELLATION_RATE)
            , textSize FontSize.a_14
            , color Color.black800
            ] <> FontStyle.body3 LanguageStyle
          ]
      ]

------------------------------ CHIP RAIL LAYOUT ---------------------------------------------
chipRailView :: forall w. ST.ChipRailData -> PrestoDOM (Effect Unit) w
chipRailView item =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , cornerRadius 20.0
    , background Color.blue600
    , padding $ Padding 12 10 12 10
    , margin $ MarginHorizontal 5 5
    , gravity CENTER_VERTICAL
    ]
    [ textView
        [ text item.mainTxt
        , width WRAP_CONTENT
        , height WRAP_CONTENT
        , textSize FontSize.a_14
        , fontStyle $ FontStyle.bold LanguageStyle
        , color Color.black900
        , margin $ MarginRight 4
        ]
    , textView
        $ [ text item.subTxt
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          , textSize FontSize.a_12
          , color Color.black700
          ]
        <> FontStyle.body3 TypoGraphy
    ]

------------------------------ BADGE LAYOUT ---------------------------------------------
badgeLayoutView :: forall w. ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
badgeLayoutView state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginTop 40
    , visibility if null state.data.analyticsData.badges then GONE else VISIBLE
    ]
    [ textView
        [ text (getString BADGES)
        , textSize FontSize.a_16
        , fontStyle $ FontStyle.medium LanguageStyle
        , color Color.black900
        ]
    , horizontalScrollView
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        , margin $ Margin 0 12 0 12
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , orientation HORIZONTAL
            ]
            (map (\item -> badgeView item) state.data.analyticsData.badges)
        ]
    ]

------------------------------------------------------- BADGE VIEW -------------------------------------------------------------
badgeView :: forall w. { badgeImage :: String, primaryText :: String, subText :: String } -> PrestoDOM (Effect Unit) w
badgeView state =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.blue600
    , cornerRadius 15.0
    , padding $ Padding 25 10 25 12
    , margin $ MarginRight 16
    ]
    [ imageView
        [ width $ V 100
        , height $ V 100
        , imageWithFallback state.badgeImage
        ]
    , textView
        [ text state.primaryText
        , layoutGravity "center_horizontal"
        , textSize FontSize.a_16
        , color Color.black900
        , fontStyle $ FontStyle.bold LanguageStyle
        , height WRAP_CONTENT
        ]
    , textView
        [ text state.subText
        , layoutGravity "center_horizontal"
        , textSize FontSize.a_14
        , color Color.black800
        , height WRAP_CONTENT
        , fontStyle $ FontStyle.medium LanguageStyle
        ]
    ]

------------------------------------------------- BADGE DATA -----------------------------------------------------------------
getBadgeData :: ST.DriverProfileScreenState -> Array { badgeImage :: String, primaryText :: String, subText :: String }
getBadgeData state =
  [ { badgeImage: fetchImage FF_ASSET "ny_ic_five_star_badge"
    , primaryText: "5-Star Rides"
    , subText: "235"
    }
  , { badgeImage: fetchImage FF_ASSET "ny_ic_safe_ride"
    , primaryText: "Safe Rides"
    , subText: "235"
    }
  , { badgeImage: fetchImage FF_ASSET "ny_ic_clean_auto_badge"
    , primaryText: "Clean Auto"
    , subText: "235"
    }
  , { badgeImage: fetchImage FF_ASSET "ny_ic_expert_driver"
    , primaryText: "Expert Driving"
    , subText: "235"
    }
  , { badgeImage: fetchImage FF_ASSET "ny_ic_navigator_badge"
    , primaryText: "Navigator"
    , subText: "235"
    }
  , { badgeImage: fetchImage FF_ASSET "ny_ic_ontime_badge"
    , primaryText: "On Time"
    , subText: "235"
    }
  , { badgeImage: fetchImage FF_ASSET "ny_ic_polite_driver_badge"
    , primaryText: "Professional"
    , subText: "235"
    }
  ]

--------------------------------------- VEHICLE DETAILS VIEW ------------------------------------------------------------
vehicleDetailsView :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
vehicleDetailsView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ MarginHorizontal 16 16
    , visibility GONE
    ]
    [] --vehicleAnalyticsView push state]

--------------------------------------- VEHICLE ANALYTICS VIEW ------------------------------------------------------------
vehicleAnalyticsView :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
vehicleAnalyticsView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , margin $ Margin 0 24 0 0
    ]
    [ textView
        [ text (getString SUMMARY)
        , margin $ Margin 0 0 16 12
        , textSize FontSize.a_16
        , color Color.black
        , fontStyle $ FontStyle.medium LanguageStyle
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        (map (\item -> infoCard state push item) (vehicleSummaryArray state))
    ]

--------------------------------------- PAYMENT VIEW ------------------------------------------------------------
payment :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
payment push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ Margin 16 40 16 0
    , orientation VERTICAL
    , visibility if state.data.payerVpa == "" && state.data.autoPayStatus == ACTIVE_AUTOPAY then GONE else VISIBLE
    ]
    ( [ textView
          $ [ text $ getString PAYMENT
            , margin $ MarginBottom 12
            , color Color.black900
            ]
          <> FontStyle.subHeading1 TypoGraphy
      ]
        <> if state.data.autoPayStatus == ACTIVE_AUTOPAY then
            [ detailsListViewComponent state push
                { backgroundColor: Color.blue600
                , separatorColor: Color.white900
                , isLeftKeyClickable: false
                , arrayList: driverPaymentsArray state
                }
            ]
          else
            [ detailsListViewComponent state push
                { backgroundColor: Color.blue600
                , separatorColor: Color.white900
                , isLeftKeyClickable: true
                , arrayList: driverNoAutoPayArray state
                }
            ]
    )

--------------------------------------- ADDITIONAL DETAILS VIEW ------------------------------------------------------------
additionalDetails :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
additionalDetails push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin $ Margin 16 40 16 0
    , orientation VERTICAL
    ]
    ( [ textView
          [ text if state.props.screenType == ST.DRIVER_DETAILS then (getString ABOUT_ME) else (getString ABOUT_VEHICLE)
          , margin $ Margin 0 0 0 12
          , textSize FontSize.a_16
          , color Color.black900
          , fontStyle $ FontStyle.semiBold LanguageStyle
          ]
      ]
        <> [ detailsListViewComponent state push
              { backgroundColor: Color.blue600
              , separatorColor: Color.white900
              , isLeftKeyClickable: false
              , arrayList: if state.props.screenType == ST.DRIVER_DETAILS then driverAboutMeArray state else vehicleAboutMeArray state
              }
          ]
    )

-------------------------------------------- DRIVER NUMBER AND GENDER VIEW ----------------------------------------------------------------
driverNumberGenderView :: ST.DriverProfileScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
driverNumberGenderView state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility VISIBLE
    , padding $ PaddingVertical EHC.safeMarginTop EHC.safeMarginBottom
    , background Color.white900
    , clickable true
    ]
    [ GenericHeader.view (push <<< DriverGenericHeaderAC) (driverGenericHeaderConfig state)
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.grey900
        , margin $ MarginTop 2
        ]
        []
    , textView
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , text $ if state.props.showGenderView then getString SELECT_YOUR_GENDER else getString ENTER_SECOND_SIM_NUMBER
        , margin $ Margin 10 15 0 0
        , color Color.black800
        , textSize if state.props.showGenderView then FontSize.a_16 else FontSize.a_12
        ]
    , if state.props.showGenderView then genderProfileLayoutView state push else alternateNumberLayoutView state push
    , linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , margin $ MarginBottom 10
        , gravity BOTTOM
        , orientation VERTICAL
        ]
        [ linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , background Color.grey900
            , margin $ MarginBottom 16
            ]
            []
        , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)
        ]
    ]

---------------------------- GENDER PROFILE VIEW ----------------------------------
genderProfileLayoutView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
genderProfileLayoutView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , margin $ MarginHorizontal 10 10
        ]
        (map (\item -> showMenuButtonView state push item.text item.value) (genderOptionsArray state))
    ]

------------------------------------------------------ ALTERNATE NUMBER VIEW ----------------------------------------------------
alternateNumberLayoutView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
alternateNumberLayoutView state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    ]
    [ if state.data.alterNumberEditableText then
        linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , stroke ("1," <> Color.grey900)
          , margin $ Margin 10 10 10 10
          , padding $ Padding 16 20 16 20
          , cornerRadius 8.0
          ]
          [ textView
              [ height WRAP_CONTENT
              , width WRAP_CONTENT
              , text $ "+91 " <> fromMaybe "" state.data.driverAlternateNumber
              , color Color.black800
              ]
          , linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , gravity RIGHT
              ]
              [ textView
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text $ getString EDIT
                  , color Color.blue900
                  , onClick push $ const $ EditNumberText
                  ]
              , textView
                  [ height WRAP_CONTENT
                  , width WRAP_CONTENT
                  , text $ getString REMOVE
                  , color Color.blue900
                  , margin $ MarginHorizontal 10 10
                  , onClick push $ const $ RemoveAlterNumber
                  ]
              ]
          ]
      else
        linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , afterRender
              ( \action -> do
                  _ <- push action
                  _ <- JB.requestKeyboardShow (EHC.getNewIDWithTag "alternateMobileNumber")
                  pure unit
              )
              (const AfterRender)
          ]
          [ PrimaryEditText.view (push <<< PrimaryEditTextActionController) (alternatePrimaryEditTextConfig state) ]
    ]

infoView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
infoView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , visibility $ MP.boolToVisibility $ state.props.screenType == ST.DRIVER_DETAILS
    , orientation VERTICAL
    , margin $ MarginHorizontal 16 16
    ]
    [ detailsListViewComponent state push
        { backgroundColor: Color.white900
        , separatorColor: Color.grey700
        , isLeftKeyClickable: false
        , arrayList: driverDetailsArray state
        }
    ]

------------------------------ ENTER OTP MODAL -------------------------------------------------------
enterOtpModal :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
enterOtpModal push state = InAppKeyboardModal.view (push <<< InAppKeyboardModalOtp) (enterOtpState state)

------------------------------------- LIVE DASHBOARD -----------------------------------------------
showLiveStatsDashboard :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
showLiveStatsDashboard push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.grey800
    , visibility if (DS.null state.data.config.dashboard.url) then GONE else VISIBLE
    , afterRender
        ( \action -> do
            JB.initialWebViewSetUp push (getNewIDWithTag "webview") HideLiveDashboard
            pure unit
        )
        (const NoAction)
    ]
    [ webView
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , id $ getNewIDWithTag "webview"
        , url state.data.config.dashboard.url
        ]
    ]

--------------------------------------------------- SETTINGS VIEW ------------------------------------------
settingsView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
settingsView state push =
  PrestoAnim.animationSet [ Anim.fadeIn (state.props.openSettings) ]
    $ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , visibility $ MP.boolToVisibility state.props.openSettings
        ]
        [ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
        , linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , background Color.grey900
            ]
            []
        , profileOptionsLayout state push
        ]

------------------------------------------------- PROFILE OPTIONS LAYOUT ------------------------------
profileOptionsLayout :: ST.DriverProfileScreenState -> (Action -> Effect Unit) -> forall w. PrestoDOM (Effect Unit) w
profileOptionsLayout state push =
  scrollView
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    ]
    [ linearLayout
        [ width MATCH_PARENT
        , height MATCH_PARENT
        , orientation VERTICAL
        , padding $ PaddingVertical 10 5
        ]
        ( mapWithIndex
            ( \index optionItem ->
                linearLayout
                  ( [ width MATCH_PARENT
                    , height WRAP_CONTENT
                    , orientation VERTICAL
                    , gravity CENTER_VERTICAL
                    , onClick push $ const $ OptionClick optionItem.menuOptions
                    , visibility if visibilityCondition optionItem then VISIBLE else GONE
                    ]
                      <> if disableCondition optionItem then [ alpha 0.5, clickable $ disabledOptionClickable optionItem ] else []
                  )
                  [ linearLayout
                      [ width MATCH_PARENT
                      , height WRAP_CONTENT
                      , orientation HORIZONTAL
                      , gravity CENTER_VERTICAL
                      , padding (Padding 15 20 15 0)
                      ]
                      [ imageView
                          [ width $ V 20
                          , height $ V 20
                          , imageWithFallback optionItem.icon
                          ]
                      , textView
                          ( [ height WRAP_CONTENT
                            , weight 1.0
                            , text $ getTitle optionItem.menuOptions
                            , margin $ MarginLeft 10
                            , color Color.black900
                            ]
                              <> FontStyle.subHeading2 TypoGraphy
                          )
                      , linearLayout
                          [ width WRAP_CONTENT
                          , height WRAP_CONTENT
                          , orientation HORIZONTAL
                          , gravity CENTER_VERTICAL
                          ]
                          [ textView
                              $ [ width WRAP_CONTENT
                                , height WRAP_CONTENT
                                , text $ "V " <> (getValueToLocalStore VERSION_NAME)
                                , visibility if (optionItem.menuOptions == ABOUT_APP) then VISIBLE else GONE
                                , margin (MarginRight 5)
                                ]
                              <> FontStyle.paragraphText TypoGraphy
                          , imageView
                              [ width $ V 18
                              , height $ V 18
                              , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right_grey"
                              ]
                          ]
                      ]
                  , if (index == length (optionList state) - 2) then (horizontalLineView 7 0.5 0 20 0) else if (optionItem.menuOptions == DRIVER_LOGOUT) then dummyTextView else horizontalLineView 1 1.0 15 15 15
                  ]
            )
            (optionList state)
        )
    ]
  where
  visibilityCondition optionItem = case optionItem.menuOptions of
    GO_TO_LOCATIONS -> state.props.enableGoto && decodeVehicleType (getValueToLocalStore VEHICLE_CATEGORY) /= Just ST.AmbulanceCategory
    DRIVER_BOOKING_OPTIONS -> state.data.config.profile.showBookingOption && not (state.data.driverVehicleType `elem` ["DELIVERY_LIGHT_GOODS_VEHICLE", "BUS_NON_AC", "BUS_AC"])
    LIVE_STATS_DASHBOARD -> state.data.config.dashboard.enable && not DS.null state.data.config.dashboard.url
    _ -> true

  disableCondition optionItem = case optionItem.menuOptions of
    GO_TO_LOCATIONS -> state.data.goHomeActive || state.props.isRideActive
    _ -> false

  disabledOptionClickable optionItem = optionItem.menuOptions /= DRIVER_BOOKING_OPTIONS

----------------------------------------------- UPDATE LANGUAGE VIEW ------------------------------------------------------------------
updateLanguageView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
updateLanguageView state push =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white900
    , margin (MarginBottom 10)
    ]
    [ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
    , linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.grey900
        ]
        []
    , languagesSpokenView state push
    , primaryButtons state push
    ]

---------------------------------------------- LANGUAGES SPOKEN VIEW -----------------------------------------------------------------
languagesSpokenView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
languagesSpokenView state push =
  scrollView
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    ]
  [linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]
    [ textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text (getString SELECT_THE_LANGUAGES_YOU_CAN_SPEAK)
        , margin (Margin 20 20 20 5)
        , textSize FontSize.a_16
        , color Color.black900
        ]
    , CheckListView.view (push <<< LanguageSelection) (checkListConfig state)
    ]]

-------------------------------------------------- PRIMARY BUTTONS -----------------------------------------------------------------
primaryButtons :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
primaryButtons state push =
  linearLayout
    [ orientation HORIZONTAL
    , height MATCH_PARENT
    , weight 1.0
    , gravity BOTTOM
    ]
    [ PrimaryButton.view (push <<< UpdateButtonClicked) (primaryButtonConfig state) ]

-------------------------------------------_ COMPONENTS _-------------------------------------------------------------
---------------------------------------- vehicleListItem COMPONENT ------------------------------------------------
vehicleListItem :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> ST.DriverVehicleDetails -> PrestoDOM (Effect Unit) w
vehicleListItem state push vehicle =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , onClick push if vehicle.isVerified then (const (ActivateRc vehicle.registrationNo (vehicle.isActive && vehicle.isVerified))) else const $ PendingVehicle vehicle.registrationNo vehicle.userSelectedVehicleCategory
    , gravity CENTER_VERTICAL
    , clickable $ not $ vehicle.isActive && vehicle.isVerified
    , visibility $ MP.boolToVisibility $ not $ state.props.screenType == ST.DRIVER_DETAILS
    , background if vehicle.isVerified then Color.white900 else Color.blue600
    , cornerRadius 15.0
    , padding $ Padding 16 16 16 16
    , margin $ Margin 16 12 16 0
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , gravity CENTER_VERTICAL
        ]
        [ imageView
            [ width $ V 36
            , height $ V 36
            , margin $ MarginRight 16
            , imageWithFallback $ fetchImage FF_COMMON_ASSET $ getVehicleImage (fromMaybe vehicle.userSelectedVehicleCategory vehicle.verifiedVehicleCategory) state
            ]
        , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , weight 1.0
            ]
            [ textView
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text vehicle.registrationNo
                , color Color.black900
                , textSize FontSize.a_16
                , fontStyle $ FontStyle.semiBold LanguageStyle
                ]
            , textView
                ( [ width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , text $ fromMaybe "" vehicle.vehicleModel
                  , textSize FontSize.a_14
                  , color Color.black700
                  ]
                    <> FontStyle.body3 TypoGraphy
                )
            ]
        , imageView
            [ height $ V 21
            , width $ V 21
            , visibility $ MP.boolToVisibility $ (not vehicle.isActive) && vehicle.isVerified
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_radio_unselected"
            ]
        , imageView
            [ width $ V 21
            , height $ V 21
            , visibility $ MP.boolToVisibility $ vehicle.isActive && vehicle.isVerified
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_radio_selected"
            ]
        , imageView
            [ width $ V 21
            , height $ V 21
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right_black"
            , visibility $ MP.boolToVisibility $ not vehicle.isVerified
            ]
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation HORIZONTAL
        , background Color.blue600
        , cornerRadius 8.0
        , visibility $ MP.boolToVisibility $ vehicle.isActive && vehicle.isVerified && not (vehicle.userSelectedVehicleCategory `elem` [ST.AmbulanceCategory, ST.TruckCategory, ST.BusCategory])
        , padding $ Padding 16 8 16 8
        , margin $ MarginTop 16
        , onClick push $ const $ OptionClick DRIVER_BOOKING_OPTIONS
        ]
        [ textView
            [ text $ getString BOOKING_OPTIONS
            , textSize FontSize.a_14
            , color Color.black900
            , fontStyle $ FontStyle.semiBold LanguageStyle
            , weight 1.0
            ]
        , imageView
            [ width $ V 21
            , height $ V 21
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right_black"
            ]
        ]
    ]
  

getRcDetails :: ST.DriverProfileScreenState -> Array { key :: String, value :: Maybe String, action :: Action, isEditable :: Boolean, keyInfo :: Boolean, isRightInfo :: Boolean }
getRcDetails state = do
  let
    config = state.data.activeRCData
  ( [ { key: (getString RC_STATUS), value: Just $ if config.rcStatus then (getString ACTIVE_STR) else (getString INACTIVE_RC), action: NoAction, isEditable: false, keyInfo: false, isRightInfo: false }
    , { key: (getString REG_NUMBER), value: Just config.rcDetails.certificateNumber, action: NoAction, isEditable: false, keyInfo: false, isRightInfo: false }
    ]
      <> ( if config.rcStatus then
            [ { key: (getString TYPE), value: Just (getVehicleType state.data.driverVehicleType), action: NoAction, isEditable: false, keyInfo: false, isRightInfo: false } ]
          else
            []
        )
      <> [ { key: (getString MODEL_NAME), value: if config.rcStatus then config.rcDetails.vehicleModel else Just "NA", action: NoAction, isEditable: false, keyInfo: false, isRightInfo: false }
        , { key: (getString COLOUR), value: if config.rcStatus then config.rcDetails.vehicleColor else Just "NA", action: NoAction, isEditable: false, keyInfo: false, isRightInfo: false }
        ]
  )

bottomPill :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
bottomPill state push =
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , padding $ PaddingVertical 20 20
    ]
    [ linearLayout
        [ width $ V $ ((screenWidth unit) - 32) / 2
        , height WRAP_CONTENT
        , gravity CENTER
        , onClick push $ const $ DirectActivateRc $ if state.data.activeRCData.rcStatus then ST.DEACTIVATING_RC else ST.ACTIVATING_RC
        ]
        [ textView
            $ [ color Color.blue900
              , text $ if state.data.activeRCData.rcStatus then (getString DEACTIVATE_RC) else (getString ACTIVATE_RC)
              ]
            <> FontStyle.body6 TypoGraphy
        ]
    , linearLayout
        [ width $ V 1
        , height MATCH_PARENT
        , background Color.grey700
        ]
        []
    , linearLayout
        [ width $ V (((screenWidth unit) - 32) / 2)
        , height WRAP_CONTENT
        , gravity CENTER
        , onClick push $ const $ UpdateRC state.data.activeRCData.rcDetails.certificateNumber state.data.activeRCData.rcStatus
        ]
        [ textView
            $ [ color Color.blue900
              , text $ getString EDIT_RC
              ]
            <> FontStyle.body6 TypoGraphy
        ]
    ]

----------------------------------------------- INFO TILE VIEW COMPONENT -------------------------------------------------------
infoTileView :: forall w. ST.DriverProfileScreenState -> { primaryText :: String, subText :: String, postImgVisibility :: Boolean, seperatorView :: Boolean, margin :: Margin } -> PrestoDOM (Effect Unit) w
infoTileView state config =
  (addAnimation state)
    $ linearLayout
        [ weight 1.0
        , height WRAP_CONTENT
        , orientation VERTICAL
        , margin $ config.margin
        , background Color.blue600
        , padding $ Padding 16 16 16 16
        , cornerRadius 10.0
        ]
        [ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , gravity CENTER_VERTICAL
            ]
            [ textView
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , text config.primaryText
                , margin (MarginLeft 7)
                , fontStyle $ FontStyle.bold LanguageStyle
                , textSize FontSize.a_22
                , color Color.black900
                ]
            , linearLayout
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , margin $ MarginLeft 9
                , visibility if config.postImgVisibility then VISIBLE else GONE
                ]
                ( map
                    ( \item ->
                        linearLayout
                          [ height WRAP_CONTENT
                          , width WRAP_CONTENT
                          , margin (MarginRight 2)
                          ]
                          [ imageView
                              [ height $ V 13
                              , width $ V 13
                              , imageWithFallback case item of
                                  0.5 -> fetchImage FF_COMMON_ASSET "ny_ic_star_half_active"
                                  1.0 -> fetchImage FF_COMMON_ASSET "ny_ic_star_active"
                                  _ -> fetchImage FF_COMMON_ASSET "ny_ic_star_inactive"
                              ]
                          ]
                    )
                    (getStarsForRating 5 (fromMaybe 0.0 state.data.analyticsData.rating))
                )
            ]
        , textView
            ( [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text config.subText
              , margin (MarginLeft 7)
              , textSize FontSize.a_14
              , color Color.black700
              ]
                <> FontStyle.body3 TypoGraphy
            )
        ]

getStarsForRating :: Int -> Number -> Array Number
getStarsForRating outOf currRating = map calculateRating (range 1 outOf)
  where
  calculateRating :: Int -> Number
  calculateRating x
    | currRating - (toNumber x) < -0.50 = 0.0
    | currRating - (toNumber x) >= 0.00 = 1.0
    | otherwise = 0.5

-------------------------------------------- MENU BUTTON VIEW COMPONENT ---------------------------------------------
showMenuButtonView :: ST.DriverProfileScreenState -> forall w. (Action -> Effect Unit) -> String -> ST.Gender -> PrestoDOM (Effect Unit) w
showMenuButtonView state push genderName genderType =
  linearLayout
    [ width $ MATCH_PARENT
    , height $ V 56
    , gravity CENTER
    , margin $ (Margin 0 10 0 10)
    , background if checkGenderSelect state.data.genderTypeSelect genderType then Color.blue600 else Color.white900
    , stroke if checkGenderSelect state.data.genderTypeSelect genderType then ("1," <> Color.blue900) else ("1," <> Color.grey700)
    , cornerRadius 6.0
    , onClick push (const $ CheckBoxClick genderType)
    ]
    [ linearLayout
        [ height $ V 20
        , width $ V 20
        , stroke if checkGenderSelect state.data.genderTypeSelect genderType then ("2," <> Color.black800) else ("2," <> Color.black600)
        , cornerRadius 10.0
        , gravity CENTER
        , margin $ MarginLeft 10
        ]
        [ imageView
            [ width $ V 10
            , height $ V 10
            , imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_radio_button"
            , visibility if checkGenderSelect state.data.genderTypeSelect genderType then VISIBLE else GONE
            ]
        ]
    , textView
        [ text genderName
        , textSize FontSize.a_14
        , width MATCH_PARENT
        , gravity CENTER_VERTICAL
        , color if checkGenderSelect state.data.genderTypeSelect genderType then Color.black900 else Color.black700
        , height WRAP_CONTENT
        , margin (MarginHorizontal 10 10)
        , fontStyle $ FontStyle.regular LanguageStyle
        ]
    ]

---------------------------------------------------- DETAILS LIST VIEW COMPONENT --------------------------------------------------
detailsListViewComponent ::
  forall w.
  ST.DriverProfileScreenState ->
  (Action -> Effect Unit) ->
  { backgroundColor :: String
  , separatorColor :: String
  , isLeftKeyClickable :: Boolean
  , arrayList ::
      Array
        ( { key :: String
          , value :: Maybe String
          , action :: Action
          , isEditable :: Boolean
          , isRightInfo :: Boolean
          , keyInfo :: Boolean
          }
        )
  } ->
  PrestoDOM (Effect Unit) w
detailsListViewComponent state push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background config.backgroundColor
    , margin $ Margin 0 0 0 0
    , orientation VERTICAL
    , cornerRadius 10.0
    ]
    $ ( mapWithIndex
          ( \index item ->
              linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , cornerRadius 10.0
                , padding $ PaddingHorizontal 16 16
                , background config.backgroundColor
                ]
                [ (addAnimation state)
                    $ linearLayout
                        [ height WRAP_CONTENT
                        , width MATCH_PARENT
                        , orientation HORIZONTAL
                        , gravity CENTER_VERTICAL
                        , padding $ PaddingVertical 16 16
                        ]
                        ( [ linearLayout
                              [ height WRAP_CONTENT
                              , width WRAP_CONTENT
                              ]
                              [ textView
                                  $ [ text item.key
                                    , color if config.isLeftKeyClickable then Color.blue900 else Color.black700
                                    ]
                                  <> FontStyle.body3 TypoGraphy
                                  <> if config.isLeftKeyClickable then [ onClick push $ const LeftKeyAction ] else []
                              , imageView
                                  [ height $ V 12
                                  , width $ V 12
                                  , margin $ Margin 7 3 0 0
                                  , onClick push $ const PaymentInfo
                                  , visibility if item.keyInfo then VISIBLE else GONE
                                  , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_blue"
                                  ]
                              ]
                          , linearLayout
                              [ height WRAP_CONTENT
                              , weight 1.0
                              ]
                              []
                          , textView
                              [ text $ fromMaybe (getString ADD) item.value
                              , textSize FontSize.a_14
                              , onClick push $ const item.action
                              , visibility if item.isRightInfo then GONE else VISIBLE
                              , color case item.value of
                                  Nothing -> Color.blue900
                                  Just val -> do
                                    let
                                      isRcActive = val == (getString ACTIVE_STR)
                                    let
                                      isRcInActive = val == (getString INACTIVE_RC)
                                    let
                                      isRCEdit = val == (getString EDIT_RC)
                                    let
                                      isPaymentView = val == (getString VIEW)
                                    if isRcActive then Color.green900 else if isRcInActive then Color.red else if isRCEdit || isPaymentView then Color.blue900 else Color.black900
                              , fontStyle $ FontStyle.semiBold LanguageStyle
                              ]
                          ]
                            <> if item.isEditable && (isJust item.value) then
                                [ imageView
                                    [ height $ V 11
                                    , width $ V 11
                                    , margin $ MarginLeft 7
                                    , onClick push $ const item.action
                                    , imageWithFallback $ fetchImage FF_ASSET "ic_edit_pencil"
                                    ]
                                ]
                              else if item.isRightInfo then
                                [ imageView
                                    [ height $ V 12
                                    , width $ V 12
                                    , margin $ MarginLeft 7
                                    , onClick push $ const PaymentInfo
                                    , imageWithFallback $ fetchImage FF_ASSET "ny_ic_info_blue"
                                    ]
                                ]
                              else
                                []
                        )
                , linearLayout
                    [ height $ V 1
                    , width MATCH_PARENT
                    , background config.separatorColor
                    , visibility if index == length (config.arrayList) - 1 && state.props.screenType == ST.VEHICLE_DETAILS then VISIBLE else GONE
                    ]
                    []
                ]
          )
          (config.arrayList)
      )
    <> (if state.props.screenType == ST.VEHICLE_DETAILS then [ (bottomPill state push) ] else [])

----------------------------------------------- INFO CARD COMPONENT ---------------------------------------------------------------
infoCard :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> { key :: String, value :: String, value1 :: String, infoImageUrl :: String, postfixImage :: String, showInfoImage :: Boolean, showPostfixImage :: Boolean, valueColor :: String, action :: Action } -> PrestoDOM (Effect Unit) w
infoCard state push config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 16 16 16 16
    , margin $ MarginTop 8
    , cornerRadius 10.0
    , background Color.blue600
    ]
    [ (addAnimation state)
        $ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            , gravity CENTER_VERTICAL
            ]
            [ textView
                [ text config.key
                , textSize FontSize.a_12
                , fontStyle $ FontStyle.regular LanguageStyle
                , color Color.black700
                ]
            , imageView
                [ imageWithFallback config.infoImageUrl
                , height $ V 24
                , width $ V 24
                , visibility if config.showInfoImage then VISIBLE else GONE
                , onClick push $ const config.action
                ]
            , linearLayout
                [ height WRAP_CONTENT
                , weight 1.0
                , gravity RIGHT
                , orientation HORIZONTAL
                ]
                [ textView
                    [ text config.value
                    , textSize FontSize.a_20
                    , color config.valueColor
                    , height WRAP_CONTENT
                    , fontStyle $ FontStyle.bold LanguageStyle
                    ]
                , textView
                    [ text ("/" <> config.value1)
                    , textSize FontSize.a_14
                    , visibility if config.value1 /= "" then VISIBLE else GONE
                    , color config.valueColor
                    , height WRAP_CONTENT
                    , fontStyle $ FontStyle.medium LanguageStyle
                    ]
                , imageView
                    [ imageWithFallback config.postfixImage
                    , height MATCH_PARENT
                    , width $ V 20
                    , visibility if config.showPostfixImage then VISIBLE else GONE
                    , margin $ MarginLeft 11
                    , gravity CENTER
                    ]
                ]
            ]
    ]

------------------------------------------ ANIMATION -----------------------------------------------------
addAnimation :: forall w. ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w -> PrestoDOM (Effect Unit) w
addAnimation state = PrestoAnim.animationSet [ Anim.fadeOut (state.props.screenType == ST.VEHICLE_DETAILS), Anim.fadeOut (state.props.screenType == ST.DRIVER_DETAILS), Anim.fadeIn (state.props.screenType == ST.VEHICLE_DETAILS), Anim.fadeOut (state.props.screenType == ST.DRIVER_DETAILS), Anim.fadeIn (state.props.screenType == ST.DRIVER_DETAILS) ]

scaleUpConfig :: Boolean -> AnimConfig.AnimConfig
scaleUpConfig ifAnim =
  let
    config = AnimConfig.animConfig

    animConfig' =
      config
        { duration = 400
        , toScaleX = 1.0
        , toScaleY = 1.0
        , fromScaleY = 0.5
        , fromScaleX = 0.5
        , ifAnim = ifAnim
        }
  in
    animConfig'

scaleDownConfig :: Boolean -> AnimConfig.AnimConfig
scaleDownConfig ifAnim =
  let
    config = AnimConfig.animConfig

    animConfig' =
      config
        { duration = 400
        , toScaleX = 0.5
        , toScaleY = 0.5
        , fromScaleY = 1.0
        , fromScaleX = 1.0
        , ifAnim = ifAnim
        }
  in
    animConfig'

---------------------------------------------- Data ARRAY -----------------------------------------------------
driverDetailsArray :: ST.DriverProfileScreenState -> Array { key :: String, value :: Maybe String, action :: Action, isEditable :: Boolean, keyInfo :: Boolean, isRightInfo :: Boolean }
driverDetailsArray state =
  [ { key: (getString NAME), value: Just state.data.driverName, action: NoAction, isEditable: false, keyInfo: false, isRightInfo: false }
  , { key: (getString MOBILE_NUMBER), value: state.data.driverMobile, action: NoAction, isEditable: false, keyInfo: false, isRightInfo: false }
  , { key: (getString ALTERNATE_NUMBER), value: state.data.driverAlternateNumber, action: UpdateAlternateNumber, isEditable: true, keyInfo: false, isRightInfo: false }
  , { key: (getString GENDER), value: (getGenderName state.data.driverGender), action: SelectGender, isEditable: true, keyInfo: false, isRightInfo: false }
  ]

-- TODO :: To remove, as this is not being used
-- vehicleDetailsArray :: ST.DriverProfileScreenState -> Array {key :: String , value :: Maybe String , action :: Action, isEditable :: Boolean , keyInfo :: Boolean, isRightInfo :: Boolean}
-- vehicleDetailsArray state = [
--     { key : (getString REG_NUMBER ) , value : Just state.data.vehicleRegNumber , action : NoAction , isEditable : false , keyInfo : false, isRightInfo : false }
--   , { key : (getString TYPE), value : Just (getVehicleType state.data.driverVehicleType), action : NoAction , isEditable : false , keyInfo : false, isRightInfo : false}
--   , { key : (getString MODEL_NAME) , value : Just state.data.vehicleModelName , action :  NoAction , isEditable : false, keyInfo : false, isRightInfo : false}
--   , { key : (getString COLOUR) , value : Just state.data.vehicleColor , action :NoAction , isEditable : false , keyInfo : false, isRightInfo : false } ]
genderOptionsArray :: ST.DriverProfileScreenState -> Array { text :: String, value :: ST.Gender }
genderOptionsArray _ =
  [ { text: (getString FEMALE), value: ST.FEMALE }
  , { text: (getString MALE), value: ST.MALE }
  , { text: (getString OTHER), value: ST.OTHER }
  , { text: (getString PREFER_NOT_TO_SAY), value: ST.PREFER_NOT_TO_SAY }
  ]

vehicleSummaryArray :: ST.DriverProfileScreenState -> Array { key :: String, value :: String, value1 :: String, infoImageUrl :: String, postfixImage :: String, showInfoImage :: Boolean, showPostfixImage :: Boolean, action :: Action, valueColor :: String }
vehicleSummaryArray state = [ { key: (getString $ TRAVELLED_ON_APP "TRAVELLED_ON_APP"), value: (state.data.analyticsData.totalDistanceTravelled), value1: "", infoImageUrl: fetchImage FF_COMMON_ASSET "ny_ic_info_blue", postfixImage: fetchImage FF_ASSET "ny_ic_api_failure_popup", showPostfixImage: false, showInfoImage: false, valueColor: Color.charcoalGrey, action: NoAction } ]

vehicleAboutMeArray :: ST.DriverProfileScreenState -> Array { key :: String, value :: Maybe String, action :: Action, isEditable :: Boolean, keyInfo :: Boolean, isRightInfo :: Boolean }
vehicleAboutMeArray state =
  [ { key: (getString YEARS_OLD), value: Nothing, action: UpdateValue ST.VEHICLE_AGE, isEditable: true, keyInfo: false, isRightInfo: false }
  , { key: (getString NAME), value: Nothing, action: UpdateValue ST.VEHICLE_NAME, isEditable: true, keyInfo: false, isRightInfo: false }
  ]

driverAboutMeArray :: ST.DriverProfileScreenState -> Array { key :: String, value :: Maybe String, action :: Action, isEditable :: Boolean, keyInfo :: Boolean, isRightInfo :: Boolean }
driverAboutMeArray state =
  [ { key: (getString LANGUAGES_SPOKEN), value: ((getLanguagesSpoken (map (\item -> (getLangFromVal item)) (state.data.languagesSpoken)))), action: UpdateValue ST.LANGUAGE, isEditable: true, keyInfo: false, isRightInfo: false }
  -- , { key : (getString HOMETOWN) , value : Nothing , action : UpdateValue ST.HOME_TOWN , isEditable : true }
  ]

driverPaymentsArray :: ST.DriverProfileScreenState -> Array { key :: String, value :: Maybe String, action :: Action, isEditable :: Boolean, keyInfo :: Boolean, isRightInfo :: Boolean }
driverPaymentsArray state =
  [ { key: (getString QR_CODE), value: Just (getString VIEW), action: UpdateValue ST.PAYMENT, isEditable: false, keyInfo: true, isRightInfo: false }
  ]

driverNoAutoPayArray :: ST.DriverProfileScreenState -> Array { key :: String, value :: Maybe String, action :: Action, isEditable :: Boolean, keyInfo :: Boolean, isRightInfo :: Boolean }
driverNoAutoPayArray state =
  [ { key: (getString GET_QR_CODE), value: Nothing, action: UpdateValue ST.PAYMENT, isEditable: false, keyInfo: false, isRightInfo: true }
  ]

getLanguagesSpoken :: Array String -> Maybe String
getLanguagesSpoken languages =
  if (length languages > 2) then
    Just $ (intercalate ", " (take 2 languages)) <> " +" <> show (length languages - 2)
  else if (length languages > 0) then
    Just (intercalate ", " languages)
  else
    Nothing

getLangFromVal :: String -> String
getLangFromVal value = case value of
  "EN_US" -> "English"
  "HI_IN" -> "Hindi"
  "TA_IN" -> "Tamil"
  "KN_IN" -> "Kannada"
  "TE_IN" -> "Telugu"
  "BN_IN" -> "Bengali"
  "ML_IN" -> "Malayalam"
  _ -> value

--------------------------------------------------------------- SEPARATOR --------------------------------------------------------
horizontalLineView :: Int -> Number -> Int -> Int -> Int -> forall w. PrestoDOM (Effect Unit) w
horizontalLineView heightOfLine lineAlpha marginLeft marginTop marginRight =
  linearLayout
    [ width MATCH_PARENT
    , height $ V heightOfLine
    , background Color.grey800
    , margin (Margin marginLeft marginTop marginRight 0)
    , alpha lineAlpha
    ]
    []

-------------------------------------------- DUMMY TEXT VIEW ---------------------------------------------
dummyTextView :: forall w. PrestoDOM (Effect Unit) w
dummyTextView =
  textView
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    ]

rcEditPopUpView :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
rcEditPopUpView push state =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.black9000
    , alignParentBottom "true,-1"
    , clickable true
    , onClick push (const RemoveEditRC)
    ]
    ( [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , background Color.white900
          , orientation VERTICAL
          , cornerRadii $ Corners 24.0 true true false false
          , padding (Padding 20 32 20 0)
          , alignParentBottom "true,-1"
          , disableClickFeedback true
          , gravity CENTER
          ]
          [ textView
              $ [ text $ (getString EDIT_RC) <> " - " <> state.data.rcNumber
                , fontStyle $ FontStyle.bold LanguageStyle
                , height WRAP_CONTENT
                , color Color.black800
                , textSize FontSize.a_18
                , margin (MarginBottom 4)
                ]
          , linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              ]
              ( map
                  ( \item ->
                      linearLayout
                        [ height WRAP_CONTENT
                        , width MATCH_PARENT
                        , orientation VERTICAL
                        ]
                        [ trackingCardRcEditView push state item
                        , if (item.type /= ST.DELETING_RC) then
                            linearLayout
                              [ height $ V 1
                              , width MATCH_PARENT
                              , background Color.grey900
                              ]
                              []
                          else
                            linearLayout [] []
                        ]
                  )
                  (rcEditPopUpData state)
              )
          ]
      ]
        <> if state.props.activateOrDeactivateRcView then
            [ activateAndDeactivateRcConfirmationPopUpView push state ]
          else
            []
              <> if state.props.deleteRcView then [ deleteRcPopUpView push state ] else []
    )

rcEditPopUpData :: ST.DriverProfileScreenState -> Array { text :: String, imageWithFallback :: String, type :: ST.EditRc }
rcEditPopUpData state =
  [ { text: if state.data.isRCActive then (getString DEACTIVATE_RC) else (getString ACTIVATE_RC)
    , imageWithFallback: fetchImage FF_ASSET $ if state.data.isRCActive then "ny_ic_deactivate" else "ny_ic_activate"
    , type: if state.data.isRCActive then ST.ACTIVATING_RC else ST.DEACTIVATING_RC
    }
  , { text: (getString DELETE_RC)
    , imageWithFallback: fetchImage FF_ASSET "ny_ic_bin"
    , type: ST.DELETING_RC
    }
  ]

trackingCardRcEditView :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> { text :: String, imageWithFallback :: String, type :: ST.EditRc } -> PrestoDOM (Effect Unit) w
trackingCardRcEditView push state item =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , padding (Padding 0 18 0 18)
    , gravity CENTER_VERTICAL
    , alpha if (item.type == ST.DELETING_RC && length state.data.rcDataArray == 1) then 0.4 else 1.0
    , onClick push (const (DeactivateRc item.type state.data.rcNumber))
    ]
    [ imageView
        [ imageWithFallback item.imageWithFallback
        , height $ V 25
        , width $ V 25
        , margin (MarginRight 20)
        ]
    , textView
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , textSize FontSize.a_18
        , fontStyle $ FontStyle.medium LanguageStyle
        , text item.text
        , color $ if item.type == ST.DELETING_RC then Color.red else Color.black800
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity RIGHT
        ]
        [ imageView
            [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right"
            , height $ V 30
            , width $ V 32
            , gravity RIGHT
            , padding (Padding 3 3 3 3)
            ]
        ]
    ]

paymentInfoPopUpView :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
paymentInfoPopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    [ PopUpModal.view (push <<< PaymentInfoPopUpModalAction) (paymentInfoPopUpConfig push state) ]

activateAndDeactivateRcConfirmationPopUpView :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
activateAndDeactivateRcConfirmationPopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    [ PopUpModal.view (push <<< ActivateOrDeactivateRcPopUpModalAction) (activateAndDeactivateRcPopUpConfig push state) ]

callDriverPopUpView :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
callDriverPopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , padding (Padding 24 20 24 20)
    , background Color.black9000
    ]
    [ PopUpModal.view (push <<< CallDriverPopUpModalAction) (callDriverPopUpConfig push state) ]

deleteRcPopUpView :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
deleteRcPopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    [ PopUpModal.view (push <<< DeleteRcPopUpModalAction) (deleteRcPopUpConfig state) ]

driverBlockedPopupView :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
driverBlockedPopupView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    [ PopUpModal.view (push <<< DriverBLockedPopupAction) (driverBLockedPopup state) ]


rcActiveOnAnotherDriverProfilePopUpView :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
rcActiveOnAnotherDriverProfilePopUpView push state =
  relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.black9000
    , alignParentBottom "true,-1"
    , clickable true
    , onClick push (const SkipActiveRc)
    ]
    ( [ linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , background Color.white900
          , orientation VERTICAL
          , cornerRadii $ Corners 24.0 true true false false
          , padding (Padding 20 32 20 25)
          , alignParentBottom "true,-1"
          , disableClickFeedback true
          , gravity CENTER
          ]
          [ textView
              $ [ text $ "RC - " <> state.data.rcNumber <> (getString ACTIVE_RC_ON_ANOTHER_DRIVER)
                , fontStyle $ FontStyle.bold LanguageStyle
                , height WRAP_CONTENT
                , color Color.black800
                , textSize FontSize.a_18
                , padding $ Padding 10 0 10 0
                , margin $ Margin 4 0 4 4
                , gravity CENTER
                ]
          , linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , background Color.white900
              , orientation VERTICAL
              , cornerRadii $ Corners 24.0 true true false false
              , alignParentBottom "true,-1"
              , disableClickFeedback true
              , gravity CENTER
              ]
              [ textView
                  $ [ text $ (getString CALL_DRIVER_OR_CONTACT_SUPPORT)
                    , height WRAP_CONTENT
                    , color Color.black700
                    , textSize FontSize.a_14
                    , padding $ PaddingHorizontal 8 8
                    , margin $ Margin 4 0 4 4
                    , gravity CENTER
                    ]
              ]
          , linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              ]
              ( map
                  ( \item ->
                      linearLayout
                        [ height WRAP_CONTENT
                        , width MATCH_PARENT
                        , orientation VERTICAL
                        ]
                        [ trackingCardRcActiveOnAnotherDriverProfileView push state item
                        , linearLayout
                            [ height $ V 1
                            , width MATCH_PARENT
                            , background Color.grey900
                            ]
                            []
                        ]
                  )
                  (activeRcOnAnotherDriverProfilePopUpData state)
              )
          , linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , gravity CENTER
              , margin $ MarginTop 24
              , onClick push $ const SkipActiveRc
              ]
              [ textView
                  $ [ text $ (getString CANCEL)
                    , height WRAP_CONTENT
                    , color Color.black700
                    , textSize FontSize.a_16
                    , margin $ Margin 4 0 4 4
                    , gravity CENTER
                    ]
              ]
          ]
      ]
        <> if state.props.callDriver then [ callDriverPopUpView push state ] else []
    )

activeRcOnAnotherDriverProfilePopUpData :: ST.DriverProfileScreenState -> Array { text :: String, type :: ST.CallOptions }
activeRcOnAnotherDriverProfilePopUpData state =
  [ { text: (getString CALL_DRIVER)
    , type: ST.CALLING_DRIVER
    }
  , { text: (getString CALL_CUSTOMER_SUPPORT)
    , type: ST.CALLING_CUSTOMER_SUPPORT
    }
  ]

trackingCardRcActiveOnAnotherDriverProfileView :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> { text :: String, type :: ST.CallOptions } -> PrestoDOM (Effect Unit) w
trackingCardRcActiveOnAnotherDriverProfileView push state item =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , padding (Padding 3 16 3 16)
    , gravity CENTER_VERTICAL
    , onClick push (const $ if item.type == ST.CALLING_DRIVER then CallDriver else CallCustomerSupport)
    ]
    [ textView
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , textSize FontSize.a_16
        , text item.text
        , color $ Color.black900
        , fontStyle $ FontStyle.medium LanguageStyle
        ]
    , linearLayout
        [ width MATCH_PARENT
        , gravity RIGHT
        ]
        [ imageView
            [ imageWithFallback $ fetchImage FF_COMMON_ASSET "ny_ic_chevron_right"
            , height $ V 30
            , width $ V 32
            , gravity RIGHT
            , padding (Padding 3 3 3 3)
            ]
        ]
    ]

type MissedOpportunity
  = { key :: String
    , value :: String
    , value1 :: String
    , infoImageUrl :: String
    , postfixImage :: String
    , showInfoImage :: Boolean
    , showPostfixImage :: Boolean
    , action :: Action
    , valueColor :: String
    }

getVehicleImage :: ST.VehicleCategory -> ST.DriverProfileScreenState -> String
getVehicleImage category state =
  let vehicleVariant = (getValueToLocalStore VEHICLE_VARIANT)
      showSpecialVariantImage = vehicleVariant == "HERITAGE_CAB"
  in 
    if showSpecialVariantImage 
      then HU.getVehicleVariantImage vehicleVariant
      else mkAsset category $ getCityConfig state.data.config.cityConfig (getValueToLocalStore DRIVER_LOCATION)
  where
    mkAsset :: ST.VehicleCategory -> CityConfig -> String
    mkAsset category cityConfig =
      case category of
        ST.AutoCategory -> cityConfig.assets.auto_image
        ST.CarCategory -> "ny_ic_sedan"
        ST.BikeCategory -> "ny_ic_bike_side"
        ST.AmbulanceCategory -> "ny_ic_ambulance_side"
        ST.TruckCategory -> "ny_ic_truck_side"
        ST.BusCategory -> "ny_ic_bus_side"
        _ -> "ny_ic_silhouette"

    getAutoImage :: CityConfig -> String
    getAutoImage cityConfig =
      if cityConfig.cityCode == "std:040" then
        "ny_ic_black_yellow_auto_side_view"
      else
        "ny_ic_auto_side_view"
