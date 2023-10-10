{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverProfileScreen.View where

import Common.Types.App
import Data.List
import Screens.DriverProfileScreen.ComponentConfig

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
import Data.Array (length, mapWithIndex, null, any, (!!), take)
import Data.Either (Either(..))
import Data.List (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (getNewIDWithTag, isPreviousVersion, liftFlow, screenWidth)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils as EHU
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink, getVehicleType)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (getValueFromConfig)
import MerchantConfig.Utils as MU
import Prelude (Unit, ($), const, map, (+), (==), (<), (||), (/), (/=), unit, bind, (-), (<>), (<=), (<<<), (>), pure, discard, show, (&&), void, negate, not)
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), horizontalScrollView, afterRender, alpha, background, color, cornerRadius, fontStyle, frameLayout, gravity, height, id, imageUrl, imageView, imageWithFallback, layoutGravity, linearLayout, margin, onBackPressed, onClick, orientation, padding, scrollView, text, textSize, textView, visibility, weight, width, webView, url, clickable, relativeLayout, stroke, alignParentBottom, disableClickFeedback)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii, scrollBarY)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens as ScreenNames
import Screens.DriverProfileScreen.Controller (Action(..), ScreenOutput, eval, getTitle, checkGenderSelect, getGenderName, optionList)
import Screens.DriverProfileScreen.ScreenData (MenuOptions(..))
import Screens.Types as ST
import Services.API (GetDriverInfoReq(..), GetDriverInfoResp(..), GetAllRcDataReq(..), GetAllRcDataResp(..))
import Services.Backend as Remote
import Storage (KeyStore(..), getValueToLocalStore)
import Storage (isLocalStageOn)
import Styles.Colors as Color
import Types.App (defaultGlobalState)

screen :: ST.DriverProfileScreenState -> Screen Action ST.DriverProfileScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "DriverProfileScreen"
  , globalEvents : [(\push -> do
      if initialState.props.openSettings then
      pure unit
      else do
        void $ launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
          getAllRcsDataResp <- Remote.getAllRcDataBT (GetAllRcDataReq)
          lift $ lift $ doAff do liftEffect $ push $ GetRcsDataResponse getAllRcsDataResp
        void $ launchAff $ EHC.flowRunner defaultGlobalState $ do
          void $ EHU.loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
          EHU.toggleLoader true
          summaryResponse <- Remote.driverProfileSummary ""
          profileResponse <- Remote.getDriverInfoApi (GetDriverInfoReq { })
          case summaryResponse, profileResponse of
            Right summaryResp , Right profileResp -> do
              liftFlow $ push $ DriverSummary summaryResp
              liftFlow $ push $ GetDriverInfoResponse profileResp
            _ , _ -> liftFlow $ push $ BackPressed
          EHU.toggleLoader false
          pure unit
      pure $ pure unit
    )]
  , eval : \action state -> do
      let _ = spy  "DriverProfileScreen action " action
      let _ = spy  "DriverProfileScreen state "  state
      eval action state
  }

view  :: forall w. (Action -> Effect Unit)  -> ST.DriverProfileScreenState  -> PrestoDOM (Effect Unit) w
view push state =
  frameLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    ][  linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push $ const BackPressed
        , background Color.white900
        , visibility if state.props.updateLanguages ||  state.props.updateDetails then GONE else VISIBLE
        ][  settingsView state push
          , profileView push state]
      , linearLayout
        [ width MATCH_PARENT
          , height MATCH_PARENT
          , background Color.lightBlack900
          , visibility if state.props.logoutModalView == true then VISIBLE else GONE
          ][ PopUpModal.view (push <<<PopUpModalAction) (logoutPopUp state) ]
        , if state.props.showLiveDashboard then showLiveStatsDashboard push state else dummyTextView
        , if state.props.showGenderView || state.props.alternateNumberView then driverNumberGenderView state push else dummyTextView
        , if state.props.removeAlternateNumber then PopUpModal.view (push <<<  RemoveAlternateNumberAC) (removeAlternateNumberConfig state ) else dummyTextView
        , if state.props.enterOtpModal then enterOtpModal push state else dummyTextView
        , if state.props.updateLanguages then updateLanguageView state push else dummyTextView
        , if  any (_ == state.props.detailsUpdationType) [Just ST.VEHICLE_AGE , Just ST.VEHICLE_NAME] then updateDetailsView state push else dummyTextView
        , if state.props.activateRcView then rcEditPopUpView push state else dummyTextView
        , if state.props.alreadyActive then rcActiveOnAnotherDriverProfilePopUpView push state else dummyTextView
        , if state.props.activateOrDeactivateRcView then activateAndDeactivateRcConfirmationPopUpView push state else dummyTextView
    ]


updateDetailsView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit )-> PrestoDOM (Effect Unit) w
updateDetailsView state push =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , orientation VERTICAL
  , afterRender (\action -> do
        _ <- push action
        _ <- JB.requestKeyboardShow (EHC.getNewIDWithTag "UpdateDetailsEditText")
        pure unit
        ) (const NoAction)
  ][  GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
    , horizontalLineView 1 1.0 0 0 0
    , PrimaryEditText.view (push <<< PrimaryEditTextAC) (primaryEditTextConfig state)
    , linearLayout
      [ height MATCH_PARENT
      , weight 1.0
      , gravity BOTTOM
      , margin $ MarginBottom 24
      ][ PrimaryButton.view (push <<< UpdateValueAC) (updateButtonConfig state)]
  ]


---------------------------------------- PROFILE VIEW -----------------------------------------------------------

profileView :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
profileView push state =
  PrestoAnim.animationSet [Anim.fadeIn (not state.props.openSettings)] $
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility $ if state.props.openSettings then GONE else VISIBLE
  ][  headerView state push
    , linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.grey900
    ][]
    , scrollView
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , scrollBarY false
      ][  linearLayout[
            height WRAP_CONTENT
          , width MATCH_PARENT
          , orientation VERTICAL
          ][  linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , background Color.blue600
              , padding $ PaddingVertical 16 24
              ][  tabView state push
                , tabImageView state push
                , infoView state push
                ]
            , if state.props.screenType == ST.DRIVER_DETAILS then driverDetailsView push state else vehicleDetailsView push state  -- TODO: Once APIs are deployed this code can be uncommented
            , if state.props.screenType == ST.DRIVER_DETAILS then additionalDetails push state else dummyTextView
            , if (not null state.data.inactiveRCArray) && state.props.screenType == ST.VEHICLE_DETAILS then vehicleRcDetails push state else dummyTextView
            , if (length state.data.inactiveRCArray < 2) && state.props.screenType == ST.VEHICLE_DETAILS then addRcView state push else dummyTextView
            ]
        ]
    ]



------------------------------------------- HEADER VIEW -------------------------------------------------------------
headerView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
headerView state push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , gravity BOTTOM
  , padding $ Padding 16 16 16 16
  ][ imageView
      [ width $ V 30
      , height $ V 30
      , imageWithFallback "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_left.png"
      , onClick push $ const BackPressed
      ]
    , textView
      ([ weight 1.0
      , height MATCH_PARENT
      , text (getString MY_PROFILE)
      , margin $ MarginLeft 20
      , color Color.black900
      ] <> FontStyle.h3 TypoGraphy)
    , linearLayout
      [ height MATCH_PARENT
      , width WRAP_CONTENT
      , gravity CENTER
      , onClick push $ const OpenSettings
      ][  imageView
          [ height $ V 20
          , width $ V 20
          , margin $ MarginRight 4
          , imageWithFallback "ic_settings,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_left.png"
          ]
        , textView
          ([ text (getString SETTINGS)
          , color Color.blue900
          , padding $ PaddingBottom 2
          ] <> FontStyle.body1 TypoGraphy)
      ]
  ]

vehicleRcDetails :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
vehicleRcDetails push state =
  linearLayout[
    height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ Margin 0 0 12 0
  , orientation VERTICAL
  ][
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    ]( map (\rcData -> additionalRcsView state push rcData) $ getRcNumDetails state.data.inactiveRCArray
    )
  ]

getRcNumDetails :: Array ST.RcData -> Array {key :: String, idx :: Int, value :: String, action :: Action, model :: Maybe String, color :: Maybe String}
getRcNumDetails config = do
  mapWithIndex (\index item -> {key : "RC", idx : index+1,  value : item.rcDetails.certificateNumber, action : NoAction, model : item.rcDetails.vehicleModel, color : item.rcDetails.vehicleColor}) config

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
  ][  textView
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
tabImageView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
tabImageView state push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_HORIZONTAL
  , padding $ PaddingVertical 32 32
  , background Color.blue600
  , orientation HORIZONTAL
  ][  PrestoAnim.animationSet
      [ Anim.motionMagnifyAnim $ (scaleUpConfig (state.props.screenType == ST.DRIVER_DETAILS)) {fromX = -44 , toX = 44}
      , Anim.motionMagnifyAnim $ (scaleDownConfig (state.props.screenType == ST.VEHICLE_DETAILS)) {fromX = 44 , toX = -44}
      ] $ 
      linearLayout[
        height $ V 88
      , width $ V 88
      , cornerRadius 44.0
      , margin $ MarginRight 10
      , onClick push $ const $ ChangeScreen ST.DRIVER_DETAILS
      , alpha if (state.props.screenType == ST.DRIVER_DETAILS) then 1.0 else 0.4
      ] [ (if state.data.profileImg == Nothing then 
          imageView[ 
            height $ V 88
          , width $ V 88
          , imageWithFallback $ "ny_ic_user," <> getAssetStoreLink FunctionCall <> "ic_new_avatar.png"
          ]
        else 
          linearLayout [
            height $ V 88
          , width $ V 88
          , afterRender (\action -> do JB.renderBase64Image (fromMaybe "" state.data.profileImg) (getNewIDWithTag "driver_prof_img") false "CENTER_CROP") (const NoAction)
          , id (getNewIDWithTag "driver_prof_img")][]
        )
      ]      
  ,  PrestoAnim.animationSet
    [ Anim.motionMagnifyAnim $ (scaleUpConfig (state.props.screenType == ST.VEHICLE_DETAILS)) {fromX = 44 , toX = -44}
    , Anim.motionMagnifyAnim $ (scaleDownConfig (state.props.screenType == ST.DRIVER_DETAILS)) {fromX = -44 , toX = 44}
    ] $ linearLayout
        [ height $ V 88
        , width $ V 88
        , cornerRadius 44.0
        , background Color.white900
        , onClick push $ const $ ChangeScreen ST.VEHICLE_DETAILS
        , gravity CENTER
        , alpha if (state.props.screenType == ST.VEHICLE_DETAILS) then 1.0 else 0.4
        ][  imageView
            [ imageWithFallback if state.data.driverVehicleType == "AUTO_RICKSHAW" then "ny_ic_auto_side_view,https://assets.juspay.in/nammayatri/images/common/ic_navigation_blue11.png" else "ny_ic_silhouette,https://assets.juspay.in/nammayatri/images/common/ic_navigation_blue11.png" --change this image link after uploading in asset store
            , height $ V 68
            , width $ V 68
            ]
        ]
  ]

---------------------------------------------- DRIVER DETAILS VIEW ------------------------------------------------------------

driverDetailsView :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
driverDetailsView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginHorizontal 16 16
  ][  driverAnalyticsView state push
    , missedOpportunityView state push
    , badgeLayoutView state  ]





------------------------------------------- MISSED OPPORTUNITY VIEW -----------------------------------------

missedOpportunityView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
missedOpportunityView state push  =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ Margin 0 40 0 0
  ][  textView
      [ text (getString MISSED_OPPORTUNITY)
      , margin $ Margin 0 0 16 4
      , textSize FontSize.a_16
      , color Color.black900
      , fontStyle $ FontStyle.semiBold LanguageStyle
      ]
    , linearLayout
      [height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ](map (\item -> infoCard state push item) (missedOppArray state.data.analyticsData))
  ]

missedOppArray :: ST.AnalyticsData -> Array MissedOpportunity
missedOppArray analyticsData = [{key : (getString CANCELLATION_RATE), value :  (show analyticsData.cancellationRate <> "%"), value1 : "" , infoImageUrl : "ny_ic_info_blue,https://assets.juspay.in/nammayatri/images/common/ny_ic_info_blue.png", postfixImage : "ny_ic_api_failure_popup,https://assets.juspay.in/nammayatri/images/driver/ny_ic_api_failure_popup.png", showPostfixImage : false, showInfoImage : false, valueColor : Color.charcoalGrey, action : NoAction},
  {key : (getString RIDES_CANCELLED), value : show analyticsData.ridesCancelled , value1 : show analyticsData.totalRidesAssigned , infoImageUrl : "ny_ic_info_blue,https://assets.juspay.in/nammayatri/images/common/ny_ic_info_blue.png", postfixImage : "ny_ic_api_failure_popup,https://assets.juspay.in/nammayatri/images/driver/ny_ic_api_failure_popup.png", showPostfixImage : false, showInfoImage : false, valueColor : Color.charcoalGrey, action : NoAction},
    {key : (getString EARNINGS_MISSED), value : "₹" <> EHC.formatCurrencyWithCommas (show analyticsData.missedEarnings) , value1 : "", infoImageUrl : "ny_ic_info_blue,https://assets.juspay.in/nammayatri/images/common/ny_ic_info_blue.png", postfixImage : "ny_ic_api_failure_popup,https://assets.juspay.in/nammayatri/images/driver/ny_ic_api_failure_popup.png", showPostfixImage : false, showInfoImage : false, valueColor : Color.charcoalGrey, action : NoAction}]
------------------------------------------- DRIVER ANALYTICS VIEW  ----------------------------------------------------------
driverAnalyticsView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
driverAnalyticsView state push =
  let analyticsData = state.data.analyticsData
  in linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ Margin 0 40 0 0
  ][  textView
      [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text (getString SUMMARY)
        , textSize FontSize.a_16
        , color Color.black900
        , fontStyle $ FontStyle.semiBold LanguageStyle
      ]
    , let bonusActivated = getValueFromConfig "BONUS_EARNED" == "true"  in
      linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , margin if bonusActivated then (MarginVertical 12 12) else (MarginVertical 4 12)
      , background if bonusActivated then Color.blue600 else Color.transparent
      , cornerRadius 10.0
      ][  if bonusActivated then
            linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            ]
            [
              infoTileView state {primaryText: "₹ " <> (EHC.formatCurrencyWithCommas analyticsData.totalEarnings), subText: (getString EARNED_ON_APP), postImgVisibility : false, seperatorView : false, margin : Margin 0 0 0 0}
            , linearLayout
              [ height MATCH_PARENT
              , width (V 1)
              , margin (Margin 0 16 0 16)
              , background Color.lightGreyShade
              ][]
            , infoTileView state {primaryText: "₹ " <> EHC.formatCurrencyWithCommas analyticsData.bonusEarned , subText: (getString NAMMA_BONUS), postImgVisibility : false, seperatorView : false, margin : Margin 0 0 0 0}
            ]
          else infoCard state push {key : (getString EARNED_ON_APP), value : "₹" <> (EHC.formatCurrencyWithCommas analyticsData.totalEarnings) , value1 : "", infoImageUrl : "", postfixImage : "", showPostfixImage : false, showInfoImage : false, valueColor : Color.charcoalGrey, action : NoAction}
        ]
      , linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , margin $ Margin 0 12 0 12
        ][  infoTileView state {primaryText: (show $ fromMaybe 0.0 analyticsData.rating), subText: (getString RATED_BY_USERS1)<> " " <> show analyticsData.totalUsersRated <> " " <> (getString RATED_BY_USERS2), postImgVisibility : true, seperatorView : true, margin : MarginRight 6}
          , infoTileView state {primaryText: show analyticsData.totalCompletedTrips, subText: (getString TRIPS_COMPLETED), postImgVisibility : false, seperatorView : true, margin : MarginLeft 6}
        ]
      , horizontalScrollView
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation HORIZONTAL
        ][  linearLayout
            [ width MATCH_PARENT
            , height MATCH_PARENT
            , orientation HORIZONTAL
            ] $ map (\item -> chipRailView item) (getChipRailArray state.data.analyticsData.lateNightTrips state.data.analyticsData.lastRegistered state.data.languagesSpoken state.data.analyticsData.totalDistanceTravelled)
           ]
  ]


addRcView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
addRcView state push =
  linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , alignParentBottom "true,-1"
      ][ PrimaryButton.view (push <<< AddRcButtonAC) (addRCButtonConfig state)]

additionalRcsView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> { key :: String , idx :: Int, value ::  String , action :: Action, model :: Maybe String, color :: Maybe String} -> PrestoDOM (Effect Unit) w
additionalRcsView state push config =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , padding $ Padding 15 7 15 7
  , background Color.blue600
  , margin $ Margin 16 15 5 0
  , orientation VERTICAL
  , cornerRadius 10.0
  , onClick push (const (OpenRcView config.idx))
  ][linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ][ (addAnimation state) $ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            , gravity CENTER_VERTICAL
            , padding $ PaddingVertical 16 16
            ]([  textView
                [ text config.key
                , textSize FontSize.a_12
                , color Color.black700
                , fontStyle $ FontStyle.regular LanguageStyle
                ]
              , linearLayout
                [ height WRAP_CONTENT
                , weight 1.0
                ][]
              , textView
                [ text config.value
                , textSize FontSize.a_14
                , onClick push $ const config.action
                , color Color.black900
                , fontStyle $ FontStyle.semiBold LanguageStyle
                ]
            ] <>  [imageView
                [ height $ V 11
                , width $ V 11
                , margin $ MarginLeft 7
                , imageWithFallback if elem config.idx state.data.openInactiveRCViewOrNotArray then "ny_ic_chevron_up,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_up.png" else "ny_ic_chevron_down,https://assets.juspay.in/beckn/nammayatri/nammayatricommon/images/ny_ic_chevron_down.png"
                ]] )
            ],
            linearLayout[
              height $ V 1,
              width MATCH_PARENT,
              background Color.white900,
              visibility if elem config.idx state.data.openInactiveRCViewOrNotArray then VISIBLE else GONE
            ][]
            ,linearLayout[
              height WRAP_CONTENT
            , width MATCH_PARENT
            , visibility if elem config.idx state.data.openInactiveRCViewOrNotArray then VISIBLE else GONE
            ][
                detailsViewForInactiveRcs state push { backgroundColor : Color.blue600
                        , lineColor : Color.white900
                        , arrayList :  getRcDetailsForInactiveRcs config
                      }
            ]
        ]

getRcDetailsForInactiveRcs config  = [
    --{ key : (getString TYPE) , value :Just  (getString AUTO_RICKSHAW) , action : NoAction , isEditable : false }
    { key : (getString MODEL_NAME) , value : config.model, action :  NoAction , isEditable : false}
  , { key : (getString COLOUR) , value : config.color, action :NoAction , isEditable : false }
  , { key : "" , value : Just (getString EDIT_RC), action : UpdateRC config.value false , isEditable : false } ]



detailsViewForInactiveRcs :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> {backgroundColor :: String , lineColor :: String , arrayList :: Array { key :: String , value :: Maybe String , action :: Action , isEditable :: Boolean }} -> PrestoDOM (Effect Unit) w
detailsViewForInactiveRcs state push config =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background config.backgroundColor
  , orientation VERTICAL
  , cornerRadius 10.0
  ](mapWithIndex(\ index item ->
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ][ (addAnimation state) $ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            , gravity CENTER_VERTICAL
            , padding $ PaddingVertical 16 16
            ]([  textView
                [ text item.key
                , textSize FontSize.a_12
                , color Color.black700
                , fontStyle $ FontStyle.regular LanguageStyle
                ]
              , linearLayout
                [ height WRAP_CONTENT
                , weight 1.0
                ][]
              , textView
                [ text $ fromMaybe "N/A" item.value
                , textSize FontSize.a_14
                , onClick push $ const item.action
                , color if (isJust item.value) then (if item.value == Just (getString EDIT_RC) then Color.blue900 else Color.black900) else Color.black900
                , fontStyle $ FontStyle.semiBold LanguageStyle
                ]
            ] <> if item.isEditable && (isJust item.value) then [imageView
                [ height $ V 11
                , width $ V 11
                , margin $ MarginLeft 7
                , imageWithFallback "ic_edit_pencil,https://assets.juspay.in/nammayatri/images/driver/ny_ic_chevron_left.png"
                ]]  else [])
          , linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , background config.lineColor
            , visibility if index == length (config.arrayList) - 1 then GONE else VISIBLE
            ][]
            ]
        ) (config.arrayList))
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
    ][  textView
        [ text item.mainTxt
        , width WRAP_CONTENT
        , height WRAP_CONTENT
        , textSize FontSize.a_14
        , fontStyle $ FontStyle.bold LanguageStyle
        , color Color.black900
        , margin $ MarginRight 4
        ]
      , textView $
        [ text item.subTxt
        , width WRAP_CONTENT
        , height WRAP_CONTENT
        , textSize FontSize.a_12
        , color Color.black700
        ] <> FontStyle.body3 TypoGraphy
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
  ][  textView
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
      ][ linearLayout
          [ width MATCH_PARENT
          , height MATCH_PARENT
          , orientation HORIZONTAL
          ](map(\item -> badgeView item) state.data.analyticsData.badges)
      ]
    ]

------------------------------------------------------- BADGE VIEW -------------------------------------------------------------

badgeView :: forall w. {badgeImage :: String, primaryText :: String, subText :: String} -> PrestoDOM (Effect Unit) w
badgeView state =
  linearLayout
    [ width WRAP_CONTENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , background Color.blue600
    , cornerRadius 15.0
    , padding $ Padding 25 10 25 12
    , margin $ MarginRight 16
    ][ imageView
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
getBadgeData :: ST.DriverProfileScreenState -> Array {badgeImage :: String, primaryText :: String, subText :: String}
getBadgeData state = [{badgeImage: "ny_ic_five_star_badge,https://assets.juspay.in/nammayatri/images/driver/ny_ic_five_star_badge.png"
                     , primaryText: "5-Star Rides"
                     , subText: "235"
                      },
                      {badgeImage: "ny_ic_safe_ride,https://assets.juspay.in/nammayatri/images/driver/ny_ic_safe_ride.png"
                     , primaryText: "Safe Rides"
                     , subText: "235"
                      },
                      {badgeImage: "ny_ic_clean_auto_badge,https://assets.juspay.in/nammayatri/images/driver/ny_ic_clean_auto_badge.png"
                     , primaryText: "Clean Auto"
                     , subText: "235"
                      },
                      {badgeImage: "ny_ic_expert_driver,https://assets.juspay.in/nammayatri/images/driver/ny_ic_expert_driver.png"
                     , primaryText: "Expert Driving"
                     , subText: "235"
                      },
                      {badgeImage: "ny_ic_navigator_badge,https://assets.juspay.in/nammayatri/images/driver/ny_ic_navigator_badge.png"
                     , primaryText: "Navigator"
                     , subText: "235"
                      },
                      {badgeImage: "ny_ic_ontime_badge,https://assets.juspay.in/nammayatri/images/driver/ny_ic_ontime_badge.png"
                     , primaryText: "On Time"
                     , subText: "235"
                      },
                      {badgeImage: "ny_ic_polite_driver_badge,https://assets.juspay.in/nammayatri/images/driver/ny_ic_polite_driver_badge.png"
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
  ][  --vehicleAnalyticsView push state
   ]


--------------------------------------- VEHICLE ANALYTICS VIEW ------------------------------------------------------------
vehicleAnalyticsView :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
vehicleAnalyticsView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ Margin 0 24 0 0
  ][  textView
      [ text (getString SUMMARY)
      , margin $ Margin 0 0 16 12
      , textSize FontSize.a_16
      , color Color.black
      , fontStyle $ FontStyle.medium LanguageStyle
      ]
    , linearLayout
      [height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ](map (\item -> infoCard state push item) (vehicleSummaryArray state))
  ]

--------------------------------------- ADDITIONAL DETAILS VIEW ------------------------------------------------------------
additionalDetails :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
additionalDetails push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , margin $ Margin 16 40 16 0
  , orientation VERTICAL
  ]([  textView
      [ text if state.props.screenType == ST.DRIVER_DETAILS then (getString ABOUT_ME) else (getString ABOUT_VEHICLE)
      , margin $ Margin 0 0 0 12
      , textSize FontSize.a_16
      , color Color.black900
      , fontStyle $ FontStyle.semiBold LanguageStyle
      ]
  ] <> [detailsListViewComponent state push {  backgroundColor : Color.blue600
                              , separatorColor : Color.white900
                              , arrayList : if state.props.screenType == ST.DRIVER_DETAILS then driverAboutMeArray state else vehicleAboutMeArray state
                                }])

-------------------------------------------- DRIVER NUMBER AND GENDER VIEW ----------------------------------------------------------------
driverNumberGenderView :: ST.DriverProfileScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
driverNumberGenderView state push=
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility VISIBLE
  , padding $ PaddingVertical EHC.safeMarginTop EHC.safeMarginBottom
  , background Color.white900
  , clickable true
  ][
    GenericHeader.view (push <<< DriverGenericHeaderAC) (driverGenericHeaderConfig state)
  , linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.grey900
    , margin $ MarginTop 2
    ][]
  , textView[
      width MATCH_PARENT
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
    [
      linearLayout
      [ height $ V 1
      , width MATCH_PARENT
      , background Color.grey900
      , margin $ MarginBottom 16
      ][]
    , PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)]
    ]

---------------------------- GENDER PROFILE VIEW ----------------------------------

genderProfileLayoutView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
genderProfileLayoutView state push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  ][
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ MarginHorizontal 10 10
    ](map(\item -> showMenuButtonView state push item.text item.value)(genderOptionsArray state))
  ]

------------------------------------------------------ ALTERNATE NUMBER VIEW ----------------------------------------------------
alternateNumberLayoutView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
alternateNumberLayoutView state push =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  ][
    if state.data.alterNumberEditableText then
      linearLayout[
        width MATCH_PARENT
      , height WRAP_CONTENT
      , stroke ("1," <> Color.grey900)
      , margin $ Margin 10 10 10 10
      , padding $ Padding 16 20 16 20
      , cornerRadius 8.0
      ][
        textView[
          height WRAP_CONTENT
        , width WRAP_CONTENT
        , text $ "+91 " <> fromMaybe "" state.data.driverAlternateNumber
        , color Color.black800
        ],
        linearLayout[
          width MATCH_PARENT
        , height WRAP_CONTENT
        , gravity RIGHT
        ][
          textView[
            height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ getString EDIT
          , color Color.blue900
          , onClick push $ const $ EditNumberText
          ],
          textView[
            height WRAP_CONTENT
          , width WRAP_CONTENT
          , text $ getString REMOVE
          , color Color.blue900
          , margin $ MarginHorizontal 10 10
          , onClick push $ const $ RemoveAlterNumber
          ]
        ]
      ]
    else
      linearLayout [
        height WRAP_CONTENT,
        width MATCH_PARENT,
        afterRender (\action -> do
        _ <- push action
        _ <- JB.requestKeyboardShow (EHC.getNewIDWithTag "alternateMobileNumber")
        pure unit
        ) (const AfterRender)
      ]
      [PrimaryEditText.view (push <<< PrimaryEditTextActionController) (alternatePrimaryEditTextConfig state)]
  ]

------------------------------ ENTER OTP MODAL -------------------------------------------------------

enterOtpModal :: forall w . (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
enterOtpModal push state =
  InAppKeyboardModal.view (push <<< InAppKeyboardModalOtp) (enterOtpState state)

------------------------------------- LIVE DASHBOARD -----------------------------------------------
showLiveStatsDashboard :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> PrestoDOM (Effect Unit) w
showLiveStatsDashboard push state =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.grey800
  , afterRender
        ( \action -> do
            JB.initialWebViewSetUp push (getNewIDWithTag "webview") HideLiveDashboard
            pure unit
        )
        (const NoAction)
  ] [ webView
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , id (getNewIDWithTag "webview")
      , url if (isPreviousVersion (getValueToLocalStore VERSION_NAME) ("1.2.8")) then "https://nammayatri.in/open/" else "https://nammayatri.in/open?source=in-app"
      ]]

--------------------------------------------------- SETTINGS VIEW ------------------------------------------
settingsView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit )-> PrestoDOM (Effect Unit) w
settingsView state push =
  PrestoAnim.animationSet [Anim.fadeIn (state.props.openSettings)]$
  linearLayout[
    height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , visibility $ if state.props.openSettings then VISIBLE else GONE
  ][  GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
  , linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.grey900
    ][]
  , profileOptionsLayout state push ]



------------------------------------------------- PROFILE OPTIONS LAYOUT ------------------------------

profileOptionsLayout :: ST.DriverProfileScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
profileOptionsLayout state push =
  scrollView
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  ][ linearLayout
      [ width MATCH_PARENT
      , height MATCH_PARENT
      , orientation VERTICAL
      , padding $ PaddingVertical 10 5
      ] (mapWithIndex
          (\index optionItem ->
              linearLayout
              ([ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              , gravity CENTER_VERTICAL
              , onClick push $ const $ OptionClick optionItem.menuOptions
              ] <>  if (optionItem.menuOptions == DRIVER_BOOKING_OPTIONS) && (null state.data.downgradeOptions && not state.props.showBookingOptionForTaxi) then 
                      [ alpha 0.5
                      , clickable false] 
                    else [])
              [ linearLayout
                [ width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation HORIZONTAL
                , gravity CENTER_VERTICAL
                , padding (Padding 15 20 15 0)
                ][  imageView
                    [ width $ V 20
                    , height $ V 20
                    , imageWithFallback optionItem.icon
                    ]
                    , textView (
                    [ height WRAP_CONTENT
                    , weight 1.0
                    , text $ getTitle optionItem.menuOptions
                    , margin $ MarginLeft 10
                    , color Color.black900
                    ] <> FontStyle.subHeading2 TypoGraphy
                    )
                    , linearLayout
                    [ width WRAP_CONTENT
                    , height WRAP_CONTENT
                    , orientation HORIZONTAL
                    , gravity CENTER_VERTICAL
                    ][ textView $
                        [ width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , text $ "V " <> (getValueToLocalStore VERSION_NAME)
                        , visibility if(optionItem.menuOptions == ABOUT_APP) then VISIBLE else GONE
                        , margin (MarginRight 5)
                        ] <> FontStyle.paragraphText TypoGraphy
                      , imageView
                        [ width $ V 18
                        , height $ V 18
                        , imageWithFallback $ "ny_ic_chevron_right_grey," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_chevron_right_grey.png"
                        ]
                    ]
                ]
                , if (index == length (optionList "lazyEvaluation") - 2) then (horizontalLineView 7 0.5 0 20 0) else if(optionItem.menuOptions == DRIVER_LOGOUT) then dummyTextView else horizontalLineView 1 1.0 15 15 15
              ]
            ) (optionList "")
      )
  ]


----------------------------------------------- UPDATE LANGUAGE VIEW ------------------------------------------------------------------
updateLanguageView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit )-> PrestoDOM (Effect Unit) w
updateLanguageView state push =
  linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , background Color.white900
  , margin (MarginBottom 10)
  ][ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
  , linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.grey900
    ][]
   , languagesSpokenView state push
   , primaryButtons state push
  ]

---------------------------------------------- LANGUAGES SPOKEN VIEW -----------------------------------------------------------------
languagesSpokenView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit )-> PrestoDOM (Effect Unit) w
languagesSpokenView state push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][
      textView
        [ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text (getString SELECT_THE_LANGUAGES_YOU_CAN_SPEAK)
        , margin (Margin 20 20 20 5)
        , textSize FontSize.a_16
        , color Color.black900
        ]
    , CheckListView.view (push <<< LanguageSelection) (checkListConfig state)
  ]

-------------------------------------------------- PRIMARY BUTTONS -----------------------------------------------------------------

primaryButtons :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit )-> PrestoDOM (Effect Unit) w
primaryButtons state push =
  linearLayout
  [ orientation HORIZONTAL
  , height MATCH_PARENT
  , weight 1.0
  , gravity BOTTOM
  ] [ PrimaryButton.view (push <<< UpdateButtonClicked) (primaryButtonConfig state)]



-------------------------------------------_ COMPONENTS _-------------------------------------------------------------

---------------------------------------- INFOVIEW COMPONENT ------------------------------------------------
infoView :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
infoView state push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , margin $ MarginHorizontal 16 16
  ][ detailsListViewComponent state push {
                              backgroundColor : Color.white900
                            , separatorColor : Color.grey700
                            , arrayList :  if state.props.screenType == ST.DRIVER_DETAILS then (driverDetailsArray state)
                                           else (getRcDetails state)
                            }
    ]
getRcDetails :: ST.DriverProfileScreenState -> Array {key :: String, value :: Maybe String, action :: Action, isEditable :: Boolean}
getRcDetails state = do
  let config = state.data.activeRCData
  ([{ key : (getString RC_STATUS) , value : Just $ if config.rcStatus then (getString ACTIVE_STR) else (getString INACTIVE_RC), action : NoAction , isEditable : false }
  , { key : (getString REG_NUMBER) , value : Just config.rcDetails.certificateNumber , action : NoAction , isEditable : false }]
  <> (if config.rcStatus then
      [{ key : (getString TYPE) , value : Just (getVehicleType state.data.driverVehicleType) , action : NoAction , isEditable : false }]
     else [])
  <>[{ key :(getString MODEL_NAME) , value : if config.rcStatus then config.rcDetails.vehicleModel else Just "NA", action :  NoAction , isEditable : false}
  , { key : (getString COLOUR) , value : if config.rcStatus then config.rcDetails.vehicleColor else Just "NA", action :NoAction , isEditable : false } ]
  )

bottomPill :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
bottomPill state push = 
  linearLayout[
    width MATCH_PARENT,
    height WRAP_CONTENT,
    padding $ PaddingVertical 20 20
  ][
    linearLayout [
      width $ V $ ((screenWidth unit)-32)/2 ,
      height WRAP_CONTENT,
      gravity CENTER ,
      onClick push $ const $ DirectActivateRc $ if state.data.activeRCData.rcStatus then ST.DEACTIVATING_RC else ST.ACTIVATING_RC
    ][
      textView $ [
        color Color.blue900,
        text $ if state.data.activeRCData.rcStatus then (getString DEACTIVATE_RC) else (getString ACTIVATE_RC)
      ] <> FontStyle.body6 TypoGraphy
    ]
  , linearLayout[
      width $ V 1, 
      height MATCH_PARENT ,
      background Color.grey700
    ][]
  , linearLayout [
      width $ V (((screenWidth unit)-32)/2) ,
      height WRAP_CONTENT,
      gravity CENTER,
      onClick push $ const $ UpdateRC  state.data.activeRCData.rcDetails.certificateNumber  state.data.activeRCData.rcStatus
    ][
      textView $ [
        color Color.blue900,
        text $ getString EDIT_RC
      ] <> FontStyle.body6 TypoGraphy
    ]
  ]
----------------------------------------------- INFO TILE VIEW COMPONENT -------------------------------------------------------
infoTileView :: forall w. ST.DriverProfileScreenState -> {primaryText :: String, subText :: String, postImgVisibility :: Boolean, seperatorView :: Boolean, margin :: Margin } -> PrestoDOM (Effect Unit) w
infoTileView state config =
  (addAnimation state) $ linearLayout
    [ weight 1.0
    , height WRAP_CONTENT
    , orientation VERTICAL
    , margin $ config.margin
    , background Color.blue600
    , padding $ Padding 16 16 16 16
    , cornerRadius 10.0
    ][ linearLayout
          [ width MATCH_PARENT
          , height WRAP_CONTENT
          , orientation HORIZONTAL
          , gravity CENTER_VERTICAL
          ][  textView
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
                ](mapWithIndex (\index item ->
                    linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , margin (MarginRight 2)
                    ][imageView
                        [ height $ V 13
                        , width $ V 13
                        , imageWithFallback if item <= (fromMaybe 0.0 state.data.analyticsData.rating) then "ny_ic_star_active,https://assets.juspay.in/nammayatri/images/common/ny_ic_star_active.png" else "ny_ic_star_inactive,https://assets.juspay.in/nammayatri/images/common/ny_ic_star_inactive.png"
                        ]
                    ]) [1.0,2.0,3.0,4.0,5.0])
          ]
      , textView
        ([ width WRAP_CONTENT
        , height WRAP_CONTENT
        , text config.subText
        , margin (MarginLeft 7)
        , textSize FontSize.a_14
        , color Color.black700
        ] <> FontStyle.body3 TypoGraphy)
    ]

-------------------------------------------- MENU BUTTON VIEW COMPONENT ---------------------------------------------

showMenuButtonView :: ST.DriverProfileScreenState -> forall w. (Action -> Effect Unit) -> String -> ST.Gender -> PrestoDOM (Effect Unit) w
showMenuButtonView state push genderName genderType=
  linearLayout
  [ width $ MATCH_PARENT
  , height $ V 56
  , gravity CENTER
  , margin $ (Margin 0 10 0 10)
  , background if checkGenderSelect state.data.genderTypeSelect genderType then Color.blue600 else Color.white900
  , stroke if checkGenderSelect state.data.genderTypeSelect genderType then ("1," <> Color.blue900) else ("1," <> Color.grey700)
  , cornerRadius 6.0
  , onClick push (const $ CheckBoxClick genderType)
  ][ linearLayout
      [ height $ V 20
      , width $ V 20
      , stroke if checkGenderSelect state.data.genderTypeSelect genderType then ("2," <> Color.black800) else ("2," <> Color.black600)
      , cornerRadius 10.0
      , gravity CENTER
      , margin $ MarginLeft 10
      ][  imageView
          [ width $ V 10
          , height $ V 10
          , imageWithFallback "ny_ic_radio_button,https://assets.juspay.in/nammayatri/images/common/ny_ic_radio_button.png"
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
detailsListViewComponent :: forall w.
  ST.DriverProfileScreenState ->
  (Action -> Effect Unit) ->
    {backgroundColor :: String
    , separatorColor :: String
    , arrayList :: Array (
                    { key :: String
                    , value :: Maybe String
                    , action :: Action
                    , isEditable :: Boolean }) } ->
  PrestoDOM (Effect Unit) w
detailsListViewComponent state push config =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background config.backgroundColor
  , margin $ Margin 0 0 0 0
  , orientation VERTICAL
  , cornerRadius 10.0
  ]$(mapWithIndex(\ index item ->
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
         , cornerRadius 10.0
        , padding $ PaddingHorizontal 16 16
        , background config.backgroundColor
        ][ (addAnimation state) $ linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            , gravity CENTER_VERTICAL
            , padding $ PaddingVertical 16 16
            ]([  textView
                [ text item.key
                , textSize FontSize.a_12
                , color Color.black700
                , fontStyle $ FontStyle.regular LanguageStyle
                ]
              , linearLayout
                [ height WRAP_CONTENT
                , weight 1.0
                ][]
              , textView
                [ text $ fromMaybe (getString ADD) item.value
                , textSize FontSize.a_14
                , onClick push $ const item.action
                , color case item.value of
                          Nothing -> Color.blue900
                          Just val -> do
                                      let isRcActive = val == (getString ACTIVE_STR)
                                      let isRcInActive = val == (getString INACTIVE_RC)
                                      let isRCEdit = val == (getString EDIT_RC)
                                      if isRcActive then Color.green900 else if isRcInActive then Color.red else if isRCEdit then Color.blue900 else Color.black900
                , fontStyle $ FontStyle.semiBold LanguageStyle
                ]
            ] <> if item.isEditable && (isJust item.value) then [imageView
                [ height $ V 11
                , width $ V 11
                , margin $ MarginLeft 7
                , onClick push $ const item.action
                , imageWithFallback "ic_edit_pencil,https://assets.juspay.in/nammayatri/images/driver/ic_edit_pencil.png"
                ]]  else [])
          , linearLayout
            [ height $ V 1
            , width MATCH_PARENT
            , background config.separatorColor
            , visibility if index == length (config.arrayList) - 1 && state.props.screenType == ST.VEHICLE_DETAILS then VISIBLE else GONE
            ][]
            ]
        ) (config.arrayList)) <> (if state.props.screenType == ST.VEHICLE_DETAILS then [(bottomPill state push)] else [])

----------------------------------------------- INFO CARD COMPONENT ---------------------------------------------------------------
infoCard :: forall w. ST.DriverProfileScreenState -> (Action -> Effect Unit) -> {key :: String, value :: String , value1 :: String , infoImageUrl :: String, postfixImage :: String, showInfoImage :: Boolean , showPostfixImage :: Boolean , valueColor :: String, action :: Action } -> PrestoDOM (Effect Unit) w
infoCard state push config =
  linearLayout[
    height WRAP_CONTENT
  , width MATCH_PARENT
  , padding $ Padding 16 16 16 16
  , margin $ MarginTop 8
  , cornerRadius 10.0
  , background Color.blue600
  ][
  (addAnimation state) $ linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  ][  textView
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
      ][  textView
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
addAnimation state = PrestoAnim.animationSet [ Anim.fadeOut (state.props.screenType == ST.VEHICLE_DETAILS), Anim.fadeOut (state.props.screenType == ST.DRIVER_DETAILS), Anim.fadeIn (state.props.screenType == ST.VEHICLE_DETAILS), Anim.fadeOut (state.props.screenType == ST.DRIVER_DETAILS), Anim.fadeIn (state.props.screenType == ST.DRIVER_DETAILS)]

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

driverDetailsArray :: ST.DriverProfileScreenState -> Array {key :: String , value :: Maybe String , action :: Action, isEditable :: Boolean}
driverDetailsArray state = [
    { key : (getString NAME) , value : Just state.data.driverName , action : NoAction , isEditable : false }
  , { key : (getString MOBILE_NUMBER), value : state.data.driverMobile , action : NoAction , isEditable : false }
  , { key : (getString ALTERNATE_NUMBER) , value : state.data.driverAlternateNumber , action : UpdateAlternateNumber , isEditable : true}
  , { key : (getString GENDER) , value : (getGenderName state.data.driverGender) , action : SelectGender , isEditable : true } ]


vehicleDetailsArray :: ST.DriverProfileScreenState -> Array {key :: String , value :: Maybe String , action :: Action, isEditable :: Boolean}
vehicleDetailsArray state = [
    { key : (getString REG_NUMBER ) , value : Just state.data.vehicleRegNumber , action : NoAction , isEditable : false }
  , { key : (getString TYPE), value : Just (getVehicleType state.data.driverVehicleType), action : NoAction , isEditable : false }
  , { key : (getString MODEL_NAME) , value : Just state.data.vehicleModelName , action :  NoAction , isEditable : false}
  , { key : (getString COLOUR) , value : Just state.data.vehicleColor , action :NoAction , isEditable : false } ]

genderOptionsArray :: ST.DriverProfileScreenState ->  Array {text :: String , value :: ST.Gender}
genderOptionsArray _ =
  [ {text : (getString FEMALE) , value : ST.FEMALE}
  , {text : (getString MALE) , value : ST.MALE}
  , {text : (getString OTHER) , value : ST.OTHER}
  , {text : (getString PREFER_NOT_TO_SAY) , value : ST.PREFER_NOT_TO_SAY}
  ]

vehicleSummaryArray :: ST.DriverProfileScreenState -> Array {key :: String, value :: String, value1 :: String, infoImageUrl :: String, postfixImage :: String, showInfoImage :: Boolean , showPostfixImage :: Boolean , action :: Action, valueColor :: String}
vehicleSummaryArray state = [{key : (getString TRAVELLED_ON_APP), value : (state.data.analyticsData.totalDistanceTravelled) , value1 : "" , infoImageUrl : "ny_ic_info_blue,https://assets.juspay.in/nammayatri/images/common/ny_ic_info_blue.png", postfixImage : "ny_ic_api_failure_popup,https://assets.juspay.in/nammayatri/images/driver/ny_ic_api_failure_popup.png", showPostfixImage : false, showInfoImage : false, valueColor : Color.charcoalGrey, action : NoAction}]

vehicleAboutMeArray :: ST.DriverProfileScreenState -> Array {key :: String, value :: Maybe String, action :: Action , isEditable :: Boolean}
vehicleAboutMeArray state =  [{ key : (getString YEARS_OLD) , value : Nothing , action : UpdateValue ST.VEHICLE_AGE , isEditable : true }
  , { key : (getString NAME) , value : Nothing , action : UpdateValue ST.VEHICLE_NAME , isEditable : true }]

driverAboutMeArray :: ST.DriverProfileScreenState -> Array {key :: String, value :: Maybe String, action :: Action , isEditable :: Boolean}
driverAboutMeArray state =  [{ key : (getString LANGUAGES_SPOKEN) , value : ((getLanguagesSpoken ( map(\item -> (getLangFromVal item)) (state.data.languagesSpoken)) )) , action : UpdateValue ST.LANGUAGE , isEditable : true }
  -- , { key : (getString HOMETOWN) , value : Nothing , action : UpdateValue ST.HOME_TOWN , isEditable : true }
  ]


getLanguagesSpoken :: Array String -> Maybe String
getLanguagesSpoken languages =
  if (length languages >2) then Just $ (intercalate ", " (take 2 languages)) <> " +" <> show (length languages - 2)
    else if (length languages > 0) then Just (intercalate ", " languages)
    else Nothing

getLangFromVal :: String -> String
getLangFromVal value =
  case value of
      "EN_US" -> "English"
      "HI_IN" -> "Hindi"
      "TA_IN" -> "Tamil"
      "KN_IN" -> "Kannada"
      "TE_IN" -> "Telugu"
      "BN_IN" -> "Bengali"
      "ML_IN" -> "Malayalam"
      _ -> value
--------------------------------------------------------------- SEPARATOR --------------------------------------------------------
horizontalLineView :: Int -> Number -> Int -> Int -> Int -> forall w . PrestoDOM (Effect Unit) w
horizontalLineView heightOfLine lineAlpha marginLeft marginTop marginRight =
 linearLayout
  [ width MATCH_PARENT
  , height $ V heightOfLine
  , background Color.grey800
  , margin (Margin marginLeft marginTop marginRight 0)
  , alpha lineAlpha
  ][]

-------------------------------------------- DUMMY TEXT VIEW ---------------------------------------------

dummyTextView :: forall w . PrestoDOM (Effect Unit) w
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
    ([ linearLayout
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
            $
              [ text $ (getString EDIT_RC)<>" - " <> state.data.rcNumber
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
                      , if(item.type /= ST.DELETING_RC) then linearLayout
                          [ height $ V 1
                          , width MATCH_PARENT
                          , background Color.grey900
                          ]
                          []
                        else linearLayout[][]
                      ]
                )
                (rcEditPopUpData state)
            )

        ]
    ]<> if state.props.activateOrDeactivateRcView then [activateAndDeactivateRcConfirmationPopUpView push state] else []
     <> if state.props.deleteRcView then [deleteRcPopUpView push state] else [])

rcEditPopUpData :: ST.DriverProfileScreenState -> Array { text :: String, imageWithFallback :: String, type :: ST.EditRc }
rcEditPopUpData state =
  [ { text: if state.data.isRCActive then (getString DEACTIVATE_RC) else (getString ACTIVATE_RC)
    , imageWithFallback: if state.data.isRCActive then "ny_ic_deactivate,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_deactivate.png" else "ny_ic_activate,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_activate.png"
    , type: if state.data.isRCActive then ST.ACTIVATING_RC else ST.DEACTIVATING_RC
    }
  , { text: (getString DELETE_RC)
    , imageWithFallback: "ny_ic_bin,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_bin.png"
    , type: ST.DELETING_RC
    }
  ]

trackingCardRcEditView :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> { text :: String, imageWithFallback :: String, type :: ST.EditRc} -> PrestoDOM (Effect Unit) w
trackingCardRcEditView push state item =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , padding (Padding 0 18 0 18)
    , gravity CENTER_VERTICAL
    , alpha if (item.type == ST.DELETING_RC && length state.data.rcDataArray == 1) then 0.4 else 1.0
    , onClick push (const (DeactivateRc item.type))
    ]
    [ imageView
        [ imageWithFallback item.imageWithFallback
        , height $ V 25
        , width $ V 25
        , margin (MarginRight 20)
        ],
      textView
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
      ][ imageView
         [ imageWithFallback "ny_ic_chevron_right,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_chevron_right.png"
         , height $ V 30
         , width $ V 32
         , gravity RIGHT
         , padding (Padding 3 3 3 3)
         ]
      ]
    ]

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
    ([ linearLayout
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
            $
              [ text $ "RC - " <> state.data.rcNumber <> (getString ACTIVE_RC_ON_ANOTHER_DRIVER)
              , fontStyle $ FontStyle.bold LanguageStyle
              , height WRAP_CONTENT
              , color Color.black800
              , textSize FontSize.a_18
              , padding $ Padding 10 0 10 0
              , margin $ Margin 4 0 4 4
              , gravity CENTER
              ]
        , linearLayout
        [
          height WRAP_CONTENT
        , width MATCH_PARENT
        , background Color.white900
        , orientation VERTICAL
        , cornerRadii $ Corners 24.0 true true false false
        , alignParentBottom "true,-1"
        , disableClickFeedback true
        , gravity CENTER
        ][
          textView
            $
              [ text $ (getString CALL_DRIVER_OR_CONTACT_SUPPORT)
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
                      ,  linearLayout
                          [ height $ V 1
                          , width MATCH_PARENT
                          , background Color.grey900
                          ][]
                      ]
                )
                (activeRcOnAnotherDriverProfilePopUpData state)
            )
            , linearLayout
            [
              height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , gravity CENTER
            , margin $ MarginTop 24
            , onClick push $ const SkipActiveRc
            ][
              textView
              $
              [ text $ (getString CANCEL)
              , height WRAP_CONTENT
              , color Color.black700
              , textSize FontSize.a_16
              , margin $ Margin 4 0 4 4
              , gravity CENTER
              ]
            ]

        ]
    ]<> if state.props.callDriver then [callDriverPopUpView push state] else [])

activeRcOnAnotherDriverProfilePopUpData :: ST.DriverProfileScreenState -> Array { text :: String, type :: ST.CallOptions}
activeRcOnAnotherDriverProfilePopUpData state =
  [
    { text: (getString CALL_DRIVER)
    , type: ST.CALLING_DRIVER
    }
  , { text: (getString CALL_CUSTOMER_SUPPORT)
    , type: ST.CALLING_CUSTOMER_SUPPORT
    }
  ]

trackingCardRcActiveOnAnotherDriverProfileView :: forall w. (Action -> Effect Unit) -> ST.DriverProfileScreenState -> { text :: String, type :: ST.CallOptions} -> PrestoDOM (Effect Unit) w
trackingCardRcActiveOnAnotherDriverProfileView push state item =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation HORIZONTAL
    , padding (Padding 3 16 3 16)
    , gravity CENTER_VERTICAL
    , onClick push (const $ if item.type == ST.CALLING_DRIVER then CallDriver else CallCustomerSupport)
    ]
    [
        textView
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , textSize FontSize.a_16
        , text item.text
        , color $ Color.black900
        , fontStyle $ FontStyle.medium LanguageStyle
        ]
        ,linearLayout[
          width MATCH_PARENT,
          gravity RIGHT
        ][
          imageView
          [ imageWithFallback "ny_ic_chevron_right,https://assets.juspay.in/beckn/nammayatri/driver/images/ny_ic_chevron_right.png"
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
