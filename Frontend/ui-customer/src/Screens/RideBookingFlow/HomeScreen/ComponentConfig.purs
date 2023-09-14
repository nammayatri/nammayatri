{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideBookingFlow.HomeScreen.Config where

import Common.Types.App
import Language.Strings
import Prelude
import PrestoDOM
import Animation.Config as AnimConfig
import Animation.Config as AnimConfig
import Animation.Config as AnimConfig
import Common.Types.App (LazyCheck(..))
import Components.Banner as Banner
import Components.ChatView as ChatView
import Components.ChatView as ChatView
import Components.ChooseYourRide as ChooseYourRide
import Components.DriverInfoCard (DriverInfoCardData)
import Components.DriverInfoCard as DriverInfoCard
import Components.EmergencyHelp as EmergencyHelp
import Components.ErrorModal as ErrorModal
import Components.MenuButton as MenuButton
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.QuoteListModel as QuoteListModel
import Components.RateCard as RateCard
import Components.RatingCard as RatingCard
import Components.RequestInfoCard as RequestInfoCard
import Components.RideCompletedCard as RideCompletedCard
import Components.SearchLocationModel as SearchLocationModel
import Components.SearchLocationModel as SearchLocationModel
import Components.SelectListModal as CancelRidePopUpConfig
import Components.SourceToDestination as SourceToDestination
import Control.Monad.Except (runExcept)
import Data.Array ((!!), sortBy)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Int as INT
import Data.Int as INT
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DS
import Data.String as DS
import Data.String as DS
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Suggestions (getSuggestionsfromKey)
import Font.Size as FontSize
import Font.Style as FontStyle
import Foreign.Class (class Encode)
import Foreign.Generic (decodeJSON, encodeJSON)
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink, parseFloat)
import Helpers.Utils as HU
import JBridge as JB
import Language.Types (STR(..))
import MerchantConfig.Utils as MU
import PrestoDOM (Accessiblity(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Resources.Constants (getKmMeter)
import Resources.Constants (getKmMeter)
import Screens.Types (DriverInfoCard, Stage(..), ZoneType(..), TipViewData, TipViewStage(..), TipViewProps)
import Screens.Types as ST
import Storage (KeyStore(..), getValueToLocalStore, isLocalStageOn, setValueToLocalStore)
import Styles.Colors as Color
import Common.Types.App (LazyCheck(..))
import Engineering.Helpers.Suggestions (getSuggestionsfromKey)

shareAppConfig :: ST.HomeScreenState -> PopUpModal.Config
shareAppConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
      gravity = CENTER,
      margin = MarginHorizontal 24 24,
      buttonLayoutMargin = Margin 16 0 16 20,
      primaryText {
        text = getString YOUR_RIDE_HAS_STARTED
      , margin = MarginHorizontal 16 16},
      secondaryText {
        text = getString(ENJOY_RIDING_WITH_US)
      , margin = MarginVertical 12 24
      , color = Color.black700},
      option1 {
        text = getString(MAYBE_LATER)
      , width = V $ (((EHC.screenWidth unit)-92)/2)
      , background = Color.white900
      , strokeColor = Color.black500
      , color = Color.black700
      },
      option2 {
        text = getString(SHARE_APP)
      , width = V $ (((EHC.screenWidth unit)-92)/2)
      , color = state.data.config.primaryTextColor
      , strokeColor = state.data.config.primaryBackground
      , background = state.data.config.primaryBackground
      , margin = MarginLeft 12
      },
      cornerRadius = Corners 15.0 true true true true,
      coverImageConfig {
        imageUrl = "ic_share_app," <> (getAssetStoreLink FunctionCall) <> "ic_share_app.png"
      , visibility = VISIBLE
      , margin = Margin 16 20 16 24
      , width = MATCH_PARENT
      , height = V 200
      }
  }
  in popUpConfig'

cancelAppConfig :: ST.HomeScreenState -> PopUpModal.Config
cancelAppConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
      gravity = BOTTOM,
      dismissPopup =true,
      optionButtonOrientation = "VERTICAL",
      buttonLayoutMargin = Margin 16 0 16 20,
      primaryText {
        text = distanceString <> getString PLEASE_CONTACT_THE_DRIVER_BEFORE_CANCELLING
      , margin = Margin 16 20 16 20},
      secondaryText { visibility = GONE },
      option1 {
        text = getString CALL_DRIVER
      , color = Color.yellow900
      , background = Color.black900
      , strokeColor = Color.transparent
      , textStyle = FontStyle.SubHeading1
      , width = MATCH_PARENT
      },
      option2 {
        text = getString CANCEL_RIDE
      , textStyle = FontStyle.SubHeading1
      , color = Color.black700
      , background = Color.white900
      , strokeColor = Color.transparent
      , width = MATCH_PARENT
      , margin = Margin 0 0 0 0
      },
      cornerRadius = Corners 15.0 true true false false,
      coverImageConfig {
        imageUrl = if state.data.driverInfoCardState.distance <= 500
                    then if state.data.driverInfoCardState.vehicleVariant == "AUTO_RICKSHAW"  then "ny_ic_driver_near_auto," <> (getAssetStoreLink FunctionCall) <> "ny_ic_driver_near_auto.png" else "ny_ic_driver_near," <> (getAssetStoreLink FunctionCall) <> "ny_ic_driver_near.png"
                    else if state.data.driverInfoCardState.vehicleVariant == "AUTO_RICKSHAW" then  "ny_ic_driver_started_auto,"  <> (getAssetStoreLink FunctionCall) <> "ny_ic_driver_started_auto.png" else "ny_ic_driver_started," <> (getAssetStoreLink FunctionCall) <> "ny_ic_driver_started.png"
      , visibility = VISIBLE
      , margin = Margin 16 20 16 24
      , width = MATCH_PARENT
      , height = V 200
      }
  }
  in popUpConfig'
      where distanceString = getDistanceString state.data.driverInfoCardState.distance (fromMaybe 0 state.data.driverInfoCardState.initDistance) state.props.zoneType.priorityTag


getDistanceString :: Int -> Int -> ZoneType -> String
getDistanceString currDistance initDistance zoneType
  | currDistance <= 15 =  getString DRIVER_IS_NEAR_YOUR_LOCATION
  | currDistance <= 500 = (if zoneType == METRO then
                              getString DRIVER_PREFERRED_YOUR_SPECIAL_REQUEST_AND_IS_JUST
                            else
                              getString YOUR_DRIVER_IS_JUST
                            ) <> show currDistance <> getString M_AWAY
  | otherwise = if zoneType == METRO then
                  getString THE_DRIVER_PREFERRED_YOUR_SPECIAL_REQUEST_AND_IS_ALREADY_ON_THE_WAY_TO_YOUR_LOCATION
                else
                  getString DRIVER_IS_ALREADY_ON_THE_WAY_TO_YOUR_LOCATION

skipButtonConfig :: ST.HomeScreenState -> PrimaryButton.Config
skipButtonConfig state =
  let
    issueFaced =  state.data.ratingViewState.issueFacedView
    config = PrimaryButton.config
    primaryButtonConfig' =
      config
        { textConfig
          { text = getString DONE
          , accessibilityHint = "Done : Button"
          , color = state.data.config.primaryTextColor
          }
        , background = state.data.config.primaryBackground
        , margin = MarginTop 22
        , id = "SkipButton"
        , enableLoader = (JB.getBtnLoader "SkipButton")
        , visibility = if not issueFaced || state.data.ratingViewState.doneButtonVisibility then VISIBLE else GONE
        , isClickable = issueFaced || state.data.ratingViewState.selectedRating > 0
        , alpha = if issueFaced || (state.data.ratingViewState.selectedRating >= 1) then 1.0 else 0.4
        }
  in
    primaryButtonConfig'

maybeLaterButtonConfig :: ST.HomeScreenState -> PrimaryButton.Config
maybeLaterButtonConfig state =
  let
    issueFaced =  state.data.ratingViewState.issueFacedView
    config = PrimaryButton.config
    primaryButtonConfig' =
      config
        { textConfig
          { text = ""
          , textFromHtml =  Just ("<u>" <> (getString MAYBE_LATER) <> "<u>")
          , accessibilityHint = "Maybe Later : Button"
          , color = Color.black650
          }
        , background = Color.white900
        , id = "MaybeLaterButton"
        , margin = (Margin 0 0 0 0)
        }
  in
    primaryButtonConfig'

updateProfileConfig :: ST.HomeScreenState -> PrimaryButton.Config
updateProfileConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' =
      config
        { textConfig
          { text = getString UPDATE_PROFILE
          , accessibilityHint = "Update Profile : Button"
          , color = state.data.config.primaryTextColor
          }
        , background = Color.black900
        , margin = MarginTop 8
        , id = "UpdateProfile"
        }
  in
    primaryButtonConfig'



whereToButtonConfig :: ST.HomeScreenState -> PrimaryButton.Config
whereToButtonConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' = config
      { textConfig
        { text = (getString WHERE_TO)
        , width = MATCH_PARENT
        , gravity = LEFT
        , color = state.data.config.primaryTextColor
        , accessibilityHint = "Where To : Button"
        }
      , height = V 60
      , gravity = CENTER_VERTICAL
      , cornerRadius = 8.0
      , margin = (MarginHorizontal 16 16)
      , isClickable = true
      , isPrefixImage = true
      , background = state.data.config.primaryBackground
      , prefixImageConfig
        { imageUrl = "ny_ic_bent_right_arrow," <> (getAssetStoreLink FunctionCall) <> "ny_ic_bent_right_arrow.png"
        , height = V 16
        , width = V 21
        , margin = (Margin 17 0 17 0)
        }
      , id = "WheretoButton"
      }
  in primaryButtonConfig'

primaryButtonRequestRideConfig :: ST.HomeScreenState -> PrimaryButton.Config
primaryButtonRequestRideConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' =
      config
        { textConfig
          { text = (getString REQUEST_RIDE)
          , color = state.data.config.primaryTextColor
          , accessibilityHint = "Request Ride : Button"
          }
        , cornerRadius = state.data.config.primaryButtonCornerRadius
        , margin = (Margin 0 32 0 0)
        , id = "RequestRideButton"
        , enableLoader = (JB.getBtnLoader "RequestRideButton")
        , background = state.data.config.primaryBackground
        }
  in
    primaryButtonConfig'

primaryButtonConfirmPickupConfig :: ST.HomeScreenState -> PrimaryButton.Config
primaryButtonConfirmPickupConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' =
      config
        { textConfig
          { text = (getString CONFIRM_LOCATION)
          , color = state.data.config.primaryTextColor
          , accessibilityHint = "Confirm PickUp Location : Button"
          }
        , cornerRadius = state.data.config.primaryButtonCornerRadius
        , margin = (MarginTop 8)
        , id = "ConfirmLocationButton"
        , background = state.data.config.primaryBackground
        }
  in
    primaryButtonConfig'



cancelRidePopUpConfig :: ST.HomeScreenState -> CancelRidePopUpConfig.Config
cancelRidePopUpConfig state =
  let
    cancelRideconfig = CancelRidePopUpConfig.config
    lastIndex = (DA.length state.props.cancellationReasons) - 1
    cancelRideconfig' =
      cancelRideconfig
        { selectionOptions = state.props.cancellationReasons
        , showAllOptionsText = (getString SHOW_ALL_OPTIONS)
        , primaryButtonTextConfig
          { firstText = getString WAIT_FOR_DRIVER
          , secondText = getString CANCEL_RIDE
          }
        , activeIndex = state.props.cancelRideActiveIndex
        , activeReasonCode = Just state.props.cancelReasonCode
        , isLimitExceeded = DS.length state.props.cancelDescription >= 100
        , cornerRadius = (MU.getValueFromConfig "primaryButtonCornerRadius")
        , isSelectButtonActive =
          ( case state.props.cancelRideActiveIndex of
              Just cancelRideIndex -> true
              Nothing -> false
          )
        , headingTextConfig{
          text = getString CANCEL_RIDE <> "?"
        }
        , subHeadingTextConfig{
          text = getString PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL
        }
        , hint = getString HELP_US_WITH_YOUR_REASON
        , strings
          { mandatory = getString MANDATORY
          , limitReached = getString MAX_CHAR_LIMIT_REACHED <> " 100 " <> getString OF <> " 100"
          }
        , config = state.data.config
        }
  in
    cancelRideconfig'

genderBannerConfig :: ST.HomeScreenState -> Banner.Config
genderBannerConfig state =
  let
    config = Banner.config
    config' = config
      {
        backgroundColor = Color.lightMintGreen
      , title = (getString COMPLETE_YOUR_PROFILE_FOR_A_PERSONALISED_RIDE_EXPERIENCE)
      , titleColor = Color.elfGreen
      , actionText = (getString UPDATE_NOW)
      , actionTextColor = Color.elfGreen
      , imageUrl = "ny_ic_banner_gender_feat,https://assets.juspay.in/beckn/merchantcommon/images/ny_ic_banner_gender_feat.png"
      , isBanner = state.props.isBanner
      }
  in config'

disabilityBannerConfig :: ST.HomeScreenState -> Banner.Config
disabilityBannerConfig state =
  let
    config = Banner.config
    config' = config
      {
        backgroundColor = Color.paleLavender
      , title = (getString NOW_GET_ASSISTED_RIDES)
      , titleColor = Color.purple
      , actionText = (getString UPDATE_PROFILE)
      , actionTextColor = Color.purple
      , imageUrl = "ny_ic_accessibility_banner_img,"<> (getAssetStoreLink FunctionCall) <>"ny_ic_accessibility_banner_img.png"
      , stroke = "1,"<> Color.fadedPurple
      }
  in config'

reportIssuePopUpConfig :: ST.HomeScreenState -> CancelRidePopUpConfig.Config
reportIssuePopUpConfig state =
  let
    reportIssueConfig = CancelRidePopUpConfig.config
    reportIssueConfig' =
      reportIssueConfig
        { selectionOptions = reportIssueOptions state
        , primaryButtonTextConfig
          { firstText = getString GO_BACK_
          , secondText = getString SUBMIT
          }
        , activeIndex = state.data.ratingViewState.issueReportActiveIndex
        , activeReasonCode = state.data.ratingViewState.issueReasonCode
        , isLimitExceeded = false
        , isSelectButtonActive =
          ( case state.data.ratingViewState.issueReportActiveIndex of
              Just issueReportActiveIndex -> true
              Nothing -> false
          )
        , headingTextConfig{
          text = getString REPORT_ISSUE_
        }
        , subHeadingTextConfig{
          text = getString PLEASE_TELL_US_WHAT_WENT_WRONG
        }
        , hint = getString HELP_US_WITH_YOUR_REASON
        , strings
          { mandatory = getString MANDATORY
          , limitReached = ((getString MAX_CHAR_LIMIT_REACHED) <> " 100 " <> (getString OF) <> " 100")
          }
        }
  in
    reportIssueConfig'

logOutPopUpModelConfig :: ST.HomeScreenState -> PopUpModal.Config
logOutPopUpModelConfig state =
  case state.props.isPopUp of
    ST.Logout ->
      let
        config' = PopUpModal.config
        popUpConfig' =
          config'
            { primaryText { text = (getString LOGOUT_) }
            , secondaryText { text = (getString ARE_YOU_SURE_YOU_WANT_TO_LOGOUT) }
            , option1 {
                background = state.data.config.popupBackground
              , strokeColor = state.data.config.primaryBackground
              , color = state.data.config.primaryBackground
              , text = (getString GO_BACK_)
              }
            , option2 {
                color = state.data.config.primaryTextColor
              , strokeColor = state.data.config.primaryBackground
              , background = state.data.config.primaryBackground
              , text = (getString LOGOUT_)
              }
            }
      in
        popUpConfig'
    ST.TipsPopUp -> PopUpModal.config{
          optionButtonOrientation = "VERTICAL"
          , dismissIconMargin = Margin 0 0 14 13
          , dismissIconVisibility = if isLocalStageOn ST.QuoteList then GONE else VISIBLE
          , backgroundClickable = true
          , customerTipAvailable = true
          , fareEstimateText = getString FARE_ESTIMATE
          , tipSelectedText = getString TIP_SELECTED
          , fareEstimate = getValueToLocalStore FARE_ESTIMATE_DATA
          , tipSelected = if state.props.customerTip.tipActiveIndex == 0 then "-" else " ₹"<> (fromMaybe "" (["0", "10", "20", "30"] DA.!! state.props.customerTip.tipActiveIndex))
          , dismissPopup = true
          , customerTipArray = [(getString NO_TIP), "₹10 🙂", "₹20 😄", "₹30 🤩"]
          , customerTipArrayWithValues = [0,10, 20, 30]
          , primaryText {
              text = if isLocalStageOn ST.QuoteList then (getString TRY_AGAIN <> "?") else getString SEARCH_AGAIN_WITH_A_TIP
            , textStyle = FontStyle.Heading1
            },
          secondaryText {
            text = (getString BOOST_YOUR_RIDE_CHANCES_AND_HELP_DRIVERS_WITH_TIPS)
          , color = Color.black650}
          , tipLayoutMargin = (Margin 22 2 22 22)
          , buttonLayoutMargin = (MarginHorizontal 16 16)
          , activeIndex = state.props.customerTip.tipActiveIndex
          , tipButton {
                background = Color.white900
              , color = Color.black800
              , strokeColor = Color.grey900
              , padding = (Padding 16 12 16 12)
            },
          option1 {
            text = if state.props.customerTip.tipActiveIndex == 0 then getString SEARCH_AGAIN_WITHOUT_A_TIP else getString SEARCH_AGAIN_WITH  <> " + ₹"<> (fromMaybe "" (["0", "10", "20", "30"] DA.!! state.props.customerTip.tipActiveIndex)) <>" "<> getString TIP
          , width = MATCH_PARENT
          , color = state.data.config.primaryTextColor
          , strokeColor = state.data.config.primaryBackground
          , background = state.data.config.primaryBackground
          , padding = (Padding 0 10 0 10)
          },
          option2 {
            text = if (isLocalStageOn ST.QuoteList) then (getString HOME) else  (getString CANCEL_SEARCH)
          , width = MATCH_PARENT
          , background = Color.white900
          , strokeColor = Color.white900
          , margin = MarginTop 14
          , padding = PaddingBottom $ getBottomMargin
          , color = Color.black650
          , height = WRAP_CONTENT
          },
          cornerRadius = (Corners 15.0 true true false false)

      }
    _ ->
      let
        config' = PopUpModal.config
        popUpConfig' =
          config'
            { primaryText { text = if (isLocalStageOn ST.QuoteList) then ((getString TRY_AGAIN) <> "?") else ((getString CANCEL_SEARCH) <> "?")}
            , buttonLayoutMargin = (MarginHorizontal 16 16)
            , dismissPopup = true
            , optionButtonOrientation = if(isLocalStageOn ST.QuoteList || isLocalStageOn ST.FindingQuotes) then  "VERTICAL" else "HORIZONTAL"
            , secondaryText { text = if (isLocalStageOn ST.QuoteList) then (getString TRY_LOOKING_FOR_RIDES_AGAIN) else (getString CANCEL_ONGOING_SEARCH)}
            , option1 {
              text = if (isLocalStageOn ST.QuoteList) then (getString YES_TRY_AGAIN) else (getString YES_CANCEL_SEARCH)
            , width = MATCH_PARENT
            , color = state.data.config.primaryTextColor
            , strokeColor = state.data.config.primaryBackground
            , background = state.data.config.primaryBackground
            , padding = (Padding 0 10 0 10)
            }
            , option2 {
               text = if (isLocalStageOn ST.QuoteList) then (getString HOME) else (getString NO_DONT)
              , width = MATCH_PARENT
              , background = Color.white900
              , strokeColor = Color.white900
              , margin = MarginTop $ if (isLocalStageOn ST.QuoteList || isLocalStageOn ST.FindingQuotes) then 14 else 3
              , color = Color.black650
              , padding = if (isLocalStageOn ST.QuoteList || isLocalStageOn ST.FindingQuotes) then (PaddingBottom getBottomMargin) else (Padding 0 0 0 0)
             }
            }
      in
        popUpConfig'


getBottomMargin :: Int
getBottomMargin = if EHC.safeMarginBottom == 0 then 24 else (EHC.safeMarginBottom)

distanceOusideLimitsConfig :: ST.HomeScreenState -> PopUpModal.Config
distanceOusideLimitsConfig state =
  let
    config' = PopUpModal.config
    popUpConfig' =
      config'
        { backgroundClickable = false
        , primaryText
          { text = (getString DESTINATION_OUTSIDE_LIMITS)
          , margin = (Margin 16 20 16 0)
          }
        , secondaryText
          { text = (getString DROP_LOCATION_FAR_AWAY)
          , margin = (Margin 0 16 0 20)
          }
        , option1 { visibility = false }
        , option2 {
            background = state.data.config.primaryBackground
          , strokeColor = state.data.config.primaryBackground
          , color = state.data.config.primaryTextColor
          , text = (getString CHANGE_DROP_LOCATION)
          , margin = (Margin 16 0 16 EHC.safeMarginBottom)
          }
        }
  in
    popUpConfig'

shortDistanceConfig :: ST.HomeScreenState -> PopUpModal.Config
shortDistanceConfig state =
  let
    config' = PopUpModal.config
    popUpConfig' =
      config'
        { backgroundClickable = false
        , primaryText
          { text = (getString YOUR_TRIP_IS_TOO_SHORT_YOU_ARE_JUST) <> HU.toStringJSON (state.props.distance) <> (getString METERS_AWAY_FROM_YOUR_DESTINATION)
          , margin = (Margin 16 20 16 0)
          }
        , secondaryText
          { text = (getString YOU_CAN_TAKE_A_WALK_OR_CONTINUE_WITH_RIDE_BOOKING)
          , margin = (Margin 0 16 0 20)
          }
        , option1 {
            background = state.data.config.popupBackground
          , strokeColor = state.data.config.primaryBackground
          , color = state.data.config.primaryBackground
          , text = (getString GO_BACK_)
          }
        , option2 {
            color = state.data.config.primaryTextColor
          , strokeColor = state.data.config.primaryBackground
          , background = state.data.config.primaryBackground
          , text = (getString BOOK_RIDE_)
          }
        }
  in
    popUpConfig'

sourceUnserviceableConfig :: ST.HomeScreenState -> ErrorModal.Config
sourceUnserviceableConfig state =
  let
    config = ErrorModal.config
    errorModalConfig' =
      config
        { height = if (MU.getMerchant FunctionCall) == MU.NAMMAYATRI && state.props.isMockLocation then MATCH_PARENT else WRAP_CONTENT
        , background = Color.white900
        , corners = (Corners 24.0 true true false false)
        , stroke = ("1," <> Color.borderGreyColor)
        , imageConfig
          { imageUrl = "ny_ic_location_unserviceable," <> (getAssetStoreLink FunctionCall) <> "ny_ic_location_unserviceable.png"
          , height = V 99
          , width = V 133
          , margin = (Margin 0 50 0 20)
          }
        , errorConfig
          { text = if state.props.isMockLocation then "Unable to get your location!" else (getString LOCATION_UNSERVICEABLE)
          , color = Color.black800
          , margin = (MarginBottom 5)
          }
        , errorDescriptionConfig
          { text = if state.props.isMockLocation then "Turn off any Mock Location app you might be using and restart the app." else (getString CURRENTLY_WE_ARE_LIVE_IN_)
          , color = Color.black700
          , margin = (Margin 20 0 20 (40 + EHC.safeMarginBottom))
          }
        , buttonConfig
          { text = (getString CHANGE_LOCATION)
          , margin = (Margin 16 0 16 (20 + EHC.safeMarginBottom))
          , background = state.data.config.primaryBackground
          , color = state.data.config.primaryTextColor
          , visibility = GONE
          }
        }
  in
    errorModalConfig'

rateCardConfig :: ST.HomeScreenState -> RateCard.Config
rateCardConfig state =
  let
    config' = RateCard.config
    rateCardConfig' =
      config'
        { nightCharges = state.data.rateCard.nightCharges
        , nightShiftMultiplier = HU.toStringJSON (state.data.rateCard.nightShiftMultiplier)
        , currentRateCardType = state.data.rateCard.currentRateCardType
        , onFirstPage = state.data.rateCard.onFirstPage
        , showDetails = state.data.config.searchLocationConfig.showRateCardDetails
        , alertDialogPrimaryColor = state.data.config.alertDialogPrimaryColor
        , description = if state.data.rateCard.nightCharges then (getString NIGHT_TIME_CHARGES) else (getString DAY_TIME_CHARGES)
        , buttonText = Just if state.data.rateCard.currentRateCardType == DefaultRateCard then (getString GOT_IT) else (getString GO_BACK_)
        , driverAdditionsImage = if (state.data.config.autoVariantEnabled && state.data.rateCard.vehicleVariant == "AUTO_RICKSHAW") then "ny_ic_driver_addition_table2,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_driver_addition_table2.png" 
                                   else "ny_ic_driver_additions_yatri,https://assets.juspay.in/beckn/yatri/user/images/ny_ic_driver_additions_yatri.png" 
        , applicableCharges = if state.data.rateCard.nightCharges && state.data.rateCard.vehicleVariant == "AUTO_RICKSHAW" then (getString NIGHT_TIMES_OF) <> (HU.toStringJSON (state.data.rateCard.nightShiftMultiplier)) <> (getString DAYTIME_CHARGES_APPLIED_AT_NIGHT)
                                 else (getString DAY_TIMES_OF) <> (HU.toStringJSON (state.data.rateCard.nightShiftMultiplier)) <> (getString DAYTIME_CHARGES_APPLICABLE_AT_NIGHT)
        , title = case MU.getMerchant FunctionCall of
                      MU.NAMMAYATRI -> getString RATE_CARD
                      MU.YATRI -> getVehicleTitle state.data.rateCard.vehicleVariant
                      _ -> ""
        , fareList = case MU.getMerchant FunctionCall of
                      MU.NAMMAYATRI -> nyRateCardList state
                      MU.YATRI -> yatriRateCardList state.data.rateCard.vehicleVariant state
                      _ -> []

        , otherOptions  = [
          {key : "DRIVER_ADDITIONS", val : (getString DRIVER_ADDITIONS)},
          {key : "FARE_UPDATE_POLICY", val : (getString FARE_UPDATE_POLICY)}]
        , fareInfoText = (getString FARE_INFO_TEXT)
        , additionalStrings = [
          {key : "DRIVER_ADDITIONS_OPTIONAL", val : (getString DRIVER_ADDITIONS_OPTIONAL)},
          {key : "THE_DRIVER_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC", val : (getString THE_DRIVER_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC)},
          {key : "DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE", val : (if (state.data.rateCard.vehicleVariant /= "AUTO_RICKSHAW") 
                                                                    then getString DRIVER_ADDITION_LIMITS_ARE_IN_INCREMENTS
                                                                   else getString DRIVER_ADDITIONS_ARE_CALCULATED_AT_RATE)},
          {key : "DRIVER_MAY_NOT_CHARGE_THIS_ADDITIONAL_FARE", val : (getString DRIVER_MAY_NOT_CHARGE_THIS_ADDITIONAL_FARE)},
          {key : "FARE_UPDATE_POLICY", val : (getString FARE_UPDATE_POLICY)},
          {key : "YOU_MAY_SEE_AN_UPDATED_FINAL_FARE_DUE_TO_ANY_OF_THE_BELOW_REASONS", val : (getString YOU_MAY_SEE_AN_UPDATED_FINAL_FARE_DUE_TO_ANY_OF_THE_BELOW_REASONS)},
          {key : "REASON_CHANGE_IN_ROUTE", val : ("<span style=\"color:black;\">" <> (getString REASON_CHANGE_IN_ROUTE_A) <> "</span>" <> (getString REASON_CHANGE_IN_ROUTE_B))}]
          <> if state.data.rateCard.vehicleVariant == "AUTO_RICKSHAW" && (MU.getValueFromConfig "showChargeDesc") then [{key : "CHARGE_DESCRIPTION", val : (getString ERNAKULAM_LIMIT_CHARGE)}] else []
        }
  in
    rateCardConfig'



yatriRateCardList :: String -> ST.HomeScreenState -> Array FareList
yatriRateCardList vehicleVariant state = do
  let lang = getValueToLocalStore LANGUAGE_KEY
  case vehicleVariant of
    "HATCHBACK" -> [ { key : if lang == "EN_US" then (getString MIN_FARE_UPTO) <> " 5 km" else "5 km " <> (getString MIN_FARE_UPTO) , val : "₹140"}
                   , { key : "5 km - 13 km" , val : "₹18 / km"}
                   , { key : "13 km - 30 km" , val : "₹25 / km"}
                   , { key : if lang == "EN_US" then (getString MORE_THAN) <> " 30 km" else "30 " <> (getString MORE_THAN), val : "₹36 / km"}
                   , { key : (getString PICKUP_CHARGE), val : "₹" <> (show state.data.pickUpCharges) }
                   , { key : (getString DRIVER_ADDITIONS) , val : "₹0 - ₹60"}]

    "SEDAN"     -> [ { key : if lang == "EN_US" then (getString MIN_FARE_UPTO) <> " 5 km" else "5 km " <> (getString MIN_FARE_UPTO), val : "₹150"}
                   , { key : "5 km - 13 km" , val : "₹18 / km"}
                   , { key : "13 km - 30 km" , val : "₹25 / km"}
                   , { key : if lang == "EN_US" then (getString MORE_THAN) <> " 30 km" else "30 " <> (getString MORE_THAN) ,val : "₹36 / km"}
                   , { key : (getString PICKUP_CHARGE), val : "₹" <> (show state.data.pickUpCharges) }
                   , { key : (getString DRIVER_ADDITIONS) ,val : "₹0 - ₹60"}]

    "SUV"       -> [ { key : if lang == "EN_US" then (getString MIN_FARE_UPTO) <> " 5 km" else "5 km " <> (getString MIN_FARE_UPTO) , val : "₹165"}
                   , { key : "5 km - 13 km" , val : "₹20 / km"}
                   , { key : "13 km - 30 km" , val : "₹28 / km"}
                   , { key : if lang == "EN_US" then (getString MORE_THAN) <> " 30 km" else "30 " <> (getString MORE_THAN) , val :"₹40 / km"}
                   , { key : (getString PICKUP_CHARGE), val : "₹" <> (show state.data.pickUpCharges) }
                   , { key : (getString DRIVER_ADDITIONS) ,val : "₹0 - ₹60"}]

    "AUTO_RICKSHAW"  -> [ { key : if lang == "EN_US" then (getString MIN_FARE_UPTO) <> " 1.5 km" else "1.5 km " <> (getString MIN_FARE_UPTO) , val : "₹30"}
                        , { key : (getString RATE_ABOVE_MIN_FARE), val : "₹15 / km"}
                        , { key : (getString PICKUP_CHARGE), val : "₹" <> (show state.data.pickUpCharges) }
                        , { key : (getString DRIVER_ADDITIONS) , val : "10% of the base fare"}]

    _ -> []

getVehicleTitle :: String -> String
getVehicleTitle vehicle =
  (case vehicle of
    "HATCHBACK" -> (getString HATCHBACK)
    "SUV" -> (getString SUV)
    "SEDAN" -> (getString SEDAN)
    "AUTO_RICKSHAW" -> (getString AUTO_RICKSHAW)
    _ -> "") <> " - " <> (getString RATE_CARD)

nyRateCardList :: ST.HomeScreenState -> Array FareList
nyRateCardList state =
  ([{key : ((getString MIN_FARE_UPTO) <> if state.data.rateCard.nightCharges then " 🌙" else ""), val : ("₹" <> HU.toStringJSON (state.data.rateCard.baseFare))},
    {key : ((getString RATE_ABOVE_MIN_FARE) <> if state.data.rateCard.nightCharges then " 🌙" else ""), val : ("₹" <> HU.toStringJSON (state.data.rateCard.extraFare) <> "/ km")},
    {key : (getString DRIVER_PICKUP_CHARGES), val : ("₹" <> HU.toStringJSON (state.data.rateCard.pickUpCharges))}
    ]) <> (if (MU.getMerchant FunctionCall) == MU.NAMMAYATRI && (state.data.rateCard.additionalFare > 0) then
    [{key : (getString DRIVER_ADDITIONS), val : (getString PERCENTAGE_OF_NOMINAL_FARE)}] else [])

estimateChangedPopupConfig :: ST.HomeScreenState -> PopUpModal.Config
estimateChangedPopupConfig state =
  let
    config' = PopUpModal.config
    popUpConfig' =
      config'
        { primaryText { text = (getString ESTIMATES_CHANGED) }
        , secondaryText { text = (getString ESTIMATES_REVISED_TO) <> "₹" <> (show state.data.suggestedAmount) <> if state.data.rateCard.additionalFare > 0 then "-" <> "₹" <> (show $ (state.data.suggestedAmount + state.data.rateCard.additionalFare)) else "" }
        , option1 {
            background = state.data.config.primaryTextColor
          , strokeColor = state.data.config.primaryBackground
          , color = state.data.config.primaryBackground
          , text = (getString GO_HOME_)
          }
        , option2 {
            color = state.data.config.primaryTextColor
          , strokeColor = state.data.config.primaryBackground
          , background = state.data.config.primaryBackground
          , text = (getString CONTINUE)
          }
        }
  in
    popUpConfig'

driverInfoCardViewState :: ST.HomeScreenState -> DriverInfoCard.DriverInfoCardState
driverInfoCardViewState state = { props:
                                  { currentStage: state.props.currentStage
                                  , trackingEnabled: state.props.isInApp
                                  , unReadMessages : state.props.unReadMessages
                                  , showChatNotification : state.props.showChatNotification && state.data.lastMessage.sentBy /= ""
                                  , showCallPopUp: state.props.showCallPopUp
                                  , isSpecialZone: state.props.isSpecialZone
                                  , estimatedTime : state.data.rideDuration
                                  , zoneType : state.props.zoneType.priorityTag
                                  , currentSearchResultType : state.data.currentSearchResultType
                                  , isChatOpened : state.props.isChatOpened
                                  , chatcallbackInitiated : state.props.chatcallbackInitiated
                                  }
                              , data: driverInfoTransformer state
                            }

chatViewConfig :: ST.HomeScreenState -> ChatView.Config
chatViewConfig state = let
  config = ChatView.config
  chatViewConfig' = config {
    userConfig
        {
          userName = state.data.driverInfoCardState.driverName
        , appType = "Customer"
        }
      , messages = state.data.messages
      , messagesSize = state.data.messagesSize
      , sendMessageActive = state.props.sendMessageActive
      , vehicleNo = HU.makeNumber $ state.data.driverInfoCardState.registrationNumber     
      , suggestionsList = if showSuggestions state then if (metersToKm state.data.driverInfoCardState.distance state) == getString AT_PICKUP then getSuggestionsfromKey "customerInitialAP" else getSuggestionsfromKey "customerInitialBP" else state.data.suggestionsList
      , hint = (getString MESSAGE)
      , suggestionHeader = (getString START_YOUR_CHAT_USING_THESE_QUICK_CHAT_SUGGESTIONS)
      , emptyChatHeader = (getString START_YOUR_CHAT_WITH_THE_DRIVER)
      , languageKey = (getValueToLocalStore LANGUAGE_KEY)
      , mapsText = "Maps"
      , grey700 = Color.grey700
      , blue600 = Color.blue600
      , blue900 = Color.blue900
      , transparentGrey = Color.transparentGrey
      , green200 = Color.green200
      , grey900 = Color.grey900
      , grey800 = Color.grey800
      , blue800 = Color.blue800
      , white900 = Color.white900
      , black800 = Color.black800
      , black700 = Color.black700
      , canSendSuggestion = state.props.canSendSuggestion
      , showAutoGeneratedText = (getValueToLocalStore NOTIFIED_CUSTOMER == "true") && (HU.secondsToHms state.data.driverInfoCardState.eta) /= ""
      , rideConfirmedAt = state.data.driverInfoCardState.startedAt
      , autoGeneratedText = state.data.config.notifyRideConfirmationConfig.autoGeneratedText <> (HU.secondsToHms state.data.driverInfoCardState.eta)
  }
  in chatViewConfig'

showSuggestions :: ST.HomeScreenState -> Boolean
showSuggestions state = (not $ state.data.lastMessage.sentBy == "Customer") && DA.null state.data.suggestionsList && ((show $ DA.length $ JB.getChatMessages "") == state.data.messagesSize || state.data.messagesSize == "-1")

metersToKm :: Int -> ST.HomeScreenState -> String
metersToKm distance state =
  if (distance <= 10) then
    (if (state.props.currentStage == ST.RideStarted) then (getString AT_DROP) else (getString AT_PICKUP))
  else if (distance < 1000) then (HU.toStringJSON distance <> " m " <> (getString AWAY_C)) else (HU.parseFloat ((INT.toNumber distance) / 1000.0)) 2 <> " km " <> (getString AWAY_C)


driverInfoTransformer :: ST.HomeScreenState -> DriverInfoCardData
driverInfoTransformer state =
  let cardState = state.data.driverInfoCardState
  in
    { otp : cardState.otp
    , driverName : cardState.driverName
    , eta : cardState.eta
    , vehicleDetails : cardState.vehicleDetails
    , registrationNumber : cardState.registrationNumber
    , rating : cardState.rating
    , startedAt : cardState.startedAt
    , endedAt : cardState.endedAt
    , source : cardState.source
    , destination : cardState.destination
    , rideId : cardState.rideId
    , price : cardState.price
    , sourceLat : cardState.sourceLat
    , sourceLng : cardState.sourceLng
    , destinationLat : cardState.destinationLat
    , destinationLng : cardState.destinationLng
    , driverLat : cardState.driverLat
    , driverLng : cardState.driverLng
    , distance : cardState.distance
    , waitingTime : cardState.waitingTime
    , driverArrived : cardState.driverArrived
    , estimatedDistance : cardState.estimatedDistance
    , driverArrivalTime : cardState.driverArrivalTime
    , estimatedDropTime : ""
    , isSpecialZone : state.props.isSpecialZone
    , isLocationTracking : state.props.isLocationTracking
    , bookingCreatedAt : cardState.createdAt
    , bppRideId : ""
    , driverNumber : cardState.driverNumber
    , merchantExoPhone : cardState.merchantExoPhone
    , lastMessage : state.data.lastMessage
    , config : state.data.config
    , vehicleVariant : cardState.vehicleVariant
    }

emergencyHelpModelViewState :: ST.HomeScreenState -> EmergencyHelp.EmergencyHelpModelState
emergencyHelpModelViewState state = { showContactSupportPopUp: state.props.emergencyHelpModelState.showContactSupportPopUp
                                , showCallPolicePopUp: state.props.emergencyHelpModelState.showCallPolicePopUp
                                , showCallContactPopUp: state.props.emergencyHelpModelState.showCallContactPopUp
                                , emergencyContactData: state.props.emergencyHelpModelState.emergencyContactData
                                , currentlySelectedContact: state.props.emergencyHelpModelState.currentlySelectedContact
                                , showCallSuccessfulPopUp : state.props.emergencyHelpModelState.showCallSuccessfulPopUp
                                , config : state.data.config
                                }

ratingCardViewState :: ST.HomeScreenState -> RatingCard.RatingCardConfig
ratingCardViewState state = {
   data: state.data.rideRatingState {
    rating = state.data.ratingViewState.selectedRating, 
    feedbackList = state.data.rideRatingState.feedbackList
  } 
  , feedbackPillData : customerFeedbackPillData FunctionCall
  , primaryButtonConfig : PrimaryButton.config {
    textConfig{
      text = getString SUBMIT_FEEDBACK
    , color = state.data.config.primaryTextColor
    },
    background = state.data.config.primaryBackground,
    margin = MarginHorizontal 16 16,
    isClickable = if state.data.ratingViewState.selectedRating == 0 then false else true,
    alpha = if not (state.data.ratingViewState.selectedRating< 1) then 1.0 else 0.4
    , id = "RateYourDriverButton"
    , enableLoader = (JB.getBtnLoader "RateYourDriverButton")
  }
  , showProfileImg : true
  , title : getString RATE_YOUR_RIDE_WITH <> state.data.rideRatingState.driverName
  , feedbackPlaceHolder : getString HELP_US_WITH_YOUR_FEEDBACK_OPTIONAL
  , showFeedbackPill : true
  , overallFeedbackArray : [(getString TERRIBLE_EXPERIENCE), (getString POOR_EXPERIENCE),(getString NEEDS_IMPROVEMENT), (getString ALMOST_PERFECT), (getString AMAZING)]
  , accessibility : ENABLE
  , closeImgVisible : GONE
}

searchLocationModelViewState :: ST.HomeScreenState -> SearchLocationModel.SearchLocationModelState
searchLocationModelViewState state = { isSearchLocation: state.props.isSearchLocation
                                    , locationList: state.data.locationList
                                    , source: state.data.source
                                    , destination: state.data.destination
                                    , isSource: state.props.isSource
                                    , isSrcServiceable: state.props.isSrcServiceable
                                    , isDestServiceable: state.props.isDestServiceable
                                    , isRideServiceable: state.props.isRideServiceable
                                    , savedlocationList: state.data.savedLocations
                                    , appConfig : state.data.config
                                    , logField : state.data.logField
                                    , crossBtnSrcVisibility: state.props.searchLocationModelProps.crossBtnSrcVisibility
                                    , crossBtnDestVisibility: state.props.searchLocationModelProps.crossBtnDestVisibility
                                    , isAutoComplete: state.props.searchLocationModelProps.isAutoComplete
                                    , showLoader: state.props.searchLocationModelProps.showLoader
                                    , prevLocation: state.data.searchLocationModelData.prevLocation
                                    }

quoteListModelViewState :: ST.HomeScreenState -> QuoteListModel.QuoteListModelState
quoteListModelViewState state = { source: state.data.source
                            , destination: state.data.destination
                            , quoteListModel: state.data.quoteListModelState
                            , selectedQuote: state.props.selectedQuote
                            , autoSelecting: state.props.autoSelecting
                            , searchExpire: state.props.searchExpire
                            , showProgress : (DA.null state.data.quoteListModelState) && isLocalStageOn FindingQuotes
                            , tipViewProps : getTipViewProps state.props.tipViewProps
                            , findingRidesAgain : state.props.findingRidesAgain
                            , progress : state.props.findingQuotesProgress
                            , appConfig : state.data.config
                            , vehicleVariant : state.data.selectedEstimatesObject.vehicleVariant
                            }

rideRequestAnimConfig :: AnimConfig.AnimConfig
rideRequestAnimConfig =
  let
    config = AnimConfig.animConfig
    rideRequestAnimConfig' =
      config
        { duration = 300
        , fromY = 10
        }
  in
    rideRequestAnimConfig'

rideCompletedAnimConfig :: AnimConfig.AnimConfig
rideCompletedAnimConfig =
  let
    config = AnimConfig.animConfig
    rideCompletedAnimConfig' =
      config
        { duration = 400
        , fromScaleY = 2.5
        , toScaleX = 1.0
        , fromScaleX = 2.5
        , toScaleY = 1.0
        }
  in
    rideCompletedAnimConfig'

autoAnimConfig :: AnimConfig.AnimConfig
autoAnimConfig =
  let
    config = AnimConfig.animConfig
    autoAnimConfig' =
      config
        { duration = 400
        , toScaleX = 1.0
        , toScaleY = 1.0
        }
  in
    autoAnimConfig'

callSupportConfig :: ST.HomeScreenState ->  PopUpModal.Config
callSupportConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
    gravity = CENTER
  , cornerRadius = (Corners 15.0 true true true true)
  , margin = (MarginHorizontal 16 16)
  , primaryText {
      text = getString CONTACT_SUPPORT <>"?"
    }
  , secondaryText {
      text = getString YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT
    , margin = (Margin 24 12 24 12)
    , color = Color.black700
    }
  , option1 {
      text =  getString CANCEL_
    , background = state.data.config.popupBackground
    , strokeColor = state.data.config.primaryBackground
    , color = state.data.config.primaryBackground
    }
  , option2 {
      text =  getString CALL_SUPPORT
    , color = state.data.config.primaryTextColor
    , strokeColor = state.data.config.primaryBackground
    , background = state.data.config.primaryBackground
    , margin = (MarginLeft 12)
    }
  }
  in popUpConfig'


zoneTimerExpiredConfig :: ST.HomeScreenState ->  PopUpModal.Config
zoneTimerExpiredConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
    gravity = CENTER
  , cornerRadius = Corners 16.0 true true true true
  , margin = Margin 24 32 24 0
  , primaryText {
      text = (getString OTP_EXPIRED) -- "OTP Expired"
    }
  , secondaryText {
      text = (getString OTP_EXPIRED_DESCRIPTION)--"Your ride OTP expired. Please book again to get a ride"
    , margin = Margin 16 4 16 24
    , color = Color.black700
    }
  , option1 {
      visibility = false
    }
  , option2 {
      text =  getString OK_GOT_IT
    , margin = (MarginHorizontal 16 16)
    }
  }
  in popUpConfig'

menuButtonConfig :: ST.HomeScreenState -> ST.Location -> MenuButton.Config
menuButtonConfig state item = let
    config = MenuButton.config
    menuButtonConfig' = config {
      titleConfig{
          text = item.place
        , gravity = CENTER_VERTICAL
      }
    , accessibilityHint = item.place
    , radioButtonConfig {
        height = V 16
        , width = V 16
        , cornerRadius = 8.0
        , buttonWidth = V 8
        , buttonHeight = V 8
        , buttonColor = Color.positive
        , margin = (MarginRight 15)
        , activeStroke = ("2," <> Color.positive)
      }
      , id = item.place
      , lat = item.lat
      , lng = item.lng
      , leftsidebutton = true
      , padding = (Padding 14 14 14 14)
      , cornerRadius = 6.0
      , height = WRAP_CONTENT
      , width = MATCH_PARENT
      , isSelected = item.place == state.props.defaultPickUpPoint
      , layoutStroke = ("1," <> if item.place == state.props.defaultPickUpPoint then Color.blue700' else Color.grey900)
      , layoutBg =  if item.place == state.props.defaultPickUpPoint then Color.blue600 else Color.white900
    }
    in menuButtonConfig'

chooseYourRideConfig :: ST.HomeScreenState -> ChooseYourRide.Config
chooseYourRideConfig state = ChooseYourRide.config
  {
    rideDistance = state.data.rideDistance,
    rideDuration = state.data.rideDuration,
    quoteList = state.data.specialZoneQuoteList,
    showTollExtraCharges = state.data.config.searchLocationConfig.showAdditionalChargesText,
    nearByDrivers = state.data.nearByDrivers
  }


specialLocationIcons :: ZoneType -> String
specialLocationIcons tag =
  case tag of
    METRO -> "ny_ic_metro_black"
    _     -> ""



specialLocationConfig :: String -> String -> Boolean -> PolylineAnimationConfig -> JB.MapRouteConfig
specialLocationConfig srcIcon destIcon isAnim animConfig = {
    sourceSpecialTagIcon : srcIcon
  , destSpecialTagIcon : destIcon
  , vehicleSizeTagIcon : (HU.getVehicleSize unit)
  , isAnimation : isAnim
  , polylineAnimationConfig : animConfig
}

updateRouteMarkerConfig :: JB.Locations -> String -> String -> String -> String -> JB.MapRouteConfig -> JB.UpdateRouteMarker
updateRouteMarkerConfig locations sourceName destName sourceIcon destIcon mapRouteConfig = {
    locations : locations
  , sourceName : sourceName
  , destName : destName
  , sourceIcon : sourceIcon
  , destIcon : destIcon
  , mapRouteConfig : mapRouteConfig
}

setTipViewData :: Encode TipViewData => TipViewData -> Effect Unit
setTipViewData object = void $ pure $ setValueToLocalStore TIP_VIEW_DATA (encodeJSON object)

getTipViewData :: String -> Maybe TipViewData
getTipViewData dummy =
  case runExcept (decodeJSON (getValueToLocalStore TIP_VIEW_DATA) :: _ TipViewData) of
    Right res -> Just res
    Left err -> Nothing

getTipViewProps :: TipViewProps -> TipViewProps
getTipViewProps tipViewProps =
  case tipViewProps.stage of
    DEFAULT ->  tipViewProps{ stage = DEFAULT
                            , onlyPrimaryText = false
                            , isprimaryButtonVisible = false
                            , primaryText = getString ADD_A_TIP_TO_FIND_A_RIDE_QUICKER
                            , secondaryText = getString IT_SEEMS_TO_BE_TAKING_LONGER_THAN_USUAL
                            }
    TIP_AMOUNT_SELECTED -> tipViewProps{ stage = TIP_AMOUNT_SELECTED
                                       , onlyPrimaryText = false
                                       , isprimaryButtonVisible = true
                                       , primaryText = getString ADD_A_TIP_TO_FIND_A_RIDE_QUICKER
                                       , secondaryText = getString IT_SEEMS_TO_BE_TAKING_LONGER_THAN_USUAL
                                       , primaryButtonText = getTipViewText tipViewProps (getString CONTINUE_SEARCH_WITH)
                                       }
    TIP_ADDED_TO_SEARCH -> tipViewProps{ onlyPrimaryText = true , primaryText = getTipViewText tipViewProps (getString CONTINUING_SEARCH_WITH) }
    RETRY_SEARCH_WITH_TIP -> tipViewProps{ onlyPrimaryText = true , primaryText = getTipViewText tipViewProps (getString SEARCHING_WITH) }



getTipViewText :: TipViewProps -> String -> String
getTipViewText tipViewProps prefixString =
  case (getValueToLocalStore LANGUAGE_KEY) of
    "EN_US" -> prefixString <> " +₹"<>show (fromMaybe 10 (tipViewProps.customerTipArrayWithValues !! tipViewProps.activeIndex))<>" "<>(getString TIP)
    _ -> " +₹"<>show (fromMaybe 10 (tipViewProps.customerTipArrayWithValues !! tipViewProps.activeIndex))<>" "<>(getString TIP) <> " " <> prefixString

requestInfoCardConfig :: LazyCheck -> RequestInfoCard.Config
requestInfoCardConfig _ = let
  config = RequestInfoCard.config
  requestInfoCardConfig' = config{
    title {
      text = getString CHOOSE_BETWEEN_MULTIPLE_RIDES
    }
  , primaryText {
      text = getString ENABLE_THIS_FEATURE_TO_CHOOSE_YOUR_RIDE
    }
  , imageConfig {
      imageUrl = "ny_ic_select_offer," <> getAssetStoreLink FunctionCall <> "ny_ic_select_offer.png",
      height = V 122,
      width = V 116
    }
  , buttonConfig {
      text = getString GOT_IT
    }
  }
  in requestInfoCardConfig'

reportIssueOptions :: ST.HomeScreenState -> Array OptionButtonList -- need to modify
reportIssueOptions state =
  [ { reasonCode: "DRIVER_WAS_NOT_READY_TO_GO"
    , description: getString DRIVER_WAS_NOT_READY_TO_GO
    , textBoxRequired : false
    , subtext : Nothing
    }
  , { reasonCode: "ASKING_FOR_MORE_MONEY"
    , description: getString ASKING_FOR_MORE_MONEY
    , textBoxRequired : false
    , subtext : Nothing
    }
  , { reasonCode: "AUTO_BROKEN"
    , description: getString AUTO_BROKEN
    , textBoxRequired : false
    , subtext : Nothing
    }
  , { reasonCode: "OTHER"
    , description: getString OTHER
    , textBoxRequired : false
    , subtext : Nothing
    }
  ]


rideCompletedCardConfig :: ST.HomeScreenState -> RideCompletedCard.Config 
rideCompletedCardConfig state = let
  config  = RideCompletedCard.config 
  config' = config{
        isDriver = false,
        customerIssueCard{
          reportIssueView = state.data.ratingViewState.openReportIssue,
          issueFaced = state.data.ratingViewState.issueFacedView,
          selectedYesNoButton = state.data.ratingViewState.selectedYesNoButton,
          reportIssuePopUpConfig = reportIssuePopUpConfig state,
          title = (getString DID_YOU_FACE_ANY_ISSUE),
          subTitle = (getString WE_NOTICED_YOUR_RIDE_ENDED_AWAY),
          option1Text = getString REPORT_ISSUE_,
          option2Text = getString GET_CALLBACK_FROM_US,
          yesText = getString YES,
          noText = getString NO
        },
        topCard {
          title =  getString RIDE_COMPLETED,
          finalAmount = state.data.finalAmount,
          initalAmount = state.data.driverInfoCardState.price,
          fareUpdatedVisiblity = state.data.finalAmount /= state.data.driverInfoCardState.price && state.props.estimatedDistance /= Nothing,
          gradient = [state.data.config.primaryBackground, state.data.config.primaryBackground, state.data.config.rideCompletedCardConfig.topCard.gradient, state.data.config.primaryBackground],
          infoPill {
            text = getFareUpdatedString state.data.rideRatingState.distanceDifference,
            image = "ny_ic_parallel_arrows," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_parallel_arrows.png",
            imageVis = VISIBLE,
            visible = if state.data.finalAmount == state.data.driverInfoCardState.price || state.props.estimatedDistance == Nothing then GONE else VISIBLE
          },
          bottomText =  getString RIDE_DETAILS
        },
        customerBottomCard {
          title = (getString RATE_YOUR_RIDE_WITH) <> state.data.rideRatingState.driverName,
          subTitle = (getString YOUR_FEEDBACK_HELPS_US),
          selectedRating = state.data.ratingViewState.selectedRating,
          visible = true
        },
        primaryButtonConfig = skipButtonConfig state,
        enableContactSupport = state.data.config.enableContactSupport
      }
  in config'


customerFeedbackPillData :: LazyCheck -> Array (Array (Array RatingCard.FeedbackItem)) 
customerFeedbackPillData lazyCheck = [feedbackPillDataWithRating1 Language, feedbackPillDataWithRating2 Language, feedbackPillDataWithRating3 Language, feedbackPillDataWithRating4 Language, feedbackPillDataWithRating5 Language]

feedbackPillDataWithRating1 :: LazyCheck -> Array (Array RatingCard.FeedbackItem)
feedbackPillDataWithRating1 lazycheck = [
  [{id : "6", text : getString RUDE_DRIVER},
  {id : "1", text : getString FELT_UNSAFE},
  {id : "1", text : getString TOO_MANY_CALLS}],
  [{id : "6", text : getString RECKLESS_DRIVING},
  {id : "6", text : getString DRIVER_CHARGED_MORE}],
  [{id : "1", text : getString LATE_DROP_OFF},
  {id : "1", text : getString LATE_PICK_UP}]
]

feedbackPillDataWithRating2 :: LazyCheck -> Array (Array RatingCard.FeedbackItem)
feedbackPillDataWithRating2 lazycheck = [
  [{id : "7", text : getString RUDE_DRIVER},
  {id : "2", text : getString FELT_UNSAFE},
  {id : "2", text : getString TOO_MANY_CALLS}],
  [{id : "7", text : getString RECKLESS_DRIVING},
  {id : "7", text : getString DRIVER_CHARGED_MORE}],
  [{id : "2", text : getString LATE_DROP_OFF},
  {id : "2", text : getString LATE_PICK_UP}]
]

feedbackPillDataWithRating3 :: LazyCheck -> Array (Array RatingCard.FeedbackItem)
feedbackPillDataWithRating3 lazycheck = [
  [{id : "8", text : getString UNPROFESSIONAL_DRIVER},
  {id : "8", text : getString RASH_DRIVING}],
  [{id : "8", text : getString DRIVER_CHARGED_MORE},
  {id : "11", text : getString UNCOMFORTABLE_AUTO}],
  [{id : "3", text : getString TRIP_GOT_DELAYED},
  {id : "3", text : getString FELT_UNSAFE}]
]

feedbackPillDataWithRating4 :: LazyCheck -> Array (Array RatingCard.FeedbackItem)
feedbackPillDataWithRating4 lazycheck = [
  [{id : "9", text : getString POLITE_DRIVER},
  {id : "9", text : getString EXPERT_DRIVING}],
  [{id : "9", text : getString ASKED_FOR_EXTRA_FARE},
  {id : "11", text : getString UNCOMFORTABLE_AUTO}],
  [{id : "4", text : getString TRIP_GOT_DELAYED},
  {id : "4", text : getString SAFE_RIDE}]
]

feedbackPillDataWithRating5 :: LazyCheck -> Array (Array RatingCard.FeedbackItem)
feedbackPillDataWithRating5 lazyCheck = [
  [{id : "10", text : getString POLITE_DRIVER},
  {id : "5", text : getString EXPERT_DRIVING}],
  [{id : "12", text : getString CLEAN_AUTO},
  {id : "10", text : getString ON_TIME}],
  [{id : "10", text : getString SKILLED_NAVIGATOR},
  {id : "5", text : getString SAFE_RIDE}]
]

getFareUpdatedString :: Int -> String
getFareUpdatedString diffInDist = do
  let dist = if diffInDist > 0 then (parseFloat (toNumber diffInDist / 1000.0) 2) else (parseFloat (toNumber (-diffInDist) / 1000.0) 2)
  if diffInDist > 0 then ((getString FARE_UPDATED) <> " - " <> case (getValueToLocalStore LANGUAGE_KEY) of
                                                        "HI_IN" -> "आपकी सवारी  "<> dist <> "किमी कम थी"
                                                        "KN_IN" -> "ನಿಮ್ಮ ಸವಾರಿ " <> dist <> " ಕಿಮೀ ಕಡಿಮೆಯಾಗಿದೆ"
                                                        "ML_IN" -> "താങ്കളുടെ യാത്ര " <> dist <> " Km കുറവായിരുന്നു"
                                                        _       -> "your ride was " <> dist <> " km shorter" )
    else ((getString FARE_UPDATED) <> " - " <> case (getValueToLocalStore LANGUAGE_KEY) of
                                                        "HI_IN" -> "आपकी सवारी  "<> dist <> "किमी लंबी थी"
                                                        "KN_IN" -> "ನಿಮ್ಮ ಸವಾರಿ " <> dist <> " ಕಿಮೀ ಉದ್ದವಾಗಿದೆ"
                                                        "ML_IN" -> "താങ്കളുടെ യാത്ര " <> dist <> " Km കൂടുതലായിരുന്നു"
                                                        _       -> "your ride was " <> dist <> " km longer")

getCarouselData :: ST.HomeScreenState -> Array CarouselData
getCarouselData state =
  map (\item -> 
    { imageConfig : { image : item.image , height : item.imageHeight , width : 200, bgColor : item.imageBgColor, cornerRadius : 8.0 },
      youtubeConfig : (EHC.getYoutubeData item.videoLink "PORTRAIT_VIDEO" item.videoHeight),
      contentType : if item.videoLink == "" then "IMAGE" else "VIDEO" ,
      gravity : item.gravity ,
      backgroundColor : item.carouselBgColor,
      titleConfig : {
        text : item.title,
        textSize : 16,
        textColor : Color.black800,
        gravity : "CENTER",
        margin : { top : 16 , bottom : 0 , right : 16 , left : 16 }
      }, 
      descriptionConfig : {
        text : item.description, 
        textSize : item.descTextSize,
        textColor : Color.black700,
        gravity : "LEFT",
        margin : { top : 0 , bottom : 0 , right : 16 , left : 16 }
      }
    }) [ {image : "carousel_4" , videoLink : (EHC.getVideoID state.data.config.purpleRideConfig.genericVideoUrl), videoHeight : 690, imageHeight : 160, imageBgColor : Color.grey700, title:  (getString EDUCATIONAL_POP_UP_SLIDE_1_TITLE), description : (getString EDUCATIONAL_POP_UP_SLIDE_1_SUBTITLE) , descTextSize : 14 , carouselBgColor : Color.grey700, gravity : 0},
        {image : "ny_ic_blind_pickup" , videoLink : "" , videoHeight :  0, imageHeight :  160, imageBgColor :  Color.blue600, title :   (getString EDUCATIONAL_POP_UP_SLIDE_2_TITLE) , description :  (getString EDUCATIONAL_POP_UP_SLIDE_2_SUBTITLE) , descTextSize : 12, carouselBgColor :  Color.grey700,  gravity : 0},
        {image : "ny_ic_deaf_pickup" , videoLink : "" , videoHeight :  0, imageHeight :  160, imageBgColor :  Color.blue600, title :   (getString EDUCATIONAL_POP_UP_SLIDE_3_TITLE) , description :  (getString EDUCATIONAL_POP_UP_SLIDE_3_SUBTITLE) , descTextSize : 12 ,carouselBgColor :  Color.grey700,  gravity : 0},
        {image : "ny_ic_locomotor_arrival" , videoLink : "" , videoHeight :  0, imageHeight :  160, imageBgColor :  Color.blue600, title :   (getString EDUCATIONAL_POP_UP_SLIDE_4_TITLE) , description :  (getString EDUCATIONAL_POP_UP_SLIDE_4_SUBTITLE) , descTextSize : 12, carouselBgColor :  Color.grey700, gravity : 0},
        {image : "ny_ic_disability_illustration" , videoLink : "" , videoHeight :  0, imageHeight :  160, imageBgColor :  Color.white900, title :   (getString EDUCATIONAL_POP_UP_SLIDE_5_TITLE) , description :  (getString EDUCATIONAL_POP_UP_SLIDE_5_SUBTITLE) , descTextSize : 12 ,carouselBgColor :  Color.grey700, gravity : 0}
      ]
