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
import ConfigProvider
import Data.Int
import Debug
import Helpers.TipConfig
import Language.Strings
import Locale.Utils
import MerchantConfig.Types
import Mobility.Prelude
import Prelude
import PrestoDOM
import Screens.RideBookingFlow.HomeScreen.BannerConfig

import Accessor (_contents, _description, _place_id, _toLocation, _lat, _lon, _estimatedDistance, _rideRating, _driverName, _computedPrice, _otpCode, _distance, _maxFare, _estimatedFare, _estimateId, _vehicleVariant, _estimateFareBreakup, _title, _priceWithCurrency, _totalFareRange, _maxFare, _minFare, _nightShiftRate, _nightShiftEnd, _nightShiftMultiplier, _nightShiftStart, _specialLocationTag, _createdAt)
import Accessor (_fareBreakup, _description, _rideEndTime, _amount, _serviceTierName)
import Animation.Config as AnimConfig
import Common.Types.App (LazyCheck(..), City(..))
import Common.Types.App (TicketType(..))
import Components.Banner as Banner
import Components.BannerCarousel as BannerCarousel
import Components.ChooseVehicle.Controller as ChooseVehicle
import Components.ChooseYourRide as ChooseYourRide
import Components.CommonComponentConfig as CommonComponentConfig
import Components.DateTimeSelector.Controller as DateSelectorController
import Components.DriverInfoCard (DriverInfoCardData)
import Components.DriverInfoCard as DriverInfoCard
import Components.EmergencyHelp as EmergencyHelp
import Components.ErrorModal as ErrorModal
import Components.LocationTagBarV2 as LocationTagBar
import Components.MenuButton as MenuButton
import Components.MessagingView as MessagingView
import Components.MessagingView.Controller (ChatContacts(..))
import Components.MessagingView.Controller as CMC
import Components.PopUpModal as PopUpModal
import Components.PopupWithCheckbox.Controller as PopupWithCheckboxController
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText.Controller as PrimaryEditTextController
import Components.QuoteListModel as QuoteListModel
import Components.RateCard as RateCard
import Components.RatingCard as RatingCard
import Components.Referral as ReferralComponent
import Components.RequestInfoCard as RequestInfoCard
import Components.RideCompletedCard as RideCompletedCard
import Components.SearchLocationModel as SearchLocationModel
import Components.SelectListModal as CancelRidePopUpConfig
import Components.ServiceTierCard.View as ServiceTierCard
import Components.SourceToDestination as SourceToDestination
import Components.DeliveryParcelImageAndOtp as DeliveryParcelImageAndOtp
import Control.Monad.Except (runExcept)
import Data.Array ((!!), sortBy, mapWithIndex, elem, length, any, all)
import Data.Array as DA
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Function.Uncurried (runFn3)
import Data.Int (toNumber)
import Data.Int as INT
import Data.Lens ((^.))
import Data.Lens ((^.), view)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe, isNothing)
import Data.String as DS
import Data.String.CodeUnits (stripPrefix, stripSuffix)
import Data.Tuple as DT
import DecodeUtil (getAnyFromWindow)
import Effect (Effect)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Suggestions (getSuggestionsfromKey)
import Engineering.Helpers.Suggestions (getSuggestionsfromKey, emChatSuggestion, chatSuggestion)
import Engineering.Helpers.Utils as EHU
import Font.Size as FontSize
import Font.Style (Style(..))
import Font.Style as FontStyle
import Foreign.Class (class Encode)
import Foreign.Generic (decode, encode, Foreign, decodeJSON, encodeJSON, class Decode, class Encode)
import Helpers.Utils (fetchImage, FetchImageFrom(..), parseFloat, getCityNameFromCode, isWeekend, getCityConfig, convertTo12HourFormat)
import Helpers.Utils as HU
import JBridge as JB
import Language.Types (STR(..))
import LocalStorage.Cache (getValueFromCache)
import MerchantConfig.Utils as MU
import PrestoDOM (Accessiblity(..), Orientation(..), Visibility(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import RemoteConfig as RemoteConfig
import Resources.Constants (dummyPrice)
import Resources.Constants (getKmMeter, emergencyContactInitialChatSuggestionId)
import Resources.LocalizableV2.Strings (getEN)
import Screens.HomeScreen.ScreenData (dummyInvalidBookingPopUpConfig, dummyRideBooking)
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails)
import Screens.NammaSafetyFlow.Components.SafetyUtils as SU
import Screens.RideBookingFlow.HomeScreen.BannerConfig (getBannerConfigs, getDriverInfoCardBanners)
import Screens.Types (DriverInfoCard, Stage(..), ZoneType(..), TipViewData, TipViewStage(..), TipViewProps, ReferralStatus(..), VehicleViewType(..), NewContacts(..), SearchLocationModelType(..), Currency(..))
import Screens.Types (FareProductType(..)) as FPT
import Screens.Types as ST
import Services.API as API
import Storage (KeyStore(..), getValueToLocalStore, isLocalStageOn, setValueToLocalStore)
import Styles.Colors as Color
import Debug (spy)
import Data.Int
import Components.CommonComponentConfig as CommonComponentConfig
import RemoteConfig as RemoteConfig
import MerchantConfig.Types
import Data.Tuple as DT
import Screens.NammaSafetyFlow.Components.SafetyUtils as SU
import Components.MessagingView.Controller as CMC

shareAppConfig :: ST.HomeScreenState -> PopUpModal.Config
shareAppConfig state =
  let
    config' = PopUpModal.config

    popUpConfig' =
      config'
        { gravity = CENTER
        , margin = MarginHorizontal 24 24
        , buttonLayoutMargin = Margin 16 0 16 20
        , primaryText
          { text = getString YOUR_RIDE_HAS_STARTED
          , margin = MarginHorizontal 16 16
          }
        , secondaryText
          { text = getString (ENJOY_RIDING_WITH_US)
          , margin = MarginVertical 12 24
          , color = Color.black700
          }
        , option1
          { text = getString (MAYBE_LATER)
          , width = V $ (((EHC.screenWidth unit) - 92) / 2)
          , background = Color.white900
          , strokeColor = Color.black500
          , color = Color.black700
          }
        , option2
          { text = getString (SHARE_APP)
          , width = V $ (((EHC.screenWidth unit) - 92) / 2)
          , color = state.data.config.primaryTextColor
          , strokeColor = state.data.config.primaryBackground
          , background = state.data.config.primaryBackground
          , margin = MarginLeft 12
          }
        , cornerRadius = Corners 15.0 true true true true
        , coverImageConfig
          { imageUrl = fetchImage FF_ASSET "ic_share_app"
          , visibility = VISIBLE
          , margin = Margin 16 20 16 24
          , width = MATCH_PARENT
          , height = V 200
          }
        }
  in
    popUpConfig'

cancelAppConfig :: ST.HomeScreenState -> PopUpModal.Config
cancelAppConfig state =
  let
    config' = PopUpModal.config
    popUpConfig' =
      config'
        { gravity = BOTTOM
        , dismissPopup = true
        , optionButtonOrientation = "VERTICAL"
        , buttonLayoutMargin = Margin 16 0 16 20
        , primaryText
          { text = distanceString <> getString PLEASE_CONTACT_THE_DRIVER_BEFORE_CANCELLING
          , margin = Margin 16 20 16 20
          }
        , secondaryText { visibility = GONE }
        , option1
          { text = getString CALL_DRIVER
          , color = state.data.config.primaryTextColor
          , background = state.data.config.primaryBackground
          , strokeColor = Color.transparent
          , textStyle = FontStyle.SubHeading1
          , width = MATCH_PARENT
          , enableRipple = true
          }
        , option2
          { text = getString CANCEL_RIDE
          , textStyle = FontStyle.SubHeading1
          , color = Color.black700
          , background = Color.white900
          , strokeColor = Color.transparent
          , width = MATCH_PARENT
          , margin = Margin 0 0 0 0
          }
        , cornerRadius = Corners 15.0 true true false false
        , coverImageConfig
          { imageUrl = fetchImage FF_ASSET $ HU.getCancellationImage state.data.driverInfoCardState.vehicleVariant state.data.driverInfoCardState.distance
          , visibility = VISIBLE
          , margin = Margin 16 20 16 24
          , width = MATCH_PARENT
          , height = V 200
          }
        }
  in
    popUpConfig'
  where
  distanceString = getDistanceString state.data.driverInfoCardState.distance (fromMaybe 0 state.data.driverInfoCardState.initDistance) state.props.zoneType.priorityTag
  
confirmRequestEditConfig :: ST.HomeScreenState -> PopUpModal.Config
confirmRequestEditConfig state =
  let
    config' = PopUpModal.config

    popUpConfig' =
      config'
        { gravity = BOTTOM
        , dismissPopup = true
        , optionButtonOrientation = "VERTICAL"
        , buttonLayoutMargin = Margin 16 0 16 0
        , primaryText
          { text = getString CONFIRM_WITH_YOUR_DRIVER
          , margin = Margin 16 10 16 10
          }
        , secondaryText { text = getString YOUR_DRIVER_MIGHT_WANT_TO_GO_TOWARDS_THE_CURRENT_DROP_KINDLY_ASK_THEM_TO_CONFIRM_AFTER_REQUESTING, margin = Margin 0 10 0 10}
        , option1
          { text = getString REQUEST_EDIT
          , color = state.data.config.primaryTextColor
          , background = state.data.config.primaryBackground
          , strokeColor = Color.transparent
          , textStyle = FontStyle.SubHeading1
          , width = MATCH_PARENT
          , enableRipple = true
          }
        , option2
          { text = getString CANCEL_
          , textStyle = FontStyle.SubHeading1
          , color = Color.black700
          , background = Color.white900
          , strokeColor = Color.transparent
          , width = MATCH_PARENT
          , margin = Margin 0 0 0 0
          }
        , cornerRadius = Corners 15.0 true true false false
        , coverImageConfig
          { imageUrl =
            fetchImage FF_ASSET
              $ if state.data.driverInfoCardState.vehicleVariant == "AUTO_RICKSHAW" then "ny_ic_cnf_edit_dest_auto" else "ny_ic_cnf_edit_dest"
          , visibility = VISIBLE
          , margin = Margin 16 20 16 12
          , width = MATCH_PARENT
          , height = V 200
          }
        }
  in
    popUpConfig'

getDistanceString :: Int -> Int -> ZoneType -> String
getDistanceString currDistance initDistance zoneType
  | currDistance <= 15 = getString DRIVER_IS_NEAR_YOUR_LOCATION
  | currDistance <= 500 =
    ( if zoneType == METRO then
        getString DRIVER_PREFERRED_YOUR_SPECIAL_REQUEST_AND_IS_JUST
      else
        getString YOUR_DRIVER_IS_JUST
    )
      <> show currDistance
      <> getString M_AWAY
  | otherwise =
    if zoneType == METRO then
      getString THE_DRIVER_PREFERRED_YOUR_SPECIAL_REQUEST_AND_IS_ALREADY_ON_THE_WAY_TO_YOUR_LOCATION
    else
      getString DRIVER_IS_ALREADY_ON_THE_WAY_TO_YOUR_LOCATION

skipButtonConfig :: ST.HomeScreenState -> PrimaryButton.Config
skipButtonConfig state =
  let
    accessibilityOption = fromMaybe {issueType: Accessibility, selectedYes: Nothing} ((DA.filter(\x -> x.issueType == Accessibility) state.data.rideCompletedData.issueReportData.customerResponse) DA.!! 0)
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
        , isClickable = clickale
        , alpha = if clickale  then 1.0 else 0.4
        , enableRipple = clickale
        , rippleColor = Color.rippleShade
        }
  in
    primaryButtonConfig'
  where 
    isRentalRide = state.data.fareProductType == FPT.RENTAL
    isIntercityRide = state.data.fareProductType == FPT.INTER_CITY
    issueFlowClickable = (DA.null $ issueReportBannerConfigs state) || state.data.rideCompletedData.issueReportData.respondedValidIssues
    ratingFlowClickable = state.data.ratingViewState.selectedRating > 0
    clickale = (if state.data.rideCompletedData.issueReportData.showIssueBanners then  issueFlowClickable else ratingFlowClickable) || isRentalRide || isIntercityRide



maybeLaterButtonConfig :: ST.HomeScreenState -> PrimaryButton.Config
maybeLaterButtonConfig state =
  let
    config = PrimaryButton.config

    primaryButtonConfig' =
      config
        { textConfig
          { text = ""
          , textFromHtml = Just ("<u>" <> (getString MAYBE_LATER) <> "<u>")
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

    primaryButtonConfig' =
      config
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
          { imageUrl = fetchImage FF_ASSET "ny_ic_bent_right_arrow"
          , height = V 16
          , width = V 21
          , margin = (Margin 17 0 17 0)
          }
        , id = "WheretoButton"
        }
  in
    primaryButtonConfig'

primaryButtonRequestRideConfig :: ST.HomeScreenState -> PrimaryButton.Config
primaryButtonRequestRideConfig state =
  let
    config = PrimaryButton.config

    primaryButtonConfig' =
      config
        { textConfig
          { text =
            if state.props.repeatRideTimer /= "0" && not DS.null state.props.repeatRideTimerId then
              ((getString REQUESTING_RIDE_IN) <> " " <> state.props.repeatRideTimer <> "s")
            else if state.props.repeatRideTimer == "0" then
              (getString REQUESTING_RIDE) <> "..."
            else
              (getString REQUEST_RIDE)
          , color = state.data.config.primaryTextColor
          , accessibilityHint = "Request Ride : Button"
          }
        , cornerRadius = state.data.config.primaryButtonCornerRadius
        , margin = (Margin 0 32 0 0)
        , id = "RequestRideButton"
        , background = state.data.config.primaryBackground
        , enableRipple = true
        , rippleColor = Color.rippleShade
        }
  in
    primaryButtonConfig'

primaryButtonConfirmPickupConfig :: ST.HomeScreenState -> PrimaryButton.Config
primaryButtonConfirmPickupConfig state =
  let
    config = PrimaryButton.config
    isBtnClickable = (state.props.currentStage == EditPickUpLocation && state.props.markerLabel /= getString(LOCATION_IS_TOO_FAR)) || state.props.currentStage == ConfirmingLocation
    primaryButtonConfig' =
      config
        { textConfig
          { text = if state.data.fareProductType == FPT.DELIVERY then (if state.props.isSource == Just true then getString CONFIRM_PICKUP else getString CONFIRM_DROP) else getString CONFIRM_LOCATION
          , color = state.data.config.primaryTextColor
          , accessibilityHint = "Confirm PickUp Location : Button"
          }
        , cornerRadius = state.data.config.primaryButtonCornerRadius
        , margin = (MarginTop 8)
        , id = "ConfirmLocationButton"
        , background = state.data.config.primaryBackground
        , isClickable = isBtnClickable
        , alpha = if isBtnClickable then 1.0 else 0.5
        , enableRipple = isBtnClickable
        , rippleColor = Color.rippleShade
        }
  in
    primaryButtonConfig'

cancelRidePopUpConfig :: ST.HomeScreenState -> CancelRidePopUpConfig.Config
cancelRidePopUpConfig state =
  let
    cancelRideconfig = CancelRidePopUpConfig.config

    lastIndex = (DA.length state.props.cancellationReasons) - 1

    cancelRideConfig = state.data.config.cancelReasonConfig
  in
    CancelRidePopUpConfig.config
      { selectionOptions = state.props.cancellationReasons
      , showAllOptionsText = (getString SHOW_ALL_OPTIONS)
      , primaryButtonTextConfig
        { firstText = getString WAIT_FOR_DRIVER
        , secondText = getString CANCEL_RIDE
        }
      , activeIndex = state.props.cancelRideActiveIndex
      , activeReasonCode = Just state.props.cancelReasonCode
      , isLimitExceeded = DS.length state.props.cancelDescription >= 100
      , cornerRadius = cancelRideConfig.buttonCornerRadius
      , isSelectButtonActive =
        ( case state.props.cancelRideActiveIndex of
            Just cancelRideIndex -> true
            Nothing -> false
        )
      , headingTextConfig
        { text = getString CANCEL_RIDE <> "?"
        }
      , subHeadingTextConfig
        { text = getString PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL
        }
      , hint = getString HELP_US_WITH_YOUR_REASON
      , strings
        { mandatory = getString MANDATORY
        , limitReached = getString MAX_CHAR_LIMIT_REACHED <> " 100 " <> getString OF <> " 100"
        }
      , config = state.data.config
      }

genderBannerConfig :: forall action. ST.HomeScreenState -> action -> BannerCarousel.Config action
genderBannerConfig state action =
  let
    config = BannerCarousel.config action

    config' =
      config
        { backgroundColor = Color.lightMintGreen
        , title = (getString COMPLETE_YOUR_PROFILE_FOR_A_PERSONALISED_RIDE_EXPERIENCE)
        , titleColor = Color.elfGreen
        , actionText = (getString UPDATE_NOW)
        , actionTextColor = Color.elfGreen
        , imageUrl = "ny_ic_banner_gender_feat"
        , type = BannerCarousel.Gender
        }
  in
    config'

disabilityBannerConfig :: forall a. ST.HomeScreenState -> a -> BannerCarousel.Config a
disabilityBannerConfig state action =
  let
    config = BannerCarousel.config action

    config' =
      config
        { backgroundColor = Color.paleLavender
        , title = (getString NOW_GET_ASSISTED_RIDES)
        , titleColor = Color.purple
        , actionText = (getString UPDATE_PROFILE)
        , actionTextColor = Color.purple
        , imageUrl = "ny_ic_accessibility_banner_img"
        , type = BannerCarousel.Disability
        }
  in
    config'

sosSetupBannerConfig :: forall a. ST.HomeScreenState -> a -> BannerCarousel.Config a
sosSetupBannerConfig state action =
  let
    config = BannerCarousel.config action

    bannerConfig = case state.props.sosBannerType of
      Just ST.SETUP_BANNER -> { title: getString COMPLETE_YOUR_NAMMA_SAFETY_SETUP_FOR_SAFE_RIDE_EXPERIENCE, actionText: getString SETUP_NOW, image: "ny_ic_banner_sos" }
      Just ST.MOCK_DRILL_BANNER -> { title: getString COMPLETE_YOUR_TEST_DRILL, actionText: getString TEST_DRILL, image: "ny_ic_mock_drill_banner" }
      Nothing -> { title: "", actionText: "", image: "" }

    config' =
      config
        { backgroundColor = Color.lightMintGreen
        , title = bannerConfig.title
        , titleColor = Color.elfGreen
        , actionText = bannerConfig.actionText
        , actionTextColor = Color.elfGreen
        , imageUrl = fetchImage FF_ASSET bannerConfig.image
        , type = BannerCarousel.Safety
        }
  in
    config'

metroBannerConfig :: forall a. ST.HomeScreenState -> a -> BannerCarousel.Config a
metroBannerConfig state action =
  let
    config = BannerCarousel.config action
    appName = fromMaybe state.data.config.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
    config' =
      config
        { backgroundColor = Color.blue600'
        , title = getString $ METRO_BANNER_TITLE appName
        , titleColor = Color.blue800
        , actionText = getString BOOK_NOW
        , actionTextColor = Color.blue700
        -- , actionTextBackgroundColour = Color.blue800
        , actionTextCornerRadius = "12.0"
        , imageUrl = fetchImage FF_ASSET "ny_ic_metro_banner"
        , margin = MarginTop 0
        , imageHeight = V 100
        , imageWidth = V 120
        , padding = Padding 0 2 5 5
        , imagePadding = PaddingLeft 24
        , type = BannerCarousel.MetroTicket
        }
  in
    config'

ticketBannerConfig :: forall action. ST.HomeScreenState -> action -> BannerCarousel.Config action
ticketBannerConfig state action =
  let
    config = BannerCarousel.config action

    config' =
      config
        { backgroundColor = "#FFF6DE"
        , title = "Book Millennium Jetty, Heritage cruise and Alipore  zoo tickets "
        , titleColor = Color.black800
        , actionText = "Book Now"
        , actionTextColor = Color.black900
        , imageUrl = fetchImage FF_ASSET "ny_ic_zoo_banner"
        , margin = MarginTop 0
        , imageHeight = V 75
        , imageWidth = V 60
        , padding = Padding 0 5 5 5
        , type = BannerCarousel.ZooTicket
        }
  in
    config'

metroTicketBannerConfig :: ST.HomeScreenState -> Banner.Config
metroTicketBannerConfig state =
  let
    config = Banner.config

    config' =
      config
        { backgroundColor = Color.blue600'
        , title = "Book metro tickets with \nNamma Yatri Now!"
        , titleColor = Color.blue800
        , actionText
          { text = "Book Now"
          , backgroundColor = Just Color.blue800
          , textColor = Color.white900
          , cornerRadius = 12.0
          }
        , imageUrl = fetchImage FF_ASSET "ny_ic_metro_banner"
        , margin = MarginTop 0
        , imageHeight = V 100
        , imageWidth = V 120
        , padding = Padding 0 2 5 5
        , imagePadding = PaddingLeft 24
        }
  in
    config'


logOutPopUpModelConfig :: ST.HomeScreenState -> PopUpModal.Config
logOutPopUpModelConfig state = case state.props.isPopUp of
  ST.Logout ->
    let
      config' = PopUpModal.config

      popUpConfig' =
        config'
          { primaryText { text = (getString LOGOUT_) }
          , secondaryText { text = (getString ARE_YOU_SURE_YOU_WANT_TO_LOGOUT) }
          , option1
            { background = state.data.config.popupBackground
            , strokeColor = state.data.config.primaryBackground
            , color = state.data.config.primaryBackground
            , text = (getString GO_BACK_)
            , enableRipple = true
            }
          , option2
            { color = state.data.config.primaryTextColor
            , strokeColor = state.data.config.primaryBackground
            , background = state.data.config.primaryBackground
            , text = (getString LOGOUT_)
            , enableRipple = true
            }
          }
    in
      popUpConfig'
  ST.TipsPopUp -> do
    let
      tipConfig = getTipConfig state.data.selectedEstimatesObject.vehicleVariant

      isTipEnabled = state.data.config.tipsEnabled && (DA.length tipConfig.customerTipArray) > 0

      customerTipArray = tipConfig.customerTipArray

      customerTipArrayWithValues = tipConfig.customerTipArrayWithValues
    PopUpModal.config
      { optionButtonOrientation = "VERTICAL"
      , dismissIconMargin = Margin 0 0 14 13
      , dismissIconVisibility = if isLocalStageOn ST.QuoteList then GONE else VISIBLE
      , backgroundClickable = true
      , customerTipAvailable = state.data.selectedEstimatesObject.providerType == ONUS
      , fareEstimateText = getString FARE_ESTIMATE
      , tipSelectedText = getString TIP_SELECTED
      , fareEstimate = getValueToLocalStore FARE_ESTIMATE_DATA
      , tipSelected = if state.props.customerTip.tipActiveIndex == 0 then "-" else " ₹" <> (show (fromMaybe 0 (customerTipArrayWithValues DA.!! state.props.customerTip.tipActiveIndex)))
      , dismissPopup = true
      , customerTipArray = customerTipArray
      , customerTipArrayWithValues = customerTipArrayWithValues
      , isTipEnabled = isTipEnabled
      , primaryText
        { text = if isLocalStageOn ST.QuoteList then (getString TRY_AGAIN <> "?") else getString SEARCH_AGAIN_WITH_A_TIP
        , textStyle = FontStyle.Heading1
        }
      , secondaryText
        { text = (getString BOOST_YOUR_RIDE_CHANCES_AND_HELP_DRIVERS_WITH_TIPS)
        , color = Color.black650
        , visibility = boolToVisibility $ isTipEnabled
        }
      , tipLayoutMargin = (Margin 22 2 22 22)
      , searchExpired = true
      , buttonLayoutMargin = (MarginHorizontal 16 16)
      , activeIndex = state.props.customerTip.tipActiveIndex
      , isVisible = state.props.tipViewProps.isVisible
      , isTipPopup = true
      , tipButton
        { background = Color.white900
        , color = Color.black800
        , strokeColor = Color.grey900
        , padding = (Padding 16 12 16 12)
        }
      , option1
        { text = if (not isTipEnabled) then getString SEARCH_AGAIN else if state.props.customerTip.tipActiveIndex == 0 then getString SEARCH_AGAIN_WITHOUT_A_TIP else getTipString state customerTipArrayWithValues
        , width = MATCH_PARENT
        , color = state.data.config.primaryTextColor
        , strokeColor = state.data.config.primaryBackground
        , background = state.data.config.primaryBackground
        , padding = (Padding 0 10 0 10)
        }
      , option2
        { text = if (isLocalStageOn ST.QuoteList) then (getString HOME) else (getString CANCEL_SEARCH)
        , width = MATCH_PARENT
        , background = Color.white900
        , strokeColor = Color.white900
        , margin = MarginTop 14
        , padding = PaddingBottom $ getBottomMargin
        , color = Color.black650
        , height = WRAP_CONTENT
        }
      , cornerRadius = (Corners 15.0 true true false false)
      }
  ST.CancelConfirmingQuotes ->
    let
      config' = PopUpModal.config

      popUpConfig' =
        config'
          { primaryText { text = (getString CANCEL_SEARCH) <> " ?" }
          , secondaryText { text = (getString CANCEL_ONGOING_SEARCH) }
          , option1
            { background = state.data.config.popupBackground
            , strokeColor = state.data.config.primaryBackground
            , color = state.data.config.primaryBackground
            , text = (getString NO_DONT)
            , enableRipple = true
            }
          , option2
            { color = state.data.config.primaryTextColor
            , strokeColor = state.data.config.primaryBackground
            , background = state.data.config.primaryBackground
            , text = (getString YES_CANCEL_SEARCH)
            , enableRipple = true
            }
          }
    in
      popUpConfig'
  _ ->
    let
      isNormalRide = not (DA.any (_ == state.data.fareProductType) [ FPT.INTER_CITY, FPT.RENTAL ])

      config' = PopUpModal.config

      popUpConfig' =
        config'
          { primaryText { text = if (isLocalStageOn ST.QuoteList) then ((getString TRY_AGAIN) <> "?") else ((getString CANCEL_SEARCH) <> "?") }
          , buttonLayoutMargin = (MarginHorizontal 16 16)
          , showRetry = isNormalRide
          , dismissPopup = true
          , optionButtonOrientation = if (isLocalStageOn ST.QuoteList || isLocalStageOn ST.FindingQuotes || state.data.iopState.providerSelectionStage) then "VERTICAL" else "HORIZONTAL"
          , secondaryText { text = if (isLocalStageOn ST.QuoteList) then (getString TRY_LOOKING_FOR_RIDES_AGAIN) else (getString CANCEL_ONGOING_SEARCH) }
          , option1
            { text = if (isLocalStageOn ST.QuoteList) then (getString YES_TRY_AGAIN) else (getString YES_CANCEL_SEARCH)
            , width = MATCH_PARENT
            , color = state.data.config.primaryTextColor
            , strokeColor = state.data.config.primaryBackground
            , background = state.data.config.primaryBackground
            , padding = (Padding 0 10 0 10)
            , enableRipple = true
            , visibility = isNormalRide
            }
          , option2
            { text = if (isLocalStageOn ST.QuoteList) then (getString HOME) else (getString NO_DONT)
            , width = MATCH_PARENT
            , background = Color.white900
            , strokeColor = Color.white900
            , margin = MarginTop $ if ((isLocalStageOn ST.QuoteList || isLocalStageOn ST.FindingQuotes) && isNormalRide) then 14 else 3
            , color = Color.black650
            , padding = if (isLocalStageOn ST.QuoteList || isLocalStageOn ST.FindingQuotes) then (PaddingBottom getBottomMargin) else (Padding 0 0 0 0)
            }
          }
    in
      popUpConfig'
  where
  getTipString :: ST.HomeScreenState -> Array Int -> String
  getTipString state customerTipArrayWithValues = do
    let
      tip = show (fromMaybe 0 (customerTipArrayWithValues DA.!! state.props.customerTip.tipActiveIndex))
    case (getLanguageLocale languageKey) of
      "EN_US" -> getString SEARCH_AGAIN_WITH <> " +₹" <> tip <> " " <> getString TIP
      _ -> "+₹" <> tip <> " " <> (getString TIP) <> " " <> getString SEARCH_AGAIN_WITH

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
        , option2
          { background = state.data.config.primaryBackground
          , strokeColor = state.data.config.primaryBackground
          , color = state.data.config.primaryTextColor
          , text = (getString CHANGE_DROP_LOCATION)
          , margin = (Margin 16 0 16 EHC.safeMarginBottom)
          , enableRipple = true
          }
        , popUpHeaderConfig { gravity = CENTER }
        }
  in
    popUpConfig'

pickUpFarFromCurrentLocationConfig :: ST.HomeScreenState -> PopUpModal.Config
pickUpFarFromCurrentLocationConfig state =
  let
    config' = PopUpModal.config

    popUpConfig' =
      config'
        { backgroundClickable = false
        , primaryText
          { text = getString YOU_SEEM_TO_BE_FAR_FROM_PICK_UP
          , margin = (Margin 16 20 16 0)
          }
        , secondaryText
          { text = getString ARE_YOU_SURE_YOU_WANT_TO_PROCEED_WITH_THE_BOOKING
          , margin = (Margin 0 16 0 20)
          }
        , option1
          { background = state.data.config.popupBackground
          , strokeColor = state.data.config.primaryBackground
          , color = state.data.config.primaryBackground
          , text = (getString GO_BACK_)
          }
        , option2
          { color = state.data.config.primaryTextColor
          , strokeColor = state.data.config.primaryBackground
          , background = state.data.config.primaryBackground
          , text = (getString BOOK_RIDE_)
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
        , option1
          { background = state.data.config.popupBackground
          , strokeColor = state.data.config.primaryBackground
          , color = state.data.config.primaryBackground
          , text = (getString GO_BACK_)
          , enableRipple = true
          }
        , option2
          { color = state.data.config.primaryTextColor
          , strokeColor = state.data.config.primaryBackground
          , background = state.data.config.primaryBackground
          , text = (getString BOOK_RIDE_)
          , enableRipple = true
          }
        }
  in
    popUpConfig'

isMockLocationConfig :: ST.HomeScreenState -> ErrorModal.Config
isMockLocationConfig state =
  let
    config = ErrorModal.config

    appConfig = state.data.config

    errorModalConfig' =
      config
        { height = MATCH_PARENT
        , background = Color.white900
        , stroke = ("1," <> Color.borderGreyColor)
        , imageConfig
          { imageUrl = fetchImage FF_ASSET "ny_ic_location_unserviceable"
          , height = V 99
          , width = V 133
          , margin = Margin 0 50 0 20
          }
        , errorConfig
          { text = if state.props.isMockLocation then "Unable to get your location!" else (getString LOCATION_UNSERVICEABLE)
          , color = Color.black800
          , margin = MarginBottom 5
          }
        , errorDescriptionConfig
          { text = if state.props.isMockLocation then "Turn off any Mock Location app you might be using and restart the app." else getString $ CURRENTLY_WE_ARE_LIVE_IN_ "CURRENTLY_WE_ARE_LIVE_IN_"
          , color = Color.black700
          , margin = Margin 20 0 20 (40 + EHC.safeMarginBottom)
          }
        , buttonConfig
          { text = getString CHANGE_LOCATION
          , margin = Margin 16 0 16 (20 + EHC.safeMarginBottom)
          , background = state.data.config.primaryBackground
          , color = state.data.config.primaryTextColor
          , visibility = GONE
          }
        }
  in
    errorModalConfig'

waitTimeInfoCardConfig :: ST.HomeScreenState -> RequestInfoCard.Config
waitTimeInfoCardConfig state = let
  isQuotes = state.data.fareProductType == FPT.ONE_WAY_SPECIAL_ZONE || state.props.isOtpRideFlow
  waitTimeConfig = textConfig isQuotes  
  config = RequestInfoCard.config
  requestInfoCardConfig' = config{
    title {
      text = getString waitTimeConfig.title,
      accessibilityHint = getEN waitTimeConfig.title
    }
  , primaryText {
      text = getString waitTimeConfig.primaryText,
      padding = Padding 16 16 0 0,
      textStyle = FontStyle.ParagraphText,
      color = Color.black700,
      accessibilityHint = getEN waitTimeConfig.primaryText
    }
  , secondaryText {
      text = if not isQuotes then getVarString waitTimeConfig.secondaryText $ if state.data.vehicleVariant == "AUTO_RICKSHAW" then ["3", "1.5"] else ["5", "1"] else getString waitTimeConfig.secondaryText,
      visibility = boolToVisibility $ waitTimeConfig.waitingChargeApplicable && state.data.currentCityConfig.enableWaitingConfig,
      padding = PaddingLeft 16,
      color = Color.black700,
      textStyle = FontStyle.ParagraphText,
      width = (V $ JB.getWidthFromPercent 75),
      accessibilityHint = getEN waitTimeConfig.secondaryText
    }
  , imageConfig {
      imageUrl = fetchImage FF_ASSET "ny_ic_wait_timer",
      height = V 130,
      width = V 130,
      padding = Padding 0 2 2 0,
      visibility = VISIBLE
    }
  , buttonConfig {
      text = getString GOT_IT,
      padding = PaddingVertical 16 20,
      accessibilityHint = (getEN GOT_IT) <> " : Button"
    }
  }
  in requestInfoCardConfig'
  where 
    textConfig :: Boolean -> {title :: STR, primaryText :: STR, secondaryText :: STR, waitingChargeApplicable :: Boolean}
    textConfig isQuotes = if isQuotes then {title : OTP_EXPIRE_TIMER, primaryText : SHOWS_FOR_HOW_LONG_YOUR_OTP_, secondaryText : IF_YOUR_OTP_EXPIRES_, waitingChargeApplicable : true}
                          else {title : WAIT_TIMER, primaryText : HOW_LONG_DRIVER_WAITED_FOR_PICKUP, secondaryText : YOU_WILL_PAY_FOR_EVERY_MINUTE waitingChargeInfo.freeMinutes waitingChargeInfo.chargePerMinute, waitingChargeApplicable : waitingChargeInfo.chargePerMinute /= "₹0/min"}
    waitingChargeInfo = 
                      let rideType = state.data.fareProductType
                          cityConfig = state.data.currentCityConfig
                          autoWaitingCharges = if rideType == FPT.RENTAL then cityConfig.rentalWaitingChargeConfig.auto else cityConfig.waitingChargeConfig.auto 
                          cabsWaitingCharges = if rideType == FPT.RENTAL then cityConfig.rentalWaitingChargeConfig.cabs else cityConfig.waitingChargeConfig.cabs
                          ambulanceWaitingCharges = cityConfig.waitingChargeConfig.ambulance
                          waitingCharges = 
                            if any (_ == state.data.vehicleVariant) ["AUTO_RICKSHAW", "EV_AUTO_RICKSHAW"] then
                                autoWaitingCharges
                            else if rideType == FPT.AMBULANCE then 
                                ambulanceWaitingCharges
                            else
                                cabsWaitingCharges
                          isIntercity = state.data.fareProductType == FPT.INTER_CITY
                      in 
                      case state.data.rateCardCache of
                          Just rateCard ->                   
                            if isIntercity then {freeMinutes : show waitingCharges.freeMinutes , chargePerMinute : "₹"<>show waitingCharges.perMinCharges <>"/min"}
                                            else {freeMinutes : rateCard.waitingTimeInfo.freeMinutes, chargePerMinute : rateCard.waitingTimeInfo.charge}
                          Nothing -> do
                            {freeMinutes : (show waitingCharges.freeMinutes) , chargePerMinute : "₹"<> show waitingCharges.perMinCharges <>"/min"}

rateCardConfig :: ST.HomeScreenState -> RateCard.Config
rateCardConfig state =
  let
    bangaloreCode = HU.getCityCodeFromCity Bangalore
    city = EHU.getCityFromString $ getValueToLocalStore CUSTOMER_LOCATION
  in RateCard.config 
        { isNightShift = state.data.rateCard.isNightShift
        , currentRateCardType = state.data.rateCard.currentRateCardType
        , onFirstPage = state.data.rateCard.onFirstPage
        , showDetails = state.data.config.searchLocationConfig.showRateCardDetails
        , description = if state.data.rateCard.isNightShift then (getString $ NIGHT_TIME_CHARGES state.data.rateCard.nightChargeFrom state.data.rateCard.nightChargeTill) else (getString $ DAY_TIME_CHARGES state.data.rateCard.nightChargeTill state.data.rateCard.nightChargeFrom)
        , buttonText = Just if state.data.rateCard.currentRateCardType == DefaultRateCard then (getString GOT_IT) else (getString GO_BACK_)
        , title = getString RATE_CARD
        , fareList = state.data.rateCard.extraFare 
        , driverAdditions = state.data.rateCard.driverAdditions
        , otherOptions = otherOptions $ (not DA.null state.data.rateCard.driverAdditions) && state.data.config.searchLocationConfig.showDriverAdditions
        , fareInfoDescription = state.data.rateCard.fareInfoDescription
        , additionalStrings = 
            (if state.data.config.searchLocationConfig.showDriverAdditions then 
              [ {key : "DRIVER_ADDITIONS_OPTIONAL", val : getString DRIVER_ADDITIONS_OPTIONAL}
              , {key : "THE_DRIVER_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC", val : getString THE_DRIVER_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC}
              , {key : "DRIVER_MAY_NOT_CHARGE_THIS_ADDITIONAL_FARE", val : getString DRIVER_MAY_NOT_CHARGE_THIS_ADDITIONAL_FARE}
              ] 
            else []) 
          <> (if state.data.config.searchLocationConfig.showAdditionalChargesText then 
              [ {key : "TOLL_OR_PARKING_CHARGES", val : getString TOLL_OR_PARKING_CHARGES}
              , {key : "TOLL_CHARGES", val : getString TOLL_CHARGES}
              , {key : "TOLL_CHARGES_DESC", val : getString TOLL_CHARGES_DESC}
              , {key : "PARKING_CHARGE", val : getString PARKING_CHARGE}
              , {key : "PARKING_CHARGES_DESC", val : getString PARKING_CHARGES_DESC}
              ] 
              else []) 
          <> (if state.data.rateCard.serviceTierName == Just "Auto" && state.data.config.searchLocationConfig.showChargeDesc then [{key : "CHARGE_DESCRIPTION", val : (getString ERNAKULAM_LIMIT_CHARGE)}] else [])
          }
  where
  otherOptions :: Boolean -> Array FareList
  otherOptions showAdditions =
    (if showAdditions then [ { key: "DRIVER_ADDITIONS", val: (getString DRIVER_ADDITIONS) } ] else [])
    <> (if state.data.config.searchLocationConfig.showAdditionalChargesText then [{key : "TOLL_OR_PARKING_CHARGES", val : getString TOLL_OR_PARKING_CHARGES }]else [])

intercityRateCardConfig :: ST.HomeScreenState -> RateCard.Config
intercityRateCardConfig state = 
  let 
    config' = RateCard.config
    driverAllowance = (DA.head $ DA.filter  (\item -> (item.key == "DRIVER_ALLOWANCE")) state.data.rateCard.extraFare)
    driverAllowanceVal = case driverAllowance of 
                              (Just driverAllowance') -> driverAllowance'.val
                              _ -> ""
    driverMaxAllowance = (DA.head $ DA.filter (\item -> (item.key == "PER_DAY_MAX_ALLOWANCE")) state.data.rateCard.extraFare)
    driverMaxAllowanceVal = case driverMaxAllowance of 
                              (Just driverMaxAllowance') -> driverMaxAllowance'.val
                              _ -> ""
    nightShiftCharges = ((DA.head $ DA.filter (\item -> (item.key == "Night Shift Charges*")) state.data.rateCard.extraFare))
    nightShiftChargesVal = case nightShiftCharges of 
                              (Just nightShiftCharges') -> nightShiftCharges'.val
                              _ -> ""
    baseFare = ((DA.head $ DA.filter (\item -> (item.key == "BASE_FARE")) state.data.rateCard.extraFare))
    baseFareVal = case baseFare of 
                              (Just baseFare') -> baseFare'.val
                              _ -> "0.0"
    plannedPerKmCharges = ((DA.head $ DA.filter (\item -> (item.key == "PLANNED_PER_KM_CHARGES")) state.data.rateCard.extraFare))
    plannedPerKmChargesVal = case plannedPerKmCharges of 
                              (Just plannedPerKmCharges') -> plannedPerKmCharges'.val
                              _ -> "0.0"
    driverAdditions = ((DA.head $ DA.filter (\item -> (item.key == "DRIVER_ALLOWANCE")) state.data.rateCard.extraFare))
    nightChargeTill = fromMaybe "" $ convertTo12HourFormat state.data.rateCard.nightChargeTill
    nightChargeFrom = fromMaybe "" $ convertTo12HourFormat state.data.rateCard.nightChargeFrom
    intercityBaseFareLimit = "30"
    expectedFareList = 
                      (if baseFareVal /= "0.0" then[{key : (getString FIXED_CHARGES), val : (getCurrency appConfig) <> (baseFareVal)},{key:(getString DISTANCE_FARE),val :(getCurrency appConfig) <> (plannedPerKmChargesVal) <> " /km"}] else [{key : getString DISTANCE_FARE,val : ((getCurrency appConfig) <> plannedPerKmChargesVal <> " /km")}])<>
                      DA.filter (\item -> all (\key -> item.key /= key) ["DRIVER_ALLOWANCE", "PER_DAY_MAX_ALLOWANCE", "Night Shift Charges*", "BASE_FARE", "PLANNED_PER_KM_CHARGES","EXTRA_TIME_FARE","EXTRA_DISTANCE_FARE"]) state.data.rateCard.extraFare
    intercityRateCardConfig' = 
      config'
      {
          currentRateCardType = state.data.rateCard.currentRateCardType
        , onFirstPage = state.data.rateCard.onFirstPage
        , showDetails = true
        , title = getString RATE_CARD
        , description = "Intercity Charges"
        , buttonText = Just if state.data.rateCard.currentRateCardType == DefaultRateCard then (getString GOT_IT) else (getString GO_BACK_)
        , primaryButtonConfig {
            margin = MarginTop 16,
            text = getString GOT_IT,
            color = Color.blue800,
            height = V 40,
            cornerRadius = 8.0,
            background = Color.white900,
            visibility = VISIBLE
          }
        , otherOptions  = otherOptions driverAdditions
        , additionalStrings = [
          {key : "DRIVER_ADDITIONS_OPTIONAL", val : (getString DRIVER_ADDITIONS_OPTIONAL)},
          {key : "THE_DRIVER_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC", val : (getString THE_DRIVER_MAY_QUOTE_EXTRA_TO_COVER_FOR_TRAFFIC)},
          {key : "DRIVER_MAY_NOT_CHARGE_THIS_ADDITIONAL_FARE", val : (getString DRIVER_MAY_NOT_CHARGE_THIS_ADDITIONAL_FARE)},
          {key : "TOLL_OR_PARKING_CHARGES", val : (getString TOLL_OR_PARKING_CHARGES)},
          {key : "TOLL_CHARGES", val : (getString TOLL_CHARGES)},
          {key : "TOLL_CHARGES_DESC", val : (getString TOLL_CHARGES_DESC)},
          {key : "PARKING_CHARGES", val : ("Parking Charges")},
          {key : "PARKING_CHARGES_DESC", val : (getString PARKING_CHARGES_DESC)},
          {key : "NIGHT_SHIFT",val : (getString NIGHT_CHARGES)},
          {key : "NIGHT_SHIFT_CHARGES", val : (getVarString NIGHT_CHARGES_DESCRIPTION [nightShiftChargesVal, nightChargeFrom , nightChargeTill])},
          {key : "STATE_PERMIT_CHARGES", val :(getString STATE_CHARGES_DESCRIPTION) },
          {key : "PARKING_CHARGES_INTERCITY",val:(getString PARKING_CHARGES_DESCRIPTION) },
          {key : "TOLL_CHARGES_INTERCITY",val : (getString TOLL_AND_PARKING_CHARGES_DESCRIPTION)},
          {key : "STATE_CHARGES",val: (getString STATE_PERMIT_CHARGES)},
          {key : "DRIVER_ALLOWANCE",val: ((getString TIME_FARE) <> "*")},
          {key : "DRIVER_ALLOWANCE_STR_INTERCITY", val : (getString $ DRIVER_ALLOWANCE_DESCRIPTION driverAllowanceVal driverMaxAllowanceVal)}
          ]
        , fareList = expectedFareList
      }
    in intercityRateCardConfig'
  where
    otherOptions :: Maybe FareList ->  Array FareList
    otherOptions driverAdditions  = do
                let 
                  driverAdditionsVal = case driverAdditions of 
                              (Just driverAdditions') -> driverAdditions'.val
                              _ -> "0.0"
                (if driverAdditionsVal /= "0.0" then [{key : "DRIVER_ALLOWANCES", val : ((getString TIME_FARE) <> "*")}] else [])
                <> [{key : "TOLL_AND_PARKING_CHARGES", val :(getString TOLL_AND_PARKING_CHARGES)}]
                <> [{key : "NIGHT_SHIFT_CHARGES", val :(getString NIGHT_SHIFT_CHARGES)}]
getVehicleTitle :: String -> String
getVehicleTitle vehicle =
  ( case vehicle of
      "HATCHBACK" -> (getString HATCHBACK)
      "SUV" -> (getString SUV)
      "SEDAN" -> (getString SEDAN)
      "AUTO_RICKSHAW" -> (getString AUTO_RICKSHAW)
      "BIKE" -> "Bike Taxi"
      "SUV_PLUS" -> "XL Plus"
      "DELIVERY_BIKE" -> "2 Wheeler"
      "EV_AUTO_RICKSHAW" -> "EV Auto Rickshaw"
      "HERITAGE_CAB" -> "Heritage Cab"
      _ -> ""
  )
    <> " - "
    <> (getString RATE_CARD)

estimateChangedPopupConfig :: ST.HomeScreenState -> PopUpModal.Config
estimateChangedPopupConfig state =
  let
    config' = PopUpModal.config

    popUpConfig' =
      config'
        { primaryText { text = (getString ESTIMATES_CHANGED) }
        , secondaryText { text = (getString ESTIMATES_REVISED_TO) <> "₹" <> (show state.data.suggestedAmount) <> if state.data.rateCard.additionalFare > 0 then "-" <> "₹" <> (show $ (state.data.suggestedAmount + state.data.rateCard.additionalFare)) else "" }
        , option1
          { background = state.data.config.primaryTextColor
          , strokeColor = state.data.config.primaryBackground
          , color = state.data.config.primaryBackground
          , text = (getString GO_HOME_)
          }
        , option2
          { color = state.data.config.primaryTextColor
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
                                  , showCallPopUp: state.props.showCallPopUp
                                  , isSpecialZone: state.props.isSpecialZone
                                  , isOtpRideFlow: state.props.isOtpRideFlow
                                  , estimatedTime : state.data.rideDuration
                                  , zoneType : state.props.zoneType
                                  , merchantCity : state.props.city
                                  , showBanner : state.props.currentStage == RideStarted
                                  , isRateCardAvailable : (isJust state.data.rateCardCache) && (state.data.fareProductType /= FPT.ONE_WAY_SPECIAL_ZONE)
                                  , isChatWithEMEnabled : state.props.isChatWithEMEnabled || state.data.fareProductType == FPT.RENTAL || state.data.fareProductType == FPT.DELIVERY
                                  , rideDurationTimer : state.props.rideDurationTimer
                                  , rideDurationTimerId : state.props.rideDurationTimerId
                                  , endOTPShown : state.props.showEndOTP
                                  , showEndOTP : state.props.showEndOTP
                                  , stageBeforeChatScreen : state.props.stageBeforeChatScreen
                                  }
                              , data: driverInfoTransformer state
                            }

messagingViewConfig :: ST.HomeScreenState -> MessagingView.Config
messagingViewConfig state =
  let
    config = MessagingView.config
    DT.Tuple safetyCheckStartSeconds safetyCheckEndSeconds = case state.props.safetySettings of
      Just (API.GetEmergencySettingsRes safetySettings) -> DT.Tuple safetySettings.safetyCheckStartTime safetySettings.safetyCheckEndTime
      Nothing -> DT.Tuple Nothing Nothing
    primaryContact = DA.head $ DA.filter (\item -> (isJust item.contactPersonId)) $ fromMaybe [] state.data.contactList

    messagingViewConfig' =
      config
        { userConfig
          { userName =
            if state.props.isChatWithEMEnabled 
              then 
                case primaryContact of
                  Nothing -> state.data.driverInfoCardState.driverName
                  Just contact -> contact.name
              else
                state.data.driverInfoCardState.driverName
          , receiver =
            if state.props.isChatWithEMEnabled then case primaryContact of
              Nothing -> "Driver"
              Just contact -> contact.name
            else
              "Driver"
          }
        , feature
          { sendMessageActive = state.props.sendMessageActive
          , canSendSuggestion = state.props.canSendSuggestion
          , showAutoGeneratedText = (getValueToLocalStore NOTIFIED_CUSTOMER == "true") && isJust state.data.driverInfoCardState.eta && (HU.secondsToHms $ fromMaybe 0 state.data.driverInfoCardState.eta) /= "--"
          , showVehicleDetails = not state.props.isChatWithEMEnabled
          , enableSuggestions = state.data.config.feature.enableSuggestions
          }
        , messages = map (\{ message, sentBy, timeStamp, type: type_, delay } -> { message, sentBy, timeStamp, type: type_, delay }) state.data.messages
        , messagesSize = state.data.messagesSize
        , vehicleNo = HU.makeNumber $ state.data.driverInfoCardState.registrationNumber
        , chatSuggestionsList = getChatSuggestions state
        , hint = (getString MESSAGE)
        , languageKey = (getLanguageLocale languageKey)
        , rideConfirmedAt = state.data.driverInfoCardState.startedAt
        , autoGeneratedText = state.data.config.notifyRideConfirmationConfig.autoGeneratedText <> (HU.secondsToHms $ fromMaybe 0 state.data.driverInfoCardState.eta)
        , driverRating = show $ state.data.driverInfoCardState.rating
        , fareAmount = show $ state.data.driverInfoCardState.price
        , config = state.data.config
        , peekHeight = if state.data.infoCardPeekHeight == 0 then getDefaultPeekHeight state else state.data.infoCardPeekHeight
        , otp = state.data.driverInfoCardState.otp
        , suggestionKey = if state.props.isChatWithEMEnabled then emChatSuggestion else chatSuggestion
        , isKeyBoardOpen = state.props.isKeyBoardOpen
        , showChatListPopUp = state.props.showChatListPopUp
        , contactList = getChatDetails state $ fromMaybe [] state.data.contactList
        , currentChatRecipient = state.data.driverInfoCardState.currentChatRecipient       
        }
  in
    messagingViewConfig'

getDefaultPeekHeight :: ST.HomeScreenState -> Int
getDefaultPeekHeight state =
  let isQuotes = state.data.fareProductType == FPT.ONE_WAY_SPECIAL_ZONE || state.props.isOtpRideFlow
      height = case state.props.currentStage == ST.RideAccepted of
        true -> if isQuotes then 285 else 381
        false -> if isQuotes then 377 else 368
  in height + if state.data.config.driverInfoConfig.footerVisibility then 44 else 0

metersToKm :: Int -> Boolean -> String
metersToKm distance towardsDrop =
  if (distance <= 10) then
    (if towardsDrop then (getString AT_DROP) else (getString AT_PICKUP))
  else if (distance < 1000) then (HU.toStringJSON distance <> " m " <> (getString AWAY_C)) else (HU.parseFloat ((INT.toNumber distance) / 1000.0)) 2 <> " km " <> (getString AWAY_C)

driverInfoTransformer :: ST.HomeScreenState -> DriverInfoCardData
driverInfoTransformer state =
  let
    cardState = state.data.driverInfoCardState
    appConfig = state.data.config { driverInfoConfig { numberPlateBackground = if any (_ == cardState.vehicleVariant) ["BIKE", "DELIVERY_BIKE"] then Color.white900 else state.data.config.driverInfoConfig.numberPlateBackground}}
  in
    { otp : cardState.otp
    , driverName : (DS.toUpper (DS.take 1 cardState.driverName)) <> (DS.toLower (DS.drop 1 cardState.driverName))
    , eta : cardState.eta
    , vehicleDetails : cardState.vehicleDetails
    , registrationNumber : cardState.registrationNumber
    , rating : cardState.rating
    , startedAt : cardState.createdAt
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
    , destinationReached : cardState.destinationReached
    , destinationReachedAt : cardState.destinationReachedAt
    , estimatedDropTime : ""
    , isSpecialZone : state.props.isSpecialZone
    , isLocationTracking : state.props.isLocationTracking
    , bookingCreatedAt : cardState.createdAt
    , bppRideId : ""
    , driverNumber : cardState.driverNumber
    , merchantExoPhone : cardState.merchantExoPhone
    , config : appConfig
    , vehicleVariant : cardState.vehicleVariant
    , defaultPeekHeight : getDefaultPeekHeight state
    , bottomSheetState : state.props.currentSheetState
    , bannerData : state.data.bannerData
    , bannerArray : getDriverInfoCardBanners state DriverInfoCard.BannerCarousel
    , vehicleModel : cardState.vehicleModel
    , vehicleColor : cardState.vehicleColor
    , serviceTierName : cardState.serviceTierName
    , providerName : cardState.providerName
    , providerType : cardState.providerType
    , cityConfig : state.data.currentCityConfig
    , rentalData : cardState.rentalData
    , fareProductType : cardState.fareProductType
    , spLocationName : cardState.spLocationName
    , addressWard : cardState.addressWard
    , hasToll : cardState.hasToll
    , senderDetails : cardState.senderDetails
    , receiverDetails : cardState.receiverDetails
    , estimatedTimeToReachDestination : cardState.estimatedTimeToReachDestination
    , requestorPartyRoles : state.data.requestorPartyRoles
    , isAirConditioned : cardState.isAirConditioned
    }

emergencyHelpModelViewState :: ST.HomeScreenState -> EmergencyHelp.EmergencyHelpModelState
emergencyHelpModelViewState state =
  { showContactSupportPopUp: state.props.emergencyHelpModelState.showContactSupportPopUp
  , showCallPolicePopUp: state.props.emergencyHelpModelState.showCallPolicePopUp
  , showCallContactPopUp: state.props.emergencyHelpModelState.showCallContactPopUp
  , emergencyContactData: state.props.emergencyHelpModelState.emergencyContactData
  , currentlySelectedContact: state.props.emergencyHelpModelState.currentlySelectedContact
  , showCallSuccessfulPopUp: state.props.emergencyHelpModelState.showCallSuccessfulPopUp
  , config: state.data.config
  }

getRateYourRideString :: String -> String -> String
getRateYourRideString str driverName = case getLanguageLocale languageKey of
  "EN_US" -> str <> " " <> driverName
  _ -> driverName <> " " <> str

searchLocationModelViewState :: ST.HomeScreenState -> SearchLocationModel.SearchLocationModelState
searchLocationModelViewState state =
  let
    suffixButtonText =
      if state.data.startTimeUTC == "" then
        getString NOW
      else
        formatDate "hh" <> ":" <> formatDate "mm" <> " " <> formatDate "A" <> ", " <> formatDate "MMM" <> " " <> formatDate "D"
  in
    { isSearchLocation: state.props.isSearchLocation
    , locationList: state.data.locationList
    , source: state.data.source
    , destination: state.data.destination
    , isSource: state.props.isSource
    , isSrcServiceable: state.props.isSrcServiceable
    , isDestServiceable: state.props.isDestServiceable
    , isRideServiceable: state.props.isRideServiceable
    , savedlocationList: state.data.savedLocations
    , appConfig: state.data.config
    , logField: state.data.logField
    , crossBtnSrcVisibility: state.props.searchLocationModelProps.crossBtnSrcVisibility
    , crossBtnDestVisibility: state.props.searchLocationModelProps.crossBtnDestVisibility
    , isAutoComplete: state.props.searchLocationModelProps.isAutoComplete
    , showLoader: state.props.searchLocationModelProps.showLoader
    , prevLocation: state.data.searchLocationModelData.prevLocation
    , headerVisibility: state.props.canScheduleRide
    , suffixButtonVisibility: boolToVisibility $ state.props.canScheduleRide
    , suffixButton:
        { text: suffixButtonText
        , fontStyle: FontStyle.subHeading2 LanguageStyle
        , prefixImage: "ny_ic_clock_unfilled"
        , suffixImage: "ny_ic_chevron_down"
        , padding: Padding 8 0 8 1
        , gravity: CENTER_VERTICAL
        }
    , headerText: getString TRIP_DETAILS_
    , isPrimaryButtonForEditDest : false
    , currentLocationText: state.props.currentLocation.place
    , isEditDestination : false
    , isDestViewEditable : true
    , tripType : state.props.searchLocationModelProps.tripType
    , pickupConfig : pickupConfig state
    , returnConfig : returnConfig state
    , totalRideDuration : state.props.searchLocationModelProps.totalRideDuration
    , totalRideDistance : state.props.searchLocationModelProps.totalRideDistance
    , showRideInfo : state.props.searchLocationModelProps.showRideInfo
    , isIntercityFlow : state.props.isIntercityFlow
    , fareProductType : state.data.fareProductType
    }
  where
  formatDate :: String -> String
  formatDate formatSTR =
    let
      startTime = if state.data.startTimeUTC == "" then EHC.getCurrentUTC "" else state.data.startTimeUTC
    in
      EHC.convertUTCtoISC startTime formatSTR

editDestSearchLocationModelViewState :: ST.HomeScreenState -> SearchLocationModel.SearchLocationModelState
editDestSearchLocationModelViewState state = { isSearchLocation: if state.props.currentStage == EditingDestinationLoc then state.props.isSearchLocation else ST.RouteMap
                                    , locationList: state.data.locationList
                                    , source: state.data.source
                                    , destination: state.data.destination
                                    , isSource: Just false
                                    , isSrcServiceable: state.props.isSrcServiceable
                                    , isDestServiceable: state.props.isDestServiceable
                                    , isRideServiceable: state.props.isRideServiceable
                                    , savedlocationList: state.data.savedLocations
                                    , appConfig : state.data.config
                                    , logField : state.data.logField
                                    , crossBtnSrcVisibility: false
                                    , crossBtnDestVisibility: state.props.searchLocationModelProps.crossBtnDestVisibility
                                    , isAutoComplete: state.props.searchLocationModelProps.isAutoComplete
                                    , showLoader: state.props.searchLocationModelProps.showLoader
                                    , prevLocation: state.data.searchLocationModelData.prevLocation
                                    , currentLocationText : state.props.currentLocation.place
                                    , isEditDestination : state.props.currentStage == EditingDestinationLoc -----fix me ----RITIKA
                                    , headerVisibility: state.props.canScheduleRide
                                    , suffixButtonVisibility: boolToVisibility $ state.props.canScheduleRide
                                    , isPrimaryButtonForEditDest : state.props.currentStage == ConfirmEditDestinationLoc
                                    , suffixButton:
                                        { text: ""
                                        , fontStyle: FontStyle.subHeading2 LanguageStyle
                                        , prefixImage: "ny_ic_clock_unfilled"
                                        , suffixImage: "ny_ic_chevron_down"
                                        , padding: Padding 8 0 8 1
                                        , gravity: CENTER_VERTICAL
                                        }
                                    , headerText: getString TRIP_DETAILS_
                                    , isDestViewEditable : state.props.currentStage == EditingDestinationLoc
                                    , tripType : state.props.searchLocationModelProps.tripType
                                    , pickupConfig : pickupConfig state
                                    , returnConfig : returnConfig state
                                    , totalRideDuration : state.props.searchLocationModelProps.totalRideDuration
                                    , totalRideDistance : state.props.searchLocationModelProps.totalRideDistance
                                    , showRideInfo : state.props.searchLocationModelProps.showRideInfo
                                    , isIntercityFlow : state.props.isIntercityFlow
                                    , fareProductType : state.data.fareProductType
                                    }

quoteListModelViewState :: ST.HomeScreenState -> QuoteListModel.QuoteListModelState
quoteListModelViewState state =
  let
    vehicleVariant = state.data.selectedEstimatesObject.vehicleVariant
    tipConfig = getTipConfig state.data.selectedEstimatesObject.vehicleVariant
    tipProps = if state.props.showBoostSearch then getTipViewProps state.props.tipViewProps state.data.boostSearchEstimate.vehicleVariant Nothing Nothing
                  else getTipViewProps state.props.tipViewProps state.data.selectedEstimatesObject.vehicleVariant state.data.selectedEstimatesObject.smartTipReason state.data.selectedEstimatesObject.smartTipSuggestion
  in
    { source: state.data.source
    , destination: state.data.destination
    , quoteListModel: state.data.quoteListModelState
    , selectedQuote: state.props.selectedQuote
    , autoSelecting: state.props.autoSelecting
    , searchExpire: state.props.searchExpire
    , showProgress: isLocalStageOn ConfirmingQuotes || (DA.null state.data.quoteListModelState) && isLocalStageOn FindingQuotes
    , tipViewProps: tipProps
    , findingRidesAgain: state.props.findingRidesAgain
    , progress: state.props.findingQuotesProgress
    , appConfig: state.data.config
    , vehicleVariant: vehicleVariant
    , city: state.props.city
    , customerTipArray: tipProps.customerTipArray
    , customerTipArrayWithValues: tipProps.customerTipArrayWithValues
    , providerSelectionStage: state.data.iopState.providerSelectionStage
    , quoteList: state.data.specialZoneQuoteList
    , selectProviderTimer: state.data.iopState.timerVal
    , selectedEstimatesObject: state.data.selectedEstimatesObject
    , showAnim: not $ state.data.iopState.showMultiProvider && isLocalStageOn FindingQuotes
    , animEndTime: state.data.currentCityConfig.iopConfig.autoSelectTime
    , isRentalSearch: isLocalStageOn ConfirmingQuotes
    , hasToll : state.data.selectedEstimatesObject.hasTollCharges
    , showBookAnyOptions : state.props.showBookAnyOptions
    , showBoostSearch : state.props.showBoostSearch
    , boostSearchEstimate : state.data.boostSearchEstimate
    , fareProductType : state.data.fareProductType
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

callSupportConfig :: ST.HomeScreenState -> PopUpModal.Config
callSupportConfig state =
  let
    config' = PopUpModal.config

    popUpConfig' =
      config'
        { gravity = CENTER
        , cornerRadius = (Corners 15.0 true true true true)
        , margin = (MarginHorizontal 16 16)
        , primaryText
          { text = getString CONTACT_SUPPORT <> "?"
          }
        , secondaryText
          { text = getString $ YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT "YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT"
          , margin = (Margin 24 12 24 12)
          , color = Color.black700
          }
        , option1
          { text = getString CANCEL_
          , background = state.data.config.popupBackground
          , strokeColor = state.data.config.primaryBackground
          , color = state.data.config.primaryBackground
          , enableRipple = true
          }
        , option2
          { text = getString CALL_SUPPORT
          , color = state.data.config.primaryTextColor
          , strokeColor = state.data.config.primaryBackground
          , background = state.data.config.primaryBackground
          , margin = (MarginLeft 12)
          , enableRipple = true
          }
        }
  in
    popUpConfig'

confirmAndBookButtonConfig :: ST.HomeScreenState -> PrimaryButton.Config
confirmAndBookButtonConfig state =
  PrimaryButton.config
    { textConfig
      { text = getBtnTextWithTimer state
      , color = Color.yellow900
      , accessibilityHint = "Confirm And Book Button"
      }
    , id = "ConfirmAndBookButton"
    , background = Color.black900
    , margin = MarginTop 16
    , enableRipple = true
    , rippleColor = Color.rippleShade
    }
  where
  getBtnTextWithTimer state =
    if state.props.currentStage == RevisedEstimate then
      (getString REQUEST_CHANGE)
    else
      if state.props.repeatRideTimer /= "0" && not DS.null state.props.repeatRideTimerId then
        ((getString REQUESTING_RIDE_IN) <> " " <> state.props.repeatRideTimer <> "s")
      else if state.props.repeatRideTimer == "0" then
        (getString REQUESTING_RIDE) <> "..."
      else
        (getString REQUEST_RIDE)

zoneTimerExpiredConfig :: ST.HomeScreenState -> PopUpModal.Config
zoneTimerExpiredConfig state =
  let
    config' = PopUpModal.config

    popUpConfig' =
      config'
        { gravity = CENTER
        , cornerRadius = Corners 16.0 true true true true
        , margin = Margin 24 32 24 0
        , primaryText
          { text = (getString OTP_EXPIRED) -- "OTP Expired"
          }
        , secondaryText
          { text = (getString OTP_EXPIRED_DESCRIPTION) --"Your ride OTP expired. Please book again to get a ride"
          , margin = Margin 16 4 16 24
          , color = Color.black700
          }
        , option1
          { visibility = false
          }
        , option2
          { text = getString OK_GOT_IT
          , margin = (MarginHorizontal 16 16)
          }
        }
  in
    popUpConfig'

menuButtonConfig :: ST.HomeScreenState -> JB.Location -> MenuButton.Config
menuButtonConfig state item =
  let
    config = MenuButton.config

    menuButtonConfig' =
      config
        { titleConfig
          { text = item.place
          , gravity = CENTER_VERTICAL
          }
        , accessibilityHint = item.place
        , radioButtonConfig
          { height = V 16
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
        , layoutBg = if item.place == state.props.defaultPickUpPoint then Color.blue600 else Color.white900
        }
  in
    menuButtonConfig'

chooseYourRideConfig :: ST.HomeScreenState -> ChooseYourRide.Config
chooseYourRideConfig state =
  let
    tipConfig = getTipConfig state.data.selectedEstimatesObject.vehicleVariant
    city = getValueToLocalStore CUSTOMER_LOCATION
    isIntercity = state.data.fareProductType == FPT.INTER_CITY
    startTimeUTC = if state.data.startTimeUTC == "" then Nothing else Just state.data.startTimeUTC
    returnTimeUTC = if state.data.returnTimeUTC == "" || state.props.searchLocationModelProps.tripType == ONE_WAY_TRIP then Nothing else Just state.data.returnTimeUTC
    roundTrip = isJust returnTimeUTC
    tipProps = getTipViewProps state.props.tipViewProps state.data.selectedEstimatesObject.vehicleVariant state.data.selectedEstimatesObject.smartTipReason state.data.selectedEstimatesObject.smartTipSuggestion
  in
    ChooseYourRide.config
      { rideDistance = state.data.rideDistance
      , rideDuration = state.data.rideDuration
      , activeIndex = state.data.selectedEstimatesObject.index
      , quoteList = if isIntercity then state.data.quoteList else state.data.specialZoneQuoteList
      , nearByDrivers = state.data.nearByDrivers
      , showPreferences = state.data.showPreferences
      , bookingPreferenceEnabled = state.data.config.estimateAndQuoteConfig.enableBookingPreference && state.props.city == Bangalore
      , flowWithoutOffers = state.props.flowWithoutOffers
      , intercity = isIntercity
      , selectedEstimateHeight = state.props.selectedEstimateHeight
      , zoneType = state.props.zoneType.sourceTag
      , tipViewProps = tipProps
      , tipForDriver = state.props.customerTip.tipForDriver
      , customerTipArray = tipProps.customerTipArray
      , customerTipArrayWithValues = tipProps.customerTipArrayWithValues
      , enableTips = not isIntercity && state.data.config.tipsEnabled && (elem city state.data.config.tipEnabledCities) && (DA.length tipConfig.customerTipArray) > 0 && not state.data.iopState.showMultiProvider
      , currentEstimateHeight = state.props.currentEstimateHeight
      , fareProductType = state.data.fareProductType
      , showMultiProvider = state.data.iopState.showMultiProvider
      , startTimeUTC = startTimeUTC
      , returnTimeUTC =  returnTimeUTC
      , roundTrip = roundTrip
      }

specialLocationConfig :: String -> String -> Boolean -> PolylineAnimationConfig -> JB.MapRouteConfig
specialLocationConfig srcIcon destIcon isAnim animConfig =
  JB.mapRouteConfig
    { sourceSpecialTagIcon = srcIcon
    , destSpecialTagIcon = destIcon
    , vehicleSizeTagIcon = HU.getVehicleSize unit
    , isAnimation = isAnim
    , autoZoom = true
    , polylineAnimationConfig = animConfig
    }

getTipViewData :: String -> Maybe TipViewData
getTipViewData dummy = case runExcept (decodeJSON (getValueToLocalStore TIP_VIEW_DATA) :: _ TipViewData) of
  Right res -> Just res
  Left err -> Nothing

requestInfoCardConfig :: ST.HomeScreenState -> RequestInfoCard.Config
requestInfoCardConfig _ =
  let
    config = RequestInfoCard.config

    requestInfoCardConfig' =
      config
        { title
          { text = getString CHOOSE_BETWEEN_MULTIPLE_RIDES
          }
        , primaryText
          { text = getString ENABLE_THIS_FEATURE_TO_CHOOSE_YOUR_RIDE
          }
        , imageConfig
          { imageUrl = fetchImage FF_ASSET "ny_ic_select_offer"
          , height = V 122
          , width = V 116
          , visibility = VISIBLE
          }
        , buttonConfig
          { text = getString GOT_IT
          }
        , backgroundColor = Color.transparent
        , gravity = RIGHT
        , padding = PaddingLeft 16
        }
  in
    requestInfoCardConfig'

multipleProvidersInfo :: ST.HomeScreenState -> RequestInfoCard.Config
multipleProvidersInfo _ =
  let
    config = RequestInfoCard.config

    requestInfoCardConfig' =
      config
        { title
          { text = getString CHOOSE_BETWEEN_PROVIDERS
          }
        , primaryText
          { text = getString CHOOSE_BETWEEN_PROVIDERS_DESC
          }
        , imageConfig
          { imageUrl = fetchImage FF_ASSET "ny_ic_multiple_providers"
          , height = V 122
          , width = V 116
          }
        , buttonConfig
          { text = getString GOT_IT
          }
        , backgroundColor = Color.transparent
        , gravity = RIGHT
        , padding = PaddingLeft 16
        }
  in
    requestInfoCardConfig'

reportIssueOptions :: ST.HomeScreenState -> Array OptionButtonList -- need to modify
reportIssueOptions state =
  [ { reasonCode: "DRIVER_WAS_NOT_READY_TO_GO"
    , description: getString DRIVER_WAS_NOT_READY_TO_GO
    , textBoxRequired: false
    , subtext: Nothing
    }
  , { reasonCode: "ASKING_FOR_MORE_MONEY"
    , description: getString ASKING_FOR_MORE_MONEY
    , textBoxRequired: false
    , subtext: Nothing
    }
  , { reasonCode: "AUTO_BROKEN"
    , description: getString VEHICLE_BROKEN
    , textBoxRequired: false
    , subtext: Nothing
    }
  , { reasonCode: "OTHER"
    , description: getString OTHER
    , textBoxRequired: false
    , subtext: Nothing
    }
  ]

sourceToDestinationConfig :: ST.HomeScreenState -> SourceToDestination.Config
sourceToDestinationConfig state =
  let
    config = SourceToDestination.config

    sourceToDestinationConfig' =
      config
        { sourceImageConfig
          { imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_source_dot"
          , margin = MarginTop 3
          , width = V 18
          , height = V 18
          }
        , sourceTextConfig
          { text = getTripTitle state.data.source
          , padding = Padding 2 0 2 2
          , margin = MarginHorizontal 12 15
          , color = Color.black800
          , ellipsize = true
          , maxLines = 1
          , textStyle = Body1
          }
        , rideStartedAtConfig
          { text = getTripSubTitle state.data.source
          , color = Color.black700
          , visibility = VISIBLE
          , padding = Padding 2 0 2 2
          , margin = MarginHorizontal 12 15
          , maxLines = 1
          , ellipsize = true
          }
        , rideEndedAtConfig
          { text = getTripSubTitle state.data.destination
          , color = Color.black700
          , visibility = VISIBLE
          , padding = Padding 2 0 2 2
          , margin = MarginHorizontal 12 15
          , maxLines = 1
          , ellipsize = true
          }
        , destinationImageConfig
          { imageUrl = fetchImage FF_COMMON_ASSET "ny_ic_destination"
          , margin = MarginTop 3
          , width = V 20
          , height = V 23
          }
        , destinationTextConfig
          { text = getTripTitle state.data.destination
          , padding = Padding 2 0 2 2
          , margin = MarginHorizontal 12 15
          , color = Color.black800
          , ellipsize = true
          , maxLines = 1
          , textStyle = Body1
          }
        , horizontalSeperatorConfig
          { visibility = VISIBLE
          , background = Color.grey900
          , padding = Padding 2 0 2 2
          , margin = Margin 12 12 15 9
          }
        }
  in
    sourceToDestinationConfig'
  where
  getTripTitle :: String -> String
  getTripTitle destination = maybe "" identity $ DA.head $ DS.split (DS.Pattern ",") destination

  getTripSubTitle :: String -> String
  getTripSubTitle destination = (DS.drop ((fromMaybe 0 (DS.indexOf (DS.Pattern ",") (destination))) + 2) (destination))

chooseVehicleConfig :: ST.HomeScreenState -> ChooseVehicle.Config
chooseVehicleConfig state = let
  config = ChooseVehicle.config
  selectedEstimates = state.data.selectedEstimatesObject
  chooseVehicleConfig' = config
    { vehicleImage = HU.getVehicleVariantImage selectedEstimates.vehicleVariant RIGHT_VIEW
    , isSelected = true
    , vehicleVariant = selectedEstimates.vehicleVariant
    , vehicleType = selectedEstimates.vehicleType
    , capacity = selectedEstimates.capacity
    , price = selectedEstimates.price
    , isCheckBox = false
    , isEnabled = true
    , index = selectedEstimates.index
    , activeIndex = selectedEstimates.activeIndex
    , id = selectedEstimates.id
    , maxPrice = selectedEstimates.maxPrice
    , basePrice = selectedEstimates.basePrice
    , showInfo = selectedEstimates.showInfo
    , searchResultType = selectedEstimates.searchResultType
    , isBookingOption = false
    , pickUpCharges = selectedEstimates.pickUpCharges 
    , layoutMargin = Margin 0 0 0 0
    , tollCharge = selectedEstimates.tollCharge
    , serviceTierName = selectedEstimates.serviceTierName
    , serviceTierShortDesc = selectedEstimates.serviceTierShortDesc
    , airConditioned = selectedEstimates.airConditioned
    , extraFare = selectedEstimates.extraFare
    , fareInfoDescription = selectedEstimates.fareInfoDescription
    , isNightShift = selectedEstimates.isNightShift
    , nightChargeTill = selectedEstimates.nightChargeTill
    , nightChargeFrom = selectedEstimates.nightChargeFrom
    , driverAdditions = selectedEstimates.driverAdditions
    , showEditButton = true
    , editBtnText = getString CHANGE
    , validTill = selectedEstimates.validTill
    , hasTollCharges = selectedEstimates.hasTollCharges
    , hasParkingCharges = selectedEstimates.hasParkingCharges
    }
  in chooseVehicleConfig'

getFareUpdatedStr :: Int -> Boolean -> String
getFareUpdatedStr diffInDist waitingChargeApplied = do
  let
    shorter = diffInDist > 0

    positiveDist = if shorter then diffInDist else -diffInDist

    distInKm = parseFloat (toNumber positiveDist / 1000.0) 2

    distanceChanged = diffInDist /= 0
  case waitingChargeApplied, distanceChanged of
    true, false -> getString FARE_UPDATED_WITH_CHARGES
    false, true -> getVarString (if shorter then FARE_UPDATED_WITH_SHORTER_DIST else FARE_UPDATED_WITH_LONGER_DIST) [ distInKm ]
    true, true -> getVarString (if shorter then FARE_UPDATED_WITH_CHARGES_SHORTER_DIST else FARE_UPDATED_WITH_CHARGES_LONGER_DIST) [ distInKm ]
    false, false -> getString FARE_HAS_BEEN_UPDATED
    
getCarouselData :: ST.HomeScreenState -> Array CarouselData
getCarouselData state =
  map
    ( \item ->
        { imageConfig: { image: item.image, height: item.imageHeight, width: 200, bgColor: item.imageBgColor, cornerRadius: 8.0, isUrl: false }
        , youtubeConfig: EHC.getYoutubeData { videoId = item.videoLink, videoType = "PORTRAIT_VIDEO", videoHeight = item.videoHeight }
        , contentType: if item.videoLink == "" then "IMAGE" else "VIDEO"
        , gravity: item.gravity
        , backgroundColor: item.carouselBgColor
        , titleConfig:
            { text: item.title
            , textSize: 16
            , textColor: Color.black800
            , gravity: "CENTER"
            , margin: { top: 16, bottom: 0, right: 16, left: 16 }
            }
        , descriptionConfig:
            { text: item.description
            , textSize: item.descTextSize
            , textColor: Color.black700
            , gravity: "LEFT"
            , margin: { top: 0, bottom: 0, right: 16, left: 16 }
            }
        }
    )
    [ { image: "carousel_4", videoLink: (EHC.getVideoID state.data.config.purpleRideConfig.genericVideoUrl), videoHeight: 690, imageHeight: 160, imageBgColor: Color.grey700, title: (getString EDUCATIONAL_POP_UP_SLIDE_1_TITLE), description: (getString EDUCATIONAL_POP_UP_SLIDE_1_SUBTITLE), descTextSize: 14, carouselBgColor: Color.grey700, gravity: 0 }
    , { image: "ny_ic_blind_pickup", videoLink: "", videoHeight: 0, imageHeight: 160, imageBgColor: Color.blue600, title: (getString EDUCATIONAL_POP_UP_SLIDE_2_TITLE), description: (getString EDUCATIONAL_POP_UP_SLIDE_2_SUBTITLE), descTextSize: 12, carouselBgColor: Color.grey700, gravity: 0 }
    , { image: "ny_ic_deaf_pickup", videoLink: "", videoHeight: 0, imageHeight: 160, imageBgColor: Color.blue600, title: (getString EDUCATIONAL_POP_UP_SLIDE_3_TITLE), description: (getString EDUCATIONAL_POP_UP_SLIDE_3_SUBTITLE), descTextSize: 12, carouselBgColor: Color.grey700, gravity: 0 }
    , { image: "ny_ic_locomotor_arrival", videoLink: "", videoHeight: 0, imageHeight: 160, imageBgColor: Color.blue600, title: (getString EDUCATIONAL_POP_UP_SLIDE_4_TITLE), description: (getString EDUCATIONAL_POP_UP_SLIDE_4_SUBTITLE), descTextSize: 12, carouselBgColor: Color.grey700, gravity: 0 }
    , { image: "ny_ic_disability_illustration", videoLink: "", videoHeight: 0, imageHeight: 160, imageBgColor: Color.white900, title: (getString EDUCATIONAL_POP_UP_SLIDE_5_TITLE), description: (getString EDUCATIONAL_POP_UP_SLIDE_5_SUBTITLE), descTextSize: 12, carouselBgColor: Color.grey700, gravity: 0 }
    ]

safetyIssueOptions :: Boolean -> Array OptionButtonList
safetyIssueOptions forceEnglish =
  let
    getValue str = if forceEnglish then getEN str else getString str
  in
    [ { reasonCode: "DRIVER_BEHAVED_INAPPROPRIATELY"
      , description: getValue DRIVER_BEHAVED_INAPPROPRIATELY
      , textBoxRequired: false
      , subtext: Nothing
      }
    , { reasonCode: "I_DID_NOT_FEEL_SAFE"
      , description: getValue I_DID_NOT_FEEL_SAFE
      , textBoxRequired: false
      , subtext: Nothing
      }
    , { reasonCode: "OTHER"
      , description: getValue OTHER
      , textBoxRequired: false
      , subtext: Nothing
      }
    ]

getChatSuggestions :: ST.HomeScreenState -> Array String
getChatSuggestions state = do
  let
    didReceiverMessage = HU.didReceiverMessage FunctionCall

    lastMessage = DA.last state.data.messages

    currentUser = if state.props.isChatWithEMEnabled then (getValueFromCache (show CUSTOMER_ID) JB.getKeyInSharedPrefKeys) else "Customer"

    canShowSuggestions = case lastMessage of
      Just value -> (value.sentBy /= currentUser) || ((not state.props.isChatWithEMEnabled) && not didReceiverMessage)
      Nothing -> true

    isAtPickup = (metersToKm state.data.driverInfoCardState.distance (state.props.currentStage == RideStarted)) == getString AT_PICKUP
  if (DA.null state.data.chatSuggestionsList) && canShowSuggestions && state.props.canSendSuggestion then
    if state.data.fareProductType ==  FPT.RENTAL && state.props.stageBeforeChatScreen == RideStarted then getSuggestionsfromKey chatSuggestion "csRentalInitial"
    else if state.props.isChatWithEMEnabled then do
      let
        hideInitial = not $ DA.null state.data.messages
      if didReceiverMessage && hideInitial then
        getSuggestionsfromKey emChatSuggestion emergencyContactInitialChatSuggestionId
      else if hideInitial then
        state.data.chatSuggestionsList
      else
        getSuggestionsfromKey emChatSuggestion emergencyContactInitialChatSuggestionId
    else if didReceiverMessage && (not $ DA.null state.data.messages) then
      if isAtPickup then getSuggestionsfromKey chatSuggestion "customerDefaultAP" else getSuggestionsfromKey chatSuggestion "customerDefaultBP"
    else if isAtPickup then
      getSuggestionsfromKey chatSuggestion "customerInitialAP"
    else do
      let
        hideInitial = not (DA.null state.data.messages) && not didReceiverMessage
      if (DA.null state.data.messages) && (EHC.getExpiryTime state.data.driverInfoCardState.createdAt true) > 30 then
        getSuggestionsfromKey chatSuggestion "customerInitialBP" --"customerInitialBP3" --TODO Revert during suggestions update
      else if hideInitial then
        getSuggestionsfromKey chatSuggestion "customerInitialBP" --"customerInitialBP2" --TODO Revert during suggestions update
      else
        getSuggestionsfromKey chatSuggestion "customerInitialBP" --"customerInitialBP1" --TODO Revert during suggestions update
  else
    state.data.chatSuggestionsList

locationTagBarConfig :: ST.HomeScreenState -> LocationTagBar.LocationTagBarConfig
locationTagBarConfig state =
  let
    locTagList =
      map
        ( \item ->
            { imageConfig:
                { height: V 32
                , width: V 32
                , imageWithFallback: fetchImage FF_ASSET item.image
                , margin: MarginRight 8
                }
            , textConfig:
                { text: item.text
                , fontStyle: FontStyle.SubHeading1
                , fontSize: FontSize.a_14
                , color: Color.black800
                }
            , stroke: "0," <> Color.grey700
            , cornerRadius: Corners 19.0 true true true true
            , background: item.background
            , height: WRAP_CONTENT
            , width: WRAP_CONTENT
            , padding: PaddingVertical 16 16
            , orientation: HORIZONTAL
            , enableRipple: true
            , rippleColor: Color.rippleShade
            , bannerConfig:
                { text: getString COMING_SOON
                , color: Color.white900
                , fontStyle: FontStyle.Body12
                , textSize: FontSize.a_12
                , cornerRadii: Corners 6.0 false false true true
                , background: Color.blue800
                }
            , showBanner: item.showBanner
            , id: item.id
            }
        )
        ( [ { image: "ny_ic_instant", text: (getString INSTANT), id: "INSTANT", background: Color.lightMintGreen, showBanner: GONE }
         , { image: "ny_ic_rental", text: (getString RENTALS_), id: "RENTALS", background: Color.moonCreme, showBanner: GONE }
          ]
            <> (if state.data.currentCityConfig.enableIntercity then [ { image: "ny_ic_intercity", text: (getString INTER_CITY_), id: "INTER_CITY", background: Color.blue600', showBanner: GONE } ] else [])
            <> ([{image: "ny_ic_delivery", text: (getString DELIVERY_STR), id: "DELIVERY", background: Color.seashell, showBanner: GONE }])
            <> if state.data.currentCityConfig.enableIntercityBus then [ { image: "ny_ic_intercity_bus", text: getString INTERCITY_BUS, id: "INTERCITY_BUS", background: Color.blue600', showBanner: GONE } ] else []
            <> ([{image: "ny_ic_bus_icon", text: "Bus", id: "BUS", background: Color.amber, showBanner: GONE }])
            <> ([{image: "ny_ic_ambulance", text: "Ambulance" , id: "AMBULANCE_SERVICE" , background: "#fdf3ec" , showBanner: GONE}])
        )
  in
    { tagList: locTagList }

safetyAlertConfig :: ST.HomeScreenState -> PopUpModal.Config
safetyAlertConfig state =
  let
    config' = PopUpModal.config

    alertData = getSafetyAlertData state

    popUpConfig' =
      config'
        { dismissPopup = true
        , optionButtonOrientation = "VERTICAL"
        , buttonLayoutMargin = Margin 24 0 24 20
        , gravity = CENTER
        , margin = MarginHorizontal 20 20
        , primaryText
          { text = getString EVERYTHING_OKAY_Q
          , margin = Margin 16 0 16 10
          }
        , secondaryText
          { text = alertData.text
          , margin = MarginHorizontal 16 16
          }
        , option1
          { text = getString I_FEEL_SAFE
          , color = Color.yellow900
          , background = Color.black900
          , strokeColor = Color.transparent
          , width = MATCH_PARENT
          , margin = MarginVertical 20 10
          }
        , option2
          { text = getString I_NEED_HELP
          , color = Color.black700
          , background = Color.white900
          , width = MATCH_PARENT
          , margin = MarginBottom 10
          }
        , cornerRadius = Corners 15.0 true true true true
        , coverImageConfig
          { imageUrl = HU.fetchImage HU.FF_ASSET alertData.image
          , visibility = VISIBLE
          , margin = Margin 16 16 16 16
          , width = MATCH_PARENT
          , height = V 225
          }
        }
  in
    popUpConfig'

getSafetyAlertData :: ST.HomeScreenState -> { text :: String, image :: String }
getSafetyAlertData state = case state.props.safetyAlertType of
  Just ST.DEVIATION -> { text: getString WE_NOTICED_YOUR_RIDE_IS_ON_DIFFERENT_ROUTE, image: if state.data.driverInfoCardState.vehicleVariant /= "AUTO_RICKSHAW" then "ny_ic_safety_alert_deroute_cab" else "ny_ic_safety_alert_deroute_auto" }
  Just ST.STATIONARY_VEHICLE -> { text: getString WE_NOTICED_YOUR_RIDE_HASNT_MOVED, image: if state.data.driverInfoCardState.vehicleVariant /= "AUTO_RICKSHAW" then "ny_ic_safety_alert_stationary_cab" else "ny_ic_safety_alert_stationary_auto" }
  _ -> { text: "", image: "" }

shareRideConfig :: ST.HomeScreenState -> PopupWithCheckboxController.Config
shareRideConfig state =
  let
    config = PopupWithCheckboxController.config

    appName = fromMaybe state.data.config.appData.name $ runFn3 getAnyFromWindow "appName" Nothing Just
    contactList = DA.filter (\item -> item.shareTripWithEmergencyContactOption.key /= API.NEVER_SHARE) $ fromMaybe [] state.data.contactList
    trustedContactsNotSetup = DA.null contactList
    shareRideConfig' =
      config
        { title = getString SHARE_RIDE
        , description = getString if trustedContactsNotSetup then TRACKING_NO_SETUP else AUTOMATIC_LIVE_TRACKING_DESC
        , primaryOptionTitle = getString AUTOMATIC_LIVE_TRACKING
        , contactList = contactList
        , primaryButtonConfig = shareRideButtonConfig state trustedContactsNotSetup
        , secondaryOption {
            buttonConfig = shareLinkButtonConfig state,
            title = getString MANUAL_LIVE_TRACKING,
            description = getString MANUAL_LIVE_TRACKING_DESC,
            visibility = true,
            background = Color.blue600,
            padding = Padding 12 12 12 12,
            margin = Margin 16 16 16 16
          }
        , primaryOptionBackground = Color.blue600
        }
  in
    shareRideConfig'

shareRideButtonConfig :: ST.HomeScreenState -> Boolean -> PrimaryButton.Config
shareRideButtonConfig state trustedContactsNotSetup =
  PrimaryButton.config
    { textConfig
      { text = getString SETUP_NOW
      , accessibilityHint = "Setup Now Button"
      , color = Color.blue800
      }
    , background = Color.transparent
    , stroke = "1," <> Color.blue800
    , id = "SetupSafetyButton"
    , margin = MarginTop 20
    , visibility = boolToVisibility trustedContactsNotSetup
    }
  where
  numberOfSelectedContacts = DA.length $ DA.filter (\contact -> contact.isSelected) $ fromMaybe [] state.data.contactList

shareLinkButtonConfig :: ST.HomeScreenState -> PrimaryButton.Config
shareLinkButtonConfig state =
  PrimaryButton.config
    { textConfig
      { text = getString SHARE_LINK
      , accessibilityHint = "Share Ride Link Button"
      }
    , id = "ShareRideLinkButton"
    , margin = MarginTop 20
    , prefixImageConfig {
        imageUrl = HU.fetchImage HU.COMMON_ASSET "ny_ic_share_yellow"
      , margin = MarginRight 8
      }
    , isPrefixImage = true
    }

referralPopUpConfig :: ST.HomeScreenState -> ST.ReferralComponentState
referralPopUpConfig state =
  let
    stage' = case state.props.referral.referralStatus of
      REFERRAL_APPLIED -> ST.APPLIED_POPUP
      REFERRAL_ALREADY_APPLIED -> ST.ALREADY_APPLIED_POPUP
      NO_REFERRAL
        | state.props.referral.showAddReferralPopup -> ST.ENTER_REFERRAL_CODE
      _ -> ST.INVALID_POPUP
  in
    state.props.referralComponentProps { stage = stage' }

specialZoneInfoPopupConfig :: ST.SpecialZoneInfoPopUp -> RequestInfoCard.Config
specialZoneInfoPopupConfig infoConfig =
  let
    config = RequestInfoCard.config

    specialZonePopupConfig =
      config
        { title
          { text = infoConfig.title
          }
        , primaryText
          { text = infoConfig.primaryText
          , padding = Padding 16 16 0 0
          , color = Color.black700
          }
        , secondaryText
          { text = infoConfig.secondaryText
          , visibility = VISIBLE
          , padding = PaddingLeft 16
          , color = Color.black700
          , textStyle = FontStyle.ParagraphText
          , width = (V $ JB.getWidthFromPercent 75)
          }
        , imageConfig
          { imageUrl = fetchImage FF_COMMON_ASSET infoConfig.icon
          , height = V 130
          , width = V 130
          , padding = Padding 0 2 2 0
          }
        , buttonConfig
          { text = infoConfig.primaryButtonText
          , padding = PaddingVertical 16 20
          }
        }
  in
    specialZonePopupConfig

generateReferralLink :: String -> String -> String -> String -> String -> String
generateReferralLink source medium term content campaign =
  let
    config = getAppConfig appConfig

    cityConfig = EHU.getCityFromString $ getValueToLocalStore CUSTOMER_LOCATION

    path = "link/rider/SJ8D"

    packageId = config.referral.customerAppId

    domain = config.referral.domain
  in
    domain <> path <> "?referrer="
      -- <> "utm_source%3D" <> source 
      
      -- <> "%26utm_medium%3D" <> medium 
      
      -- <> "%26utm_term%3D" <> term 
      
      -- <> "%26utm_content%3D" <> content 
      
      <> "utm_campaign%3D"
      <> campaign

-- <> "%26anid%3Dadmob&id=" <> packageId
-- TODO: Add the above query params when needed
acWorkingPopupConfig :: ST.HomeScreenState -> PopUpModal.Config
acWorkingPopupConfig state =
  let
    config = PopUpModal.config

    isAcCabRide = ServiceTierCard.showACDetails (fromMaybe "" state.data.driverInfoCardState.serviceTierName) Nothing state.data.fareProductType

    primaryText_ =
      getString
        $ if isAcCabRide then
            HAS_YOUR_DRIVER_SET_THE_AC_AS_PER_YOUR_PREFERENCE
          else
            ENJOY_YOUR_BUDGET_FRIENDLY_NON_AC_RIDE

    optionOneText =
      getString
        $ if isAcCabRide then
            YES
          else
            OK_GOT_IT

    imageName = if isAcCabRide then "ny_ic_ac_preference_popup" else "ny_ic_non_ac_ride_popup"

    acWorkingPopupConfig' =
      config
        { dismissPopup = true
        , optionButtonOrientation =
          if isAcCabRide then
            "VERTICAL"
          else
            "HORIZONTAL"
        , buttonLayoutMargin = Margin 24 0 24 20
        , gravity = CENTER
        , margin = MarginHorizontal 20 20
        , backgroundClickable = true
        , primaryText
          { text = primaryText_
          , margin = MarginTop 0
          }
        , secondaryText
          { visibility = GONE
          }
        , option1
          { text = optionOneText
          , color = Color.yellow900
          , background = Color.black900
          , strokeColor = Color.transparent
          , width = MATCH_PARENT
          , margin = MarginVertical 20 10
          }
        , option2
          { text = getString NO_REPORT_AN_ISSUE
          , color = Color.black700
          , background = Color.white900
          , width = MATCH_PARENT
          , margin = MarginBottom 10
          , visibility = isAcCabRide
          }
        , cornerRadius = Corners 15.0 true true true true
        , coverImageConfig
          { imageUrl = HU.fetchImage HU.FF_ASSET imageName
          , visibility = VISIBLE
          , margin = Margin 16 16 16 0
          , width = MATCH_PARENT
          , height = V 225
          }
        }
  in
    acWorkingPopupConfig'

rentalInfoViewConfig :: ST.HomeScreenState -> PopUpModal.Config
rentalInfoViewConfig state =
  let
    config' = PopUpModal.config

    popUpConfig' =
      config'
        { gravity = CENTER
        , cornerRadius = Corners 16.0 true true true true
        , margin = Margin 24 32 24 0
        , primaryText
          { text = "Rental Ride Info"
          }
        , secondaryText
          { text = "Please verify the odometer reading before sharing end ride otp"
          , margin = Margin 16 4 16 24
          , color = Color.black700
          }
        , option1
          { visibility = false
          }
        , option2
          { text = getString OK_GOT_IT
          , margin = (MarginHorizontal 16 16)
          }
        }
  in
    popUpConfig'

intercityInSpecialZonePopupConfig :: ST.HomeScreenState -> PopUpModal.Config
intercityInSpecialZonePopupConfig state =
  let
    config' = PopUpModal.config

    popUpConfig' =
      config'
        { gravity = CENTER
        , margin = (MarginHorizontal 16 16)
        , buttonLayoutMargin = (Margin 0 16 16 0)
        , editTextVisibility = GONE
        , dismissPopupConfig
          { visibility = GONE
          }
        , primaryText
          { text = (getString LOCATION_UNSERVICEABLE)
          , gravity = CENTER
          , margin = MarginTop 16
          }
        , secondaryText
          { text = getString $ if state.props.showNormalRideNotSchedulablePopUp then SCHEDULING_ALLOWED_IN_INTERCITY_RENTAL else SPECIAL_ZONE_INTERCITY_INELIGIBLE
          , margin = MarginTop 4
          }
        , option1
          { visibility = false
          }
        , option2
          { text = (getString GOT_IT)
          , padding = (Padding 16 0 16 0)
          }
        , cornerRadius = (Corners 15.0 true true true true)
        , coverImageConfig
          { imageUrl = HU.fetchImage HU.FF_ASSET "ny_ic_unavailable"
          , visibility = VISIBLE
          , margin = Margin 16 16 16 24
          , width = MATCH_PARENT
          , height = V 200
          }
        }
  in
    popUpConfig'

scheduledRideExistsPopUpConfig :: ST.HomeScreenState -> PopUpModal.Config
scheduledRideExistsPopUpConfig state =
  let
    config' = PopUpModal.config

    popUpConfig' =
      config'
        { gravity = CENTER
        , margin = (MarginHorizontal 16 16)
        , buttonLayoutMargin = (Margin 0 16 16 0)
        , editTextVisibility = GONE
        , dismissPopupConfig
          { visibility = GONE
          }
        , primaryText
          { text = (getString A_RIDE_ALREADY_EXISTS)
          , gravity = CENTER
          , margin = MarginTop 16
          }
        , secondaryText
          { text = (invalidScheduledBookingDetails state)
          , margin = MarginTop 4
          }
        , option1
          { visibility = false
          }
        , option2
          { text = (getString GOT_IT)
          , padding = (Padding 16 0 16 0)
          }
        , cornerRadius = (Corners 15.0 true true true true)
        , coverImageConfig
          { imageUrl = HU.fetchImage FF_COMMON_ASSET "ny_ic_time_conflicts"
          , visibility = VISIBLE
          , margin = Margin 16 16 16 24
          , width = MATCH_PARENT
          , height = V 200
          }
        }
  in
    popUpConfig'
  where
  invalidScheduledBookingDetails :: ST.HomeScreenState -> String
  invalidScheduledBookingDetails state = 
    case state.data.overLappingBooking of
      Just (API.RideBookingRes overLappingBooking) ->
        let 
          (API.RideBookingAPIDetails details) = overLappingBooking.bookingDetails
          (API.RideBookingDetails contents) = details.contents
          (API.BookingLocationAPIEntity fromLocationResp) = overLappingBooking.fromLocation
          stopLocation = fromMaybe dummyBookingDetails (case details.fareProductType of 
                                                            "RENTAL" -> contents.stopLocation
                                                            "INTER_CITY" -> contents.toLocation
                                                            _ -> contents.toLocation)          
          rideType = case details.fareProductType of 
                              "INTER_CITY" -> "intercity"
                              "RENTAL" -> "rental"
                              _ -> "one way"
          destinationNotGiven =  (details.fareProductType == "RENTAL" && (isNothing contents.stopLocation))
          rideScheduledTime = fromMaybe "" overLappingBooking.rideScheduledTime
          rideEndTime = HU.calculateBookingEndTime (API.RideBookingRes overLappingBooking)
          fromLocation = HU.fetchAddressDetails  overLappingBooking.fromLocation
          toLocation = HU.fetchAddressDetails stopLocation
        in
          if destinationNotGiven then getVarString YOU_HAVE_AN_RIDE_FROM_WITHOUT_TO [rideType , fromLocation , formatDateInHHMM rideScheduledTime , formatDateInHHMM rideEndTime]
                                 else getVarString YOU_HAVE_AN_RIDE_FROM_TO_SCHEDULED_FROM_TILL [ rideType, fromLocation, toLocation, formatDateInHHMM rideScheduledTime,formatDateInHHMM rideEndTime ]
      Nothing -> ""


  textDetails :: ST.BookingTime -> String
  textDetails bookingDetails =
    let
      invalidBookingPopUpConfig = fromMaybe dummyInvalidBookingPopUpConfig state.data.invalidBookingPopUpConfig
      rideType = if invalidBookingPopUpConfig.fareProductType == FPT.RENTAL then "rental" else "intercity"
      rideEndTime = formatDateInHHMM $ EHC.getUTCAfterNSeconds invalidBookingPopUpConfig.rideScheduledTime $ (invalidBookingPopUpConfig.maxEstimatedDuration + 30) * 60
    in
      getVarString YOU_HAVE_AN_RIDE_FROM_TO_SCHEDULED_FROM_TILL [ rideType, invalidBookingPopUpConfig.fromLocation, invalidBookingPopUpConfig.toLocation, formatDateInHHMM invalidBookingPopUpConfig.rideScheduledTime, rideEndTime ]

  formatDateInHHMM :: String -> String
  formatDateInHHMM timeUTC = EHC.convertUTCtoISC timeUTC "HH" <> ":" <> EHC.convertUTCtoISC timeUTC "mm"

getMarkerActionImageConifg :: ST.HomeScreenState -> Boolean -> JB.ActionImageConfig 
getMarkerActionImageConifg state driverWithinPickupThreshold = do
  let isHybridFlowParcel = state.data.fareProductType == FPT.DELIVERY && HU.isParentView FunctionCall
      conditionForPickupImage = any (\stage -> isLocalStageOn stage) [ RideAccepted, ChatWithDriver] && state.data.config.feature.enableEditPickupLocation && driverWithinPickupThreshold && not isHybridFlowParcel
      conditionForDestinationImage = isLocalStageOn RideStarted && state.data.config.feature.enableEditDestination && (not state.props.isOtpRideFlow) && not (DA.any (_ == state.data.fareProductType) [FPT.RENTAL, FPT.ONE_WAY_SPECIAL_ZONE, FPT.INTER_CITY]) && not isHybridFlowParcel
      imageName = if conditionForPickupImage then "ic_edit_pencil_white" else if conditionForDestinationImage then "ic_pencil_blue" else ""
      bgForPickupMarker = if EHC.os == "IOS" then Color.blue800 else "ic_blue_bg"
      bgForDestMarker = if EHC.os == "IOS" then Color.blue600 else "ic_blue600_bg"
      backgroundColor = if conditionForPickupImage then bgForPickupMarker else if conditionForDestinationImage then bgForDestMarker else ""
  if conditionForPickupImage || conditionForDestinationImage then
    JB.defaultActionImageConfig {
      image = imageName,
      height = if EHC.os == "IOS" then 30 else 28,
      width = if EHC.os == "IOS" then 30 else 28,
      background = backgroundColor,
      orientation = "HORIZONTAL",
      padding = if EHC.os == "IOS" then {left : 12, top : 12, right : 12, bottom : 12} else {left : 16, top : 16, right : 16, bottom : 16},
      layoutMargin = {left : 15, top : 13, right : 15, bottom : 15},
      layoutPadding = {left : 0, top : 0, right : 0, bottom : 0}
    }
  else JB.defaultActionImageConfig

nammaServices :: LazyCheck -> Array RemoteConfig.Service
nammaServices dummy = 
  let enabledServices = RemoteConfig.getEnabledServices $ DS.toLower $ getValueToLocalStore CUSTOMER_LOCATION
      allServices = getAllServices FunctionCall
  in DA.foldl (\acc x -> do 
                          let mbService = DA.find(\service -> (show service.type == x)) allServices
                          case mbService of 
                            Just value -> acc <> [value]
                            Nothing -> acc
              ) [] enabledServices

mapLottieConfig :: LazyCheck -> RemoteConfig.MapLottieConfig
mapLottieConfig dummy = RemoteConfig.getMapViewLottieConfig FunctionCall

getAllServices :: LazyCheck -> Array RemoteConfig.Service
getAllServices dummy = 
  let config = getAppConfig appConfig
      enableBusBooking = config.feature.enableBusBooking -- For backward Compatibility
  in
    [ {type: RemoteConfig.INSTANT, image: fetchImage COMMON_ASSET "ny_ic_instant_new", name: INSTANT, backgroundColor: "#f2f9f3" , preferredEstimateOrder : [], secondaryPillColor : "#f2f9f3", hasSecondaryPill: false}
    , {type: RemoteConfig.TRANSIT, image: fetchImage COMMON_ASSET "ny_ic_transit", name: TRANSIT, backgroundColor: "#faeeee" , preferredEstimateOrder : [], secondaryPillColor : "#faeeee", hasSecondaryPill: false}
    , {type: RemoteConfig.INTERCITY, image: fetchImage COMMON_ASSET "ny_ic_intercity_service", name: INTERCITY_STR, backgroundColor: "#f1f8fe" , preferredEstimateOrder : [], secondaryPillColor : "#f1f8fe", hasSecondaryPill: false}
    , {type: RemoteConfig.RENTAL, image: fetchImage COMMON_ASSET "ny_ic_rental_service", name: RENTAL_STR, backgroundColor: "#fef9eb" , preferredEstimateOrder : [], secondaryPillColor : "#fef9eb", hasSecondaryPill: false}
    , {type: RemoteConfig.INTERCITY_BUS, image: fetchImage COMMON_ASSET "ny_ic_intercity_bus_service", name: INTERCITY_BUS, backgroundColor: "#fdf3ec" , preferredEstimateOrder : [], secondaryPillColor : "#fdf3ec", hasSecondaryPill: false}
    , {type: RemoteConfig.BIKE_TAXI, image: fetchImage COMMON_ASSET "ny_ic_bike_taxi_service", name: BIKE_TAXI, backgroundColor: "#F0FAF0" , preferredEstimateOrder : ["BIKE"], secondaryPillColor : "#F0FAF0", hasSecondaryPill: false}
    , {type: RemoteConfig.METRO, image: fetchImage COMMON_ASSET "ny_ic_metro_service", name: METRO_TICKETS, backgroundColor: "#1AE55454" , preferredEstimateOrder : [], secondaryPillColor : "#E55454", hasSecondaryPill: false}
    , {type: RemoteConfig.METRO_OFFER, image: fetchImage COMMON_ASSET "ny_ic_metro_service", name: METRO_TICKETS, backgroundColor: "#1AE55454" , preferredEstimateOrder : [], secondaryPillColor : "#E55454", hasSecondaryPill: true}
    , {type: RemoteConfig.AMBULANCE_SERVICE, image: fetchImage COMMON_ASSET "ny_ic_ambulance", name: AMBULANCE_, backgroundColor: "#fdf3ec", preferredEstimateOrder : [], secondaryPillColor : "#E55454", hasSecondaryPill: false}
    ] <> (if config.enableDeliveryService then [{type: RemoteConfig.DELIVERY, image: fetchImage COMMON_ASSET "ny_ic_delivery_service", name: DELIVERY_STR, backgroundColor: "#fef9eb", preferredEstimateOrder : [], secondaryPillColor : "#E55454", hasSecondaryPill: false}] else [])
      <> (if enableBusBooking then [{type: RemoteConfig.BUS, image: fetchImage COMMON_ASSET "ny_ic_bus_icon", name: BUS__, backgroundColor: "#FFF3EB" , preferredEstimateOrder : ["BUS"], secondaryPillColor : "#E55454", hasSecondaryPill: false}] else [])

getChatDetails :: ST.HomeScreenState -> Array NewContacts -> Array ChatContacts
getChatDetails state contacts = 
  let createContact :: Maybe NewContacts -> ChatContacts
      createContact Nothing = 
        { name : state.data.driverInfoCardState.driverName
        , number : ""
        , uuid : state.data.driverInfoCardState.bppRideId
        , recipient : CMC.DRIVER
        , enableForShareRide : false
        , contactPersonId : Nothing
        , notifiedViaFCM : Nothing
        , shareTripWithEmergencyContactOption : API.NEVER_SHARE
        }
      createContact (Just item) = 
        let channelId = if item.priority == 0 then state.data.driverInfoCardState.rideId else state.data.driverInfoCardState.rideId <> "$" <> fromMaybe "" item.contactPersonId
        in { name : item.name
            , number : item.number
            , uuid : channelId
            , recipient : CMC.USER
            , enableForShareRide : item.enableForShareRide
            , contactPersonId : item.contactPersonId
            , notifiedViaFCM : item.notifiedViaFCM
            , shareTripWithEmergencyContactOption : item.shareTripWithEmergencyContactOption.key
          }

      multiChatContacts = 
        foldl (\acc item -> maybe acc (\_ -> acc <> [createContact (Just item)]) item.contactPersonId) [] contacts

      driverChatContact = 
        if state.props.stageBeforeChatScreen == ST.RideStarted && state.data.fareProductType /= FPT.RENTAL
        then []
        else [createContact Nothing]

  in multiChatContacts <> driverChatContact

editPickupPopupOnCancel :: ST.HomeScreenState -> PopUpModal.Config
editPickupPopupOnCancel state = do
  let config = PopUpModal.config
      popUpConfig' = config {
        gravity = BOTTOM
      , cornerRadius = Corners 20.0 true true true true
      , backgroundColor =  Color.black9000
      , backgroundClickable = true
      , dismissPopup = true
      , padding = Padding 16 16 16 0
      , optionButtonOrientation = "VERTICAL"
      , primaryText {
          text = getString EDIT_YOUR_PICKUP_LOCATION_INSTEAD
        , margin = (Margin 10 20 10 10)
        , gravity = CENTER
        }
      , secondaryText {visibility = GONE}
      , option1 {
          text = getString CHANGE_PICKUP_LOCATION
        , color = Color.yellow900
        , background = Color.black
        , enableRipple = true
        , rippleColor = Color.rippleShade
        , width = MATCH_PARENT
        }
      , option2 {
          text = getString CANCEL_RIDE
        , color = Color.black700
        , background = Color.white900
        , enableRipple = true
        , rippleColor = Color.rippleShade
        , width = MATCH_PARENT
        , margin = (MarginTop 12)
        }
      , coverImageConfig {
          imageUrl = fetchImage FF_ASSET "ny_ic_edit_pickup"
        , visibility = VISIBLE
        , height = V 225
        , width = MATCH_PARENT
      }
      }
  popUpConfig'


pickupConfig :: ST.HomeScreenState -> DateSelectorController.DateSelectorConfig
pickupConfig state = 
  let pickupConfig' =  {
  baseWidth: MATCH_PARENT,
  baseHeight: WRAP_CONTENT,
  baseOrientation: VERTICAL,
  baseMargin: Margin 16 0 16 20,
  titleConfig: (getString PICKUP_DATE_AND_TIME),
  textColor: Color.black900,
  textMargin: MarginBottom 8,
  pickerHeight: WRAP_CONTENT,
  pickerWidth: MATCH_PARENT,
  pickerCornerRadius: 8.0,
  pickerBackground: Color.white900,
  pickerPadding: Padding 20 15 20 15,
  selectDateText: case state.data.tripTypeDataConfig.tripPickupData of 
                   Just obj ->  EHC.convertUTCtoISC obj.tripDateUTC "DD MMM YYYY"
                   Nothing -> (getString PICKUP_INPUT),
  selectTimeText : case state.data.tripTypeDataConfig.tripPickupData of 
                  Just obj ->  EHC.convertUTCtoISC obj.tripDateUTC "h:mm A"
                  Nothing -> (getString PICKUP_INPUT),
  dateHeight: WRAP_CONTENT,
  dateWidth: WRAP_CONTENT,
  dateColor: Color.black800,
  iconHeight: V 22,
  iconWidth: V 22,
  iconMargin: MarginLeft 8,
  iconGravity: BOTTOM,
  id : "Pickup",
  radioButtonViewVisibilty : true,
  returnTextViewVisibilty : false,
  isEnabled : state.props.isTripSchedulable,
  radioButtonTextConfig : {
    primaryText : (getString LEAVE_NOW),
    primaryTextAccessibilityHint : (getEN LEAVE_NOW),
    secondaryText : (getString RESERVE),
    secondaryTextAccessibilityHint : (getEN RESERVE)
  },
  dateIconImage : "ny_ic_calendar",
  timeIconImage : "ny_ic_clock_unfilled"
}
  in pickupConfig'

returnConfig :: ST.HomeScreenState -> DateSelectorController.DateSelectorConfig
returnConfig state =
  
  let 
    srcCity = fromMaybe (show state.props.city) state.data.sourceAddress.city 
    returnConfig' =  {
    baseWidth: MATCH_PARENT,
    baseHeight: WRAP_CONTENT,
    baseOrientation: VERTICAL,
    baseMargin: Margin 16 8 16 20,
    titleConfig: (getString $ DROP_BACK_IN_AT srcCity),
    textColor: Color.black900,
    textMargin: MarginBottom 8,
    pickerHeight: WRAP_CONTENT,
    pickerWidth: MATCH_PARENT,
    pickerCornerRadius: 8.0,
    pickerBackground: Color.white900,
    pickerPadding: Padding 20 15 20 15,
    selectDateText: case state.data.tripTypeDataConfig.tripReturnData of 
                   Just obj ->  EHC.convertUTCtoISC obj.tripDateUTC "DD MMM YYYY"
                   Nothing -> (getString PICKUP_INPUT),
    selectTimeText : case state.data.tripTypeDataConfig.tripReturnData of 
                  Just obj ->  EHC.convertUTCtoISC obj.tripDateUTC "h:mm A"
                  Nothing -> (getString PICKUP_INPUT),
    dateHeight: WRAP_CONTENT,
    dateWidth: WRAP_CONTENT,
    dateColor: Color.black800,
    iconHeight: V 22,
    iconWidth: V 22,
    iconMargin: MarginLeft 8,
    iconGravity: BOTTOM,
    id : "Return",
    radioButtonViewVisibilty : false,
    returnTextViewVisibilty : true,
    isEnabled : true,
    radioButtonTextConfig : {
      primaryText : (getString LEAVE_NOW),
      primaryTextAccessibilityHint : (getEN LEAVE_NOW),
      secondaryText : (getString RESERVE),
      secondaryTextAccessibilityHint : (getEN RESERVE)
    },
  dateIconImage : "ny_ic_calendar",
  timeIconImage : "ny_ic_clock_unfilled"
  }
  in returnConfig'

fetchRideDetails :: ST.HomeScreenState -> ST.BookingAPIEntity
fetchRideDetails state = 
  let
    isScheduled = (state.data.startTimeUTC > (EHC.getCurrentUTC ""))
    returnTime = if state.data.returnTimeUTC == "" then "" else state.data.returnTimeUTC
    startTime = if state.data.startTimeUTC == "" then (EHC.getCurrentUTC "") else state.data.startTimeUTC
    currentSelectedEstimatesObject = state.data.selectedEstimatesObject
    currIndex = currentSelectedEstimatesObject.index
    selectedEstimatesObject = if (currIndex == 0) then (fromMaybe currentSelectedEstimatesObject ((state.data.quoteList)!!0)) else currentSelectedEstimatesObject
                    
    rideDetails =  ST.BookingAPIEntity{
      currency : INR,
      estimatedDistance : Just state.data.rideDistance,
      estimatedDuration : Just state.props.searchLocationModelProps.totalRideDuration,
      estimatedFare : selectedEstimatesObject.price,
      fromLocation : ST.LocationInformation{
        address : state.data.sourceAddress,
        placeId  : fromMaybe "" state.props.sourcePlaceId,
        fullAddress : state.data.source
      },
      id : fromMaybe "" state.data.selectedQuoteId,
      isScheduled : isScheduled,
      returnTime : Just returnTime,
      roundTrip : Just (state.props.searchLocationModelProps.tripType == ROUND_TRIP),
      isAirConditioned: selectedEstimatesObject.airConditioned,
      startTime : startTime,
      toLocation : Just (ST.LocationInformation {
        address : state.data.destinationAddress,
        placeId  : fromMaybe "" state.props.destinationPlaceId,
        fullAddress : state.data.destination
      }),
      tripCategory : TripCategory {
        contents : Nothing,
        tag : InterCity
      },
      vehicleServiceTier : selectedEstimatesObject.vehicleVariant,
      vehicleServiceTierAirConditioned : Nothing,
      vehicleServiceTierName : fromMaybe "" selectedEstimatesObject.serviceTierName,
      vehicleServiceTierSeatingCapacity : Just (fromMaybe 4 $ fromString selectedEstimatesObject.capacity)
    }
  in rideDetails

fetchExtraFares :: ST.HomeScreenState -> Array FareList
fetchExtraFares state = 
  let 
    currentSelectedEstimatesObject = state.data.selectedEstimatesObject
    currIndex = currentSelectedEstimatesObject.index
    selectedEstimatesObject = if (currIndex == 0) then (fromMaybe currentSelectedEstimatesObject ((state.data.quoteList)!!0)) else currentSelectedEstimatesObject
    extraFares = selectedEstimatesObject.extraFare
  in 
    extraFares             

  
deliveryParcelImageAndOtpConfig :: ST.HomeScreenState -> DeliveryParcelImageAndOtp.Config
deliveryParcelImageAndOtpConfig state =
  let
    config = DeliveryParcelImageAndOtp.config

    deliveryParcelImageAndOtpConfig' =
      config
        { image = fromMaybe "" state.data.deliveryImage
        , imageVisibility = isJust state.data.deliveryImage
        , otp = state.data.driverInfoCardState.otp
        , rideStarted = state.props.currentStage == ST.RideStarted
        , refreshAnimation = state.props.loadingDeliveryImage
        }
  in
    deliveryParcelImageAndOtpConfig'

parcelFeedbackPillData :: ST.HomeScreenState -> Array ( Array (Array RatingCard.FeedbackItem))
parcelFeedbackPillData state = [ parcelFeedbackPillDataWithRating1 state, parcelFeedbackPillDataWithRating2 state, parcelFeedbackPillDataWithRating3 state, parcelFeedbackPillDataWithRating4 state, parcelFeedbackPillDataWithRating5 state ]

parcelFeedbackPillDataWithRating1 :: ST.HomeScreenState -> Array (Array RatingCard.FeedbackItem)
parcelFeedbackPillDataWithRating1 state =
  [ [ { id: "6", text: getString RUDE_BEHAVIOUR }
    , { id: "1", text: getString TOO_MANY_CALLS }
    ]
  , [ { id: "6", text: getString RECKLESS_HANDLING }
    , { id: "6", text: getString ASKED_FOR_EXTRA_FARE }
    ]
  , ( [ { id: "15", text: getString DELIVERY_DELAYED } 
      , { id: "15", text: getString ITEMS_MISSING } ]
    )
  ]

parcelFeedbackPillDataWithRating2 :: ST.HomeScreenState -> Array (Array RatingCard.FeedbackItem)
parcelFeedbackPillDataWithRating2 state = parcelFeedbackPillDataWithRating1 state

parcelFeedbackPillDataWithRating3 :: ST.HomeScreenState -> Array (Array RatingCard.FeedbackItem)
parcelFeedbackPillDataWithRating3 state =
  [ [ { id: "8", text: getString UNPROFESSIONAL_DRIVER }]
  , [ { id: "8", text: getString ASKED_FOR_EXTRA_FARE }
    , { id: "11", text: getString TOO_MANY_CALLS}
    ]
  , [ { id: "3", text: getString DELIVERY_DELAYED }
    , { id: "3", text: getString RUDE_BEHAVIOUR }
    ]
  ]

parcelFeedbackPillDataWithRating4 :: ST.HomeScreenState -> Array (Array RatingCard.FeedbackItem)
parcelFeedbackPillDataWithRating4 state =
  [ [ { id: "9", text: getString POLITE_ATTITUDE }
    , { id: "9", text: getString SMOOTH_EXPERIENCE }
    ]
  , [ { id: "9", text: getString SECURE_DELIVERY }
    , { id: "11", text: getString ASKED_FOR_EXTRA_FARE}
    ]
  , [ { id: "4", text: getString DELIVERY_DELAYED }
    , { id: "4", text: getString TOO_MANY_CALLS }
    ]
  ]

parcelFeedbackPillDataWithRating5 :: ST.HomeScreenState -> Array (Array RatingCard.FeedbackItem)
parcelFeedbackPillDataWithRating5 state =
  [ [ { id: "10", text: getString POLITE_ATTITUDE }
    , { id: "5", text: getString SMOOTH_EXPERIENCE }
    ]
  , [ { id: "12", text: getString ON_TIME }
    , { id: "10", text: getString MINIMAL_CALLING }
    ]
  , [ { id: "10", text: getString SECURE_DELIVERY }]
  ]