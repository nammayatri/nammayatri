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
import Components.CancelRide as CancelRidePopUpConfig
import Components.DriverInfoCard (DriverInfoCardData)
import Components.DriverInfoCard as DriverInfoCard
import Components.EmergencyHelp as EmergencyHelp
import Components.ErrorModal as ErrorModal
import Components.FareBreakUp as FareBreakUp
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.SearchLocationModel as SearchLocationModel
import Components.ChooseYourRide as ChooseYourRide
import Components.MenuButton as MenuButton
import Components.QuoteListModel as QuoteListModel
import Components.RateCard as RateCard
import Components.RatingCard as RatingCard
import Components.ChatView as ChatView
import PrestoDOM.Types.DomAttributes (Corners(..))
import Data.String as DS
import Animation.Config as AnimConfig
import Components.SearchLocationModel as SearchLocationModel
import Components.SourceToDestination as SourceToDestination
import Data.Array as DA
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DS
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils as HU
import JBridge as JB
import Language.Types (STR(..))
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (DriverInfoCard)
import Screens.Types as ST
import Styles.Colors as Color
import Data.Int as INT
import Storage (KeyStore(..), getValueToLocalStore, isLocalStageOn)

import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Common.Types.App (LazyCheck(..))
import Prelude ((<>))

shareAppConfig :: ST.HomeScreenState -> PopUpModal.Config
shareAppConfig state = let
  config' = PopUpModal.config
  popUpConfig' = config'{
      gravity = CENTER,
      margin = (MarginHorizontal 24 24),
      buttonLayoutMargin = (Margin 16 0 16 20),
      primaryText {
        text = getString(YOUR_RIDE_HAS_STARTED) 
      , margin = (MarginHorizontal 16 16)},
      secondaryText { 
        text = getString(ENJOY_RIDING_WITH_US)
      , margin = MarginVertical 12 24  
      , fontSize = FontSize.a_14
      , color = Color.black700},
      option1 {
        text = getString(MAYBE_LATER) 
      , fontSize = FontSize.a_16
      , width = V $ (((EHC.screenWidth unit)-92)/2) 
      , background = Color.white900
      , strokeColor = Color.black500
      , color = Color.black700
      , fontStyle = FontStyle.semiBold LanguageStyle
      },
      option2 {
        text = getString(SHARE_APP) 
      , fontSize = FontSize.a_16
      , width = V $ (((EHC.screenWidth unit)-92)/2)
      , color = state.data.config.primaryTextColor
      , strokeColor = state.data.config.primaryBackground
      , background = state.data.config.primaryBackground
      , margin = MarginLeft 12
      ,fontStyle = FontStyle.semiBold LanguageStyle
      },
      cornerRadius = (Corners 15.0 true true true true),
      coverImageConfig {
        imageUrl = "ny_ic_share_app," <> (getCommonAssetStoreLink FunctionCall) <> "/user/images/ny_ic_share_app.png"
      , visibility = VISIBLE
      , margin = Margin 16 20 16 24
      , width = MATCH_PARENT
      , height = V 200
      }
  }
  in popUpConfig'


skipButtonConfig :: ST.HomeScreenState -> PrimaryButton.Config
skipButtonConfig state =
  let
    config = PrimaryButton.config
    skipButtonConfig' =
      config
        { textConfig
          { text = (getString SKIP)
          , color = state.data.config.rateCardColor
          , fontStyle = FontStyle.bold LanguageStyle
          , textSize = FontSize.a_16
          }
        , width = V (EHC.screenWidth unit / 4)
        , background = Color.white900
        , stroke = ("1," <> state.data.config.rateCardColor)
        , margin = (Margin 0 0 0 0)
        , id = "SkipRatingButton"
        , enableLoader = (JB.getBtnLoader "SkipRatingButton")
        }
  in
    skipButtonConfig'

sourceToDestinationConfig :: ST.HomeScreenState -> SourceToDestination.Config
sourceToDestinationConfig state =
  let
    config = SourceToDestination.config
    sourceToDestinationConfig' =
      config
        { margin = (Margin 0 0 0 0)
        , width = V (EHC.screenWidth unit - 70)
        , sourceMargin = (Margin 0 0 0 14)
        , lineMargin = (Margin 20 10 0 0)
        , sourceImageConfig
          { imageUrl = "ny_ic_source_dot," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_source_dot.png"
          , height = V 13
          , width = V 13
          , margin = (Margin 14 2 0 0)
          }
        , rideStartedAtConfig
          { text = state.data.startedAt
          , visibility = VISIBLE
          , padding = (Padding 1 1 1 1)
          , margin = (Margin 12 2 0 0)
          }
        , sourceTextConfig
          { text = state.data.driverInfoCardState.source
          , textSize = FontSize.a_14
          , padding = (Padding 0 0 0 0)
          , margin = (Margin 11 0 15 0)
          , fontStyle = FontStyle.medium LanguageStyle
          , color = Color.black800
          , ellipsize = true
          , maxLines = 1
          }
        , destinationImageConfig
          { imageUrl = "ny_ic_loc_red," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_loc_red.png"
          , height = V 17
          , width = V 14
          , margin = (Margin 13 3 0 0)
          }
        , destinationTextConfig
          { text = state.data.driverInfoCardState.destination
          , textSize = FontSize.a_14
          , padding = (Padding 2 0 2 2)
          , margin = (Margin 10 0 15 0)
          , maxLines = 1
          , color = Color.black800
          , fontStyle = FontStyle.medium LanguageStyle
          , ellipsize = true
          }
        , rideEndedAtConfig
          { text = state.data.endedAt
          , visibility = VISIBLE
          , padding = (Padding 1 1 1 1)
          , margin = (Margin 13 2 0 0)
          }
        }
  in
    sourceToDestinationConfig'

fareBreakUpConfig :: ST.HomeScreenState -> FareBreakUp.Config
fareBreakUpConfig state =
  let
    config = FareBreakUp.config
    fareBreakUpConfig' =
      config
        { fareDetails = []
        , headingText = (getString VIEW_BREAKDOWN)
        , totalAmount =
          { text: ""
          , textSize: FontSize.a_16
          , fontStyle: FontStyle.semiBold LanguageStyle
          , color: Color.black800
          , margin: (Margin 0 0 0 20)
          , visibility: GONE
          , priceDetails:
              { text: 0
              , textSize: FontSize.a_16
              , fontStyle: FontStyle.semiBold LanguageStyle
              , offeredFare: state.data.driverInfoCardState.price
              , distanceDifference: state.data.previousRideRatingState.distanceDifference
              }
          }
        , rideDetails =
          { destination: state.data.driverInfoCardState.destination
          , destinationTitle: (fromMaybe "" ((DS.split (DS.Pattern ",") (state.data.driverInfoCardState.destination)) DA.!! 0))
          , source: state.data.driverInfoCardState.source
          , sourceTitle: (fromMaybe "" ((DS.split (DS.Pattern ",") (state.data.driverInfoCardState.source)) DA.!! 0))
          , rideStartTime: state.data.startedAt
          , rideStartDate: ((fromMaybe "" ((DS.split (DS.Pattern ",") (HU.convertUTCtoISC (state.data.startedAtUTC) "llll")) DA.!! 0)) <> ", " <> (HU.convertUTCtoISC (state.data.startedAtUTC) "Do MMM"))
          , estimatedDistance: state.props.estimatedDistance
          }
        }
  in
    fareBreakUpConfig'

whereToButtonConfig :: ST.HomeScreenState -> PrimaryButton.Config
whereToButtonConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      { textConfig
        { text = (getString WHERE_TO)
        , textSize = FontSize.a_16
        , width = MATCH_PARENT
        , gravity = LEFT
        , color = state.data.config.primaryTextColor 
        }
      , height = V 60
      , gravity = CENTER
      , cornerRadius = 8.0
      , margin = (MarginHorizontal 16 16)  
      , isClickable = true 
      , isPrefixImage = true
      , background = state.data.config.primaryBackground
      , prefixImageConfig
        { imageUrl = if state.data.config.merchantId == "PAYTM" then "ny_ic_bent_right_arrow_white," <> (getCommonAssetStoreLink FunctionCall) <> "/user/images/ny_ic_bent_right_arrow_white.png" else  "ny_ic_bent_right_arrow," <> (getCommonAssetStoreLink FunctionCall) <> "/user/images/ny_ic_bent_right_arrow.png"
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
          { text = state.data.config.estimateConfirmText
          , textSize = FontSize.a_16
          ,  color = state.data.config.primaryTextColor
          }
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
          , textSize = FontSize.a_16
          , fontStyle = FontStyle.regular LanguageStyle
          , color = state.data.config.primaryTextColor
          }
        , margin = (Margin 0 22 0 0)
        , id = "ConfirmLocationButton"
        , background = state.data.config.primaryBackground
        }
  in
    primaryButtonConfig'

rateRideButtonConfig :: ST.HomeScreenState -> PrimaryButton.Config
rateRideButtonConfig state =
  let
    config = PrimaryButton.config
    primaryButtonConfig' =
      config
        { textConfig
          { text = (getString RATE_YOUR_DRIVER)
          , textSize = FontSize.a_16
          , fontStyle = FontStyle.bold LanguageStyle
          ,  color = state.data.config.primaryTextColor 
          }
        , background = state.data.config.rateCardColor
        , margin = (MarginLeft 12)
        , id = "RateYourDriverButton"
        , enableLoader = (JB.getBtnLoader "RateYourDriverButton")
        }
  in
    primaryButtonConfig'

cancelRidePopUpConfig :: ST.HomeScreenState -> CancelRidePopUpConfig.Config
cancelRidePopUpConfig state =
  let
    cancelRideconfig = CancelRidePopUpConfig.config
    lastIndex = ((DA.length state.props.cancellationReasons) - 1)
    cancelRideconfig' =
      cancelRideconfig
        { cancelRideReasons = state.props.cancellationReasons
        , primaryButtonTextConfig
          { firstText = (getString GO_BACK_)
          , secondText = (getString CANCEL_RIDE)
          }
        , activeIndex = state.props.cancelRideActiveIndex
        , activeReasonCode = Just state.props.cancelReasonCode
        , isLimitExceeded = ((DS.length (state.props.cancelDescription)) >= 100)
        , isCancelButtonActive =
          ( case state.props.cancelRideActiveIndex of
              Just cancelRideIndex -> true
              Nothing -> false
          )
        , headingText = ((getString CANCEL_RIDE) <> "?")
        , subHeadingText = (getString PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL)
        , hint = (getString HELP_US_WITH_YOUR_REASON)
        , strings
          { mandatory = (getString MANDATORY)
          , limitReached = ((getString MAX_CHAR_LIMIT_REACHED) <> " 100 " <> (getString OF) <> " 100")
          }
        }
  in
    cancelRideconfig'

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
            , option1 { text = (getString GO_BACK_) }
            , option2 { text = (getString LOGOUT_) }
            }
      in
        popUpConfig'
    ST.TipsPopUp -> PopUpModal.config{
          optionButtonOrientation = "VERTICAL"
          , backgroundClickable = true
          , customerTipAvailable = true
          , dismissPopup = true
          , customerTipArray = [(getString NO_TIP), "₹10 🙂", "₹15 😄", "₹20 🤩"]
          , customerTipArrayWithValues = [0,10, 15, 20]
          , primaryText {
              text =  if(isLocalStageOn ST.QuoteList)then (getString TRY_AGAIN_WITH_A_TIP) else (getString SEARCH_AGAIN_WITH_A_TIP)
            , fontSize = FontSize.a_22
            },
          secondaryText { 
            text = (getString BOOST_YOUR_RIDE_CHANCES_AND_HELP_DRIVERS_WITH_TIPS)
          , fontSize = FontSize.a_14
          , color = Color.black650}
          , tipLayoutMargin = (Margin 22 0 22 22)
          , buttonLayoutMargin = (MarginHorizontal 16 16)
          , activeIndex = state.props.customerTip.tipActiveIndex
          , tipButton {
                background = Color.white900
              , color = Color.black800
              , strokeColor = Color.grey900
              , padding = (Padding 16 12 16 12)
            },
          option1 {
            text = if (state.props.customerTip.tipForDriver == 0) then ( if(isLocalStageOn ST.QuoteList) then (getString TRY_AGAIN_WITHOUT_TIP)else (getString SEARCH_AGAIN_WITHOUT_A_TIP)) else ((if (isLocalStageOn ST.QuoteList) then (getString TRY_AGAIN_WITH)else(getString SEARCH_AGAIN_WITH) ) <> " + ₹"<> (fromMaybe "" (["0", "10", "15", "20"] DA.!! state.props.customerTip.tipActiveIndex))) <>" "<>(getString TIP)
          , fontSize = FontSize.a_16 
          , width = MATCH_PARENT
          , color = state.data.config.primaryTextColor
          , strokeColor = state.data.config.primaryBackground
          , background = state.data.config.primaryBackground
          , padding = (Padding 0 10 0 10)
          , fontStyle = FontStyle.semiBold LanguageStyle
          },
          option2 {
            text = if (isLocalStageOn ST.QuoteList) then (getString HOME) else  (getString CANCEL_SEARCH)
          , fontSize = FontSize.a_16
          , width = MATCH_PARENT 
          , background = Color.white900
          , strokeColor = Color.white900
          , margin = MarginTop 14
          , padding = PaddingBottom $ getBottomMargin
          , color = Color.black650
          , fontStyle = FontStyle.semiBold LanguageStyle
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
            , fontSize = FontSize.a_16 
            , width = MATCH_PARENT
            , color = state.data.config.primaryTextColor
            , strokeColor = state.data.config.primaryBackground
            , background = state.data.config.primaryBackground
            , padding = (Padding 0 10 0 10)
            , fontStyle = FontStyle.semiBold LanguageStyle
            }
            , option2 { 
               text = if (isLocalStageOn ST.QuoteList) then (getString HOME) else (getString NO_DONT) 
              , fontSize = FontSize.a_16
              , width = MATCH_PARENT 
              , background = Color.white900
              , strokeColor = Color.white900
              , margin = MarginTop $ if (isLocalStageOn ST.QuoteList || isLocalStageOn ST.FindingQuotes) then 14 else 3
              , color = Color.black650
              , padding = if (isLocalStageOn ST.QuoteList || isLocalStageOn ST.FindingQuotes) then (PaddingBottom getBottomMargin) else (Padding 0 0 0 0)
              , fontStyle = FontStyle.semiBold LanguageStyle
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
          , fontSize = FontSize.a_20
          }
        , secondaryText
          { text = (getString DROP_LOCATION_FAR_AWAY)
          , margin = (Margin 0 16 0 20)
          }
        , option1 { visibility = false }
        , option2
          { text = (getString CHANGE_DROP_LOCATION)
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
          { text = (getString YOUR_TRIP_IS_TOO_SHORT_YOU_ARE_JUST) <> HU.toString (state.props.distance) <> (getString METERS_AWAY_FROM_YOUR_DESTINATION)
          , margin = (Margin 16 20 16 0)
          , fontSize = FontSize.a_20
          }
        , secondaryText
          { text = (getString YOU_CAN_TAKE_A_WALK_OR_CONTINUE_WITH_RIDE_BOOKING)
          , margin = (Margin 0 16 0 20)
          }
        , option1 { text = (getString GO_BACK_) }
        , option2 { text = (getString BOOK_RIDE_) }
        }
  in
    popUpConfig'

sourceUnserviceableConfig :: ST.HomeScreenState -> ErrorModal.Config
sourceUnserviceableConfig state =
  let
    config = ErrorModal.config
    errorModalConfig' =
      config
        { height = MATCH_PARENT
        , background = Color.white900
        , stroke = ("1," <> Color.borderGreyColor)
        , corners = (Corners 20.0 true true false false)
        , imageConfig
          { imageUrl = "ny_ic_location_unserviceable," <> (getCommonAssetStoreLink FunctionCall) <> "/user/images/ny_ic_location_unserviceable.png"
          , height = V 99
          , width = V 133
          , margin = (Margin 0 50 0 20)
          }
        , errorConfig
          { text = if state.props.isMockLocation then "Unable to get your location!" else (getString LOCATION_UNSERVICEABLE)
          , textSize = FontSize.a_22
          , color = Color.black800
          , margin = (MarginBottom 5)
          , fontStyle = FontStyle.bold LanguageStyle
          }
        , errorDescriptionConfig
          { text = if state.props.isMockLocation then "Turn off any Mock Location app you might be using and restart the app." else (getString CURRENTLY_WE_ARE_LIVE_IN_)
          , color = Color.black700
          , textSize = FontSize.a_16
          , margin = (Margin 20 0 20 (40 + EHC.safeMarginBottom))
          , fontStyle = FontStyle.regular LanguageStyle
          }
        , buttonConfig
          { text = (getString CHANGE_LOCATION)
          , textSize = FontSize.a_16
          , margin = (Margin 16 0 16 (20 + EHC.safeMarginBottom))
          , fontStyle = FontStyle.medium LanguageStyle
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
        { baseFare = "₹" <> HU.toString (state.data.rateCard.baseFare)
        , extraFare = "₹" <> HU.toString (state.data.rateCard.extraFare)
        , nightCharges = state.data.rateCard.nightCharges
        , pickUpCharges = "₹" <> HU.toString (state.data.rateCard.pickUpCharges)
        , additionalFare = "₹" <> HU.toString (state.data.rateCard.additionalFare)
        , nightShiftMultiplier = HU.toString (state.data.rateCard.nightShiftMultiplier)
        }
  in
    rateCardConfig'

estimateChangedPopupConfig :: ST.HomeScreenState -> PopUpModal.Config
estimateChangedPopupConfig state =
  let
    config' = PopUpModal.config
    popUpConfig' =
      config'
        { primaryText { text = (getString ESTIMATES_CHANGED) }
        , secondaryText { text = (getString ESTIMATES_REVISED_TO) <> "₹" <> (show state.data.suggestedAmount) <> "-" <> "₹" <> (show $ (state.data.suggestedAmount + state.data.rateCard.additionalFare)) }
        , option1 { text = (getString GO_HOME_) }
        , option2 { text = (getString CONTINUE) }
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
                                  , estimatedTime : state.data.rideDuration
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
      , sendMessageActive = state.props.sendMessageActive
      , distance = metersToKm state.data.driverInfoCardState.distance state
      , suggestionsList = if (metersToKm state.data.driverInfoCardState.distance state) == (getString AT_PICKUP) then pickupSuggestions ""  else initialSuggestions ""
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
  }
  in chatViewConfig'

initialSuggestions :: String -> Array String
initialSuggestions _ = 
  [
    (getString ARE_YOU_STARING),
    (getString PLEASE_COME_SOON),
    (getString OK_I_WILL_WAIT)
  ]

pickupSuggestions :: String -> Array String
pickupSuggestions _ = 
  [
    (getString PLEASE_WAIT_I_WILL_BE_THERE),
    (getString LOOKING_FOR_YOU_AT_PICKUP),
    (getString UNREACHABLE_PLEASE_CALL_BACK)
  ]


metersToKm :: Int -> ST.HomeScreenState -> String
metersToKm distance state =
  if (distance <= 10) then
    (if (state.props.currentStage == ST.RideStarted) then (getString AT_DROP) else (getString AT_PICKUP))
  else if (distance < 1000) then (HU.toString distance <> " m " <> (getString AWAY_C)) else (HU.parseFloat ((INT.toNumber distance) / 1000.0)) 2 <> " km " <> (getString AWAY_C)


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
    , config : state.data.config
    }

emergencyHelpModelViewState :: ST.HomeScreenState -> EmergencyHelp.EmergencyHelpModelState
emergencyHelpModelViewState state = { showContactSupportPopUp: state.props.emergencyHelpModelState.showContactSupportPopUp
                                , showCallPolicePopUp: state.props.emergencyHelpModelState.showCallPolicePopUp
                                , showCallContactPopUp: state.props.emergencyHelpModelState.showCallContactPopUp
                                , emergencyContactData: state.props.emergencyHelpModelState.emergencyContactData
                                , currentlySelectedContact: state.props.emergencyHelpModelState.currentlySelectedContact
                                , showCallSuccessfulPopUp : state.props.emergencyHelpModelState.showCallSuccessfulPopUp
                                }

ratingCardViewState :: ST.HomeScreenState -> RatingCard.RatingCardState
ratingCardViewState state = { props:
                            { currentStage: state.props.currentStage
                            , estimatedDistance: state.props.estimatedDistance
                            , showFareBreakUp: false
                            , enableFeedback: true
                            }
                        , data: state.data.previousRideRatingState
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
                                    , homeScreenConfig : state.data.config
                                    }

quoteListModelViewState :: ST.HomeScreenState -> QuoteListModel.QuoteListModelState
quoteListModelViewState state = { source: state.data.source
                            , destination: state.data.destination
                            , quoteListModel: state.data.quoteListModelState
                            , selectedQuote: state.props.selectedQuote
                            , autoSelecting: state.props.autoSelecting
                            , searchExpire: state.props.searchExpire
                            , appConfig : state.data.config
                            }

previousRideRatingViewState :: ST.HomeScreenState -> RatingCard.RatingCardState
previousRideRatingViewState state = { props:
                                        { currentStage: state.props.currentStage
                                        , estimatedDistance: state.props.estimatedDistance
                                        , enableFeedback: false
                                        , showFareBreakUp: true
                                        }
                                    , data: state.data.previousRideRatingState
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
    , fontStyle = FontStyle.semiBold LanguageStyle
    }
  , secondaryText {
      text = getString YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT
    , margin = (Margin 24 12 24 32)
    , color = Color.black700
    }
  , option1 {
      text =  getString CANCEL_
    , fontSize = FontSize.a_16
    , color = Color.black700
    , strokeColor = Color.black700
    }
  , option2 {
      text =  getString CALL_SUPPORT
    , fontSize = FontSize.a_16
    , margin = (MarginLeft 12)
    }
  }
  in popUpConfig'
menuButtonConfig :: ST.HomeScreenState -> ST.Location -> MenuButton.Config
menuButtonConfig state item = let  
    config = MenuButton.config
    menuButtonConfig' = config {
      titleConfig{
          text = item.place
          ,selectedFontStyle = FontStyle.bold LanguageStyle
          ,unselectedFontStyle = FontStyle.regular LanguageStyle
      }
      , radioButtonConfig {
        height = V 16
        , width = V 16
        , imageHeight = V 10
        , imageWidth = V 10
        , imageUrl = "ny_ic_pickup"
        , cornerRadius = 10.0
        , buttonMargin = (MarginRight 15)
        , activeStroke = ("2," <> Color.positive)
      }
      , height = V 40
      , id = item.place
      , lat = item.lat
      , lng = item.lng
      , leftsidebutton = true
      , padding = PaddingBottom 10 
      , isSelected = item.place == state.props.defaultPickUpPoint
    }
    in menuButtonConfig'

chooseYourRideConfig :: ST.HomeScreenState -> ChooseYourRide.Config
chooseYourRideConfig state = ChooseYourRide.config
  { 
    rideDistance = state.data.rideDistance,
    rideDuration = state.data.rideDuration,
    quoteList = state.data.specialZoneQuoteList
  }
