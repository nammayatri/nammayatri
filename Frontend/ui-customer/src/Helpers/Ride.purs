{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Helpers.Ride where

import Prelude
import Types.App (FlowBT, ScreenType(..))
import Control.Monad.Except (runExcept)
import JBridge
import Presto.Core.Types.Language.Flow (getLogFields)
import ModifyScreenState (modifyScreenState)
import Control.Monad.Except.Trans (lift)
import Services.Backend as Remote
import Engineering.Helpers.BackTrack (getState)
import Types.App (GlobalState(..))
import Data.Either (Either(..))
import Services.API
import Data.Array (null, head, length, (!!))
import Data.Maybe (Maybe(..), fromMaybe, isNothing, isJust, maybe', maybe)
import Screens.HomeScreen.ScreenData (dummyRideBooking, initData) as HSD
import Screens.HomeScreen.Transformer (dummyRideAPIEntity, getDriverInfo, getSpecialTag)
import Screens.RideBookingFlow.HomeScreen.Config (setTipViewData)
import Data.Lens ((^.))
import Accessor
import Screens.Types (Stage(..), SearchResultType(..), PopupType(..), FlowStatusData(..), TipViewData(..))
import Engineering.Helpers.Commons (liftFlow, convertUTCtoISC)
import Engineering.Helpers.LogEvent (logEvent, logEventWithTwoParams)
import Storage (KeyStore(..), getValueToLocalStore, isLocalStageOn, setValueToLocalNativeStore, setValueToLocalStore, updateLocalStage)
import Helpers.Utils (getCurrentDate, getCityNameFromCode)
import Resources.Constants (DecodeAddress(..), decodeAddress, getAddressFromBooking)
import Data.String (split, Pattern(..))
import Foreign.Generic (decodeJSON)
import Screens.HomeScreen.ScreenData as HomeScreenData

checkRideStatus :: Boolean -> FlowBT String Unit --TODO:: Need to refactor this function
checkRideStatus rideAssigned = do
  logField_ <- lift $ lift $ getLogFields
  rideBookingListResponse <- lift $ lift $ Remote.rideBookingList "1" "0" "true"
  case rideBookingListResponse of
    Right (RideBookingListRes listResp) -> do
      if not (null listResp.list) then do
        (GlobalState state') <- getState
        let state = state'.homeScreen
            (RideBookingRes resp) = (fromMaybe HSD.dummyRideBooking (head listResp.list))
            status = (fromMaybe dummyRideAPIEntity (head resp.rideList))^._status
            rideStatus = if status == "NEW" then RideAccepted else if status == "INPROGRESS" then RideStarted else HomeScreen
            fareProductType = ((resp.bookingDetails) ^. _fareProductType)
            otpCode = ((resp.bookingDetails) ^. _contents ^. _otpCode)
            isQuotes = (fareProductType == "OneWaySpecialZoneAPIDetails" || otpCode /= Nothing)
            newState = 
              state
                { data
                    { driverInfoCardState = getDriverInfo state.data.specialZoneSelectedVariant (RideBookingRes resp) isQuotes
                    , finalAmount = fromMaybe 0 $ (fromMaybe dummyRideAPIEntity (head resp.rideList) )^. _computedPrice
                    , sourceAddress = getAddressFromBooking resp.fromLocation
                    , destinationAddress = getAddressFromBooking (resp.bookingDetails ^._contents^._toLocation)
                    , currentSearchResultType = if isQuotes then QUOTES else ESTIMATES},
                  props
                    { currentStage = rideStatus
                    , rideRequestFlow = true
                    , bookingId = resp.id
                    , isPopUp = NoPopUp
                    , zoneType = getSpecialTag resp.specialLocationTag
                  }
                }
        if rideStatus == HomeScreen then
          updateLocalStage HomeScreen
        else do
          when (not rideAssigned) $ do
            lift $ lift $ liftFlow $ logEvent logField_ "ny_active_ride_with_idle_state"
            void $ pure $ logEventWithTwoParams logField_ "ny_active_ride_with_idle_state" "status" status "bookingId" resp.id
          modifyScreenState $ HomeScreenStateType (\homeScreen → newState)
          updateLocalStage rideStatus
          maybe' (\_ -> pure unit) updateCity (getFlowStatusData "LazyCheck")
          let (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
              (RideBookingDetails contents) = bookingDetails.contents
              otpCode = contents.otpCode
              rideListItem = head resp.rideList
          case rideListItem of
            Nothing -> do
              case otpCode of
                Just otp' -> do
                  setValueToLocalStore TRACKING_ENABLED "True"
                  modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{props{isSpecialZone = true, isInApp = true}, data{driverInfoCardState{otp = otp'}}})
                Nothing -> pure unit
            Just (RideAPIEntity _) ->
              if isJust otpCode then do
                setValueToLocalStore TRACKING_ENABLED "True"
                modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{props{isSpecialZone = true,isInApp = true }}) else
                pure unit
      else if ((getValueToLocalStore RATING_SKIPPED) == "false") then do
        updateLocalStage HomeScreen
        rideBookingListResponse <- lift $ lift $ Remote.rideBookingListWithStatus "1" "0" "COMPLETED"
        case rideBookingListResponse of
          Right (RideBookingListRes listResp) -> do
            let (RideBookingRes resp) = fromMaybe HSD.dummyRideBooking $ head listResp.list
                (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
                (RideBookingDetails contents) = bookingDetails.contents
                (RideAPIEntity currRideListItem) = fromMaybe dummyRideAPIEntity $ head resp.rideList
                differenceOfDistance = fromMaybe 0 contents.estimatedDistance - (fromMaybe 0 currRideListItem.chargeableRideDistance)
                lastRideDate = (case currRideListItem.rideStartTime of
                                Just startTime -> (convertUTCtoISC startTime "DD/MM/YYYY")
                                Nothing        -> "")
                currentDate =  getCurrentDate ""
            if(lastRideDate /= currentDate) then do
              setValueToLocalStore FLOW_WITHOUT_OFFERS "true"
              setValueToLocalStore TEST_MINIMUM_POLLING_COUNT "4"
              setValueToLocalStore TEST_POLLING_INTERVAL "8000.0"
              setValueToLocalStore TEST_POLLING_COUNT "22"
              pure unit
              else pure unit
            when (isNothing currRideListItem.rideRating) $ do
              when (length listResp.list > 0) $ do
                let nightSafetyFlow = showNightSafetyFlow resp.hasNightIssue resp.rideStartTime resp.rideEndTime
                modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{
                    props { currentStage = RideCompleted
                          , estimatedDistance = contents.estimatedDistance
                          , zoneType = getSpecialTag resp.specialLocationTag
                          , nightSafetyFlow = nightSafetyFlow
                          , showOfferedAssistancePopUp = resp.hasDisability == Just true
                          }
                  , data { rideRatingState
                          { driverName = currRideListItem.driverName
                          , rideId = currRideListItem.id
                          , finalAmount = (fromMaybe 0 currRideListItem.computedPrice)
                          , source = decodeAddress (Booking resp.fromLocation)
                          , destination = (decodeAddress (Booking (resp.bookingDetails ^._contents^._toLocation)))
                          , vehicleNumber = (currRideListItem.vehicleNumber)
                          , status = (currRideListItem.status)
                          , shortRideId = currRideListItem.shortRideId
                          , rideEndTimeUTC = ""
                          , offeredFare = resp.estimatedTotalFare
                          , distanceDifference = differenceOfDistance
                          , bookingId = resp.id
                          , feedback = ""
                          , rideStartTime = case currRideListItem.rideStartTime of
                                              Just startTime -> (convertUTCtoISC startTime "h:mm A")
                                              Nothing        -> ""
                          , rideEndTime   = case currRideListItem.rideEndTime of
                                              Just endTime   -> " " <>(convertUTCtoISC endTime "h:mm A")
                                              Nothing        -> ""
                          , rideStartDate = case currRideListItem.rideStartTime of
                                              Just startTime ->( (fromMaybe "" (head (split (Pattern ",") (convertUTCtoISC startTime "llll")) )) <> ", " <>  (convertUTCtoISC startTime "Do MMM") )
                                              Nothing        -> ""
                          , dateDDMMYY =  case currRideListItem.rideStartTime of
                                            Just startTime -> (convertUTCtoISC startTime "DD/MM/YYYY")
                                            Nothing        -> ""
                          }
                          , finalAmount = (fromMaybe 0 currRideListItem.computedPrice)
                          , driverInfoCardState {
                            price = resp.estimatedTotalFare,
                            rideId = currRideListItem.id
                          }
                          , ratingViewState { rideBookingRes = (RideBookingRes resp), issueFacedView = nightSafetyFlow}
                          }
                })
                updateLocalStage RideCompleted
          Left err -> updateLocalStage HomeScreen
      else do
        updateLocalStage HomeScreen
    Left err -> updateLocalStage HomeScreen
  if not (isLocalStageOn RideAccepted) then removeChatService "" else pure unit
  where 
    updateCity :: FlowStatusData -> FlowBT String Unit
    updateCity (FlowStatusData flowStatusData) = modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{city = getCityNameFromCode flowStatusData.source.city}})
        

removeChatService :: String -> FlowBT String Unit
removeChatService _ = do
  let state = HomeScreenData.initData.data
  _ <- lift $ lift $ liftFlow $ stopChatListenerService
  _ <- pure $ setValueToLocalNativeStore READ_MESSAGES "0"
  modifyScreenState $ HomeScreenStateType (\homeScreen -> 
    homeScreen{
      props{sendMessageActive = false, chatcallbackInitiated = false, unReadMessages = false, openChatScreen = false, showChatNotification = false, canSendSuggestion = true, isChatNotificationDismissed = false, isNotificationExpanded = false, removeNotification = true, enableChatWidget = false},
      data{messages = [], messagesSize = "-1", chatSuggestionsList = [], messageToBeSent = "", lastMessage = state.lastMessage, waitTimeInfo = false, lastSentMessage = state.lastSentMessage, lastReceivedMessage = state.lastReceivedMessage}})

getFlowStatusData :: String -> Maybe FlowStatusData
getFlowStatusData dummy =
  case runExcept (decodeJSON (getValueToLocalStore FLOW_STATUS_DATA) :: _ FlowStatusData) of
    Right res -> Just res
    Left err -> Nothing

showNightSafetyFlow :: Maybe Boolean -> Maybe String -> Maybe String -> Boolean
showNightSafetyFlow hasNightIssue rideStartTime rideEndTime = not (fromMaybe true hasNightIssue) && (isNightRide rideStartTime || isNightRide rideEndTime)

isNightRide :: Maybe String -> Boolean
isNightRide = maybe false (\time -> withinTimeRange "21:00:00" "06:00:00" $ convertUTCtoISC time "HH:mm:ss")