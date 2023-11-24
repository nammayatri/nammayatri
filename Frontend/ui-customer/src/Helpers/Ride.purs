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
import JBridge (stopChatListenerService)
import Presto.Core.Types.Language.Flow (getLogFields)
import ModifyScreenState (modifyScreenState)
import Control.Monad.Except.Trans (lift)
import Services.Backend as Remote
import Engineering.Helpers.BackTrack (getState)
import Types.App (GlobalState(..))
import Data.Either (Either(..))
import Services.API (RideAPIEntity(..), RideBookingAPIDetails(..), RideBookingDetails(..), RideBookingListRes(..), RideBookingRes(..))
import Data.Array (null, head, length)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, isJust)
import Screens.HomeScreen.ScreenData (dummyRideBooking)
import Screens.HomeScreen.Transformer (dummyRideAPIEntity)
import Data.Lens ((^.))
import Accessor (_computedPrice, _contents, _status, _toLocation)
import Screens.Types (Stage(..), SearchResultType(..), PopupType(..))
import Screens.HomeScreen.Transformer (getDriverInfo, getSpecialTag)
import Engineering.Helpers.Commons (liftFlow, convertUTCtoISC)
import Engineering.Helpers.LogEvent (logEvent, logEventWithTwoParams)
import Storage (KeyStore(..), getValueToLocalStore, isLocalStageOn, setValueToLocalNativeStore, setValueToLocalStore, updateLocalStage)
import Helpers.Utils (getCurrentDate)
import Resources.Constants (DecodeAddress(..), decodeAddress)
import Data.String (split, Pattern(..))
import Domain.Cache (getAppConfigFromCache)


checkRideStatus :: Boolean -> FlowBT String Unit --TODO:: Need to refactor this function
checkRideStatus rideAssigned = do
  logField_ <- lift $ lift $ getLogFields
  config <- getAppConfigFromCache
  modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{data {config = config}})
  rideBookingListResponse <- lift $ lift $ Remote.rideBookingList "1" "0" "true"
  case rideBookingListResponse of
    Right (RideBookingListRes listResp) -> do
      if not (null listResp.list) then do
        (GlobalState state') <- getState
        let state = state'.homeScreen
            (RideBookingRes resp) = (fromMaybe dummyRideBooking (head listResp.list))
            status = (fromMaybe dummyRideAPIEntity (head resp.rideList))^._status
            rideStatus = if status == "NEW" then RideAccepted else RideStarted
            newState = 
              state
                { data
                    { driverInfoCardState = getDriverInfo state.data.specialZoneSelectedVariant (RideBookingRes resp) (null resp.rideList)
                    , finalAmount = fromMaybe 0 $ (fromMaybe dummyRideAPIEntity (head resp.rideList) )^. _computedPrice
                    , currentSearchResultType = if null resp.rideList then QUOTES else ESTIMATES},
                  props
                    { currentStage = rideStatus
                    , rideRequestFlow = true
                    , bookingId = resp.id
                    , isPopUp = NoPopUp
                    , zoneType = getSpecialTag resp.specialLocationTag
                  }
                }
        when (not rideAssigned) $ do
          lift $ lift $ liftFlow $ logEvent logField_ "ny_active_ride_with_idle_state"
          void $ pure $ logEventWithTwoParams logField_ "ny_active_ride_with_idle_state" "status" status "bookingId" resp.id

        modifyScreenState $ HomeScreenStateType (\homeScreen → newState)
        updateLocalStage rideStatus
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
        rideBookingListResponse <- lift $ lift $ Remote.rideBookingList "1" "0" "false"
        case rideBookingListResponse of
          Right (RideBookingListRes listResp) -> do
            let (RideBookingRes resp) = fromMaybe dummyRideBooking $ head listResp.list
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
              when (resp.status /= "CANCELLED" && length listResp.list > 0) $ do
                modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{
                    props { currentStage = RideCompleted
                          , estimatedDistance = contents.estimatedDistance
                          , zoneType = getSpecialTag resp.specialLocationTag}
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
                          , config = config
                          , finalAmount = (fromMaybe 0 currRideListItem.computedPrice)
                          , driverInfoCardState {
                            price = resp.estimatedTotalFare,
                            rideId = currRideListItem.id
                          }
                          , ratingViewState { rideBookingRes = (RideBookingRes resp)}
                          }
                })
                updateLocalStage RideCompleted
          Left err -> updateLocalStage HomeScreen
      else do
        updateLocalStage HomeScreen
    Left err -> updateLocalStage HomeScreen
  if not (isLocalStageOn RideAccepted) then removeChatService "" else pure unit

removeChatService :: String -> FlowBT String Unit
removeChatService _ = do
  void $ lift $ lift $ liftFlow $ stopChatListenerService
  setValueToLocalNativeStore READ_MESSAGES "0"
  pure unit