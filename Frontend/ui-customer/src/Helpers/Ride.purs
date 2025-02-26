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
import Language.Strings (getString)
import Language.Types as LT
import Types.App (FlowBT, ScreenType(..))
import Control.Monad.Except (runExcept)
import JBridge
import Presto.Core.Types.Language.Flow (getLogFields)
import ModifyScreenState (modifyScreenState)
import Control.Monad.Except.Trans (lift)
import Services.Backend as Remote
import Engineering.Helpers.BackTrack (getState,liftFlowBT)
import Types.App (GlobalState(..))
import Data.Either (Either(..))
import Services.API
import Data.Array (any, null, head, length, (!!),last)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, isJust, maybe', maybe)
import Screens.HomeScreen.ScreenData (dummyRideBooking, initData) as HSD
import Screens.HomeScreen.Transformer (dummyRideAPIEntity, getDriverInfo, getFareProductType)
import Helpers.TipConfig (setTipViewData)
import Data.Lens ((^.))
import Screens.MyRidesScreen.ScreenData (dummyBookingDetails)
import Accessor
import Screens.Types (Stage(..), PopupType(..), FlowStatusData(..),HomeScreenState(..),DriverInfoCard)
import Engineering.Helpers.Commons (liftFlow, convertUTCtoISC)
import Engineering.Helpers.Utils (isAmbulance)
import Engineering.Helpers.LogEvent (logEvent, logEventWithTwoParams)
import Storage (KeyStore(..), getValueToLocalStore, isLocalStageOn, setValueToLocalNativeStore, setValueToLocalStore, updateLocalStage)
import Helpers.Utils (getCurrentDate, getCityNameFromCode,getCityCodeFromCity,fetchDriverInformation)
import Resources.Constants (DecodeAddress(..), decodeAddress, getAddressFromBooking)
import Data.String (split, Pattern(..))
import Foreign.Generic (decodeJSON,encodeJSON)
import Screens.HomeScreen.ScreenData as HomeScreenData
import Common.Types.App as CTA
import Helpers.SpecialZoneAndHotSpots (getSpecialTag,zoneLabelIcon)
import Screens.Types (FareProductType(..)) as FPT
import Resources.Constants (getFareFromArray)
import Data.Array as DA
import Screens.Types as ST
import Engineering.Helpers.Commons as EHC
import Screens.NammaSafetyFlow.Components.SafetyUtils as SU
import Components.ServiceTierCard.View as ServiceTierCard
import Helpers.Utils hiding (withinTimeRange)
import Data.Int as INT
import Language.Types (STR(..))
import Language.Strings (getVarString)
import PrestoDOM
import Screens.RideBookingFlow.HomeScreen.Config (getFareUpdatedStr)
import DecodeUtil
import Mobility.Prelude (boolToVisibility)
import Styles.Colors as Color
import ConfigProvider (getCurrency)
import Constants (appConfig)
import Resources.LocalizableV2.Strings (getStringV2)
import Resources.LocalizableV2.Types

customerFeedbackPillData :: RideBookingRes -> String -> Array (Array (Array ST.FeedbackItem))
customerFeedbackPillData (RideBookingRes state) vehicleVariant = if ((state.bookingDetails) ^. _fareProductType) == "DELIVERY" then parcelFeedbackPillData else normalRideFeedbackPillData (RideBookingRes state) vehicleVariant

normalRideFeedbackPillData :: RideBookingRes -> String -> Array (Array (Array ST.FeedbackItem))
normalRideFeedbackPillData (RideBookingRes state) vehicleVariant = [ feedbackPillDataWithRating1 (RideBookingRes state), feedbackPillDataWithRating2 (RideBookingRes state), feedbackPillDataWithRating3 vehicleVariant, feedbackPillDataWithRating4 vehicleVariant, feedbackPillDataWithRating5 vehicleVariant ]

feedbackPillDataWithRating1 :: RideBookingRes -> Array (Array ST.FeedbackItem)
feedbackPillDataWithRating1 (RideBookingRes state) =
  [ [ { id: "6", text: getString LT.RUDE_DRIVER }
    , { id: "1", text: getString LT.FELT_UNSAFE }
    ]
  , [ { id: "6", text: getString LT.RECKLESS_DRIVING }
    , { id: "6", text: getString LT.DRIVER_CHARGED_MORE }
    ]
  , ( [ { id: "15", text: getString LT.TRIP_DELAYED } ] <> acNotWorkingPill (RideBookingRes state)
    )
  ]

acNotWorkingPill :: RideBookingRes -> Array ST.FeedbackItem
acNotWorkingPill (RideBookingRes state) =
  ( case state.serviceTierName of
      Just serviceTierName ->
        if ServiceTierCard.showACDetails serviceTierName Nothing (getFareProductType ((state.bookingDetails) ^. _fareProductType)) then
          [ { id: "14", text: getString LT.AC_TURNED_OFF } ]
        else
          []
      Nothing -> []
  )

feedBack :: String -> String -> String 
feedBack id vehicleVariant  = case id of 
                              "3" -> case vehicleVariant of 
                                      _ | DA.elem vehicleVariant ["AUTO_RICKSHAW", "EV_AUTO_RICKSHAW"] -> getString LT.DIFFERENT_AUTO
                                      "BIKE" -> getString LT.DIFFERENT_BIKE
                                      _ -> getString LT.DIFFERENT_CAB
                              "11" -> case vehicleVariant of 
                                      _ | DA.elem vehicleVariant ["AUTO_RICKSHAW", "EV_AUTO_RICKSHAW"] -> getString LT.UNCOMFORTABLE_AUTO
                                      "BIKE" -> getString LT.UNCOMFORTABLE_BIKE
                                      _ -> if isAmbulance vehicleVariant then getStringV2 uncomfortable_ambulance else  getString LT.UNCOMFORTABLE_CAB
                              "12" -> case vehicleVariant of 
                                      _ | DA.elem vehicleVariant ["AUTO_RICKSHAW", "EV_AUTO_RICKSHAW"] -> getString CLEAN_AUTO
                                      "BIKE" -> getString CLEAN_BIKE
                                      _ -> if isAmbulance vehicleVariant then getStringV2 clean_ambulance else getString CLEAN_CAB
                              _ -> ""
                              

feedbackPillDataWithRating2 :: RideBookingRes -> Array (Array ST.FeedbackItem)
feedbackPillDataWithRating2 (RideBookingRes state) = feedbackPillDataWithRating1 (RideBookingRes state)

feedbackPillDataWithRating3 :: String -> Array (Array ST.FeedbackItem)
feedbackPillDataWithRating3 vehicleVariant =
  [ [ { id: "8", text: getString LT.UNPROFESSIONAL_DRIVER }
    , { id: "8", text: getString LT.DRIVER_CHARGED_MORE }
    ]
  , [ { id: "3", text: getString LT.FELT_UNSAFE }
    , { id: "8", text: getString LT.RASH_DRIVING }
    , { id: "3", text: feedBack "3" vehicleVariant }
    ]
  , [ { id: "3", text: getString LT.TRIP_GOT_DELAYED }
    , { id: "11", text: feedBack "11" vehicleVariant }
    ]
  ]

feedbackPillDataWithRating4 :: String -> Array (Array ST.FeedbackItem)
feedbackPillDataWithRating4 vehicleVariant =
  [ [ { id: "9", text: getString LT.POLITE_DRIVER }
    , { id: "9", text: getString LT.EXPERT_DRIVING }
    , { id: "4", text: getString LT.SAFE_RIDE }
    ]
  , [ { id: "9", text: getString LT.ASKED_FOR_EXTRA_FARE }
    , { id: "11", text: feedBack "11" vehicleVariant }
    ]
  , [ { id: "4", text: getString LT.TRIP_GOT_DELAYED }
    , { id: "3", text: feedBack "3" vehicleVariant }
    ]
  ]

feedbackPillDataWithRating5 :: String -> Array (Array ST.FeedbackItem)
feedbackPillDataWithRating5 vehicleVariant =
  [ [ { id: "10", text: getString LT.POLITE_DRIVER }
    , { id: "5", text: getString LT.EXPERT_DRIVING }
    ]
  , [ { id: "12", text : feedBack "12" vehicleVariant }
    , { id: "10", text: getString LT.ON_TIME }
    ]
  , [ { id: "10", text: getString LT.SKILLED_NAVIGATOR }
    , { id: "5", text: getString LT.SAFE_RIDE }
    ]
  ]


parcelFeedbackPillData :: Array ( Array (Array ST.FeedbackItem))
parcelFeedbackPillData = [ parcelFeedbackPillDataWithRating1, parcelFeedbackPillDataWithRating2, parcelFeedbackPillDataWithRating3, parcelFeedbackPillDataWithRating4, parcelFeedbackPillDataWithRating5 ]

parcelFeedbackPillDataWithRating1 :: Array (Array ST.FeedbackItem)
parcelFeedbackPillDataWithRating1 =
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

parcelFeedbackPillDataWithRating2 :: Array (Array ST.FeedbackItem)
parcelFeedbackPillDataWithRating2 = parcelFeedbackPillDataWithRating1 

parcelFeedbackPillDataWithRating3 :: Array (Array ST.FeedbackItem)
parcelFeedbackPillDataWithRating3 =
  [ [ { id: "8", text: getString UNPROFESSIONAL_DRIVER }]
  , [ { id: "8", text: getString ASKED_FOR_EXTRA_FARE }
    , { id: "11", text: getString TOO_MANY_CALLS}
    ]
  , [ { id: "3", text: getString DELIVERY_DELAYED }
    , { id: "3", text: getString RUDE_BEHAVIOUR }
    ]
  ]

parcelFeedbackPillDataWithRating4 :: Array (Array ST.FeedbackItem)
parcelFeedbackPillDataWithRating4 =
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

parcelFeedbackPillDataWithRating5 :: Array (Array ST.FeedbackItem)
parcelFeedbackPillDataWithRating5 =
  [ [ { id: "10", text: getString POLITE_ATTITUDE }
    , { id: "5", text: getString SMOOTH_EXPERIENCE }
    ]
  , [ { id: "12", text: getString ON_TIME }
    , { id: "10", text: getString MINIMAL_CALLING }
    ]
  , [ { id: "10", text: getString SECURE_DELIVERY }]
  ]


checkRideStatus :: Boolean -> Boolean -> FlowBT String Unit --TODO:: Need to refactor this function
checkRideStatus rideAssigned prioritizeRating = do
  logField_ <- lift $ lift $ getLogFields

  rideBookingListResponse <- lift $ lift $ Remote.rideBookingList "2" "0" "true"
  (GlobalState state') <- getState
  let state = state'.homeScreen
  case rideBookingListResponse of
    Right (RideBookingListRes listResp) -> do
      if not (null listResp.list) && not prioritizeRating then do
        let _ = setKeyInWindow "forceAppToNoInternetScreen" false
        let multipleScheduled = length listResp.list > 1
            (RideBookingRes resp) = (fromMaybe HSD.dummyRideBooking (head listResp.list))
            status = (fromMaybe dummyRideAPIEntity (head resp.rideList))^._status
            bookingStatus = resp.status
            fareProductType = getFareProductType ((resp.bookingDetails) ^. _fareProductType)
            otpCode = ((resp.bookingDetails) ^. _contents ^. _otpCode)
            rideStatus = if state.data.sourceFromFCM == "TRUSTED_CONTACT" then ChatWithDriver else if status == "NEW" || (((bookingStatus == "CONFIRMED") || (bookingStatus == "TRIP_ASSIGNED" && not state.props.isOtpRideFlow ))  &&  (fareProductType == FPT.ONE_WAY_SPECIAL_ZONE || isJust otpCode)) then RideAccepted else if status == "INPROGRESS" then RideStarted else HomeScreen
            rideScheduledAt = if bookingStatus == "CONFIRMED" || bookingStatus == "TRIP_ASSIGNED" then fromMaybe "" resp.rideScheduledTime else ""
            dropLocation = if (fareProductType == FPT.RENTAL) then _stopLocation else _toLocation
            stopLocationDetails = (resp.bookingDetails ^._contents^._stopLocation)
            (BookingLocationAPIEntity dropLocationDetails)= fromMaybe dummyBookingDetails (resp.bookingDetails ^._contents^.dropLocation)
            (BookingLocationAPIEntity pickupLocationDetails) = resp.fromLocation
            response = listResp.list
            activeRideListData = map (\resp -> let listData = fetchListData resp state 
              in listData) (response)
            requestorPartyRoles = (resp.bookingDetails ^._contents^._requestorPartyRoles)
            newState = 
              state
                { data
                    { driverInfoCardState = getDriverInfo state.data.specialZoneSelectedVariant (RideBookingRes resp) (fareProductType == FPT.ONE_WAY_SPECIAL_ZONE || isJust otpCode) state.data.driverInfoCardState
                    , activeRidesList = activeRideListData
                    , upcomingRideDetails = 
                        maybe 
                          Nothing 
                          (\ride -> 
                            let rideScheduledAt = fromMaybe (EHC.getUTCAfterNSeconds (EHC.getCurrentUTC "") 1800) ride.rideScheduledAtUTC
                                rideScheduledTimeInIST = convertUTCtoISC rideScheduledAt "D" <> " " <> convertUTCtoISC rideScheduledAt "MMMM" <> " " <> convertUTCtoISC rideScheduledAt "YYYY" <> " , " <> convertUTCtoISC rideScheduledAt "HH" <> ":" <> convertUTCtoISC rideScheduledAt "mm"
                            in if ride.status == "NEW" 
                                then Just {bookingId : "", rideScheduledAt : rideScheduledTimeInIST } 
                                else Nothing) 
                          (activeRideListData!!1)
                    , sourceAddress = getAddressFromBooking resp.fromLocation
                    , destinationAddress = getAddressFromBooking (fromMaybe dummyBookingDetails (resp.bookingDetails ^._contents^.dropLocation))
                    , fareProductType = fareProductType
                    , vehicleVariant = (fromMaybe dummyRideAPIEntity (head resp.rideList))^._vehicleVariant
                    , startedAtUTC = fromMaybe "" resp.rideStartTime
                    , sourceFromFCM = ""
                    , rentalsInfo = (if rideScheduledAt == "" then Nothing else (Just{
                        rideScheduledAtUTC : rideScheduledAt
                      , bookingId : resp.id
                      , multipleScheduled : multipleScheduled
                      , fareProductType : fareProductType
                      , nearestRideScheduledAtUTC : ""
                      , vehicleVariant : fromMaybe "" resp.vehicleServiceTierType
                      , driverInformation : fetchDriverInformation resp.rideList
                      }))
                    , toll {
                        estimatedCharges = getFareFromArray resp.estimatedFareBreakup "TOLL_CHARGES"
                      , showIncludedPopUp = rideStatus == RideAccepted && (getFareFromArray resp.estimatedFareBreakup "TOLL_CHARGES" > 0.0)
                      }
                    , requestorPartyRoles = requestorPartyRoles
                      },
                  props
                    { currentStage = rideStatus
                    , rideRequestFlow = true
                    , bookingId = resp.id
                    , isPopUp = NoPopUp
                    , stopLoc = maybe (Nothing) (\loc -> Just {
                        lat : loc^._lat ,
                        lng : loc^._lon,
                        stopLocAddress : decodeAddress (Booking loc)
                      }) stopLocationDetails
                    , zoneType = getSpecialTag resp.specialLocationTag
                    , showAcWorkingPopup = rideStatus == RideStarted
                  }
                }
        setValueToLocalStore IS_SOS_ACTIVE $ show $ Just CTA.Pending == resp.sosStatus
        void $ liftFlowBT
          $ setFlowStatusData
              ( FlowStatusData
                  { source:
                      { lat: pickupLocationDetails.lat
                      , lng: pickupLocationDetails.lon
                      , place: decodeAddress (Booking resp.fromLocation)
                      , address: Nothing
                      , city: getCityCodeFromCity newState.props.city
                      , isSpecialPickUp: Just false
                      }
                  , destination:
                      { lat: dropLocationDetails.lat
                      , lng: dropLocationDetails.lon
                      , place: (decodeAddress (Booking (fromMaybe dummyBookingDetails (resp.bookingDetails ^._contents^._toLocation))))
                      , address: Nothing
                      , city: Nothing
                      , isSpecialPickUp: Just false
                      }
                  , sourceAddress: getAddressFromBooking resp.fromLocation
                  , destinationAddress:  getAddressFromBooking (fromMaybe dummyBookingDetails (resp.bookingDetails ^._contents^.dropLocation))
                  , sourceLabelIcon: Just $ zoneLabelIcon newState.props.zoneType.sourceTag
                  , destLabelIcon: Just $ zoneLabelIcon newState.props.zoneType.destinationTag
                  , sourceGeoJson: newState.props.locateOnMapProps.sourceGeoJson
                  , sourceGates: newState.props.locateOnMapProps.sourceGates
                  }
              )
        if rideStatus == HomeScreen then do 
          modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{data{rentalsInfo = (if rideScheduledAt == "" then Nothing else (Just{
                        rideScheduledAtUTC : rideScheduledAt
                      , bookingId : resp.id
                      , multipleScheduled : multipleScheduled
                      , fareProductType : fareProductType
                      , nearestRideScheduledAtUTC : ""
                      , vehicleVariant : fromMaybe "" resp.vehicleServiceTierType
                      , driverInformation : fetchDriverInformation resp.rideList
                      }))}})
          updateLocalStage HomeScreen
        else do
          when (not rideAssigned) $ do
            lift $ lift $ liftFlow $ logEvent logField_ "ny_active_ride_with_idle_state"
            void $ pure $ logEventWithTwoParams logField_ "ny_active_ride_with_idle_state" "status" status "bookingId" resp.id
          modifyScreenState $ HomeScreenStateType (\homeScreen → newState)
          updateLocalStage rideStatus
          maybe' (\_ -> pure unit) updateFlowStatusData (getFlowStatusData "LazyCheck")
          let (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
              (RideBookingDetails contents) = bookingDetails.contents
              otpCode = contents.otpCode
              rideListItem = head resp.rideList
          case rideListItem of
            Nothing -> do
              case otpCode of
                Just otp' -> do
                  setValueToLocalStore TRACKING_ENABLED "True"
                  modifyScreenState $ HomeScreenStateType (\homeScreen → 
                    homeScreen
                    { props
                      { isSpecialZone = true
                      , isOtpRideFlow = true
                      , isInApp = true
                      }
                    , data
                      { driverInfoCardState
                        { otp = otp' }
                      }
                    })
                Nothing -> pure unit
            Just (RideAPIEntity _) ->
              if isJust otpCode then do
                setValueToLocalStore TRACKING_ENABLED "True"
                modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{props{isSpecialZone = true,isInApp = true, isOtpRideFlow = true }}) else
                pure unit
      else if (DA.notElem (getValueToLocalStore RATING_SKIPPED) ["__failed", "null", "true"]) then do
        updateLocalStage HomeScreen
        rideBookingListResponse <- lift $ lift $ Remote.rideBookingListWithStatus "1" "0" "COMPLETED" Nothing
        case rideBookingListResponse of
          Right (RideBookingListRes listResp) -> do
            let (RideBookingRes resp) = (fromMaybe HSD.dummyRideBooking (head listResp.list))
                (RideAPIEntity ride) = fromMaybe dummyRideAPIEntity (resp.rideList !! 0)
            if not (null listResp.list) then do
              if (getValueToLocalStore RATING_SKIPPED == ride.id) then do 
                modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{props{currentStage = HomeScreen}})
                updateLocalStage HomeScreen
              else do
                let _ = setKeyInWindow "forceAppToNoInternetScreen" false
                let (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
                    (RideBookingDetails contents) = bookingDetails.contents
                    (RideAPIEntity currRideListItem) = fromMaybe dummyRideAPIEntity $ head resp.rideList
                    differenceOfDistance = fromMaybe 0 contents.estimatedDistance - (fromMaybe 0 currRideListItem.chargeableRideDistance)
                    lastRideDate = (case currRideListItem.rideStartTime of
                                    Just startTime -> (convertUTCtoISC startTime "DD/MM/YYYY")
                                    Nothing        -> "")
                    currentDate =  getCurrentDate ""
                    fareBreakup = resp.fareBreakup
                setValueToLocalStore IS_SOS_ACTIVE $ show false
                if(lastRideDate /= currentDate) then do
                  setValueToLocalStore FLOW_WITHOUT_OFFERS "true"
                  setValueToLocalStore TEST_MINIMUM_POLLING_COUNT "4"
                  setValueToLocalStore TEST_POLLING_INTERVAL "8000.0"
                  setValueToLocalStore TEST_POLLING_COUNT "22"
                  setValueToLocalStore CONFIRM_QUOTES_POLLING_COUNT "100"
                  pure unit
                  else pure unit
                when (isNothing currRideListItem.rideRating) $ do
                  when (length listResp.list > 0) $ do
                    let 
                      fareProductType = getFareProductType ((resp.bookingDetails) ^. _fareProductType)
                      (RideBookingAPIDetails bookingDetails) = resp.bookingDetails
                      (RideBookingDetails contents) = bookingDetails.contents
                      (RideAPIEntity ride) = fromMaybe dummyRideAPIEntity (resp.rideList !! 0)
                      (BookingLocationAPIEntity bookingLocationAPIEntity) = resp.fromLocation
                      isBlindPerson = getValueToLocalStore DISABILITY_NAME == "BLIND_LOW_VISION"
                      hasAccessibilityIssue' =  resp.hasDisability == Just true 
                      postRideCheckCache = SU.getPostRideCheckSettingsFromCache ""
                      hasSafetyIssue' = maybe false (\settings -> SU.showNightSafetyFlow resp.hasNightIssue resp.rideStartTime resp.rideEndTime settings.safetyCheckStartSeconds settings.safetyCheckEndSeconds settings.enablePostRideSafetyCheck && not isBlindPerson) postRideCheckCache
                      hasTollIssue' = finalFareHasToll && not isBlindPerson
                      waitingChargesApplied = isJust $ DA.find (\entity  -> entity ^._description == "WAITING_OR_PICKUP_CHARGES") ((RideBookingRes resp) ^._fareBreakup)
                      isRecentRide = EHC.getExpiryTime (fromMaybe "" ((RideBookingRes resp) ^. _rideEndTime)) true / 60 < state.data.config.safety.pastRideInterval
                      finalFareHasToll = DA.any (\entity  -> entity ^._description == "TOLL_CHARGES") (resp.fareBreakup)
                      estimateFareHasToll =  DA.any (\entity  -> entity ^._description == "TOLL_CHARGES") (resp.estimatedFareBreakup)
                      demandExtraTollAmountIssue' = estimateFareHasToll && (not finalFareHasToll)

                    modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{
                        props { currentStage = RideCompleted
                              , estimatedDistance = contents.estimatedDistance
                              , zoneType = getSpecialTag resp.specialLocationTag
                              }
                      , data { rideRatingState
                              { driverName = currRideListItem.driverName
                              , rideId = currRideListItem.id
                              , finalAmount = (fromMaybe 0 currRideListItem.computedPrice)
                              , source = decodeAddress (Booking resp.fromLocation)
                              , destination = (decodeAddress (Booking (fromMaybe dummyBookingDetails (resp.bookingDetails ^._contents^._toLocation))))
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
                              , driverInfoCardState {
                                price = resp.estimatedTotalFare,
                                rideId = currRideListItem.id,
                                spLocationName = resp.specialLocationName,
                                addressWard = bookingLocationAPIEntity.ward,
                                providerType = maybe CTA.ONUS (\valueAdd -> if valueAdd then CTA.ONUS else CTA.OFFUS) resp.isValueAddNP,
                                rentalData 
                                    { finalDuration = (fromMaybe 0 resp.duration) / (60)
                                    , finalDistance = (fromMaybe 0 ride.chargeableRideDistance)/1000
                                    , baseDuration = (fromMaybe 0 resp.estimatedDuration) / (60*60)
                                    , baseDistance = (fromMaybe 0 resp.estimatedDistance) / 1000
                                    , rideStartedAt = case currRideListItem.rideStartTime of
                                                        Just startTime -> (convertUTCtoISC startTime "h:mm A")
                                                        Nothing        -> ""
                                    , rideEndedAt = case currRideListItem.rideEndTime of
                                                  Just endTime   -> " " <>(convertUTCtoISC endTime "h:mm A")
                                                  Nothing        -> ""
                                    , extraDistanceFare = show $ getFareFromArray fareBreakup "DIST_BASED_FARE"
                                    , extraTimeFare = show $ getFareFromArray fareBreakup "TIME_BASED_FARE"
                                    }
                              }
                              , ratingViewState { rideBookingRes = (RideBookingRes resp)}
                              , vehicleVariant = currRideListItem.vehicleVariant
                              , fareProductType = fareProductType
                              , rideCompletedData { 
                                  issueReportData { 
                                    hasAccessibilityIssue = hasAccessibilityIssue'
                                  , hasSafetyIssue = hasSafetyIssue'
                                  , hasTollIssue = hasTollIssue'
                                  , showIssueBanners = hasAccessibilityIssue' || hasSafetyIssue' || hasTollIssue'
                                  }
                                }
                              , toll {
                                confidence = ride.tollConfidence
                              , showAmbiguousPopUp = ride.tollConfidence == Just CTA.Unsure
                              }
                              , requestorPartyRoles = contents.requestorPartyRoles
                            }
                          }
                        )
                    changeRideCompletedState state (RideAPIEntity currRideListItem) (RideBookingRes resp) (RideBookingDetails contents) differenceOfDistance waitingChargesApplied fareProductType isRecentRide fareBreakup finalFareHasToll estimateFareHasToll demandExtraTollAmountIssue' hasAccessibilityIssue' hasSafetyIssue' (RideAPIEntity ride)
                    updateLocalStage RideCompleted
                  when (length listResp.list == 0 && not state.props.isSharedLocationFlow) $ do 
                    modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{props{currentStage = HomeScreen}})
                    updateLocalStage HomeScreen
            else do
              modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{props{currentStage = HomeScreen}})
              updateLocalStage HomeScreen
          Left err -> do 
            modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{props{currentStage = HomeScreen}})
            updateLocalStage HomeScreen
      else do
        modifyScreenState $ HomeScreenStateType (\homeScreen → homeScreen{props{currentStage = HomeScreen}})
        updateLocalStage HomeScreen
    Left err -> updateLocalStage HomeScreen
  if not (any isLocalStageOn [RideAccepted, RideStarted]) then removeChatService "" else pure unit
  where 
    updateFlowStatusData :: FlowStatusData -> FlowBT String Unit
    updateFlowStatusData (FlowStatusData flowStatusData) = 
      modifyScreenState $ HomeScreenStateType (\homeScreen -> homeScreen{props{ city = getCityNameFromCode flowStatusData.source.city
                                                                              , locateOnMapProps{ sourceLocationName = flowStatusData.source.address
                                                                                                , sourceGeoJson = flowStatusData.sourceGeoJson
                                                                                                , sourceGates = flowStatusData.sourceGates } }})
        
changeRideCompletedState :: HomeScreenState -> RideAPIEntity -> RideBookingRes -> RideBookingDetails -> Int -> Boolean -> FPT.FareProductType -> Boolean -> Array FareBreakupAPIEntity -> Boolean -> Boolean -> Boolean -> Boolean -> Boolean -> RideAPIEntity -> FlowBT String Unit
changeRideCompletedState state (RideAPIEntity currRideListItem) (RideBookingRes resp) (RideBookingDetails contents) differenceOfDistance waitingChargesApplied fareProductType isRecentRide fareBreakup finalFareHasToll estimateFareHasToll demandExtraTollAmountIssue' hasAccessibilityIssue' hasSafetyIssue' (RideAPIEntity ride) = do
  let parkingCharges = DA.find (\entity  -> entity ^._description == "PARKING_CHARGE") (resp.fareBreakup)
  modifyScreenState
    $ RiderRideCompletedScreenStateType
        ( \riderRideCompletedScreen ->
            riderRideCompletedScreen
              {
                topCard {
                  title = getString LT.RIDE_COMPLETED,
                  finalAmount = (fromMaybe 0 currRideListItem.computedPrice),
                  initialAmount = resp.estimatedTotalFare,
                  fareUpdatedVisiblity = (fromMaybe 0 currRideListItem.computedPrice) /= resp.estimatedTotalFare && contents.estimatedDistance /= Nothing,
                  infoPill {
                    text = getFareUpdatedStr differenceOfDistance waitingChargesApplied,
                    imageVis = VISIBLE,
                    visible = if (fromMaybe 0 currRideListItem.computedPrice) == resp.estimatedTotalFare || contents.estimatedDistance == Nothing then GONE else VISIBLE
                  }
                }
              , driverInfoCardState
                  {
                    driverName =  currRideListItem.driverName,
                    isAlreadyFav = fromMaybe false resp.isAlreadyFav,
                    fareProductType = fareProductType,
                    favCount = fromMaybe 0 resp.favCount
                  }
              , showSafetyCenter = state.data.config.feature.enableSafetyFlow && isRecentRide && not state.props.isSafetyCenterDisabled
              , rideDuration = resp.duration
              , rideId = currRideListItem.id
              , rentalRowDetails
                { rideTime = getStringV2 ride_time
                , rideDistance = getString LT.RIDE_DISTANCE
                , rideDistanceInfo = "( " <> getString LT.CHARGEABLE <> " / " <> getString LT.BOOKED <> " )"
                , rideStartedAt = getString LT.RIDE_STARTED_AT
                , rideEndedAt = getString LT.RIDE_ENDED_AT
                , estimatedFare = getString LT.ESTIMATED_FARE
                , extraTimeFare = getString LT.EXTRA_TIME_FARE
                , extraDistanceFare = getString LT.EXTRA_DISTANCE_FARE
                , totalFare = getString LT.TOTAL_FARE
                , rideDetailsTitle = getString LT.RIDE_DETAILS
                , fareUpdateTitle = getString LT.FARE_UPDATE
                , surcharges = getString LT.SURCHARGES
                }
              , rentalBookingData
                { baseDuration = (fromMaybe 0 resp.estimatedDuration) / (60*60)
                , baseDistance = (fromMaybe 0 resp.estimatedDistance) / 1000
                , finalDuration = (fromMaybe 0 resp.duration) / (60)
                , finalDistance = (fromMaybe 0 ride.chargeableRideDistance)/1000
                , rideStartedAt = case currRideListItem.rideStartTime of
                                    Just startTime -> (convertUTCtoISC startTime "h:mm A")
                                    Nothing        -> ""
                , rideEndedAt = case currRideListItem.rideEndTime of
                                  Just endTime   -> " " <>(convertUTCtoISC endTime "h:mm A")
                                  Nothing        -> ""
                , extraTimeFare = show $ getFareFromArray fareBreakup "TIME_BASED_FARE"
                , extraDistanceFare = show $ getFareFromArray fareBreakup "DIST_BASED_FARE"
                }
              , showRentalRideDetails = fareProductType == FPT.RENTAL
              , ratingCard
                {
                  feedbackPillData = customerFeedbackPillData (RideBookingRes resp) ride.vehicleVariant
                }
              , rideRatingState
                  { driverName = currRideListItem.driverName
                  , rideId = currRideListItem.id
                  , distanceDifference = differenceOfDistance
                  , rideStartTime = maybe "" (\startTime -> convertUTCtoISC startTime "h:mm A") currRideListItem.rideStartTime
                  , rideEndTime = maybe "" (\endTime -> convertUTCtoISC endTime "h:mm A") currRideListItem.rideEndTime
                  }
              , ratingViewState
                  { rideBookingRes = (RideBookingRes resp)
                  }
              , isSafetyCenterDisabled = state.props.isSafetyCenterDisabled 
              , bookingId = resp.id
              , additionalCharges = [
                  {
                    text :  getString if ride.tollConfidence == (Just CTA.Unsure) then  TOLL_ROAD_CHANGED else if finalFareHasToll then  TOLL_CHARGES_INCLUDED else TOLL_ROAD_CHANGED 
                  , visibility : boolToVisibility $ finalFareHasToll || estimateFareHasToll
                  , image :  fetchImage FF_COMMON_ASSET "ny_ic_grey_toll"
                  , textColor : Color.black700
                  },
                  {
                    text : maybe "" (\parking ->  getString $ PARKING_CHARGES_INCLUDED $ (getCurrency appConfig) <>  (show $ INT.ceil $ parking ^. _amount)) parkingCharges
                  , visibility : boolToVisibility $ isJust parkingCharges 
                  , image : fetchImage FF_COMMON_ASSET "ny_ic_parking_logo_grey"
                  , textColor : Color.black700
                  }
                ]
              , customerIssue = riderRideCompletedScreen.customerIssue
                  { showIssueBanners = hasAccessibilityIssue' || hasSafetyIssue' || demandExtraTollAmountIssue'
                  , hasAccessibilityIssue = hasAccessibilityIssue'
                  , hasSafetyIssue = hasSafetyIssue'
                  , demandExtraTollAmountIssue = demandExtraTollAmountIssue'
                  }
            }
        )

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


fetchListData :: RideBookingRes -> HomeScreenState -> DriverInfoCard
fetchListData (RideBookingRes resp) state = let 
    otp = ((resp.bookingDetails) ^. _contents ^. _otpCode)
    fPT = getFareProductType ((resp.bookingDetails) ^. _fareProductType)
    ans = getDriverInfo state.data.specialZoneSelectedVariant (RideBookingRes resp) (fPT == FPT.ONE_WAY_SPECIAL_ZONE || isJust otp ) state.data.driverInfoCardState
  in ans
  
setFlowStatusData object = void $ pure $ setValueToLocalStore FLOW_STATUS_DATA (encodeJSON object)
