{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Booking where

import qualified API.Types.UI.MultimodalConfirm as APITypes
import qualified API.Types.UI.Pass as PassAPI
import qualified Beckn.ACL.Cancel as CancelACL
import qualified Beckn.ACL.Common as Common
import qualified Beckn.ACL.Status as StatusACL
import qualified Beckn.ACL.Update as ACL
import qualified BecknV2.OnDemand.Utils.Common as Utils
import BecknV2.Utils
import Data.Maybe
import Data.OpenApi (ToSchema (..))
import qualified Data.Sequence as Seq
import qualified Data.Time as DT
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.Pass as DPass
import Domain.Action.UI.Serviceability
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Booking.API as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.BookingStatus as SRB
import Domain.Types.CancellationReason
import qualified Domain.Types.Client as DC
import qualified Domain.Types.Journey as DJ
import Domain.Types.Location
import Domain.Types.LocationAddress
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Merchant
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PassType
import qualified Domain.Types.Person as Person
import qualified Domain.Types.PurchasedPass as DPurchasedPass
import qualified Domain.Types.Ride as DTR
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id, pack, safeHead)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Maps (LatLong (..))
import Kernel.Prelude (intToNominalDiffTime)
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Common
import Kernel.Types.Flow
import Kernel.Types.Id
import Kernel.Types.Version (CloudType (GCP))
import Kernel.Utils.Common
import Lib.JourneyModule.Base (generateJourneyInfoResponse, getAllLegsInfo)
import Lib.JourneyModule.Types (GetStateFlow)
import qualified Lib.JourneyModule.Utils as JMU
import qualified SharedLogic.Booking as SB
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.Type as SLT
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant as CQMerchant
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Booking as QRB
import Storage.Queries.JourneyExtra as SQJ
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.PassTypeExtra as QPassType
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PurchasedPassExtra as QPurchasedPass
import qualified Storage.Queries.Ride as QR
import Tools.Error

data StopReq = StopReq
  { gps :: LatLong,
    address :: LocationAddress
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype DriverNo = DriverNo
  { driverNumber :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype BookingListRes = BookingListRes
  { list :: [SRB.BookingAPIEntity]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype FavouriteBookingListRes = FavouriteBookingListRes
  { list :: [SRB.FavouriteBookingAPIEntity]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

bookingStatus :: Id SRB.Booking -> (Id Person.Person, Id Merchant.Merchant) -> Flow SRB.BookingAPIEntity
bookingStatus bookingId (personId, _merchantId) = runInMultiCloud $ do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  fork "booking status update" $ checkBookingsForStatus [booking]
  fork "creating cache for emergency contact SOS" $ emergencyContactSOSCache booking personId
  logInfo $ "booking: test " <> show booking
  void $ handleConfirmTtlExpiry booking
  SRB.buildBookingAPIEntity booking booking.riderId

bookingStatusPolling :: Id SRB.Booking -> (Id Person.Person, Id Merchant.Merchant) -> Flow SRB.BookingStatusAPIEntity
bookingStatusPolling bookingId _ = runInMultiCloud $ do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  fork "booking status update" $ checkBookingsForStatus [booking]
  logInfo $ "booking: test " <> show booking
  handleConfirmTtlExpiry booking
  SRB.buildBookingStatusAPIEntity booking

handleConfirmTtlExpiry :: SRB.Booking -> Flow ()
handleConfirmTtlExpiry booking = do
  bapConfigs <- QBC.findByMerchantIdDomainandMerchantOperatingCityId booking.merchantId "MOBILITY" booking.merchantOperatingCityId
  bapConfig <- listToMaybe bapConfigs & fromMaybeM (InvalidRequest $ "BecknConfig not found for merchantId " <> show booking.merchantId.getId <> " merchantOperatingCityId " <> show booking.merchantOperatingCityId.getId) -- Using findAll for backward compatibility, TODO : Remove findAll and use findOne
  confirmBufferTtl <- bapConfig.confirmBufferTTLSec & fromMaybeM (InternalError "Invalid ttl")
  now <- getCurrentTime
  confirmTtl <- bapConfig.confirmTTLSec & fromMaybeM (InternalError "Invalid ttl")
  initTtl <- bapConfig.initTTLSec & fromMaybeM (InternalError "Invalid ttl")
  let ttlInInt = initTtl + confirmTtl + confirmBufferTtl
      ttlToNominalDiffTime = intToNominalDiffTime ttlInInt
      ttlUtcTime = addDurationToUTCTime booking.createdAt ttlToNominalDiffTime
  when (booking.status == SRB.NEW && (ttlUtcTime < now)) do
    dCancelRes <- DCancel.cancel booking Nothing cancelReq SBCR.ByApplication
    void . withShortRetry $ CallBPP.cancelV2 booking.merchantId dCancelRes.bppUrl =<< CancelACL.buildCancelReqV2 dCancelRes Nothing
    throwError $ RideInvalidStatus "Booking Invalid"
  where
    cancelReq =
      DCancel.CancelReq
        { reasonCode = CancellationReasonCode "External/Beckn API failure",
          reasonStage = OnConfirm,
          additionalInfo = Nothing,
          reallocate = Nothing,
          blockOnCancellationRate = Nothing
        }

callOnStatus :: SRB.Booking -> Flow ()
callOnStatus currBooking = do
  merchant <- CQMerchant.findById currBooking.merchantId >>= fromMaybeM (MerchantNotFound currBooking.merchantId.getId)
  city <- CQMOC.findById currBooking.merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound currBooking.merchantOperatingCityId.getId)
  let dStatusReq = StatusACL.DStatusReq currBooking merchant city
  becknStatusReq <- StatusACL.buildStatusReqV2 dStatusReq
  messageId <- Utils.getMessageId becknStatusReq.statusReqContext
  -- TODO: REMOVE ALL THE CHECKS WHICH ARE NOT FORWARD COMPATIBLE MEANING FOR BOOKING NEW CAN GO TO CONFIRMED BUT NOT OTHER STATUS CAN GOTO CONFIRM.
  Hedis.setExp (Common.makeContextMessageIdStatusSyncKey messageId) True 3600
  void $ withShortRetry $ CallBPP.callStatusV2 currBooking.providerUrl becknStatusReq merchant.id

checkBookingsForStatus :: [SRB.Booking] -> Flow ()
checkBookingsForStatus (currBooking : bookings) = do
  riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow currBooking.merchantOperatingCityId currBooking.configInExperimentVersions >>= fromMaybeM (RiderConfigDoesNotExist currBooking.merchantOperatingCityId.getId)
  case (riderConfig.bookingSyncStatusCallSecondsDiffThreshold, currBooking.estimatedDuration) of
    (Just timeDiffThreshold, Just estimatedEndDuration) -> do
      now <- getCurrentTime
      let estimatedEndTime = DT.addUTCTime (fromIntegral estimatedEndDuration.getSeconds) currBooking.startTime
      let diff = DT.diffUTCTime now estimatedEndTime
      let callStatusConditionNew = (currBooking.status == SRB.NEW && diff > fromIntegral timeDiffThreshold) || (currBooking.status == SRB.CONFIRMED && diff > fromIntegral timeDiffThreshold)
          callStatusConditionTripAssigned = currBooking.status == SRB.TRIP_ASSIGNED && diff > fromIntegral timeDiffThreshold
      when callStatusConditionNew $ do
        callOnStatus currBooking
      when callStatusConditionTripAssigned $ do
        callOnStatus currBooking
      checkBookingsForStatus bookings
    (_, _) -> logError "Nothing values for time diff threshold or booking end duration"
checkBookingsForStatus [] = pure ()

getBookingList :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Maybe Text -> Bool -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe SRB.BookingStatus -> Maybe (Id DC.Client) -> Maybe Integer -> Maybe Integer -> [SRB.BookingStatus] -> Maybe (Id DMOC.MerchantOperatingCity) -> Flow ([SRB.Booking], [SRB.Booking])
getBookingList (mbPersonId, merchantId) mbAgentId onlyDashboard mbLimit mbOffset mbOnlyActive mbBookingStatus mbClientId mbFromDate' mbToDate' mbBookingStatusList mbMerchantOperatingCityId = do
  logInfo $ "getBookingList: Executing query for personId=" <> show mbPersonId <> ", merchantId=" <> show merchantId <> ", onlyActive=" <> show mbOnlyActive
  let mbFromDate = millisecondsToUTC <$> mbFromDate'
      mbToDate = millisecondsToUTC <$> mbToDate'
  let mbOnlyDashboard = if onlyDashboard then Just True else Nothing
  (rbList, allbookings) <- if Just True == mbOnlyActive then runInMultiCloud $ QR.findAllByRiderIdAndRide mbPersonId mbAgentId mbOnlyDashboard mbLimit mbOffset mbOnlyActive mbBookingStatus mbClientId mbFromDate mbToDate mbBookingStatusList mbMerchantOperatingCityId else QR.findAllByRiderIdAndRide mbPersonId mbAgentId mbOnlyDashboard mbLimit mbOffset mbOnlyActive mbBookingStatus mbClientId mbFromDate mbToDate mbBookingStatusList mbMerchantOperatingCityId
  let limit = maybe 10 fromIntegral mbLimit
  if null rbList
    then do
      now <- getCurrentTime
      let allBookingsNotScheduled = length $ filter (\booking -> booking.startTime < now) allbookings
      if allBookingsNotScheduled == limit
        then do
          getBookingList (mbPersonId, merchantId) mbAgentId onlyDashboard (Just (fromIntegral (limit + 1))) mbOffset mbOnlyActive mbBookingStatus mbClientId mbFromDate' mbToDate' mbBookingStatusList mbMerchantOperatingCityId
        else do
          return (rbList, allbookings)
    else do
      return (rbList, allbookings)

getJourneyList :: Id Person.Person -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Integer -> [DJ.JourneyStatus] -> Maybe Bool -> Flow [DJ.Journey]
getJourneyList personId mbLimit mbOffset mbFromDate' mbToDate' mbJourneyStatusList mbIsPaymentSuccess = do
  logInfo $ "getJourneyList: Executing query for personId=" <> show personId <> ", limit=" <> show mbLimit <> ", offset=" <> show mbOffset
  let mbFromDate = millisecondsToUTC <$> mbFromDate'
      mbToDate = millisecondsToUTC <$> mbToDate'
  SQJ.findAllByRiderId personId mbLimit mbOffset mbFromDate mbToDate mbJourneyStatusList mbIsPaymentSuccess

bookingList :: (Maybe (Id Person.Person), Id Merchant.Merchant) -> Maybe Text -> Bool -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe SRB.BookingStatus -> Maybe (Id DC.Client) -> Maybe Integer -> Maybe Integer -> [SRB.BookingStatus] -> Maybe (Id DMOC.MerchantOperatingCity) -> Flow BookingListRes
bookingList (mbPersonId, merchantId) mbAgentId onlyDashboard mbLimit mbOffset mbOnlyActive mbBookingStatus mbClientId mbFromDate' mbToDate' mbBookingStatusList mbMerchantOperatingCityId = do
  (rbList, allbookings) <- getBookingList (mbPersonId, merchantId) mbAgentId onlyDashboard mbLimit mbOffset mbOnlyActive mbBookingStatus mbClientId mbFromDate' mbToDate' mbBookingStatusList mbMerchantOperatingCityId
  case mbPersonId of
    Just personId -> returnResonseAndClearStuckRides allbookings rbList personId
    Nothing -> BookingListRes <$> traverse (\booking -> SRB.buildBookingAPIEntity booking booking.riderId) rbList
  where
    returnResonseAndClearStuckRides allbookings rbList personId = do
      fork "booking list status update" $ checkBookingsForStatus allbookings
      logInfo $ "rbList: test " <> show rbList
      BookingListRes <$> traverse (`SRB.buildBookingAPIEntity` personId) rbList

getPassList :: Id Merchant.Merchant -> Id Person.Person -> Maybe Int -> Maybe Int -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe [Domain.Types.PassType.PassEnum] -> Flow [DPurchasedPass.PurchasedPass]
getPassList merchantId personId limitIntMaybe mbInitialPassOffsetInt mbFromDate' mbToDate' mbSendEligiblePassIfAvailable mbPassTypes = do
  let mbFromDateDay = millisecondsToDay <$> mbFromDate'
      mbToDateDay = millisecondsToDay <$> mbToDate'
  passTypeIds <-
    if mbSendEligiblePassIfAvailable == Just True
      then case mbPassTypes of
        Just passTypes | not (null passTypes) -> do
          types <- QPassType.findAllByPassEnums passTypes
          pure . Just $ map (.id) types
        _ ->
          pure Nothing
      else pure Nothing
  passes <-
    if mbSendEligiblePassIfAvailable == Just True
      then QPurchasedPass.findAllActiveByPersonIdWithFiltersV2 personId merchantId passTypeIds mbFromDateDay mbToDateDay limitIntMaybe mbInitialPassOffsetInt
      else pure []
  today <- DT.utctDay <$> getLocalCurrentTime (19800 :: Seconds)
  forM_ passes $ \pass -> DPass.expirePassStatus pass today
  pure passes
  where
    millisecondsToDay =
      DT.utctDay . millisecondsToUTC

data BookingListResV2 = BookingListResV2
  { list :: [BookingAPIEntityV2],
    bookingOffset :: Maybe Int,
    journeyOffset :: Maybe Int,
    passOffset :: Maybe Int,
    hasMoreData :: Bool
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data BookingAPIEntityV2 = Ride SRB.BookingAPIEntity | MultiModalRide APITypes.JourneyInfoResp | MultiModalPass PassAPI.PurchasedPassAPIEntity
  deriving (Generic, FromJSON, ToJSON, ToSchema)

bookingListV2ByCustomerLookup :: Id Merchant.Merchant -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Integer -> [SLT.BillingCategory] -> [SLT.RideType] -> Maybe [SRB.BookingStatus] -> Maybe [DJ.JourneyStatus] -> Maybe Bool -> Maybe SRB.BookingRequestType -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Flow BookingListResV2
bookingListV2ByCustomerLookup merchantId mbLimit mbOffset mbBookingOffset mbJourneyOffset mbFromDate' mbToDate' billingCategoryList rideTypeList mbBookingStatusList mbJourneyStatusList mbIsPaymentSuccess mbBookingRequestType mbMobileNo mbCountryCode mbEmail mbCustomerId = do
  personId <- case mbCustomerId of
    Just customerId -> pure (Id customerId)
    Nothing -> case mbMobileNo of
      Just mobileNo -> do
        mobileNoHash <- getDbHash mobileNo
        mbPerson <- QPerson.findByMobileNumberAndMerchantId (fromMaybe "+91" mbCountryCode) mobileNoHash merchantId
        case mbPerson of
          Just person -> pure person.id
          Nothing -> tryEmail
      Nothing -> tryEmail
  bookingListV2 (personId, merchantId) mbLimit mbOffset mbBookingOffset mbJourneyOffset Nothing mbFromDate' mbToDate' billingCategoryList rideTypeList (fromMaybe [] mbBookingStatusList) (fromMaybe [] mbJourneyStatusList) mbIsPaymentSuccess mbBookingRequestType Nothing Nothing
  where
    tryEmail =
      case mbEmail of
        Just email -> do
          person <- QPerson.findByEmailAndMerchantId merchantId email >>= fromMaybeM (InternalError "Person with given email does not exist")
          pure person.id
        Nothing -> throwError $ InternalError "No Person Found"

applyMasterDbIfGcp :: Flow a -> Flow a
applyMasterDbIfGcp computation = do
  mbCloudType <- asks (.cloudType)
  case mbCloudType of
    Just cloudType | cloudType == GCP -> do
      logInfo "applyMasterDbIfGcp: CloudType is GCP, routing queries to Master DB (AWS)"
      B.runInMasterDb computation
    _ -> do
      logInfo $ "applyMasterDbIfGcp: CloudType is " <> show mbCloudType <> ", using default replica/local DB"
      computation

bookingListV2 :: (Id Person.Person, Id Merchant.Merchant) -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Maybe Integer -> [SLT.BillingCategory] -> [SLT.RideType] -> [SRB.BookingStatus] -> [DJ.JourneyStatus] -> Maybe Bool -> Maybe SRB.BookingRequestType -> Maybe Bool -> Maybe [Domain.Types.PassType.PassEnum] -> Flow BookingListResV2
bookingListV2 (personId, merchantId) mbLimit mbOffset mbBookingOffset mbJourneyOffset mbPassOffset mbFromDate' mbToDate' billingCategoryList rideTypeList mbBookingStatusList mbJourneyStatusList mbIsPaymentSuccess mbBookingRequestType mbSendEligiblePassIfAvailable mbPassTypes = do
  applyMasterDbIfGcp $ do
    allPasses <- getPassList merchantId personId limitIntMaybe mbInitialPassOffsetInt mbFromDate' mbToDate' mbSendEligiblePassIfAvailable mbPassTypes
    (apiEntity, nextBookingOffset, nextJourneyOffset, nextPassOffset, hasMoreData) <- case mbBookingRequestType of
      Just SRB.BookingRequest -> do
        (rbList, allbookings) <- getBookingList (Just personId, merchantId) Nothing False integralLimit mbInitialBookingOffset Nothing Nothing Nothing mbFromDate' mbToDate' mbBookingStatusList Nothing

        -- Filter by ride type and billing category
        let (rbRideTypeFilteredList, allBookingsRideTypeFilteredList) = if not (null rideTypeList) then (filter (SB.matchesRideType rideTypeList) rbList, filter (SB.matchesRideType rideTypeList) allbookings) else (rbList, allbookings)

            (rbFilteredList, filteredAllBookingsList) = if not (null billingCategoryList) then (filter (SB.matchesBillingCategory billingCategoryList) rbRideTypeFilteredList, filter (SB.matchesBillingCategory billingCategoryList) allBookingsRideTypeFilteredList) else (rbRideTypeFilteredList, allBookingsRideTypeFilteredList)

        clearStuckRides (Just filteredAllBookingsList) rbFilteredList

        logDebug $ "myrides PersonId: " <> show personId <> " Limit: " <> show limit <> " offset: " <> show mbInitialBookingOffset <> " BookingRequest rbList (id, startTime): " <> show (map (\b -> (b.id, b.startTime)) rbFilteredList)

        let hasMoreData = length rbFilteredList + length allPasses >= limit

        (entitiesWithSource, finalBookingOffset, _, finalPassOffset) <- buildApiEntityForRideOrJourneyOrPassWithCounts personId limit rbFilteredList [] allPasses mbInitialBookingOffset (Just 0) (Just 0)

        pure (entitiesWithSource, Just finalBookingOffset, Nothing, Just finalPassOffset, hasMoreData)
      Just SRB.JourneyRequest -> do
        -- Journeys (NammaTransit) should only be included for PERSONAL billing category and NORMAL ride type
        let shouldIncludeJourneys = shouldIncludeJourneysForFilters billingCategoryList rideTypeList
        allJourneys <-
          if shouldIncludeJourneys
            then getJourneyList personId integralLimit mbInitialJourneyOffset mbFromDate' mbToDate' mbJourneyStatusList mbIsPaymentSuccess
            else pure []
        clearStuckRides Nothing []

        logDebug $ "myrides PersonId: " <> show personId <> " Limit: " <> show limit <> " offset: " <> show mbInitialJourneyOffset <> " JourneyRequest allJourneys (id, createdAt): " <> show (map (\j -> (j.id, j.createdAt)) allJourneys)

        let hasMoreData = length allJourneys + length allPasses >= limit

        (entitiesWithSource, _, finalJourneyOffset, finalPassOffset) <- buildApiEntityForRideOrJourneyOrPassWithCounts personId limit [] allJourneys allPasses (Just 0) mbInitialJourneyOffset (Just 0)

        pure (entitiesWithSource, Nothing, Just finalJourneyOffset, Just finalPassOffset, hasMoreData)
      _ -> do
        bookingListFork <-
          awaitableFork "bookingListV2->getBookingList" $
            applyMasterDbIfGcp $ getBookingList (Just personId, merchantId) Nothing False integralLimit mbInitialBookingOffset Nothing Nothing Nothing mbFromDate' mbToDate' mbBookingStatusList Nothing

        -- Journeys (NammaTransit) should only be included for PERSONAL billing category and NORMAL ride type
        let shouldIncludeJourneys = shouldIncludeJourneysForFilters billingCategoryList rideTypeList
        journeyListFork <-
          awaitableFork "bookingListV2->getJourneyList" $
            applyMasterDbIfGcp $
              if shouldIncludeJourneys
                then getJourneyList personId integralLimit mbInitialJourneyOffset mbFromDate' mbToDate' mbJourneyStatusList mbIsPaymentSuccess
                else pure []

        (rbList, allbookings) <-
          L.await Nothing bookingListFork >>= \case
            Left err -> throwError $ InternalError $ "Failed to get booking list: " <> show err
            Right result -> pure result

        -- Filter by ride type and billing category
        let (rbRideTypeFilteredList, allBookingsRideTypeFilteredList) = if not (null rideTypeList) then (filter (SB.matchesRideType rideTypeList) rbList, filter (SB.matchesRideType rideTypeList) allbookings) else (rbList, allbookings)

            (rbFilteredList, filteredAllBookingsList) = if not (null billingCategoryList) then (filter (SB.matchesBillingCategory billingCategoryList) rbRideTypeFilteredList, filter (SB.matchesBillingCategory billingCategoryList) allBookingsRideTypeFilteredList) else (rbRideTypeFilteredList, allBookingsRideTypeFilteredList)

        logDebug $ "myrides PersonId: " <> show personId <> " Limit: " <> show limit <> " offset: " <> show mbInitialBookingOffset <> " BookingRequest rbList (id, startTime): " <> show (map (\b -> (b.id, b.startTime)) rbFilteredList)

        allJourneys <-
          L.await Nothing journeyListFork >>= \case
            Left err -> throwError $ InternalError $ "Failed to get journey list: " <> show err
            Right result -> pure result

        logDebug $ "myrides PersonId: " <> show personId <> " Limit: " <> show limit <> " offset: " <> show mbInitialJourneyOffset <> " JourneyRequest allJourneys (id, createdAt): " <> show (map (\j -> (j.id, j.createdAt)) allJourneys)
        logDebug $ "myrides PersonId: " <> show personId <> " Limit: " <> show mbInitialPassOffset <> " PassRequest allPasses (id, startDate): " <> show (map (\p -> (p.id, p.startDate)) allPasses)

        let hasMoreData = length rbFilteredList + length allJourneys + length allPasses >= limit

        clearStuckRides (Just filteredAllBookingsList) rbFilteredList

        (entitiesWithSource, finalBookingOffset, finalJourneyOffset, finalPassOffset) <- buildApiEntityForRideOrJourneyOrPassWithCounts personId limit rbFilteredList allJourneys allPasses mbInitialBookingOffset mbInitialJourneyOffset mbInitialPassOffset

        pure (entitiesWithSource, Just finalBookingOffset, Just finalJourneyOffset, Just finalPassOffset, hasMoreData)

    pure $
      BookingListResV2
        { list = apiEntity,
          bookingOffset = nextBookingOffset,
          journeyOffset = nextJourneyOffset,
          passOffset = nextPassOffset,
          hasMoreData = hasMoreData
        }
  where
    mbInitialBookingOffset = mbBookingOffset <|> mbOffset
    mbInitialJourneyOffset = mbJourneyOffset <|> mbOffset
    mbInitialPassOffset = mbPassOffset <|> mbOffset
    mbInitialPassOffsetInt = fromIntegral <$> mbInitialPassOffset

    limit = maybe 10 fromIntegral mbLimit
    integralLimit = Just (fromIntegral limit)
    limitIntMaybe = Just limit :: Maybe Int

    -- Journeys (NammaTransit) should only be included when:
    -- 1. billingCategoryList contains PERSONAL or is empty
    -- 2. rideTypeList contains NORMAL or is empty
    shouldIncludeJourneysForFilters :: [SLT.BillingCategory] -> [SLT.RideType] -> Bool
    shouldIncludeJourneysForFilters billingCategories rideTypes =
      let billingCategoryOk = null billingCategories || SLT.PERSONAL `elem` billingCategories
          rideTypeOk = null rideTypes || SLT.NORMAL `elem` rideTypes
       in billingCategoryOk && rideTypeOk

    clearStuckRides mbAllbookings rbList = do
      case mbAllbookings of
        Just allbookings -> do
          fork "booking list status update" $ checkBookingsForStatus allbookings
          logInfo $ "rbList: test " <> show rbList
        Nothing -> do
          fork "booking list status update" $
            applyMasterDbIfGcp $ do
              (rbList_, allbookings_) <- getBookingList (Just personId, merchantId) Nothing False integralLimit mbInitialBookingOffset Nothing Nothing Nothing mbFromDate' mbToDate' mbBookingStatusList Nothing
              checkBookingsForStatus allbookings_
              logInfo $ "rbList: test " <> show rbList_

data MergedItem = MBooking SRB.Booking | MJourney DJ.Journey | MPass DPurchasedPass.PurchasedPass

buildApiEntityForRideOrJourneyOrPassWithCounts :: Id Person.Person -> Int -> [SRB.Booking] -> [DJ.Journey] -> [DPurchasedPass.PurchasedPass] -> Maybe Integer -> Maybe Integer -> Maybe Integer -> Flow ([BookingAPIEntityV2], Int, Int, Int)
buildApiEntityForRideOrJourneyOrPassWithCounts personId finalLimit bookings journeys passes initialBookingOffset initialJourneyOffset initialPassOffset = do
  istTime <- getLocalCurrentTime (19800 :: Seconds)
  let today = DT.utctDay istTime

  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)

  let (mergedList, bookingOffset, journeyOffset, passOffset) = mergeWithCounts bookings journeys passes finalLimit 0 0 0 []
      finalBookingOffset = bookingOffset + maybe 0 fromIntegral initialBookingOffset
      finalJourneyOffset = journeyOffset + maybe 0 fromIntegral initialJourneyOffset
      finalPassOffset = passOffset + maybe 0 fromIntegral initialPassOffset

  entities <- JMU.measureLatency (buildBookingListV3 personId person today (reverse mergedList)) "buildBookingListV3 measureLatency: "
  return (entities, finalBookingOffset, finalJourneyOffset, finalPassOffset)
  where
    mergeWithCounts ::
      [SRB.Booking] ->
      [DJ.Journey] ->
      [DPurchasedPass.PurchasedPass] ->
      Int -> -- maxItems
      Int -> -- bookingCount
      Int -> -- journeyCount
      Int -> -- passCount
      [MergedItem] -> -- resultRev
      ([MergedItem], Int, Int, Int)
    mergeWithCounts
      bookings'
      journeys'
      passes'
      maxItems
      bookingCount
      journeyCount
      passCount
      resultRev
        | bookingCount + journeyCount + passCount >= maxItems =
          (resultRev, bookingCount, journeyCount, passCount)
        | otherwise =
          case pickLatestItem bookings' journeys' passes' of
            Nothing ->
              (resultRev, bookingCount, journeyCount, passCount)
            Just (MBooking booking', remainingBookings, remainingJourneys, remainingPasses) ->
              mergeWithCounts
                remainingBookings
                remainingJourneys
                remainingPasses
                maxItems
                (bookingCount + 1)
                journeyCount
                passCount
                (MBooking booking' : resultRev)
            Just (MJourney journey', remainingBookings, remainingJourneys, remainingPasses) ->
              mergeWithCounts
                remainingBookings
                remainingJourneys
                remainingPasses
                maxItems
                bookingCount
                (journeyCount + 1)
                passCount
                (MJourney journey' : resultRev)
            Just (MPass pass', remainingBookings, remainingJourneys, remainingPasses) ->
              mergeWithCounts
                remainingBookings
                remainingJourneys
                remainingPasses
                maxItems
                bookingCount
                journeyCount
                (passCount + 1)
                (MPass pass' : resultRev)

    pickLatestItem ::
      [SRB.Booking] ->
      [DJ.Journey] ->
      [DPurchasedPass.PurchasedPass] ->
      Maybe
        ( MergedItem,
          [SRB.Booking],
          [DJ.Journey],
          [DPurchasedPass.PurchasedPass]
        )
    pickLatestItem [] [] [] = Nothing
    pickLatestItem (booking' : remainingBookings) [] [] =
      Just (MBooking booking', remainingBookings, [], [])
    pickLatestItem [] (journey' : remainingJourneys) [] =
      Just (MJourney journey', [], remainingJourneys, [])
    pickLatestItem [] [] (pass' : remainingPasses) =
      Just (MPass pass', [], [], remainingPasses)
    pickLatestItem (booking' : remainingBookings) (journey' : remainingJourneys) [] =
      if booking'.startTime >= journey'.createdAt
        then Just (MBooking booking', remainingBookings, journey' : remainingJourneys, [])
        else Just (MJourney journey', booking' : remainingBookings, remainingJourneys, [])
    pickLatestItem (booking' : remainingBookings) [] (pass' : remainingPasses) =
      if booking'.startTime >= pass'.createdAt
        then Just (MBooking booking', remainingBookings, [], pass' : remainingPasses)
        else Just (MPass pass', booking' : remainingBookings, [], remainingPasses)
    pickLatestItem [] (journey' : remainingJourneys) (pass' : remainingPasses) =
      if journey'.createdAt >= pass'.createdAt
        then Just (MJourney journey', [], remainingJourneys, pass' : remainingPasses)
        else Just (MPass pass', [], journey' : remainingJourneys, remainingPasses)
    pickLatestItem
      (booking' : remainingBookings)
      (journey' : remainingJourneys)
      (pass' : remainingPasses) =
        let bookingTime = booking'.startTime
            journeyTime = journey'.createdAt
            passTime = pass'.createdAt
         in if bookingTime >= journeyTime && bookingTime >= passTime
              then Just (MBooking booking', remainingBookings, journey' : remainingJourneys, pass' : remainingPasses)
              else
                if journeyTime >= bookingTime && journeyTime >= passTime
                  then Just (MJourney journey', booking' : remainingBookings, remainingJourneys, pass' : remainingPasses)
                  else Just (MPass pass', booking' : remainingBookings, journey' : remainingJourneys, remainingPasses)

    buildBookingListV3 :: Id Person.Person -> Person.Person -> DT.Day -> [MergedItem] -> Flow [BookingAPIEntityV2]
    buildBookingListV3 riderId person today items = go riderId items Seq.empty
      where
        go _ [] acc = pure (toList acc)
        go riderId' (MBooking booking : ls) acc = do
          bookingEntity <- SRB.buildBookingAPIEntity booking riderId'
          go riderId' ls (acc Seq.|> Ride bookingEntity)
        go riderId' (MJourney journey : ls) acc = do
          mbJourneyEntity <- JMU.measureLatency (buildJourneyApiEntity journey) (show journey.id <> " buildJourneyApiEntity measureLatency: ")
          case mbJourneyEntity of
            Just journeyEntity -> go riderId' ls (acc Seq.|> MultiModalRide journeyEntity)
            Nothing -> go riderId' ls acc
        go riderId' (MPass pass' : ls) acc = do
          passEntity <- DPass.buildPurchasedPassAPIEntity Nothing person Nothing today pass'
          go riderId' ls (acc Seq.|> MultiModalPass passEntity)

    buildJourneyApiEntity :: (GetStateFlow m r c, m ~ Kernel.Types.Flow.FlowR AppEnv) => DJ.Journey -> m (Maybe APITypes.JourneyInfoResp)
    buildJourneyApiEntity journey = do
      legsInfo <-
        withTryCatch "getAllLegsInfo:buildJourneyApiEntity" (JMU.measureLatency (getAllLegsInfo journey.riderId journey.id) (show journey.id <> " getAllLegsInfo journey myrides measureLatency: "))
          >>= \case
            Left err -> do
              logError $ "Error getting legs info for journeyId: " <> show journey.id <> ", skipping from booking list : " <> show err
              return []
            Right legsInfo -> do
              return legsInfo
      if null legsInfo
        then do
          logError $ "No legs info for journeyId: " <> show journey.id <> ", skipping from booking list"
          return Nothing
        else Just <$> generateJourneyInfoResponse journey legsInfo

favouriteBookingList :: (Id Person.Person, Id Merchant.Merchant) -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe SRB.BookingStatus -> Maybe (Id DC.Client) -> DriverNo -> Flow FavouriteBookingListRes
favouriteBookingList (personId, _) mbLimit mbOffset mbOnlyActive mbBookingStatus mbClientId driver = do
  mobileNumberHash <- getDbHash driver.driverNumber
  rides <- runInReplica $ QR.findAllByRiderIdAndDriverNumber personId mbLimit mbOffset mbOnlyActive mbBookingStatus mbClientId mobileNumberHash
  pure $ FavouriteBookingListRes $ SRB.favouritebuildBookingAPIEntity <$> rides

addStop :: (Id Person.Person, Id Merchant) -> Id SRB.Booking -> StopReq -> Flow APISuccess
addStop (_, merchantId) bookingId req = do
  processStop bookingId req merchantId False
  pure Success

editStop :: (Id Person.Person, Id Merchant) -> Id SRB.Booking -> StopReq -> Flow APISuccess
editStop (_, merchantId) bookingId req = do
  processStop bookingId req merchantId True
  pure Success

processStop :: Id SRB.Booking -> StopReq -> Id Merchant -> Bool -> Flow ()
processStop bookingId loc merchantId isEdit = do
  booking <- runInReplica $ QRB.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
  uuid <- generateGUID
  merchant <- CQMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  validateStopReq booking isEdit loc merchant
  location <- buildLocation merchantId booking.merchantOperatingCityId loc
  prevOrder <- QLM.maxOrderByEntity booking.id.getId
  locationMapping <- buildLocationMapping location.id booking.id.getId isEdit (Just booking.merchantId) (Just booking.merchantOperatingCityId) prevOrder
  QL.create location
  QLM.create locationMapping
  QRB.updateStop booking (Just location) (Just True)
  bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
  let details =
        if isEdit
          then do
            let stopLocation = location{id = Id $ show prevOrder}
            ACL.UEditStopBuildReqDetails $
              ACL.EditStopBuildReqDetails
                { stops = [stopLocation]
                }
          else do
            let stopLocation = location{id = Id $ show (prevOrder + 1)}
            ACL.UAddStopBuildReqDetails $
              ACL.AddStopBuildReqDetails
                { stops = [stopLocation]
                }
  let dUpdateReq =
        ACL.UpdateBuildReq
          { bppId = booking.providerId,
            bppUrl = booking.providerUrl,
            transactionId = booking.transactionId,
            messageId = uuid,
            city = merchant.defaultCity,
            ..
          }
  becknUpdateReq <- ACL.buildUpdateReq dUpdateReq
  void . withShortRetry $ CallBPP.updateV2 booking.providerUrl becknUpdateReq

validateStopReq :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => SRB.Booking -> Bool -> StopReq -> Merchant.Merchant -> m ()
validateStopReq booking isEdit loc merchant = do
  unless (booking.status `elem` SRB.activeBookingStatus) $ throwError (RideInvalidStatus $ "Cannot edit/add stop in this booking " <> booking.id.getId)
  case booking.bookingDetails of
    SRB.OneWayDetails _ -> throwError $ RideInvalidStatus "Cannot add/edit stop in static offer on demand rides"
    SRB.RentalDetails SRB.RentalBookingDetails {..} ->
      if isEdit
        then unless (isJust stopLocation) $ throwError (InvalidRequest $ "Can't find stop to be edited " <> booking.id.getId)
        else unless (isNothing stopLocation) $ throwError (InvalidRequest $ "Can't add next stop before reaching previous stop " <> booking.id.getId)
    SRB.DriverOfferDetails _ -> throwError $ RideInvalidStatus "Cannot add/edit stop in dynamic offer on demand rides"
    SRB.OneWaySpecialZoneDetails _ -> throwError $ RideInvalidStatus "Cannot add/edit stop in special zone rides"
    SRB.InterCityDetails _ -> throwError $ RideInvalidStatus "Cannot add/edit stop in intercity rides"
    SRB.AmbulanceDetails _ -> throwError $ RideInvalidStatus "Cannot add/edit stop in ambulance rides"
    SRB.DeliveryDetails _ -> throwError $ RideInvalidStatus "Cannot add/edit stop in delivery rides"
    SRB.MeterRideDetails _ -> throwError $ RideInvalidStatus "Cannot add/edit stop in meter rides"

  nearestCity <- getNearestOperatingCityHelper merchant (merchant.geofencingConfig.origin) loc.gps (CityState {city = merchant.defaultCity, state = merchant.defaultState})
  fromLocCity <- getNearestOperatingCityHelper merchant (merchant.geofencingConfig.origin) (LatLong booking.fromLocation.lat booking.fromLocation.lon) (CityState {city = merchant.defaultCity, state = merchant.defaultState})
  case (nearestCity, fromLocCity) of
    (Just nearest, Just source) -> unless (nearest.currentCity.city == source.currentCity.city) $ throwError (InvalidRequest "Outside city stops are allowed in Intercity rides only.")
    _ -> throwError (InvalidRequest "Ride Unserviceable")

buildLocation ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  StopReq ->
  m Location
buildLocation merchantId merchantOperatingCityId req = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    Location
      { lat = req.gps.lat,
        lon = req.gps.lon,
        address = req.address,
        createdAt = now,
        updatedAt = now,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOperatingCityId,
        ..
      }

buildLocationMapping :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Location -> Text -> Bool -> Maybe (Id DM.Merchant) -> Maybe (Id DMOC.MerchantOperatingCity) -> Int -> m DLM.LocationMapping
buildLocationMapping locationId entityId isEdit merchantId merchantOperatingCityId prevOrder = do
  id <- generateGUID
  now <- getCurrentTime
  when isEdit $ QLM.updatePastMappingVersions entityId prevOrder
  let version = QLM.latestTag
      tag = DLM.BOOKING
  return $
    DLM.LocationMapping
      { order = if isEdit then prevOrder else prevOrder + 1,
        createdAt = now,
        updatedAt = now,
        ..
      }

emergencyContactSOSCache :: SRB.Booking -> Id Person.Person -> Flow ()
emergencyContactSOSCache booking personId = do
  now <- getCurrentTime
  when (booking.riderId /= personId) $ do
    mbRide <- runInReplica $ QR.findActiveByRBId booking.id
    case mbRide of
      Just ride -> do
        logDebug "Creating cache for emergency contact SOS"
        let hashKey = makeEmergencyContactSOSCacheKey (ride.id)
        Hedis.hSetExp hashKey (personId.getId) now 86400 -- expiration is set to 24 hours
      Nothing -> logDebug "No active ride found, skipping SOS cache creation."

makeEmergencyContactSOSCacheKey :: Id DTR.Ride -> Text
makeEmergencyContactSOSCacheKey rideId = "emergencyContactSOS:" <> rideId.getId
