{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Cancel
  ( cancel,
    softCancel,
    CancelReq (..),
    CancelRes (..),
    CancelSearch (..),
    CancellationDuesDetailsRes (..),
    mkDomainCancelSearch,
    cancelSearch,
    getCancellationDuesDetails,
    makeCustomerBlockingKey,
    isBookingCancellable,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.DriverOffer as DDO
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as Ride
import Domain.Types.SearchRequest (SearchRequest)
import qualified Domain.Types.VehicleVariant as DVeh
import Environment
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.DriverOffer as QDOffer
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QR
import Tools.Error
import qualified Tools.Maps as Maps

data CancelReq = CancelReq
  { reasonCode :: SCR.CancellationReasonCode,
    reasonStage :: SCR.CancellationStage,
    additionalInfo :: Maybe Text,
    reallocate :: Maybe Bool,
    blockOnCancellationRate :: Maybe Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CancelRes = CancelRes
  { bppBookingId :: Id SRB.BPPBooking,
    bppId :: Text,
    bppUrl :: BaseUrl,
    cancellationSource :: SBCR.CancellationSource,
    transactionId :: Text,
    merchant :: DM.Merchant,
    cancelStatus :: Text,
    city :: Context.City,
    vehicleVariant :: DVeh.VehicleVariant,
    cancellationReason :: Maybe Text
  }

data CancelSearch = CancelSearch
  { estimateId :: Id DEstimate.Estimate,
    providerUrl :: BaseUrl,
    providerId :: Text,
    estimateStatus :: DEstimate.EstimateStatus,
    searchReqId :: Id SearchRequest,
    sendToBpp :: Bool,
    merchant :: DM.Merchant,
    city :: Context.City,
    vehicleVariant :: DVeh.VehicleVariant
  }

data CancellationDuesDetailsRes = CancellationDuesDetailsRes
  { cancellationDues :: Maybe PriceAPIEntity,
    disputeChancesUsed :: Maybe Int,
    canBlockCustomer :: Maybe Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

softCancel :: (EncFlow m r, Esq.EsqDBReplicaFlow m r, EsqDBFlow m r, CacheFlow m r, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => Id SRB.Booking -> (Id Person.Person, Id Merchant.Merchant) -> m CancelRes
softCancel bookingId _ = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  bppBookingId <- fromMaybeM (BookingFieldNotPresent "bppBookingId") booking.bppBookingId
  city <-
    CQMOC.findById booking.merchantOperatingCityId
      >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
  return $
    CancelRes
      { bppBookingId = bppBookingId,
        bppId = booking.providerId,
        bppUrl = booking.providerUrl,
        cancellationSource = SBCR.ByUser,
        transactionId = booking.transactionId,
        merchant = merchant,
        cancelStatus = show Enums.SOFT_CANCEL,
        vehicleVariant = DVeh.castServiceTierToVariant booking.vehicleServiceTierType,
        cancellationReason = Nothing,
        ..
      }

cancel ::
  ( EncFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  SRB.Booking ->
  Maybe Ride.Ride ->
  CancelReq ->
  SBCR.CancellationSource ->
  m CancelRes
cancel booking mRide req cancellationSource = do
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  when (booking.status == SRB.CANCELLED) $ throwError (BookingInvalidStatus "This booking is already cancelled")
  canCancelBooking <- isBookingCancellable booking mRide
  unless canCancelBooking $
    throwError $ RideInvalidStatus "Cannot cancel this ride"
  city <-
    CQMOC.findById booking.merchantOperatingCityId
      >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
  bppBookingId <- fromMaybeM (BookingFieldNotPresent "bppBookingId") booking.bppBookingId
  cancellationReason <-
    case mRide of
      Just ride -> do
        res <- try @_ @SomeException (CallBPP.callGetDriverLocation ride.trackingUrl)
        case res of
          Right res' -> do
            let merchantOperatingCityId = booking.merchantOperatingCityId
            disToPickup <- driverDistanceToPickup booking merchantOperatingCityId (getCoordinates res'.currPoint) (getCoordinates booking.fromLocation)
            -- Temporary for debug issue with huge values
            let disToPickupThreshold = Meters 1000000 -- 1000km can be max valid distance
            disToPickupUpd :: Maybe Meters <-
              if abs disToPickup > disToPickupThreshold
                then do
                  logWarning $ "Invalid disToPickup received: " <> show disToPickup
                  pure Nothing
                else do
                  logInfo $ "Valid disToPickup received: " <> show disToPickup
                  pure $ Just disToPickup
            buildBookingCancellationReason (Just res'.currPoint) disToPickupUpd (Just ride.id)
          Left err -> do
            logTagInfo "DriverLocationFetchFailed" $ show err
            buildBookingCancellationReason Nothing Nothing (Just ride.id)
      Nothing -> buildBookingCancellationReason Nothing Nothing Nothing
  QBCR.upsert cancellationReason
  when (req.blockOnCancellationRate == Just True) $ do
    Redis.setExp (makeCustomerBlockingKey booking.id.getId) True 60
  isValueAddNP <- CQVAN.isValueAddNP booking.providerId
  return $
    CancelRes
      { bppBookingId = bppBookingId,
        bppId = booking.providerId,
        bppUrl = booking.providerUrl,
        transactionId = booking.transactionId,
        merchant = merchant,
        cancelStatus = show Enums.CONFIRM_CANCEL,
        vehicleVariant = DVeh.castServiceTierToVariant booking.vehicleServiceTierType,
        cancellationReason = if isValueAddNP then Just $ SCR.getCancellationReasonCode req.reasonCode else Nothing,
        ..
      }
  where
    buildBookingCancellationReason currentDriverLocation disToPickup mbRideId = do
      let CancelReq {..} = req
      now <- getCurrentTime
      return $
        SBCR.BookingCancellationReason
          { bookingId = booking.id,
            rideId = mbRideId,
            merchantId = Just booking.merchantId,
            source = cancellationSource,
            reasonCode = Just reasonCode,
            reasonStage = Just reasonStage,
            additionalInfo = additionalInfo,
            driverCancellationLocation = currentDriverLocation,
            driverDistToPickup = convertMetersToDistance booking.distanceUnit <$> disToPickup,
            riderId = Just booking.riderId,
            distanceUnit = booking.distanceUnit,
            createdAt = now,
            updatedAt = now,
            ..
          }

isBookingCancellable :: (CacheFlow m r, EsqDBFlow m r) => SRB.Booking -> Maybe Ride.Ride -> m Bool
isBookingCancellable booking mbRide
  | booking.status `elem` [SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT, SRB.NEW] = pure True
  | booking.status == SRB.TRIP_ASSIGNED = do
    case mbRide of
      Just ride -> pure (ride.status `elem` [Ride.NEW, Ride.UPCOMING])
      Nothing -> pure True
  | otherwise = pure False

mkDomainCancelSearch ::
  (HasFlowEnv m r '["nwAddress" ::: BaseUrl], EsqDBFlow m r, Esq.EsqDBReplicaFlow m r, CacheFlow m r) =>
  Id Person.Person ->
  Id DEstimate.Estimate ->
  m CancelSearch
mkDomainCancelSearch personId estimateId = do
  estStatus <- QEstimate.getStatus estimateId >>= fromMaybeM (EstimateStatusDoesNotExist estimateId.getId)
  let isEstimateNotNew = estStatus /= DEstimate.NEW
  buildCancelReq estimateId isEstimateNotNew estStatus
  where
    buildCancelReq estId isEstimateNotNew estStatus = do
      estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
      person <- B.runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      merchant <- CQM.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
      isValueAddNP <- CQVAN.isValueAddNP estimate.providerId
      let searchRequestId = estimate.requestId
      city <- case estimate.merchantOperatingCityId of
        Nothing -> pure merchant.defaultCity
        Just mOCId ->
          CQMOC.findById mOCId
            >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound mOCId.getId)
      pure
        CancelSearch
          { estimateId = estId,
            providerUrl = estimate.providerUrl,
            providerId = estimate.providerId,
            searchReqId = searchRequestId,
            estimateStatus = estStatus,
            sendToBpp = isEstimateNotNew && isValueAddNP,
            merchant = merchant,
            vehicleVariant = DVeh.castServiceTierToVariant estimate.vehicleServiceTierType,
            ..
          }

cancelSearch ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id Person.Person ->
  CancelSearch ->
  m ()
cancelSearch personId dcr = do
  _ <-
    if dcr.estimateStatus == DEstimate.GOT_DRIVER_QUOTE
      then -- then Esq.runTransaction $ do
      do
        _ <- QPFS.updateStatus personId DPFS.IDLE
        void $ QEstimate.updateStatus DEstimate.DRIVER_QUOTE_CANCELLED dcr.estimateId
        QDOffer.updateStatus DDO.INACTIVE dcr.estimateId
      else do
        _ <- QPFS.updateStatus personId DPFS.IDLE
        void $ QEstimate.updateStatus DEstimate.CANCELLED dcr.estimateId
        QDOffer.updateStatus DDO.INACTIVE dcr.estimateId
  return ()

driverDistanceToPickup ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Maps.HasCoordinates tripStartPos,
    Maps.HasCoordinates tripEndPos
  ) =>
  SRB.Booking ->
  Id DMOC.MerchantOperatingCity ->
  tripStartPos ->
  tripEndPos ->
  m Meters
driverDistanceToPickup booking merchantOperatingCityId tripStartPos tripEndPos = do
  distRes <-
    Maps.getDistanceForCancelRide booking.merchantId merchantOperatingCityId $
      Maps.GetDistanceReq
        { origin = tripStartPos,
          destination = tripEndPos,
          travelMode = Just Maps.CAR,
          sourceDestinationMapping = Nothing,
          distanceUnit = booking.distanceUnit
        }
  return distRes.distance

-- disputeCancellationDues :: (Id Person.Person, Id Merchant.Merchant) -> Flow APISuccess
-- disputeCancellationDues (personId, merchantId) = do
--   person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId) >>= decrypt
--   merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
--   case (person.mobileNumber, person.mobileCountryCode) of
--     (Just mobileNumber, Just countryCode) -> do
--       CallBPPInternal.disputeCancellationDues merchant.driverOfferApiKey merchant.driverOfferBaseUrl merchant.driverOfferMerchantId mobileNumber countryCode person.currentCity
--     _ -> throwError (PersonMobileNumberIsNULL person.id.getId)

getCancellationDuesDetails :: Maybe (Id SRB.Booking) -> (Id Person.Person, Id Merchant.Merchant) -> Flow CancellationDuesDetailsRes
getCancellationDuesDetails mbBookingId (personId, merchantId) = do
  mbBooking <- maybe (return Nothing) QRB.findById mbBookingId
  mbRide <- maybe (return Nothing) QR.findActiveByRBId mbBookingId
  let cancellationFees = (.cancellationFeeIfCancelled) =<< mbRide
      currency = (.estimatedFare.currency) <$> mbBooking
  case (cancellationFees, currency) of
    (Just customerCancellationDues, Just bookingCurrency) -> do
      return $ CancellationDuesDetailsRes {cancellationDues = Just PriceAPIEntity {amount = customerCancellationDues, currency = bookingCurrency}, disputeChancesUsed = Nothing, canBlockCustomer = Nothing}
    _ -> do
      person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId) >>= decrypt
      merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
      case (person.mobileNumber, person.mobileCountryCode) of
        (Just mobileNumber, Just countryCode) -> do
          res <- CallBPPInternal.getCancellationDuesDetails merchant.driverOfferApiKey merchant.driverOfferBaseUrl merchant.driverOfferMerchantId mobileNumber countryCode person.currentCity
          return $ CancellationDuesDetailsRes {cancellationDues = res.customerCancellationDuesWithCurrency, disputeChancesUsed = Just res.disputeChancesUsed, canBlockCustomer = res.canBlockCustomer}
        _ -> throwError (PersonMobileNumberIsNULL person.id.getId)

makeCustomerBlockingKey :: Text -> Text
makeCustomerBlockingKey bid = "CCRBlock:" <> bid
