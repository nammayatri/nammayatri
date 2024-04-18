{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Cancel
  ( BookingDetails (..),
    cancelHandler,
    bookingCancellationValidations,
    cancellationUpdates,
  )
where

import qualified Beckn.ACL.Cancel as ACL
import qualified BecknV2.OnDemand.Enums as Enums
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as Ride
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.VehicleServiceTier as DVST
import Environment ()
import qualified Kernel.Beam.Functions as B
import Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.CallBPP as CallBPP
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Ride as QR
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import Tools.Event
import qualified Tools.Maps as Maps
import TransactionLogs.Types

data BookingDetails = BookingDetails
  { booking :: SRB.Booking,
    mbRide :: Maybe Ride.Ride
  }

cancelHandler ::
  ( EncFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    EventStreamFlow m r
  ) =>
  Id SRB.Booking ->
  (Id Person.Person, Id Merchant.Merchant) ->
  DCancel.CancelReq ->
  m ()
cancelHandler bookingId (personId, merchantId) req = do
  (dCancelRes, bookingDetails) <- mkDomainCancel bookingId (personId, merchantId) req
  if isJust dCancelRes.bppBookingId
    then void . withShortRetry $ CallBPP.cancelV2 merchantId dCancelRes.bppUrl =<< ACL.buildCancelReqV2 dCancelRes
    else void $ cancellationUpdates bookingDetails.booking bookingDetails.mbRide
  return ()

mkDomainCancel ::
  ( EncFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Id SRB.Booking ->
  (Id Person.Person, Id Merchant.Merchant) ->
  DCancel.CancelReq ->
  m (DCancel.CancelRes, BookingDetails)
mkDomainCancel bookingId _ req = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  when (booking.status == SRB.CANCELLED) $ throwError (BookingInvalidStatus "This booking is already cancelled")
  mbRide <- B.runInReplica $ QR.findActiveByRBId booking.id
  void $ bookingCancellationValidations booking mbRide
  city <- CQMOC.findById booking.merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
  let bppBookingId = booking.bppBookingId
  cancellationReason <-
    case mbRide of
      Just ride -> do
        res <- try @_ @SomeException (CallBPP.callGetDriverLocation ride.trackingUrl)
        case res of
          Right res' -> do
            let merchantOperatingCityId = booking.merchantOperatingCityId
            disToPickup <- driverDistanceToPickup booking.merchantId merchantOperatingCityId (getCoordinates res'.currPoint) (getCoordinates booking.fromLocation)
            -- Temporary for debug issue with huge values
            let disToPickupThreshold = Distance 1000000 Meter --1000km can be max valid distance
            disToPickupUpd :: Maybe Distance <-
              if abs disToPickup > disToPickupThreshold
                then do
                  logWarning $ "Invalid disToPickup received: " <> show disToPickup
                  pure Nothing
                else do
                  logInfo $ "Valid disToPickup received: " <> show disToPickup
                  pure $ Just disToPickup
            buildBookingCancelationReason (Just res'.currPoint) disToPickupUpd (Just booking.merchantId)
          Left err -> do
            logTagInfo "DriverLocationFetchFailed" $ show err
            buildBookingCancelationReason Nothing Nothing (Just booking.merchantId)
      Nothing -> buildBookingCancelationReason Nothing Nothing (Just booking.merchantId)
  QBCR.upsert cancellationReason
  let bookingDetails = BookingDetails {..}
      cancelRes =
        DCancel.CancelRes
          { bppBookingId = bppBookingId,
            bppId = booking.providerId,
            bppUrl = booking.providerUrl,
            cancellationSource = SBCR.ByUser,
            transactionId = booking.transactionId,
            merchant = merchant,
            cancelStatus = show Enums.CONFIRM_CANCEL,
            vehicleVariant = DVST.castServiceTierToVariant booking.vehicleServiceTierType, -- TODO: fix it
            ..
          }
  return (cancelRes, bookingDetails)
  where
    buildBookingCancelationReason currentDriverLocation disToPickup merchantId = do
      let DCancel.CancelReq {..} = req
      now <- getCurrentTime
      return $
        SBCR.BookingCancellationReason
          { bookingId = bookingId,
            rideId = Nothing,
            merchantId = merchantId,
            source = SBCR.ByUser,
            reasonCode = Just reasonCode,
            reasonStage = Just reasonStage,
            additionalInfo = additionalInfo,
            driverCancellationLocation = currentDriverLocation,
            driverDistToPickup = disToPickup,
            createdAt = now,
            updatedAt = now,
            ..
          }

bookingCancellationValidations :: (MonadFlow m) => SRB.Booking -> Maybe SRide.Ride -> m ()
bookingCancellationValidations booking mbRide = do
  let isRideCancellable = maybe False (\ride -> ride.status `notElem` [SRide.INPROGRESS, SRide.CANCELLED]) mbRide
      bookingAlreadyCancelled = booking.status == SRB.CANCELLED
      rideStatus = mbRide <&> (.status)
  unless (isBookingCancellable booking.status || (isRideCancellable && bookingAlreadyCancelled)) $
    throwError (InvalidRequest $ "bookingStatus:-" <> show booking.status <> ", rideStatus:-" <> show rideStatus)
  where
    isBookingCancellable bookingStatus =
      bookingStatus `elem` [SRB.NEW, SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT, SRB.TRIP_ASSIGNED]

driverDistanceToPickup ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Maps.HasCoordinates tripStartPos,
    Maps.HasCoordinates tripEndPos
  ) =>
  Id Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  tripStartPos ->
  tripEndPos ->
  m Distance
driverDistanceToPickup merchantId merchantOperatingCityId tripStartPos tripEndPos = do
  distRes <-
    Maps.getDistanceForCancelRide merchantId merchantOperatingCityId $
      Maps.GetDistanceReq
        { origin = tripStartPos,
          destination = tripEndPos,
          travelMode = Just Maps.CAR
        }
  return $ metersToDistance distRes.distance

cancellationUpdates ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EventStreamFlow m r
  ) =>
  SRB.Booking ->
  Maybe SRide.Ride ->
  m ()
cancellationUpdates booking mbRide = do
  triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}}
  _ <- QPFS.updateStatus booking.riderId DPFS.IDLE
  unless (booking.status == SRB.CANCELLED) $ void $ QRB.updateStatus booking.id SRB.CANCELLED
  whenJust mbRide $ \ride -> void $ do
    triggerRideCancelledEvent RideEventData {ride = ride{status = SRide.CANCELLED}, personId = booking.riderId, merchantId = booking.merchantId}
    unless (ride.status == SRide.CANCELLED) $ void $ QRide.updateStatus ride.id SRide.CANCELLED
  QPFS.clearCache booking.riderId
