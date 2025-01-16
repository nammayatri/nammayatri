{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.SyncRide
  ( rideSync,
    RideCompletedInfo (..),
    fetchRideCompletedInfo,
    BookingCancelledInfo (..),
    fetchBookingCancelledInfo,
    BookingReallocationInfo,
    fetchBookingReallocationInfo,
    DCommon.BookingDetails (..),
    fetchBookingDetails,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Ride as Common
import qualified BecknV2.OnDemand.Utils.Common as Utils
import Domain.Types.Beckn.Status
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.BookingCancellationReason as DBCReason
import qualified Domain.Types.FareParameters as DFParams
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude (whenNothing_)
import Kernel.Beam.Functions
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import qualified SharedLogic.Beckn.Common as DCommon
import qualified SharedLogic.CallBAP as CallBAP
import qualified SharedLogic.MerchantPaymentMethod as DMPM
import qualified Storage.CachedQueries.BecknConfig as QBC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.BookingCancellationReason as QBCReason
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FareParameters as QFareParams
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error

rideSync :: Maybe DBCR.CancellationSource -> Maybe DRide.Ride -> DB.Booking -> DM.Merchant -> Flow Common.RideSyncRes
rideSync mbCancellationSource (Just ride) booking merchant =
  case ride.status of
    DRide.UPCOMING -> syncUpcomingRide ride booking
    DRide.NEW -> syncNewRide ride booking
    DRide.INPROGRESS -> syncInProgressRide ride booking
    DRide.COMPLETED -> syncCompletedRide ride booking
    DRide.CANCELLED -> syncCancelledRide mbCancellationSource (Just ride) booking merchant
rideSync mbCancellationSource Nothing booking merchant =
  syncCancelledRide mbCancellationSource Nothing booking merchant

--UPCOMING --

syncUpcomingRide :: DRide.Ride -> DB.Booking -> Flow Common.RideSyncRes
syncUpcomingRide ride' booking' = do
  DCommon.BookingDetails {..} <- fetchBookingDetails ride' booking'
  handle (errHandler (Just ride.status) booking.status "scheduled ride assigned") $ do
    CallBAP.sendRideAssignedUpdateToBAP booking ride driver vehicle False
  pure $ Common.RideSyncRes Common.RIDE_UPCOMING "Success. Sent scheduled ride started update to bap"

-- NEW --

syncNewRide :: DRide.Ride -> DB.Booking -> Flow Common.RideSyncRes
syncNewRide ride' booking' = do
  DCommon.BookingDetails {..} <- fetchBookingDetails ride' booking'
  handle (errHandler (Just ride.status) booking.status "ride assigned") $ do
    CallBAP.sendRideAssignedUpdateToBAP booking ride driver vehicle False
  pure $ Common.RideSyncRes Common.RIDE_NEW "Success. Sent ride started update to bap"

fetchBookingDetails :: DRide.Ride -> DB.Booking -> Flow DCommon.BookingDetails
fetchBookingDetails ride booking = do
  merchant <- CQM.findById booking.providerId >>= fromMaybeM (MerchantNotFound booking.providerId.getId)
  bppConfig <- QBC.findByMerchantIdDomainAndVehicle merchant.id "MOBILITY" (Utils.mapServiceTierToCategory booking.vehicleServiceTier) >>= fromMaybeM (InternalError "Beckn Config not found")
  driver <- QP.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  driverStats <- QDriverStats.findById driver.id >>= fromMaybeM DriverInfoNotFound
  isValueAddNP <- CQVAN.isValueAddNP booking.bapId
  mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
    CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId
      >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
  let paymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
  let paymentUrl = Nothing
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (VehicleNotFound ride.driverId.getId)
  riderDetails <- maybe (return Nothing) (runInReplica . QRD.findById) booking.riderId
  riderPhone <- fmap (fmap (.mobileNumber)) (traverse decrypt riderDetails)
  pure DCommon.BookingDetails {..}

-- IN_PROGRESS --

syncInProgressRide :: DRide.Ride -> DB.Booking -> Flow Common.RideSyncRes
syncInProgressRide ride booking = do
  handle (errHandler (Just ride.status) booking.status "ride started") $
    CallBAP.sendRideStartedUpdateToBAP booking ride Nothing
  pure $ Common.RideSyncRes Common.RIDE_INPROGRESS "Success. Sent ride started update to bap"

-- CANCELLED --

syncCancelledRide :: Maybe DBCR.CancellationSource -> Maybe DRide.Ride -> DB.Booking -> DM.Merchant -> Flow Common.RideSyncRes
syncCancelledRide mbCancellationSource mbRide booking merchant = do
  cancellationSource <- maybe (findCancellationSource mbRide) pure mbCancellationSource
  handle (errHandler (mbRide <&> (.status)) booking.status "booking cancellation") $
    CallBAP.sendBookingCancelledUpdateToBAP booking merchant cancellationSource Nothing
  pure $ Common.RideSyncRes Common.RIDE_CANCELLED "Success. Sent booking cancellation update to bap"

fetchBookingCancelledInfo :: Maybe DRide.Ride -> Flow BookingCancelledInfo
fetchBookingCancelledInfo mbRide = do
  cancellationSource <- findCancellationSource mbRide
  pure BookingCancelledInfo {..}

findCancellationSource :: Maybe DRide.Ride -> Flow DBCR.CancellationSource
findCancellationSource (Just ride) = do
  mbBookingCReason <- runInReplica $ QBCReason.findByRideId (Just ride.id)
  -- mbBookingCReason <- QBCReason.findByRideId ride.id
  case mbBookingCReason of
    Just bookingCReason -> pure bookingCReason.source
    Nothing -> do
      mbBookingCReason' <- runInReplica $ QBCReason.findByBookingId ride.bookingId
      -- mbBookingCReason' <- QBCReason.findByBookingId ride.bookingId
      case mbBookingCReason' of
        Just bookingCReason' -> pure bookingCReason'.source
        Nothing -> do
          logWarning $
            "No cancellation reason found for ride "
              <> show ride.id
              <> " and booking "
              <> show ride.bookingId
              <> "; Using ByMerchant as cancellation source"
          pure DBCReason.ByMerchant
findCancellationSource Nothing = pure DBCReason.ByMerchant

-- REALLOCATION --

fetchBookingReallocationInfo :: Maybe DRide.Ride -> DB.Booking -> Flow BookingCancelledInfo
fetchBookingReallocationInfo ride _ = fetchBookingCancelledInfo ride

-- COMPLETED --

data RideCompletedInfo = RideCompletedInfo
  { --ride :: DRide.Ride,
    fareParams :: DFParams.FareParameters,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo,
    paymentUrl :: Maybe Text
  }

syncCompletedRide :: DRide.Ride -> DB.Booking -> Flow Common.RideSyncRes
syncCompletedRide ride booking = do
  RideCompletedInfo {..} <- fetchRideCompletedInfo ride booking
  handle (errHandler (Just ride.status) booking.status "ride completed") $
    CallBAP.sendRideCompletedUpdateToBAP booking ride fareParams paymentMethodInfo paymentUrl Nothing
  pure $ Common.RideSyncRes Common.RIDE_COMPLETED "Success. Sent ride completed update to bap"

fetchRideCompletedInfo :: DRide.Ride -> DB.Booking -> Flow RideCompletedInfo
fetchRideCompletedInfo ride booking = do
  whenNothing_ ride.fareParametersId $ do
    -- only for old rides
    logWarning "No fare params linked to ride. Using fare params linked to booking, they may be not actual"
  let fareParametersId = fromMaybe booking.fareParams.id ride.fareParametersId
  fareParams <- runInReplica $ QFareParams.findById fareParametersId >>= fromMaybeM (FareParametersNotFound fareParametersId.getId)
  mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
    CQMPM.findByIdAndMerchantOpCityId paymentMethodId ride.merchantOperatingCityId
      >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
  let paymentUrl = Nothing -- DMPM.getPostpaidPaymentUrl =<< mbPaymentMethod -- TODO : Remove paymentUrl altogether
  let paymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod
  pure RideCompletedInfo {..}

errHandler :: Maybe DRide.RideStatus -> DB.BookingStatus -> Text -> SomeException -> Flow ()
errHandler mbRideStatus bookingStatus desc exc
  | Just (BecknAPICallError _endpoint err) <- fromException @BecknAPICallError exc = do
    case err.code of
      code | code == toErrorCode (BookingInvalidStatus "") -> do
        throwError $ InvalidRequest $ bookingErrMessage <> maybe "" (" Bap booking status: " <>) err.message
      code | code == toErrorCode (RideInvalidStatus "") -> do
        throwError $ InvalidRequest $ rideErrMessage <> maybe "" (" Bap ride status: " <>) err.message
      _ -> throwError $ InternalError bookingErrMessage
  | otherwise = throwError $ InternalError bookingErrMessage
  where
    bookingErrMessage = "Fail to send " <> desc <> " update. Bpp booking status: " <> show bookingStatus <> "."
    rideErrMessage = "Fail to send " <> desc <> " update. Bpp ride status: " <> maybe "no ride provided" show mbRideStatus <> "."
