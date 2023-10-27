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
  )
where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Ride as Common
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.BookingCancellationReason as DBCReason
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Ride as DRide
import Environment
import EulerHS.Prelude (whenNothing_)
-- import Kernel.Storage.Esqueleto.Transactionable (runInReplica)

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError
import qualified SharedLogic.CallBAP as CallBAP
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.Queries.BookingCancellationReason as QBCReason
import qualified Storage.Queries.FareParameters as QFareParams
import Tools.Error

rideSync :: Maybe DBCR.CancellationSource -> Maybe DRide.Ride -> DBooking.Booking -> DM.Merchant -> Flow Common.RideSyncRes
rideSync mbCancellationSource (Just ride) booking merchant =
  case ride.status of
    DRide.NEW -> syncNewRide ride booking
    DRide.INPROGRESS -> syncInProgressRide ride booking
    DRide.COMPLETED -> syncCompletedRide ride booking
    DRide.CANCELLED -> syncCancelledRide mbCancellationSource (Just ride) booking merchant
rideSync mbCancellationSource Nothing booking merchant =
  syncCancelledRide mbCancellationSource Nothing booking merchant

syncNewRide :: DRide.Ride -> DBooking.Booking -> Flow Common.RideSyncRes
syncNewRide ride booking = do
  handle (errHandler (Just ride.status) booking.status "ride assigned") $
    CallBAP.sendRideAssignedUpdateToBAP booking ride
  pure $ Common.RideSyncRes Common.RIDE_NEW "Success. Sent ride started update to bap"

syncInProgressRide :: DRide.Ride -> DBooking.Booking -> Flow Common.RideSyncRes
syncInProgressRide ride booking = do
  handle (errHandler (Just ride.status) booking.status "ride started") $
    CallBAP.sendRideStartedUpdateToBAP booking ride
  pure $ Common.RideSyncRes Common.RIDE_INPROGRESS "Success. Sent ride started update to bap"

syncCancelledRide :: Maybe DBCR.CancellationSource -> Maybe DRide.Ride -> DBooking.Booking -> DM.Merchant -> Flow Common.RideSyncRes
syncCancelledRide mbCancellationSource mbRide booking merchant = do
  cancellationSource <- maybe (findCancellationSource mbRide) pure mbCancellationSource
  handle (errHandler (mbRide <&> (.status)) booking.status "booking cancellation") $
    CallBAP.sendBookingCancelledUpdateToBAP booking merchant cancellationSource
  pure $ Common.RideSyncRes Common.RIDE_CANCELLED "Success. Sent booking cancellation update to bap"

findCancellationSource :: Maybe DRide.Ride -> Flow DBCR.CancellationSource
findCancellationSource (Just ride) = do
  mbBookingCReason <- runInReplica $ QBCReason.findByRideId ride.id
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

syncCompletedRide :: DRide.Ride -> DBooking.Booking -> Flow Common.RideSyncRes
syncCompletedRide ride booking = do
  whenNothing_ ride.fareParametersId $ do
    -- only for old rides
    logWarning "No fare params linked to ride. Using fare params linked to booking, they may be not actual"
  let fareParametersId = fromMaybe booking.fareParams.id ride.fareParametersId
  fareParameters <- runInReplica $ QFareParams.findById fareParametersId >>= fromMaybeM (FareParametersNotFound fareParametersId.getId)
  -- fareParameters <- QFareParams.findById fareParametersId >>= fromMaybeM (FareParametersNotFound fareParametersId.getId)
  mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId -> do
    CQMPM.findByIdAndMerchantOpCityId paymentMethodId ride.merchantOperatingCityId
      >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
  let mbPaymentUrl = DMPM.getPostpaidPaymentUrl =<< mbPaymentMethod
  let mbPaymentMethodInfo = DMPM.mkPaymentMethodInfo <$> mbPaymentMethod

  handle (errHandler (Just ride.status) booking.status "ride completed") $
    CallBAP.sendRideCompletedUpdateToBAP booking ride fareParameters mbPaymentMethodInfo mbPaymentUrl
  pure $ Common.RideSyncRes Common.RIDE_COMPLETED "Success. Sent ride completed update to bap"

errHandler :: Maybe DRide.RideStatus -> DBooking.BookingStatus -> Text -> SomeException -> Flow ()
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
