{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Domain.Action.Beckn.OnCancel
  ( onCancel,
    validateRequest,
    onSoftCancel,
    OnCancelReq (..),
    ValidatedOnCancelReq (..),
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import qualified Data.Text as T
import qualified Domain.Action.Beckn.Common as Common
import qualified Domain.SharedLogic.Cancel as SharedCancel
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.BookingStatus as SRB
import qualified Domain.Types.PaymentInvoice as DPI
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideStatus as SRide
import Environment
import Environment ()
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Tools.Error
-- import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC

data OnCancelReq = BookingCancelledReq
  { bppBookingId :: Id SRB.BPPBooking,
    cancellationSource :: Maybe Text,
    cancellationFee :: Maybe PriceAPIEntity,
    cancellationReasonCode :: Maybe Text
  }

data ValidatedOnCancelReq = ValidatedBookingCancelledReq
  { bppBookingId :: Id SRB.BPPBooking,
    cancellationSource :: Maybe Text,
    booking :: SRB.Booking,
    cancellationFee :: Maybe PriceAPIEntity,
    mbRide :: Maybe SRide.Ride,
    cancellationReasonCode :: Maybe Text
  }

onCancel :: ValidatedOnCancelReq -> Flow ()
onCancel ValidatedBookingCancelledReq {..} = do
  let cancellationSource_ :: Maybe Enums.CancellationSource = readMaybe . T.unpack =<< cancellationSource
  logTagInfo ("BookingId-" <> getId booking.id) ""
  whenJust cancellationSource $ \source -> logTagInfo ("Cancellation source " <> source) ""
  let castedCancellationSource = castCancellatonSource cancellationSource_
  -- riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow booking.merchantOperatingCityId booking.configInExperimentVersions >>= fromMaybeM (RiderConfigDoesNotExist booking.merchantOperatingCityId.getId)
  let validCancellationReasonCodesForImmediateCharge = ["NO_SHOW_CHARGE"] :: [Text] -- make it config in riderConfig
  let paymentPurpose = if isJust cancellationFee && maybe False (`elem` validCancellationReasonCodesForImmediateCharge) cancellationReasonCode
                       then Just DPI.CANCELLATION_FEE
                       else Nothing
  Common.cancellationTransaction booking mbRide castedCancellationSource cancellationFee paymentPurpose
  SharedCancel.releaseCancellationLock booking.transactionId
  where
    castCancellatonSource = \case
      Just Enums.CONSUMER -> SBCR.ByUser
      _ -> SBCR.ByDriver

onSoftCancel :: ValidatedOnCancelReq -> Flow ()
onSoftCancel ValidatedBookingCancelledReq {..} =
  case cancellationFee of
    Just fee -> do
      let cancellationFeeToBeSettled = Just fee.amount
      whenJust mbRide $ \ride -> do
        let rideId = ride.id
        QRide.updateCancellationFeeIfCancelledField cancellationFeeToBeSettled rideId
    _ -> pure ()

validateRequest ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance)
  ) =>
  OnCancelReq ->
  m ValidatedOnCancelReq
validateRequest BookingCancelledReq {..} = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  mbRide <- QRide.findActiveByRBId booking.id
  let isRideCancellable = maybe False (\ride -> ride.status `notElem` [SRide.INPROGRESS, SRide.CANCELLED]) mbRide
      bookingAlreadyCancelled = booking.status == SRB.CANCELLED
  unless (isBookingCancellable booking || (isRideCancellable && bookingAlreadyCancelled)) $
    throwError (BookingInvalidStatus (show booking.status))
  return $ ValidatedBookingCancelledReq {cancellationReasonCode = cancellationReasonCode, ..}
  where
    isBookingCancellable booking =
      booking.status `elem` [SRB.NEW, SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT, SRB.TRIP_ASSIGNED]
