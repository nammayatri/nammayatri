module Domain.Action.Internal.CancellationDues where

-- import qualified Data.Aeson as A
-- import Data.Text (unpack)

import qualified Domain.Types.Booking as DBooking
-- import qualified Domain.Types.RiderDetails as DRiderDetails
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Ride as DRide
import Environment (Flow)
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
-- import qualified Domain.Types.TransporterConfig as DTransporterConfig
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Merchant as QM
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import Tools.Error

data CustomerCancellationDuesWaiveOffReq = CustomerCancellationDuesWaiveOffReq
  { rideId :: Text,
    bookingId :: Text,
    waivedOffAmount :: HighPrecMoney
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

customerCancellationDuesWaiveOff :: Id DMerchant.Merchant -> Maybe Text -> CustomerCancellationDuesWaiveOffReq -> Flow APISuccess
customerCancellationDuesWaiveOff merchantId apiKey req = do
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  let rideId = (Id req.rideId) :: Id DRide.Ride
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound req.rideId)
  let bookingId = (Id req.bookingId) :: Id DBooking.Booking
  booking <- QBooking.findById bookingId >>= fromMaybeM (BookingNotFound req.bookingId)
  riderId <- booking.riderId & fromMaybeM (BookingFieldNotPresent "rider_id")
  riderDetails <- QRD.findById riderId >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
  _transporterConfig <- SCTC.findByMerchantOpCityId ride.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
  logInfo $ "Cancellation Due Amount is not equal to the waived off amount for riderId " <> riderDetails.id.getId <> " rideId " <> req.rideId <> " bookingId " <> req.bookingId <> " waivedOffAmount " <> show req.waivedOffAmount <> " cancellationDues " <> show booking.fareParams.customerCancellationDues
  unless (booking.fareParams.customerCancellationDues == Just req.waivedOffAmount) $ do
    logWarning $ "Cancellation Due Amount is not equal to the waived off amount for riderId " <> riderDetails.id.getId <> " rideId " <> req.rideId <> " bookingId " <> req.bookingId <> " waivedOffAmount " <> show req.waivedOffAmount <> " cancellationDues " <> show booking.fareParams.customerCancellationDues
    throwError $ InvalidRequest $ "Cancellation Due Amount is not equal to the waived off amount for riderId " <> riderDetails.id.getId <> " rideId " <> req.rideId <> " bookingId " <> req.bookingId <> " waivedOffAmount " <> show req.waivedOffAmount <> " cancellationDues " <> show booking.fareParams.customerCancellationDues <> " and riderDetails.cancellationDues " <> show riderDetails.cancellationDues
  when (riderDetails.cancellationDues < req.waivedOffAmount) $
    throwError $ InvalidRequest $ "Cancellation Due Amount is less than the waived off amount for riderId " <> riderDetails.id.getId
  QRD.updateWaivedOffAmount req.waivedOffAmount riderDetails.id.getId
  QRD.updateNoOfTimesWaiveOffUsed riderDetails.id.getId
  QRD.updateCancellationDues (riderDetails.cancellationDues - req.waivedOffAmount) riderDetails.id
  return $ Success
