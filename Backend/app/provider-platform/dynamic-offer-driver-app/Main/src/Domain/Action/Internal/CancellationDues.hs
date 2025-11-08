module Domain.Action.Internal.CancellationDues where

import qualified Data.Aeson as A
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Ride as DRide
import Environment (Flow)
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.UserCancellationDues as UserCancellationDues
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Merchant as QM
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import Tools.DynamicLogic
import Tools.Error

data CustomerCancellationDuesWaiveOffReq = CustomerCancellationDuesWaiveOffReq
  { rideId :: Text,
    bookingId :: Text,
    waiveOffAmount :: HighPrecMoney
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

customerCancellationDuesWaiveOff :: Id DMerchant.Merchant -> Maybe Text -> CustomerCancellationDuesWaiveOffReq -> Flow APISuccess
customerCancellationDuesWaiveOff merchantId apiKey req = withLogTag ("customerCancellationDuesWaiveOff: rideId-" <> req.rideId <> " bookingId-" <> req.bookingId) $ do
  logInfo $ "customerCancellationDuesWaiveOff: received request" <> show req
  merchant <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  let rideId = (Id req.rideId) :: Id DRide.Ride
  ride <- QRide.findById rideId >>= fromMaybeM (RideNotFound req.rideId)
  let bookingId = (Id req.bookingId) :: Id DBooking.Booking
  booking <- QBooking.findById bookingId >>= fromMaybeM (BookingNotFound req.bookingId)
  riderId <- booking.riderId & fromMaybeM (BookingFieldNotPresent "rider_id")
  riderDetails <- QRD.findById riderId >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
  transporterConfig <- SCTC.findByMerchantOpCityId ride.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
  logInfo $ "Cancellation Due Amount is not equal to the waived off amount for riderId " <> riderDetails.id.getId <> " rideId " <> req.rideId <> " bookingId " <> req.bookingId <> " waiveOffAmount " <> show req.waiveOffAmount <> " cancellationDues " <> show ride.cancellationChargesOnCancel
  unless (ride.cancellationChargesOnCancel == Just req.waiveOffAmount) $ do
    logWarning $ "Cancellation Due Amount is not equal to the waived off amount for riderId " <> riderDetails.id.getId <> " rideId " <> req.rideId <> " bookingId " <> req.bookingId <> " waiveOffAmount " <> show req.waiveOffAmount <> " cancellationDues " <> show ride.cancellationChargesOnCancel
    throwError $ InvalidRequest $ "Cancellation Due Amount is not equal to the waived off amount for riderId " <> riderDetails.id.getId <> " rideId " <> req.rideId <> " bookingId " <> req.bookingId <> " waiveOffAmount " <> show req.waiveOffAmount <> " cancellationDues " <> show ride.cancellationChargesOnCancel <> " and riderDetails.cancellationDues " <> show riderDetails.cancellationDues
  when (riderDetails.cancellationDues < req.waiveOffAmount) $
    throwError $ InvalidRequest $ "Cancellation Due Amount is less than the waived off amount for riderId " <> riderDetails.id.getId
  let logicInput =
        UserCancellationDues.UserCancellationDuesWaiveOffData
          { cancellationDues = riderDetails.cancellationDues,
            cancelledRides = riderDetails.cancelledRides,
            totalBookings = riderDetails.totalBookings,
            completedRides = riderDetails.completedRides,
            validCancellations = riderDetails.validCancellations,
            cancellationDueRides = riderDetails.cancellationDueRides,
            cancellationDuesPaid = riderDetails.cancellationDuesPaid,
            noOfTimesWaiveOffUsed = riderDetails.noOfTimesWaiveOffUsed,
            noOfTimesCanellationDuesPaid = riderDetails.noOfTimesCanellationDuesPaid,
            waivedOffAmount = riderDetails.waivedOffAmount,
            currentWaivingOffAmount = req.waiveOffAmount
          }
  canWaiveOffResult <-
    if transporterConfig.canAddCancellationFee
      then do
        localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
        (allLogics, _mbVersion) <- getAppDynamicLogic (cast ride.merchantOperatingCityId) LYT.USER_CANCELLATION_DUES_WAIVE_OFF localTime Nothing Nothing
        response <- withTryCatch "runLogics:canWaiveOffResult" $ LYTU.runLogics allLogics logicInput
        case response of
          Left e -> do
            logError $ "Error in running UserCancellationDuesLogics - " <> show e <> " - " <> show logicInput <> " - " <> show allLogics
            return False
          Right resp -> do
            case (A.fromJSON resp.result :: A.Result UserCancellationDues.UserCancellationDuesWaiveOffResult) of
              A.Success result -> do
                logTagInfo ("bookingId-" <> getId booking.id) ("result.waiveOff: " <> show result.canWaiveOff)
                return result.canWaiveOff
              A.Error e -> do
                logError $ "Error in parsing UserCancellationDuesResult - " <> show e <> " - " <> show resp.result <> " - " <> show logicInput <> " - " <> show allLogics
                return False
      else return False

  when canWaiveOffResult $ do
    QRD.updateWaivedOffAmount req.waiveOffAmount riderDetails.id.getId
    QRD.updateNoOfTimesWaiveOffUsed riderDetails.id.getId
    QRD.updateCancellationDues (riderDetails.cancellationDues - req.waiveOffAmount) riderDetails.id
  when (not canWaiveOffResult) $ throwError $ InvalidRequest $ "Failed to waive off cancellation dues for riderId from jsonLogic - " <> riderDetails.id.getId <> " rideId " <> req.rideId <> " bookingId " <> req.bookingId <> " waiveOffAmount " <> show req.waiveOffAmount
  return $ Success
