module Domain.Action.Internal.CancellationDues where

import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.CancellationDuesDetails as DCDD
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Ride as DRide
import Environment (Flow)
import qualified EulerHS.Language as L
import Kernel.Beam.Types (TxnIdKey (..))
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.CancellationConsequenceMatrix as QCCM
import qualified Storage.Queries.CancellationDuesDetails as QCDD
import qualified Storage.Queries.Merchant as QM
import qualified Storage.Queries.QueriesExtra.BookingLite as QBookingLite
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
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
  booking <- QBookingLite.findByIdLite bookingId >>= fromMaybeM (BookingNotFound req.bookingId)
  L.setOptionLocal TxnIdKey booking.transactionId
  riderId <- booking.riderId & fromMaybeM (BookingFieldNotPresent "rider_id")
  riderDetails <- QRD.findById riderId >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
  -- Check if cancellation dues for this ride are still pending
  mbCancellationDuesDetails <- QCDD.findByRideId rideId
  case mbCancellationDuesDetails of
    Just duesDetails -> do
      when (duesDetails.paymentStatus /= DCDD.PENDING) $
        throwError $ InvalidRequest $ "Cancellation dues for rideId " <> req.rideId <> " are already " <> show duesDetails.paymentStatus <> ". Cannot waive off."
      -- The consequence-matrix row that produced this charge can forbid waive-off outright.
      whenJust duesDetails.cancellationConsequenceRowId $ \rowId -> do
        mbConsequenceRow <- QCCM.findByPrimaryKey (Id rowId)
        whenJust mbConsequenceRow $ \row ->
          unless row.waiveOffAllowed $
            throwError $ InvalidRequest $ "Waive-off is not allowed for this cancellation charge (consequence matrix row " <> rowId <> "), rideId: " <> req.rideId
    Nothing -> logWarning $ "No CancellationDuesDetails entry found for rideId: " <> req.rideId <> ". Proceeding with legacy flow."
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = ride.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound ride.merchantOperatingCityId.getId)
  logInfo $ "Cancellation Due Amount is not equal to the waived off amount for riderId " <> riderDetails.id.getId <> " rideId " <> req.rideId <> " bookingId " <> req.bookingId <> " waiveOffAmount " <> show req.waiveOffAmount <> " cancellationDues " <> show ride.cancellationChargesOnCancel
  unless (ride.cancellationChargesOnCancel == Just req.waiveOffAmount) $ do
    logWarning $ "Cancellation Due Amount is not equal to the waived off amount for riderId " <> riderDetails.id.getId <> " rideId " <> req.rideId <> " bookingId " <> req.bookingId <> " waiveOffAmount " <> show req.waiveOffAmount <> " cancellationDues " <> show ride.cancellationChargesOnCancel
    throwError $ InvalidRequest $ "Cancellation Due Amount is not equal to the waived off amount for riderId " <> riderDetails.id.getId <> " rideId " <> req.rideId <> " bookingId " <> req.bookingId <> " waiveOffAmount " <> show req.waiveOffAmount <> " cancellationDues " <> show ride.cancellationChargesOnCancel <> " and riderDetails.cancellationDues " <> show riderDetails.cancellationDues
  when (riderDetails.cancellationDues < req.waiveOffAmount) $
    throwError $ InvalidRequest $ "Cancellation Due Amount is less than the waived off amount for riderId " <> riderDetails.id.getId
  -- Waive-off decision is matrix-driven (the USER_CANCELLATION_DUES_WAIVE_OFF JsonLogic
  -- is retired): the row must allow it (hard-checked above) and the rider must be within
  -- the row's maxWaiveOffsPerPeriod over waiveOffPeriodDays (default 30), counted from
  -- WAIVED dues rows in that window.
  canWaiveOffResult <-
    if transporterConfig.canAddCancellationFee
      then case mbCancellationDuesDetails >>= (.cancellationConsequenceRowId) of
        Nothing -> do
          -- pre-matrix dues row: no row to consult; the amount + PENDING checks above
          -- still guard the operation
          logWarning $ "Waive-off on a pre-matrix dues row (no consequence row id), allowing: rideId " <> req.rideId
          pure True
        Just consequenceRowId -> do
          mbConsequenceRow <- QCCM.findByPrimaryKey (Id consequenceRowId)
          case mbConsequenceRow of
            Nothing -> do
              logError $ "Consequence matrix row " <> consequenceRowId <> " not found for waive-off, rideId " <> req.rideId
              pure False
            Just row
              | not row.waiveOffAllowed -> pure False
              | otherwise -> case row.maxWaiveOffsPerPeriod of
                Nothing -> pure True
                Just maxWaives -> do
                  now <- getCurrentTime
                  let periodDays = fromMaybe 30 row.waiveOffPeriodDays
                      since = addUTCTime (negate $ fromIntegral periodDays * 86400) now
                  waivedRows <- QCDD.findAllWaivedByRiderId riderDetails.id
                  let waivesInPeriod = length $ filter (\d -> d.updatedAt >= since) waivedRows
                  when (waivesInPeriod >= maxWaives) $
                    logWarning $ "Waive-off limit reached (" <> show waivesInPeriod <> "/" <> show maxWaives <> " in last " <> show periodDays <> " days) for riderId " <> riderDetails.id.getId
                  pure (waivesInPeriod < maxWaives)
      else pure False

  when canWaiveOffResult $ do
    QRD.updateWaivedOffAmount req.waiveOffAmount riderDetails.id.getId
    QRD.updateNoOfTimesWaiveOffUsed riderDetails.id.getId
    QRD.updateCancellationDues (riderDetails.cancellationDues - req.waiveOffAmount) riderDetails.id
    QCDD.updatePaymentStatusByRideId DCDD.WAIVED rideId
  when (not canWaiveOffResult) $ throwError $ InvalidRequest $ "Failed to waive off cancellation dues for riderId from jsonLogic - " <> riderDetails.id.getId <> " rideId " <> req.rideId <> " bookingId " <> req.bookingId <> " waiveOffAmount " <> show req.waiveOffAmount
  return $ Success
