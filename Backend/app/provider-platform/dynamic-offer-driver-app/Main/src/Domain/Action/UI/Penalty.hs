{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.Penalty (postPenaltyCheck) where

import qualified API.Types.UI.Penalty
import Data.OpenApi (ToSchema)
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride as DRide
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified SharedLogic.CancellationConsequence as CancellationConsequence
import qualified SharedLogic.CancellationFault as CancellationFault
import qualified SharedLogic.CancellationOrchestrator as Orchestrator
import SharedLogic.DriverCancellationPenalty (applyCancellationPenaltyGst)
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Ride as QRide
import Tools.Auth
import Tools.Error

postPenaltyCheck ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.Penalty.PenaltyCheckReq ->
    Environment.Flow API.Types.UI.Penalty.PenaltyCheckRes
  )
postPenaltyCheck (mbPersonId, _merchantId, _merchantOpCityId) req = do
  driverId <- mbPersonId & fromMaybeM (InvalidRequest "Driver not authenticated")

  ride <- QRide.findById (Kernel.Types.Id.Id req.rideId) >>= fromMaybeM (RideNotFound req.rideId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)

  unless (ride.driverId == Kernel.Types.Id.cast driverId) $
    throwError (InvalidRequest "Driver does not own this ride")

  unless (ride.status == DRide.NEW) $
    throwError (InvalidRequest "Ride cannot be cancelled in current state")

  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  (mbDriverDistToPickup, _) <- Orchestrator.getDistanceToPickup booking (Just ride)
  -- The SAME decision pipeline as a real driver cancel (signals → fault verdict →
  -- matrix row), via the orchestrator's dry-run entry: no Redis caches, no ride-row
  -- persistence — the cancellation may never happen.
  decision <- Orchestrator.previewCancellationConsequences booking ride transporterConfig SBCR.ByDriver Nothing mbDriverDistToPickup
  let rawPenaltyAmount = (\row -> CancellationConsequence.driverMoneyDeduction row booking.estimatedFare) =<< decision.consequenceRow
      -- The matrix stores the driver penalty GST-exclusive; the real charge
      -- (SharedLogic.DriverCancellationPenalty.applyCancellationPenaltyGst, applied once the
      -- DriverFee moves to PAYMENT_PENDING) grosses it up. Apply the same multiplier here so
      -- this preview matches what the driver is actually charged. Only positive amounts (an
      -- actual charge) are grossed up — a negative amount is compensation credited via the
      -- wallet, which never carries GST.
      penaltyAmount = rawPenaltyAmount <&> \amt -> if amt > 0 then applyCancellationPenaltyGst amt else amt
      isApplicable = isJust penaltyAmount
      -- Verdict-based validity (the RideCancel tag rules are retired). "Valid" follows
      -- the legacy DriverCancellation#Valid convention: the cancellation VALIDLY COUNTS
      -- AGAINST the driver (driver at fault — penalty/rate apply); "Invalid" = it does
      -- not (customer at fault); no verdict (no rules / shared / no fault) -> no label.
      cancellationValidity = case (.atFault) <$> decision.faultVerdict of
        Just CancellationFault.DriverAtFault -> Just "Valid"
        Just CancellationFault.CustomerAtFault -> Just "Invalid"
        _ -> Nothing

  return $
    API.Types.UI.Penalty.PenaltyCheckRes
      { isCancellationPenaltyApplicable = Just isApplicable,
        cancellationPenaltyAmount = penaltyAmount,
        cancellationValidity = cancellationValidity
      }
