{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.PayoutDriverStatus (getPayoutStatus) where

import qualified API.Types.UI.PayoutDriverStatus as API
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PayoutStatusHistory as DPSH
import qualified Domain.Types.Person
import qualified Domain.Types.ScheduledPayout as DSP
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.PayoutStatusHistory as QPSH
import qualified Storage.Queries.ScheduledPayout as QSP
import Tools.Error

getPayoutStatus ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow API.DriverPayoutStatusResp
  )
getPayoutStatus (mbPersonId, _merchantId, _merchantOpCityId) rideId = do
  -- Fetch scheduled payout by rideId
  payout <- QSP.findByRideId rideId >>= fromMaybeM (InvalidRequest "Payout not found for this ride")

  -- Validate that the driver owns this payout (optional but good practice)
  case mbPersonId of
    Just personId ->
      unless (payout.driverId == Kernel.Types.Id.getId personId) $
        throwError $ InvalidRequest "Unauthorized: You can only view your own payouts"
    Nothing -> pure ()

  -- Fetch status history (ordered by createdAt)
  statusHistory <- QPSH.findByScheduledPayoutId Nothing Nothing payout.id

  pure $
    API.DriverPayoutStatusResp
      { status = payout.status,
        amount = payout.amount,
        rideId = payout.rideId,
        payoutTransactionId = payout.payoutTransactionId,
        expectedCreditTime = payout.expectedCreditTime,
        failureReason = payout.failureReason,
        statusHistory = map convertHistory statusHistory,
        createdAt = payout.createdAt,
        updatedAt = payout.updatedAt
      }

-- Helper to convert domain status history to API status event
convertHistory :: DPSH.PayoutStatusHistory -> API.DriverPayoutStatusEvent
convertHistory h =
  API.DriverPayoutStatusEvent
    { status = h.status,
      timestamp = h.createdAt,
      message = h.message
    }
