{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.PayoutDriverStatus (getPayoutStatus) where

import qualified API.Types.UI.PayoutDriverStatus as API
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PayoutStatusHistory as DPSH
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Person
import qualified Domain.Types.ScheduledPayout as DSP
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.PayoutStatusHistory as QPSH
import qualified Storage.Queries.ScheduledPayout as QSP
import qualified Storage.Queries.ScheduledPayoutExtra as QSP
import Tools.Error
import qualified Kernel.External.Payout.Interface.Types as IPayout
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPO
import Storage.Beam.Payment ()
import qualified Tools.Payout as Payout
import qualified Kernel.External.Payout.Types as TPayout
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Types.Common as DPayment
import Data.Maybe (listToMaybe)

getPayoutStatus ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow API.DriverPayoutStatusResp
  )
getPayoutStatus (mbPersonId, merchantId, merchantOpCityId) rideId = do
  -- Fetch scheduled payout by rideId
  payout <- QSP.findByRideId rideId >>= fromMaybeM (InvalidRequest "Payout not found for this ride")

  -- Validate that the driver owns this payout (optional but good practice)
  case mbPersonId of
    Just personId ->
      unless (payout.driverId == Kernel.Types.Id.getId personId) $
        throwError $ InvalidRequest "Unauthorized: You can only view your own payouts"
    Nothing -> pure ()

  payoutOrders <- QPO.findAllByEntityNameAndEntityIds (Just 1) (Just 0) (Just DPayment.SPECIAL_ZONE_PAYOUT) [Just payout.id.getId]
  updatedStatus <- do
    case (listToMaybe payoutOrders) of
      Just payoutOrder -> do
        let createPayoutOrderStatusReq = IPayout.PayoutOrderStatusReq {orderId = payoutOrder.orderId, mbExpand = Nothing}
            createPayoutOrderStatusCall = Payout.payoutOrderStatus merchantId merchantOpCityId (DEMSC.RidePayoutService TPayout.Juspay) (Just payout.driverId)
        resp <- DPayment.payoutStatusService (Kernel.Types.Id.cast merchantId) (Kernel.Types.Id.Id payout.driverId) createPayoutOrderStatusReq createPayoutOrderStatusCall
        let newStatus = QSP.castPayoutOrderStatusToScheduledPayoutStatus resp.status
        if (payout.status /= newStatus && payout.status /= DSP.CREDITED)
          then do
            let statusMsg = "Juspay Order Status Updated: " <> show resp.status
            QSP.updateStatusWithHistoryById newStatus (Just statusMsg) payout
            pure newStatus
          else pure payout.status
      Nothing -> do
        logError $ "Payout Order not found for scheduled payoutId: " <> show payout.id
        pure payout.status

  statusHistory <- QPSH.findByScheduledPayoutId Nothing Nothing payout.id

  pure $
    API.DriverPayoutStatusResp
      { status = updatedStatus,
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
