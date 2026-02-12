{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Payment.Payout.Status
  ( refreshPayoutStatus,
    refreshPayoutStatusWithResponse,
  )
where

import Kernel.External.Encryption (EncFlow)
import qualified Kernel.External.Payout.Interface.Types as IPayout
import qualified Kernel.External.Payout.Juspay.Types.Payout as JPayout
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM)
import qualified Lib.Payment.Domain.Action as DPayment
import qualified Lib.Payment.Domain.Action as PayoutUpdates
import Lib.Payment.Domain.Types.Common (Merchant, Person)
import qualified Lib.Payment.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPO

refreshPayoutStatus ::
  ( EncFlow m r,
    BeamFlow.BeamFlow m r
  ) =>
  Id Merchant ->
  Id Person ->
  IPayout.PayoutOrderStatusReq ->
  (IPayout.PayoutOrderStatusReq -> m IPayout.PayoutOrderStatusResp) ->
  status ->
  (JPayout.PayoutOrderStatus -> status) ->
  (status -> status -> Bool) ->
  (status -> JPayout.PayoutOrderStatus -> m ()) ->
  m status
refreshPayoutStatus merchantId personId statusReq statusCall currentStatus mapStatus shouldUpdate onUpdate = do
  resp <- DPayment.payoutStatusService merchantId personId statusReq statusCall
  let newStatus = mapStatus resp.status
  if shouldUpdate currentStatus newStatus
    then do
      onUpdate newStatus resp.status
      pure newStatus
    else pure currentStatus

refreshPayoutStatusWithResponse ::
  ( EncFlow m r,
    BeamFlow.BeamFlow m r
  ) =>
  Text ->
  IPayout.PayoutOrderStatusReq ->
  (IPayout.PayoutOrderStatusReq -> m IPayout.PayoutOrderStatusResp) ->
  status ->
  (JPayout.PayoutOrderStatus -> status) ->
  (status -> status -> Bool) ->
  (status -> IPayout.PayoutOrderStatusResp -> m ()) ->
  m IPayout.PayoutOrderStatusResp
refreshPayoutStatusWithResponse orderId statusReq statusCall currentStatus mapStatus shouldUpdate onUpdate = do
  _ <- QPO.findByOrderId orderId >>= fromMaybeM (PayoutOrderNotFound orderId)
  statusResp <- statusCall statusReq
  PayoutUpdates.payoutStatusUpdates statusResp.status orderId (Just statusResp)
  let newStatus = mapStatus statusResp.status
  when (shouldUpdate currentStatus newStatus) $
    onUpdate newStatus statusResp
  pure statusResp
