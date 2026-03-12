{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.PPF.Recon
  ( processRecon,
  )
where

import qualified Beckn.ACL.Recon as ACLRecon
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common ()
import Lib.Finance.Domain.Types.PPFRecon (PPFRecon)
import qualified Lib.Finance.Recon.Interface as FinRecon
import qualified Lib.Finance.Recon.Service as FinRecon
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Storage.Queries.Ride as QRide

-- | Process incoming recon request from BAP (Collector).
-- Delegates to the finance-kernel's processRecon, providing a
-- ride fare lookup callback for amount validation.
processRecon ::
  ( BeamFlow.BeamFlow m r
  ) =>
  Text ->
  Text ->
  Text ->
  ACLRecon.DReconReq ->
  m [(PPFRecon, Bool)]
processRecon bppSubscriberId merchantId merchantOperatingCityId reconReq = do
  let reconInput = toReconInput bppSubscriberId merchantId merchantOperatingCityId reconReq
  results <- FinRecon.processRecon lookupRideFare FinRecon.defaultTolerance reconInput
  pure [r | Right r <- results]

-- | Look up the final fare for a ride by its BPP ride ID (networkOrderId).
lookupRideFare ::
  ( BeamFlow.BeamFlow m r
  ) =>
  Text ->
  m (Maybe HighPrecMoney)
lookupRideFare networkOrderId = do
  mbRide <- QRide.findById (Id networkOrderId)
  pure $ mbRide >>= (.fare)

-- | Convert driver-side ACL domain types to finance-kernel input types.
toReconInput :: Text -> Text -> Text -> ACLRecon.DReconReq -> FinRecon.ReconInput
toReconInput bppSubscriberId merchantId merchantOperatingCityId reconReq =
  FinRecon.ReconInput
    { collectorSubscriberId = reconReq.collectorSubscriberId,
      receiverSubscriberId = bppSubscriberId,
      domain = reconReq.domain,
      currency = fromMaybe "INR" $ listToMaybe reconReq.orderEntries >>= \e -> Just $ show e.orderAmount.currency,
      merchantId = merchantId,
      merchantOperatingCityId = merchantOperatingCityId,
      orderEntries = map toOrderInput reconReq.orderEntries
    }

toOrderInput :: ACLRecon.DReconOrderEntry -> FinRecon.ReconOrderInput
toOrderInput entry =
  FinRecon.ReconOrderInput
    { orderId = entry.orderId,
      orderAmountSettled = entry.orderAmount.amount,
      settlements = map toSettlementInput entry.settlements
    }

toSettlementInput :: ACLRecon.DReconSettlement -> FinRecon.ReconSettlementInput
toSettlementInput s =
  FinRecon.ReconSettlementInput
    { settlementId = s.settlementId,
      paymentId = s.paymentId,
      status = s.status,
      amount = (.amount) <$> s.amount,
      commission = (.amount) <$> s.commission,
      withholdingAmount = (.amount) <$> s.withholdingAmount,
      tds = (.amount) <$> s.tds,
      tcs = (.amount) <$> s.tcs,
      settlementRefNo = s.settlementRefNo
    }
