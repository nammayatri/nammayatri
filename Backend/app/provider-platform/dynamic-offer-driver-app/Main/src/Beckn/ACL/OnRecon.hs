{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnRecon
  ( buildOnReconReq,
  )
where

import qualified BecknV2.OnDemand.Types as Spec
import qualified BecknV2.OnDemand.Types.Recon as ReconSpec
import qualified Data.UUID as UUID
import qualified Domain.Types.BecknConfig as DBC
import Kernel.Prelude
import qualified Kernel.Types.TimeRFC339 as TimeRFC339
import Kernel.Utils.Common
import Lib.Finance.Domain.Types.PPFRecon

-- | Build a BECKN on_recon response from BPP-side recon processing results.
buildOnReconReq ::
  (MonadFlow m) =>
  DBC.BecknConfig ->
  Text ->
  Maybe Text ->
  [(PPFRecon, Bool)] ->
  m ReconSpec.OnReconReq
buildOnReconReq bppConfig bapSubscriberId mbBapUri results = do
  now <- getCurrentTime
  messageId <- generateGUID
  let orders = map (buildOnReconOrder now) results
      context =
        Spec.Context
          { contextAction = Just "on_recon",
            contextBapId = Just bapSubscriberId,
            contextBapUri = mbBapUri,
            contextBppId = Just bppConfig.subscriberId,
            contextBppUri = Just $ showBaseUrl bppConfig.subscriberUrl,
            contextDomain = Just bppConfig.domain,
            contextKey = Nothing,
            contextLocation = Nothing,
            contextMessageId = UUID.fromText messageId,
            contextTimestamp = Just $ TimeRFC339.UTCTimeRFC3339 now,
            contextTransactionId = Nothing,
            contextTtl = Nothing,
            contextVersion = Just "2.0.0"
          }
  pure
    ReconSpec.OnReconReq
      { onReconReqContext = context,
        onReconReqMessage =
          ReconSpec.OnReconMessage
            { onReconMessageOrders = orders
            }
      }

buildOnReconOrder :: UTCTime -> (PPFRecon, Bool) -> ReconSpec.OnReconOrder
buildOnReconOrder now (recon, accord) =
  let diffAmount = case recon.orderAmount.expected of
        Just exp' -> Just $ show (recon.orderAmount.settled - exp')
        Nothing -> Nothing
      orderAmt =
        ReconSpec.ReconAmount
          { reconAmountCurrency = recon.currency,
            reconAmountValue = show recon.orderAmount.settled,
            reconAmountDiffAmount = diffAmount
          }
      settlement = buildOnReconSettlement recon now
   in ReconSpec.OnReconOrder
        { onReconOrderId = recon.networkOrderId,
          onReconOrderAmount = orderAmt,
          onReconOrderReconAccord = accord,
          onReconOrderSettlements = [settlement]
        }

buildOnReconSettlement :: PPFRecon -> UTCTime -> ReconSpec.OnReconSettlement
buildOnReconSettlement recon now =
  let mkAmt val = ReconSpec.ReconAmount recon.currency (show val) Nothing
   in ReconSpec.OnReconSettlement
        { onReconSettlementId = Just recon.id.getId,
          onReconSettlementPaymentId = Nothing,
          onReconSettlementStatus = Just $ show recon.settlementStatus,
          onReconSettlementDueDate = Nothing,
          onReconSettlementAmount = Just $ mkAmt recon.orderAmount.settled,
          onReconSettlementCommission = Just $ mkAmt recon.buyerAppCommission.settled,
          onReconSettlementWithholdingAmount = Nothing,
          onReconSettlementTcs = Nothing,
          onReconSettlementTds = Nothing,
          onReconSettlementUpdatedAt = Just $ show (TimeRFC339.UTCTimeRFC3339 now),
          onReconSettlementSettlementRefNo = Nothing
        }
