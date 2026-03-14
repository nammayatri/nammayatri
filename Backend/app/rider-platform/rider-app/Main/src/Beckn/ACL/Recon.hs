{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Recon
  ( buildReconReq,
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

buildReconReq ::
  (MonadFlow m) =>
  DBC.BecknConfig ->
  [PPFRecon] ->
  m ReconSpec.ReconReq
buildReconReq bapConfig reconEntries = do
  now <- getCurrentTime
  messageId <- generateGUID
  let orders = map buildReconOrder reconEntries
      context =
        Spec.Context
          { contextAction = Just "recon",
            contextBapId = Just bapConfig.subscriberId,
            contextBapUri = Just $ showBaseUrl bapConfig.subscriberUrl,
            contextBppId = Nothing,
            contextBppUri = Nothing,
            contextDomain = Just bapConfig.domain,
            contextKey = Nothing,
            contextLocation = Nothing,
            contextMessageId = UUID.fromText messageId,
            contextTimestamp = Just $ TimeRFC339.UTCTimeRFC3339 now,
            contextTransactionId = Nothing,
            contextTtl = Nothing,
            contextVersion = Just "2.0.0"
          }
  pure
    ReconSpec.ReconReq
      { reconReqContext = context,
        reconReqMessage =
          ReconSpec.ReconMessage
            { reconMessageOrders = orders
            }
      }

buildReconOrder :: PPFRecon -> ReconSpec.ReconOrder
buildReconOrder recon =
  let orderAmt = ReconSpec.ReconAmount recon.currency (show recon.orderAmount.settled) Nothing
      settlement = buildReconSettlement recon
   in ReconSpec.ReconOrder
        { reconOrderId = recon.networkOrderId,
          reconOrderAmount = orderAmt,
          reconOrderSettlements = [settlement]
        }

buildReconSettlement :: PPFRecon -> ReconSpec.ReconSettlement
buildReconSettlement recon =
  let mkAmt val = ReconSpec.ReconAmount recon.currency (show val) Nothing
   in ReconSpec.ReconSettlement
        { reconSettlementId = Just recon.id.getId,
          reconSettlementPaymentId = Nothing,
          reconSettlementStatus = Just $ show recon.settlementStatus,
          reconSettlementAmount = Just $ mkAmt recon.orderAmount.settled,
          reconSettlementCommission = Just $ mkAmt recon.buyerAppCommission.settled,
          reconSettlementWithholdingAmount = Nothing,
          reconSettlementTcs = Nothing,
          reconSettlementTds = Nothing,
          reconSettlementUpdatedAt = Just $ show (TimeRFC339.UTCTimeRFC3339 recon.updatedAt),
          reconSettlementSettlementRefNo = Nothing
        }
