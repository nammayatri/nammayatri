{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.Recon
  ( DReconReq (..),
    DReconOrderEntry (..),
    DReconSettlement (..),
    buildReconDomainReq,
  )
where

import qualified BecknV2.OnDemand.Types.Recon as ReconSpec
import qualified BecknV2.Utils as Utils
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Common
import Tools.Error

-- | Domain-level recon request parsed from BECKN recon message.
-- Used on the BPP (Receiver) side.
data DReconReq = DReconReq
  { collectorSubscriberId :: Text,
    domain :: Text,
    orderEntries :: [DReconOrderEntry]
  }
  deriving (Generic)

-- | Individual order entry from a recon request (per ONDC NTS RSF 2.0.0)
data DReconOrderEntry = DReconOrderEntry
  { orderId :: Text,
    orderAmount :: Price,
    settlements :: [DReconSettlement]
  }
  deriving (Generic)

-- | Settlement entry parsed from recon order
data DReconSettlement = DReconSettlement
  { settlementId :: Maybe Text,
    paymentId :: Maybe Text,
    status :: Maybe Text,
    amount :: Maybe Price,
    commission :: Maybe Price,
    withholdingAmount :: Maybe Price,
    tds :: Maybe Price,
    tcs :: Maybe Price,
    settlementRefNo :: Maybe Text
  }
  deriving (Generic)

-- | Parse BECKN recon request into domain types.
-- Called by the BPP when receiving recon from BAP.
buildReconDomainReq ::
  (MonadFlow m) =>
  ReconSpec.ReconReq ->
  m DReconReq
buildReconDomainReq reconReq = do
  let ctx = reconReq.reconReqContext
  collectorSubId <- ctx.contextBapId & fromMaybeM (InvalidRequest "Missing BAP subscriber ID in recon context")
  domainText <- ctx.contextDomain & fromMaybeM (InvalidRequest "Missing domain in recon context")
  let orders = reconReq.reconReqMessage.reconMessageOrders
  entries <- mapM parseOrderEntry orders
  pure
    DReconReq
      { collectorSubscriberId = collectorSubId,
        domain = domainText,
        orderEntries = entries
      }

-- | Parse an individual order entry from a BECKN recon message.
parseOrderEntry :: (MonadFlow m) => ReconSpec.ReconOrder -> m DReconOrderEntry
parseOrderEntry order = do
  let orderId = order.reconOrderId
  orderAmt <- Utils.parseReconAmount order.reconOrderAmount & fromMaybeM (InvalidRequest $ "Invalid orderAmount in recon for orderId: " <> orderId)
  settlements <- mapM (parseSettlement orderId) order.reconOrderSettlements
  pure
    DReconOrderEntry
      { orderId = orderId,
        orderAmount = orderAmt,
        settlements = settlements
      }

-- | Parse a settlement entry from a recon order
parseSettlement :: (MonadFlow m) => Text -> ReconSpec.ReconSettlement -> m DReconSettlement
parseSettlement orderId settlement = do
  amount <- mapM (\amt -> Utils.parseReconAmount amt & fromMaybeM (InvalidRequest $ "Invalid settlement.amount for orderId: " <> orderId)) settlement.reconSettlementAmount
  commission <- mapM (\amt -> Utils.parseReconAmount amt & fromMaybeM (InvalidRequest $ "Invalid settlement.commission for orderId: " <> orderId)) settlement.reconSettlementCommission
  withholdingAmt <- mapM (\amt -> Utils.parseReconAmount amt & fromMaybeM (InvalidRequest $ "Invalid settlement.withholding_amount for orderId: " <> orderId)) settlement.reconSettlementWithholdingAmount
  tdsAmt <- mapM (\amt -> Utils.parseReconAmount amt & fromMaybeM (InvalidRequest $ "Invalid settlement.tds for orderId: " <> orderId)) settlement.reconSettlementTds
  tcsAmt <- mapM (\amt -> Utils.parseReconAmount amt & fromMaybeM (InvalidRequest $ "Invalid settlement.tcs for orderId: " <> orderId)) settlement.reconSettlementTcs
  pure
    DReconSettlement
      { settlementId = settlement.reconSettlementId,
        paymentId = settlement.reconSettlementPaymentId,
        status = settlement.reconSettlementStatus,
        amount = amount,
        commission = commission,
        withholdingAmount = withholdingAmt,
        tds = tdsAmt,
        tcs = tcsAmt,
        settlementRefNo = settlement.reconSettlementSettlementRefNo
      }
