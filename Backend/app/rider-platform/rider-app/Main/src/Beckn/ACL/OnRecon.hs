{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Beckn.ACL.OnRecon
  ( DOnReconReq (..),
    DOrderReconResult (..),
    DSettlementResult (..),
    buildOnReconDomainReq,
  )
where

import qualified BecknV2.OnDemand.Types.Recon as ReconSpec
import qualified BecknV2.Utils as Utils
import Kernel.Prelude
import Kernel.Utils.Common
import Tools.Error

data DOnReconReq = DOnReconReq
  { orderReconResults :: [DOrderReconResult]
  }
  deriving (Generic)

data DOrderReconResult = DOrderReconResult
  { orderId :: Text,
    orderAmount :: Price,
    reconAccord :: Bool,
    settlements :: [DSettlementResult]
  }
  deriving (Generic)

data DSettlementResult = DSettlementResult
  { settlementId :: Maybe Text,
    status :: Maybe Text,
    amount :: Maybe Price,
    diffValue :: Maybe HighPrecMoney,
    settlementRefNo :: Maybe Text,
    updatedAt :: Maybe Text
  }
  deriving (Generic)

buildOnReconDomainReq ::
  (MonadFlow m) =>
  ReconSpec.OnReconReq ->
  m DOnReconReq
buildOnReconDomainReq onReconReq = do
  let orders = onReconReq.onReconReqMessage.onReconMessageOrders
  orderResults <- mapM parseOnReconOrder orders
  pure DOnReconReq {orderReconResults = orderResults}

parseOnReconOrder :: (MonadFlow m) => ReconSpec.OnReconOrder -> m DOrderReconResult
parseOnReconOrder order = do
  amount <- Utils.parseReconAmount order.onReconOrderAmount & fromMaybeM (InvalidRequest $ "Invalid onReconOrderAmount for orderId: " <> order.onReconOrderId)
  settlements <- mapM (parseOnReconSettlement order.onReconOrderId) order.onReconOrderSettlements
  pure
    DOrderReconResult
      { orderId = order.onReconOrderId,
        orderAmount = amount,
        reconAccord = order.onReconOrderReconAccord,
        settlements = settlements
      }

parseOnReconSettlement :: (MonadFlow m) => Text -> ReconSpec.OnReconSettlement -> m DSettlementResult
parseOnReconSettlement orderId settlement = do
  amount <- mapM (\amt -> Utils.parseReconAmount amt & fromMaybeM (InvalidRequest $ "Invalid settlement amount for orderId: " <> orderId)) settlement.onReconSettlementAmount
  let diffValue = settlement.onReconSettlementAmount >>= Utils.parseReconDiffAmount
  pure
    DSettlementResult
      { settlementId = settlement.onReconSettlementId,
        status = settlement.onReconSettlementStatus,
        amount = amount,
        diffValue = diffValue,
        settlementRefNo = settlement.onReconSettlementSettlementRefNo,
        updatedAt = settlement.onReconSettlementUpdatedAt
      }
