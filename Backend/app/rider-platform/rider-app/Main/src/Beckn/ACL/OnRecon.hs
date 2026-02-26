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
    buildOnReconDomainReq,
  )
where

import qualified BecknV2.OnDemand.Types.Recon as ReconSpec
import Kernel.Prelude
import Kernel.Utils.Common

-- | Domain-level on_recon request parsed from BECKN on_recon message
data DOnReconReq = DOnReconReq
  { orderReconResults :: [DOrderReconResult]
  }
  deriving (Show, Generic)

-- | Individual order recon result from BPP
data DOrderReconResult = DOrderReconResult
  { orderId :: Text,
    status :: Text, -- ACCEPTED | DISPUTED | SETTLED
    message :: Maybe Text,
    settlementRefNo :: Maybe Text,
    updatedAt :: Text
  }
  deriving (Show, Generic)

-- | Parse BECKN on_recon request into domain types.
-- Called by the BAP when receiving on_recon from BPP.
buildOnReconDomainReq ::
  (MonadFlow m) =>
  ReconSpec.OnReconReq ->
  m DOnReconReq
buildOnReconDomainReq onReconReq = do
  let reconResponses = onReconReq.onReconReqMessage.onReconMessageOrderRecons
  let orderResults = map parseOrderReconResponse reconResponses
  pure DOnReconReq {orderReconResults = orderResults}

parseOrderReconResponse :: ReconSpec.OrderReconResponse -> DOrderReconResult
parseOrderReconResponse resp =
  DOrderReconResult
    { orderId = resp.orderReconResponseOrderId,
      status = resp.orderReconResponseStatus,
      message = resp.orderReconResponseMessage,
      settlementRefNo = resp.orderReconResponseSettlementRefNo,
      updatedAt = resp.orderReconResponseUpdatedAt
    }
