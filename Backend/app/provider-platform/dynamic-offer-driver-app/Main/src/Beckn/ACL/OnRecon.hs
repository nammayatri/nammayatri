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
import qualified Data.UUID.V4 as UUIDv4
import qualified Domain.Types.BecknConfig as DBC
import Domain.Types.PPFRecon
import Kernel.Prelude
import qualified Kernel.Types.TimeRFC339 as TimeRFC339
import Kernel.Utils.Common

-- | Build a BECKN on_recon response from BPP-side recon processing results.
-- Called by the BPP (Receiver) to respond to BAP (Collector).
buildOnReconReq ::
  (MonadFlow m) =>
  DBC.BecknConfig ->
  Text -> -- BAP subscriber ID (from incoming recon)
  [(PPFRecon, Text)] -> -- (recon entry, status: ACCEPTED/DISPUTED/SETTLED)
  m ReconSpec.OnReconReq
buildOnReconReq bppConfig bapSubscriberId results = do
  now <- getCurrentTime
  messageId <- liftIO UUIDv4.nextRandom
  let responses = map buildResponse results
      context =
        Spec.Context
          { contextAction = Just "on_recon",
            contextBapId = Just bapSubscriberId,
            contextBapUri = Nothing,
            contextBppId = Just bppConfig.subscriberId,
            contextBppUri = Just $ showBaseUrl bppConfig.subscriberUrl,
            contextDomain = Just bppConfig.domain,
            contextKey = Nothing,
            contextLocation = Nothing,
            contextMessageId = Just messageId,
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
            { onReconMessageOrderRecons = responses
            }
      }

buildResponse :: (PPFRecon, Text) -> ReconSpec.OrderReconResponse
buildResponse (recon, status) =
  ReconSpec.OrderReconResponse
    { orderReconResponseOrderId = recon.networkOrderId,
      orderReconResponseStatus = status,
      orderReconResponseMessage = recon.message,
      orderReconResponseSettlementRefNo = recon.settlementRefNo,
      orderReconResponseUpdatedAt = show (TimeRFC339.UTCTimeRFC3339 recon.updatedAt)
    }
