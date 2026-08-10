{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Outbound seller callbacks.
--
-- A Beckn seller answers asynchronously: the inbound request already received its
-- @ack@, and the real answer travels back on a NEW signed request to the buyer's
-- @bap_uri@. That is why a seller needs a signing key at all.
module SharedLogic.FRFSSeller.CallBAP (sendOnSearch) where

import qualified BecknV2.FRFS.APIs as Spec
import qualified BecknV2.FRFS.Types as Spec
import qualified Domain.Types.Merchant as DM
import Environment (Flow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallFRFSBPP as CallBPP

-- | POST a signed @on_search@ back to the buyer app.
--
-- Runs inside a fork with no caller to return to, so delivery failure is logged
-- rather than thrown. A buyer that rejects or is unreachable must not take down the
-- request path that already acknowledged.
sendOnSearch :: Id DM.Merchant -> BaseUrl -> Spec.OnSearchReq -> Flow ()
sendOnSearch merchantId bapUri req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <-
    req.onSearchReqContext.contextBapId
      & fromMaybeM (InvalidRequest "BapId missing on on_search context")
  result <-
    try @_ @SomeException $
      CallBPP.callBecknAPIWithSignature'
        merchantId
        bapId
        "on_search"
        Spec.onSearchAPI
        bapUri
        internalEndPointHashMap
        req
  case result of
    Right _ -> logInfo $ "on_search delivered to " <> showBaseUrl bapUri
    Left err ->
      logError $
        "on_search delivery failed to " <> showBaseUrl bapUri <> ": " <> show err
