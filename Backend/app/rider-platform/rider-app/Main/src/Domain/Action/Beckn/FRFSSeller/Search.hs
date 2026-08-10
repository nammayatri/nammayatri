{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The work behind an inbound seller @search@.
--
-- Runs inside a fork: the buyer already has its @ack@, so nothing here is on the
-- request's critical path and nothing here may throw back to it.
module Domain.Action.Beckn.FRFSSeller.Search (handleSearch) where

import qualified Beckn.ACL.FRFSSeller.OnSearch as ACL
import qualified BecknV2.FRFS.Types as Spec
import qualified Domain.Types.Merchant as DM
import Environment (Flow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FRFSSeller.CallBAP as CallBAP
import qualified Storage.CachedQueries.Merchant as CQM

-- | Phase 1: answer every search with a fixed, valid catalog.
--
-- Deliberately makes no operator call. Its job is to prove the protocol path end to
-- end — signature in, ack, fork, signed callback out, buyer accepts. Phase 2 replaces
-- 'mkCatalog' with real station and fare resolution via the journey planner.
handleSearch :: Spec.SearchReq -> Flow ()
handleSearch req = do
  let ctx = req.searchReqContext
  bapUriText <-
    ctx.contextBapUri
      & fromMaybeM (InvalidRequest "BapUri missing on search context")
  bapUri <- parseBaseUrl bapUriText
  merchant <-
    CQM.findByShortId sellerMerchantShortId
      >>= fromMaybeM (MerchantNotFound sellerMerchantShortId.getShortId)
  let onSearchReq = ACL.buildOnSearchReq ctx mkCatalog
  CallBAP.sendOnSearch merchant.id bapUri onSearchReq
  where
    -- Phase 1 uses one hard-coded seller merchant. Phase 2 selects it per city from
    -- the inbound context's city code, once Kochi exists alongside Chennai.
    sellerMerchantShortId :: ShortId DM.Merchant
    sellerMerchantShortId = ShortId "FRFS_SELLER_CMRL"

    -- Phase 1 placeholder catalog: enough to be a valid on_search, no operator call.
    mkCatalog =
      ACL.SellerCatalog
        { providerId = "CMRL",
          providerName = "Chennai Metro Rail Limited",
          items = []
        }
