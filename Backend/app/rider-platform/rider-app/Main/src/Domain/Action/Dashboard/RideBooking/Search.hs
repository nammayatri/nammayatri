module Domain.Action.Dashboard.RideBooking.Search (postSearchRide) where

import qualified API.UI.Search
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import SharedLogic.Merchant (findMerchantByShortId)

postSearchRide ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  API.UI.Search.SearchReq ->
  Environment.Flow API.UI.Search.SearchResp
postSearchRide merchantShortId _opCity personId req = do
  m <- findMerchantByShortId merchantShortId
  API.UI.Search.search' (personId, m.id) req Nothing Nothing Nothing Nothing Nothing Nothing (Just True) (Just False) (Just [])
