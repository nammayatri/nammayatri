module Domain.Action.UI.Route
  ( Maps.GetRoutesReq,
    Maps.GetRoutesResp,
    getRoutes,
  )
where

import Beckn.Prelude
import Beckn.Types.Error (PersonError (PersonNotFound))
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Person as DP
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.Queries.Person as QP
import qualified Tools.Maps as Maps
import Tools.Metrics (CoreMetrics)

getRoutes :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id DP.Person -> Maps.GetRoutesReq -> m Maps.GetRoutesResp
getRoutes personId req = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Maps.getRoutes person.merchantId req
