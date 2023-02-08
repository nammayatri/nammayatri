module Domain.Action.UI.Route
  ( Maps.GetRoutesReq,
    Maps.GetRoutesResp,
    getRoutes,
  )
where

import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.Queries.Person as QP
import Tools.Error
import qualified Tools.Maps as Maps
import Tools.Metrics (CoreMetrics)

getRoutes :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id DP.Person -> Maps.GetRoutesReq -> m Maps.GetRoutesResp
getRoutes personId req = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Maps.getRoutes person.merchantId req
