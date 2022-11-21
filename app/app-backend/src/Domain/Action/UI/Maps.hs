module Domain.Action.UI.Maps
  ( Maps.AutoCompleteReq,
    Maps.AutoCompleteResp,
    Maps.GetPlaceDetailsReq,
    Maps.GetPlaceDetailsResp,
    Maps.GetPlaceNameReq,
    Maps.GetPlaceNameResp,
    autoComplete,
    getPlaceDetails,
    getPlaceName,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Beckn.Types.Error (PersonError (PersonNotFound))
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Person as DP
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.Queries.Person as QP
import qualified Tools.Maps as Maps
import Tools.Metrics (CoreMetrics)

autoComplete :: (EncFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, CoreMetrics m) => Id DP.Person -> Maps.AutoCompleteReq -> m Maps.AutoCompleteResp
autoComplete personId req = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Maps.autoComplete person.merchantId req

getPlaceDetails :: (EncFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, CoreMetrics m) => Id DP.Person -> Maps.GetPlaceDetailsReq -> m Maps.GetPlaceDetailsResp
getPlaceDetails personId req = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Maps.getPlaceDetails person.merchantId req

getPlaceName :: (EncFlow m r, EsqDBReplicaFlow m r, CacheFlow m r, CoreMetrics m) => Id DP.Person -> Maps.GetPlaceNameReq -> m Maps.GetPlaceNameResp
getPlaceName personId req = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Maps.getPlaceName person.merchantId req
