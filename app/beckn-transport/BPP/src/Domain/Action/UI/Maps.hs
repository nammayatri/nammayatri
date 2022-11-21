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
import Beckn.Types.Error (PersonError (PersonFieldNotPresent, PersonNotFound))
import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Types.Person as DP
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.Queries.Person as QP
import qualified Tools.Maps as Maps
import Tools.Metrics (CoreMetrics)

autoComplete :: (EncFlow m r, CacheFlow m r, EsqDBReplicaFlow m r, CoreMetrics m) => Id DP.Person -> Maps.AutoCompleteReq -> m Maps.AutoCompleteResp
autoComplete personId req = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  orgId <- person.merchantId & fromMaybeM (PersonFieldNotPresent "merchantId")
  Maps.autoComplete orgId req

getPlaceDetails :: (EncFlow m r, CacheFlow m r, EsqDBReplicaFlow m r, CoreMetrics m) => Id DP.Person -> Maps.GetPlaceDetailsReq -> m Maps.GetPlaceDetailsResp
getPlaceDetails personId req = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  orgId <- person.merchantId & fromMaybeM (PersonFieldNotPresent "merchantId")
  Maps.getPlaceDetails orgId req

getPlaceName :: (EncFlow m r, CacheFlow m r, EsqDBReplicaFlow m r, CoreMetrics m) => Id DP.Person -> Maps.GetPlaceNameReq -> m Maps.GetPlaceNameResp
getPlaceName personId req = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  orgId <- person.merchantId & fromMaybeM (PersonFieldNotPresent "merchantId")
  Maps.getPlaceName orgId req
