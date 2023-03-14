{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

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

import qualified Domain.Types.Person as DP
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Error (PersonError (PersonNotFound))
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.Queries.Person as QP
import qualified Tools.Maps as Maps
import Tools.Metrics (CoreMetrics)

autoComplete ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text]
  ) =>
  Id DP.Person ->
  Maps.AutoCompleteReq ->
  m Maps.AutoCompleteResp
autoComplete personId req = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Maps.autoComplete person.merchantId req

getPlaceDetails ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text]
  ) =>
  Id DP.Person ->
  Maps.GetPlaceDetailsReq ->
  m Maps.GetPlaceDetailsResp
getPlaceDetails personId req = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Maps.getPlaceDetails person.merchantId req

getPlaceName ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["appPrefix" ::: Text]
  ) =>
  Id DP.Person ->
  Maps.GetPlaceNameReq ->
  m Maps.GetPlaceNameResp
getPlaceName personId req = do
  person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  Maps.getPlaceName person.merchantId req
