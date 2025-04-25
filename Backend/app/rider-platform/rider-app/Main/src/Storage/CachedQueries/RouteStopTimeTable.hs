{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.RouteStopTimeTable
  ( findByRouteCodeAndStopCode,
  )
where

import Data.Text as Text
import Domain.Types.IntegratedBPPConfig
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.RouteStopTimeTable
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude as P
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.GraphqlQueries.RouteStopTimeTable as Queries

findByRouteCodeAndStopCode ::
  ( MonadFlow m,
    ServiceFlow m r
  ) =>
  Id IntegratedBPPConfig ->
  Id Merchant ->
  Id MerchantOperatingCity ->
  [Text] ->
  Text ->
  m [RouteStopTimeTable]
findByRouteCodeAndStopCode integratedBPPConfig merchantId merchantOpId routeCodes stopCode = do
  Hedis.safeGet (routeTimeTableKey routeCodes stopCode) >>= \case
    Just a -> pure a
    Nothing -> cacheRouteStopTimeInfo routeCodes stopCode /=<< Queries.findByRouteCodeAndStopCode integratedBPPConfig merchantId merchantOpId routeCodes stopCode

cacheRouteStopTimeInfo :: (CacheFlow m r, MonadFlow m) => [Text] -> Text -> [RouteStopTimeTable] -> m ()
cacheRouteStopTimeInfo routeCodes stopCode routeStopInfo = do
  let expTime = 60 * 60
  let idKey = routeTimeTableKey routeCodes stopCode
  Hedis.setExp idKey routeStopInfo expTime

routeTimeTableKey :: [Text] -> Text -> Text
routeTimeTableKey routeCodes stopCode = "routeStop-time-table:" <> Text.pack (P.unwords (Text.unpack <$> routeCodes)) <> ":" <> stopCode
