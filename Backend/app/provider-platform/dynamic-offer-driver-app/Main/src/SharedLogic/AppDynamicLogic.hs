{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module SharedLogic.AppDynamicLogic where

import Domain.Types.MerchantOperatingCity
import Kernel.Prelude as KP
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.TimeBound
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Storage.CachedQueries.AppDynamicLogic as DAL
import Lib.Yudhishthira.Types.AppDynamicLogic
import Storage.Beam.Yudhishthira ()
import qualified Storage.Cac.TransporterConfig as CTC

getAppDynamicLogic ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id MerchantOperatingCity ->
  Text ->
  m [AppDynamicLogic]
getAppDynamicLogic merchantOpCityId domain = do
  configs <- DAL.findByMerchantOpCityAndDomain (cast merchantOpCityId) domain
  transporterConfig <- CTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc -- bounds, all these params, timeDiffFromUTC
  let boundedConfigs = findBoundedDomain (filter (\cfg -> cfg.timeBounds /= Unbounded) configs) localTime
  return $ if null boundedConfigs then filter (\cfg -> cfg.timeBounds == Unbounded) configs else boundedConfigs
