{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantMessage
  ( findByMerchantOpCityIdAndMessageKey,
    clearCache,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant.ConfigMapping (ConfigMapping)
import Domain.Types.Merchant.MerchantMessage
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Vehicle.Variant (Variant)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Common (Meters)
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.ConfigMapping as CMQ
import qualified Storage.Queries.Merchant.MerchantMessage as Queries

findByMerchantOpCityIdAndMessageKey :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> MessageKey -> Meters -> Maybe Variant -> m (Maybe MerchantMessage)
findByMerchantOpCityIdAndMessageKey id messageKey distance mbvt = do
  currTime <- getLocalCurrentTime 19800
  cmId <- CMQ.getConfigMapId id distance mbvt currTime "merchant_message" >>= fromMaybeM (InternalError $ "ConfigMapping not found for MerchantMessage : mocid, distance, mbvt, currTime" <> show id <> "," <> show distance <> ", " <> show mbvt <> ", " <> show currTime)
  Hedis.safeGet (makeConfigMapKey cmId messageKey) >>= \case
    Just a -> return . Just $ coerce @(MerchantMessageD 'Unsafe) @MerchantMessage a
    Nothing -> flip whenJust cacheMerchantMessage /=<< Queries.findByConfigMappingAndMessageKey cmId messageKey

cacheMerchantMessage :: CacheFlow m r => MerchantMessage -> m ()
cacheMerchantMessage merchantMessage = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeConfigMapKey merchantMessage.configMapId merchantMessage.messageKey
  Hedis.setExp idKey (coerce @MerchantMessage @(MerchantMessageD 'Unsafe) merchantMessage) expTime

makeMerchantOpCityIdAndMessageKey :: Id MerchantOperatingCity -> MessageKey -> Text
makeMerchantOpCityIdAndMessageKey id messageKey = "CachedQueries:MerchantMessage:MerchantOperatingCityId-" <> id.getId <> ":MessageKey-" <> show messageKey

makeConfigMapKey :: Id ConfigMapping -> MessageKey -> Text
makeConfigMapKey id mk = "driver-offer:CachedQueries:ConfigMapping:ConfigMapId-" <> id.getId <> ":LeaderBoardType-" <> show mk

--CQTODO: Handle clear cache ig, dashboard only, calls
-- Call it after any update
clearCache :: Hedis.HedisFlow m r => Id MerchantOperatingCity -> MessageKey -> m ()
clearCache merchantOpCityId messageKey = do
  Hedis.del (makeMerchantOpCityIdAndMessageKey merchantOpCityId messageKey)
