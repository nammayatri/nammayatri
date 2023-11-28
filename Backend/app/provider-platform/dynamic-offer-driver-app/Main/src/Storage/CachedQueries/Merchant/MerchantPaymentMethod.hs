{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Merchant.MerchantPaymentMethod
  ( findAllByMerchantOpCityId,
    findByIdAndMerchantOpCityId,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant.ConfigMapping (ConfigMapping)
import Domain.Types.Merchant.MerchantOperatingCity
import Domain.Types.Merchant.MerchantPaymentMethod
import Domain.Types.Vehicle.Variant (Variant)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.ConfigMapping as CMQ
import qualified Storage.Queries.Merchant.MerchantPaymentMethod as Queries

findAllByMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id MerchantOperatingCity -> Meters -> Maybe Variant -> m [MerchantPaymentMethod]
findAllByMerchantOpCityId id distance mbvt = do
  currTime <- getLocalCurrentTime 19800
  cmId <- CMQ.getConfigMapId id distance mbvt currTime "merchant_payment_method" >>= fromMaybeM (InternalError $ "ConfigMapping not found for MerchantPaymentMethod : mocid, distance, mbvt, currTime" <> show id <> "," <> show distance <> ", " <> show mbvt <> ", " <> show currTime)
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeConfigMapKey cmId) >>= \case
    Just a -> return $ fmap (coerce @(MerchantPaymentMethodD 'Unsafe) @MerchantPaymentMethod) a
    Nothing -> cacheMerchantPaymentMethods cmId /=<< Queries.findAllByConfigMapId cmId

findByIdAndMerchantOpCityId :: (CacheFlow m r, EsqDBFlow m r) => Id MerchantPaymentMethod -> Id MerchantOperatingCity -> Meters -> Maybe Variant -> m (Maybe MerchantPaymentMethod)
findByIdAndMerchantOpCityId id merchantOperatingCityId distance mbvt = find (\mpm -> mpm.id == id) <$> findAllByMerchantOpCityId merchantOperatingCityId distance mbvt

cacheMerchantPaymentMethods :: (CacheFlow m r) => Id ConfigMapping -> [MerchantPaymentMethod] -> m ()
cacheMerchantPaymentMethods cmId cfg = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let cmIdKey = makeConfigMapKey cmId
  Hedis.withCrossAppRedis $ Hedis.setExp cmIdKey (coerce @[MerchantPaymentMethod] @[MerchantPaymentMethodD 'Unsafe] cfg) expTime

makeMerchantIdKey :: Id MerchantOperatingCity -> Text
makeMerchantIdKey id = "driver-offer:CachedQueries:MerchantPaymentMethod:MerchantOpCityId-" <> id.getId

makeConfigMapKey :: Id ConfigMapping -> Text
makeConfigMapKey id = "driver-offer:CachedQueries:ConfigMapping:ConfigMapId-" <> id.getId
