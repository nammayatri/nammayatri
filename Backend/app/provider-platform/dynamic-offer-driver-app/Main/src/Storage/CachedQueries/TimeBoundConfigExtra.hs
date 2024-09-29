{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.TimeBoundConfigExtra where

import qualified Domain.Types.MerchantOperatingCity as MOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Yudhishthira.Types
import qualified Storage.Queries.TimeBoundConfig as Queries

-- Extra code goes here --
delete :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id MOC.MerchantOperatingCity -> LogicDomain -> Text -> m ()
delete = Queries.delete

clearCache :: (CacheFlow m r, MonadFlow m, EsqDBFlow m r) => Id MOC.MerchantOperatingCity -> LogicDomain -> Text -> m ()
clearCache merchantOperatingCityId timeBoundDomain name = do
  Hedis.withCrossAppRedis $ Hedis.del ("driverOfferCachedQueries:TimeBoundConfig:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":TimeBoundDomain-" <> show timeBoundDomain)
  Hedis.withCrossAppRedis $ Hedis.del ("driverOfferCachedQueries:TimeBoundConfig:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":Name-" <> show name <> ":TimeBoundDomain-" <> show timeBoundDomain)
