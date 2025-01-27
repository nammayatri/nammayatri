module Storage.CachedQueries.InterCityTravelCities where

import qualified Domain.Types.InterCityTravelCities
import qualified Domain.Types.Merchant
import Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.InterCityTravelCities as Queries

findByMerchantIdAndState ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.IndianState -> m [Domain.Types.InterCityTravelCities.InterCityTravelCities])
findByMerchantIdAndState merchantId state = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ "driverOfferCachedQueries:InterCityTravelCities:" <> ":MerchantId-" <> Kernel.Types.Id.getId merchantId <> ":State-" <> show state)
    >>= ( \case
            Just a -> pure a
            Nothing ->
              ( \dataToBeCached -> do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.withCrossAppRedis $ Hedis.setExp ("driverOfferCachedQueries:InterCityTravelCities:" <> ":MerchantId-" <> Kernel.Types.Id.getId merchantId <> ":State-" <> show state) dataToBeCached expTime
              )
                /=<< Queries.findByMerchantAndState merchantId state
        )

findInterCityAreasContainingGps :: forall m r. (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => LatLong -> m [Domain.Types.InterCityTravelCities.InterCityTravelCities]
findInterCityAreasContainingGps = Queries.findInterCityAreasContainingGps
