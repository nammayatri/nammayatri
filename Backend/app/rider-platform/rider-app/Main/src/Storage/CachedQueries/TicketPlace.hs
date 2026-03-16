module Storage.CachedQueries.TicketPlace
  ( getTicketPlaces,
    clearCacheByMerchantOperatingCityId,
  )
where

import Domain.Types.MerchantOperatingCity
import Domain.Types.TicketPlace
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.TicketPlace as Queries

getTicketPlaces :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id MerchantOperatingCity -> m [TicketPlace]
getTicketPlaces mocId =
  Hedis.safeGet (makeMocKey mocId) >>= \case
    Just a -> return a
    Nothing -> cacheResults mocId /=<< Queries.getTicketPlaces mocId

clearCacheByMerchantOperatingCityId :: (CacheFlow m r) => Id MerchantOperatingCity -> m ()
clearCacheByMerchantOperatingCityId mocId = Hedis.del (makeMocKey mocId)

cacheResults :: (CacheFlow m r) => Id MerchantOperatingCity -> [TicketPlace] -> m ()
cacheResults mocId places = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeMocKey mocId) places expTime

makeMocKey :: Id MerchantOperatingCity -> Text
makeMocKey mocId = "CachedQueries:TicketPlace:MocId-" <> mocId.getId
