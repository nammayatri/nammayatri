module Storage.CachedQueries.SearchRequestCache
  ( cacheSearchRequest,
  )
where

import Data.Text
import Domain.Types.Merchant as DMerchant
import Domain.Types.Person
import Domain.Types.SearchRequest
import Kernel.Prelude
import Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Storage.CachedQueries.CacheConfig

cacheSearchRequest :: (CacheFlow m r) => Id DMerchant.Merchant -> SearchRequest -> Person -> m ()
cacheSearchRequest merchantId searchRequest person = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeIdKey searchRequest person) merchantId expTime

makeIdKey :: SearchRequest -> Person -> Text
makeIdKey searchRequest person = "CachedQueries:SimulatedUser:SearchRequest:" <> person.id.getId <> searchRequest.id.getId
