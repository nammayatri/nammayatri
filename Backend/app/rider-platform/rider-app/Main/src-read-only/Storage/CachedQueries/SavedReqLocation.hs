{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.CachedQueries.SavedReqLocation (module Storage.CachedQueries.SavedReqLocation, module ReExport) where
import Kernel.Prelude
import Kernel.Utils.Common
import Storage.CachedQueries.SavedReqLocationExtra as ReExport
import qualified Domain.Types.SavedReqLocation
import qualified Storage.Queries.SavedReqLocation as Queries
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Kernel.External.Maps
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis



deleteSavedLocation :: CacheFlow m r => (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.External.Maps.LatLong -> m ())
deleteSavedLocation riderId latLong = do (Hedis.del $ makeIdKey   riderId   latLong)
findByLatLonAndRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                          (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.External.Maps.LatLong -> m (Kernel.Prelude.Maybe Domain.Types.SavedReqLocation.SavedReqLocation))
findByLatLonAndRiderId riderId latLong = do (Hedis.safeGet $ makeIdKey   riderId   latLong) >>= (\case
                                                                                                     Just a -> pure (Just a)
                                                                                                     Nothing -> flip whenJust (\dataToBeCached -> do {expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime);
                                                                                                                                                      Hedis.setExp (makeIdKey   riderId   latLong) dataToBeCached expTime}) /=<< Queries.findByLatLonAndRiderId   riderId   latLong)



