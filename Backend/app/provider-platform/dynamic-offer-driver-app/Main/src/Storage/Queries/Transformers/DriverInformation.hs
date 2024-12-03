module Storage.Queries.Transformers.DriverInformation where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Lib.Queries.SpecialLocation as SpecialLocation

getPreferredPrimarySpecialLoc :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Text -> m (Maybe SpecialLocation.SpecialLocationWarrior)
getPreferredPrimarySpecialLoc Nothing = return Nothing
getPreferredPrimarySpecialLoc (Just specialLocId) = do
  Hedis.safeGet (makeSpecialLocIdKey specialLocId) >>= \case
    Just a -> pure a
    Nothing -> do
      mbSpecialLoc <- SpecialLocation.findById (Id specialLocId)
      case mbSpecialLoc of
        Nothing -> return Nothing
        Just specialLoc -> do
          specialLocation <- SpecialLocation.specialLocToSpecialLocWarrior specialLoc
          expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
          let idKey = makeSpecialLocIdKey specialLocId
          Hedis.setExp idKey specialLocation expTime
          return $ Just specialLocation

makeSpecialLocIdKey :: Text -> Text
makeSpecialLocIdKey locId = "CachedQueries:SpecialLoc:Id:" <> locId
