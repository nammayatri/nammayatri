module Storage.Queries.Transformers.DriverInformation where

import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Lib.Queries.SpecialLocation as SpecialLocation

getPreferredPrimarySpecialLoc :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Text -> m (Maybe SpecialLocation.SpecialLocationWarrior)
getPreferredPrimarySpecialLoc Nothing = return Nothing
getPreferredPrimarySpecialLoc (Just specialLocId) = do
  mbSpecialLoc <- SpecialLocation.findById (Id specialLocId)
  case mbSpecialLoc of
    Nothing -> return Nothing
    Just specialLoc -> do
      specialLocation <- SpecialLocation.specialLocToSpecialLocWarrior specialLoc
      return $ Just specialLocation
