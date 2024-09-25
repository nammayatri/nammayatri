module Storage.CachedQueries.Driver.OnBoarding where

import Data.Text (pack)
import Domain.Types.Person (Person)
import qualified Kernel.External.Verification as KEV
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Id (Id)

getVerificationPriorityList :: CacheFlow m r => Id Person -> m (Maybe [KEV.VerificationService])
getVerificationPriorityList driverId = Hedis.withCrossAppRedis $ Hedis.safeGet (makeVerificationPriorityListKey driverId)

setVerificationPriorityList :: CacheFlow m r => Id Person -> [KEV.VerificationService] -> m ()
setVerificationPriorityList driverId priorityList = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeVerificationPriorityListKey driverId) priorityList expTime

makeVerificationPriorityListKey :: Id Person -> Text
makeVerificationPriorityListKey = pack . ("CachedQueries:RCVerificationPriorityList-driverId:" <>) . show
