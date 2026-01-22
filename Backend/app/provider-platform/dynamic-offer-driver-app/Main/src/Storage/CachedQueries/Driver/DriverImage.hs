module Storage.CachedQueries.Driver.DriverImage where

import Control.Monad
import Data.Text (unpack)
import Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.CacheFlow
import Kernel.Types.Id (Id, getId)
import Kernel.Types.MonadGuid (MonadGuid)
import Kernel.Utils.Common (MonadFlow)
import qualified Storage.Flow as Storage
import qualified Storage.Types as StorageTypes

driverAadhaarImageKey :: Id Person -> Text
driverAadhaarImageKey driverId = "Driver-Aadhaar-Image-Key:DriverId" <> getId driverId

getDriverImageByDriverId :: (CacheFlow m r, CoreMetrics m, MonadFlow m, MonadGuid m, HasField "storageConfig" r StorageTypes.StorageConfig) => Id Person -> Text -> m Text
getDriverImageByDriverId driverId s3Path =
  Hedis.safeGet (driverAadhaarImageKey driverId)
    >>= maybe (cacheDriverImageByDriverId driverId /=<< Storage.get (unpack s3Path)) pure

cacheDriverImageByDriverId :: (CacheFlow m r) => Id Person -> Text -> m ()
cacheDriverImageByDriverId driverId base64Image = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (driverAadhaarImageKey driverId) base64Image expTime
