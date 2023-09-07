module Storage.CachedQueries.Driver.DriverImage where

import AWS.S3 as S3
import Control.Monad
import Data.Text (unpack)
import Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.CacheFlow
import Kernel.Types.Id (Id, getId)

driverAadhaarImageKey :: Id Person -> Text
driverAadhaarImageKey driverId = "Driver-Aadhaar-Image-Key:DriverId" <> getId driverId

getDriverImageByDriverId :: (CacheFlow m r, HasField "s3Env" r (S3.S3Env m)) => Id Person -> Text -> m Text
getDriverImageByDriverId driverId s3Path =
  Hedis.safeGet (driverAadhaarImageKey driverId)
    >>= maybe (cacheDriverImageByDriverId driverId /=<< S3.get (unpack s3Path)) pure

cacheDriverImageByDriverId :: (CacheFlow m r) => Id Person -> Text -> m ()
cacheDriverImageByDriverId driverId base64Image = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (driverAadhaarImageKey driverId) base64Image expTime
