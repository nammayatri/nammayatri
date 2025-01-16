module Storage.Queries.DriverGoHomeRequestExtra where

import Data.Time (UTCTime (..))
import qualified Domain.Types.DriverGoHomeRequest as DDGR
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id as Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import Storage.Beam.DriverGoHomeRequest as BeamDHR
import Storage.Queries.OrphanInstances.DriverGoHomeRequest ()

-- Extra code goes here --

todaySuccessCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> m Int
todaySuccessCount driverId = do
  now <- getCurrentTime
  length <$> findAllWithKV [Se.Is BeamDHR.driverId $ Se.Eq $ getId driverId, Se.Is BeamDHR.status $ Se.Eq DDGR.SUCCESS, Se.Is BeamDHR.createdAt $ Se.GreaterThanOrEq now {utctDayTime = 0}, Se.Is BeamDHR.createdAt $ Se.LessThanOrEq now {utctDayTime = 86400}]

findActive :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Log m) => Id Driver -> m (Maybe DDGR.DriverGoHomeRequest)
findActive (Id.Id driverId) = findAllWithOptionsKV [Se.And [Se.Is BeamDHR.driverId $ Se.Eq driverId, Se.Is BeamDHR.status $ Se.Eq DDGR.ACTIVE]] (Se.Desc BeamDHR.createdAt) (Just 1) Nothing <&> listToMaybe
