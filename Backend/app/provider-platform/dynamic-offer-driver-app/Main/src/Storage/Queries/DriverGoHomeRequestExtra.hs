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
import qualified SharedLogic.DriverPool.LTSDataSync as LTSSync
import Storage.Beam.DriverGoHomeRequest as BeamDHR
import Storage.Queries.OrphanInstances.DriverGoHomeRequest ()

-- Extra code goes here --

-- | Wrapper for src-read-only finishWithStatus with LTS sync.
-- Takes driverId as extra parameter so we can sync to the driver's LTS key.
finishWithStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  DDGR.DriverGoHomeRequestStatus ->
  Maybe Bool ->
  Id DDGR.DriverGoHomeRequest ->
  Id Driver ->
  m ()
finishWithStatus status mbReachedHome goHomeReqId driverId = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set BeamDHR.status status, Se.Set BeamDHR.reachedHome mbReachedHome, Se.Set BeamDHR.updatedAt _now] [Se.Is BeamDHR.id $ Se.Eq (getId goHomeReqId)]
  LTSSync.syncDriverPoolDataToLTS driverId $
    LTSSync.emptyUpdate {LTSSync.goHomeStatus = LTSSync.Set (Just status)}

todaySuccessCount :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Driver -> m Int
todaySuccessCount driverId = do
  now <- getCurrentTime
  length <$> findAllWithKV [Se.Is BeamDHR.driverId $ Se.Eq $ getId driverId, Se.Is BeamDHR.status $ Se.Eq DDGR.SUCCESS, Se.Is BeamDHR.createdAt $ Se.GreaterThanOrEq now {utctDayTime = 0}, Se.Is BeamDHR.createdAt $ Se.LessThanOrEq now {utctDayTime = 86400}]

findActive :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Log m) => Id Driver -> m (Maybe DDGR.DriverGoHomeRequest)
findActive (Id.Id driverId) = findAllWithOptionsKV [Se.And [Se.Is BeamDHR.driverId $ Se.Eq driverId, Se.Is BeamDHR.status $ Se.Eq DDGR.ACTIVE]] (Se.Desc BeamDHR.createdAt) (Just 1) Nothing <&> listToMaybe
